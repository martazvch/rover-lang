const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const LexicalScope = @import("LexicalScope.zig");
const ModuleInterner = @import("../ModuleInterner.zig");
const oom = @import("../utils.zig").oom;

pub const Type = union(enum) {
    void,
    int,
    float,
    bool,
    str,
    null,
    array: Array,
    function: Function,
    module: InternerIdx,
    structure: Structure,

    pub const Array = struct {
        child: *const Type,

        pub fn getChild(self: *const Array) *const Type {
            var child = self.child;

            while (child.* == .array) {
                child = child.array.child;
            }

            return child;
        }
    };

    pub const Loc = struct { name: InternerIdx, container: InternerIdx };

    pub const Function = struct {
        loc: ?Loc,
        params: AutoArrayHashMapUnmanaged(InternerIdx, Parameter),
        return_type: *const Type,
        kind: Kind,

        pub const Kind = enum { normal, method, bound };
        pub const Parameter = struct { type: *const Type, default: bool, captured: bool };
        pub const Proto = AutoArrayHashMapUnmanaged(InternerIdx, bool);

        pub fn proto(self: *const Function, allocator: Allocator) Proto {
            var res: Proto = .empty;
            res.ensureTotalCapacity(allocator, self.params.count()) catch oom();

            var first = true;
            var kv = self.params.iterator();
            while (kv.next()) |entry| {
                if (first and self.kind == .method) {
                    first = false;
                    continue;
                }

                res.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.default);
            }

            return res;
        }

        pub fn toBoundMethod(self: *const Function, allocator: Allocator) Function {
            var params = self.params.clone(allocator) catch oom();
            params.orderedRemoveAt(0);

            for (params.values()) |*val| {
                val.default = false;
                val.captured = false;
            }

            return .{ .loc = null, .params = params, .return_type = self.return_type, .kind = .bound };
        }

        pub fn toAnon(self: *const Function, allocator: Allocator) Function {
            var params = self.params.clone(allocator) catch oom();

            for (params.values()) |*val| {
                val.default = false;
                val.captured = false;
            }

            return .{ .loc = null, .params = params, .return_type = self.return_type, .kind = .normal };
        }
    };

    pub const Structure = struct {
        loc: ?Loc,
        functions: AutoArrayHashMapUnmanaged(InternerIdx, *const Type),
        fields: AutoArrayHashMapUnmanaged(InternerIdx, Field),
        defaults: usize,

        pub const Field = struct {
            type: *const Type,
            default: bool,
        };

        pub fn proto(self: *const Structure, allocator: Allocator) AutoArrayHashMapUnmanaged(InternerIdx, bool) {
            var res: AutoArrayHashMapUnmanaged(InternerIdx, bool) = .empty;
            res.ensureTotalCapacity(allocator, self.fields.count()) catch oom();

            var kv = self.fields.iterator();
            while (kv.next()) |entry| {
                res.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.default);
            }

            return res;
        }
    };

    pub fn is(self: *const Type, tag: std.meta.Tag(Type)) bool {
        return std.meta.activeTag(self.*) == tag;
    }

    pub fn isNumeric(self: *const Type) bool {
        return self.is(.int) or self.is(.float);
    }

    pub fn isSymbol(self: *const Type) bool {
        return self.is(.function);
    }

    pub fn isHeap(self: *const Type) bool {
        return switch (self.*) {
            .void, .int, .float, .bool, .str, .null, .function => false,
            else => true,
        };
    }

    pub fn canCastTo(self: *const Type, other: *const Type) bool {
        return self.is(.int) and other.is(.float);
    }

    pub fn hash(self: Type, hasher: anytype) void {
        const asBytes = std.mem.asBytes;

        comptime {
            if (@typeInfo(@TypeOf(hasher)) != .pointer) {
                @compileError("You must pass a pointer to a haser");
            }

            if (!@hasDecl(@TypeOf(hasher.*), "update")) {
                @compileError("Hasher must have an 'update' method");
            }
        }

        hasher.update(asBytes(&@intFromEnum(self)));

        switch (self) {
            .void, .int, .float, .bool, .str, .null => {},
            .array => |ty| ty.child.hash(hasher),
            .function => |ty| {
                if (ty.loc) |loc| {
                    hasher.update(asBytes(&loc.name));
                    hasher.update(asBytes(&loc.container));
                } else {
                    for (ty.params.values()) |param| {
                        param.type.hash(hasher);
                    }
                    ty.return_type.hash(hasher);
                }
            },
            .module => |interned| hasher.update(asBytes(&interned)),
            .structure => |ty| {
                if (ty.loc) |loc| {
                    hasher.update(asBytes(&loc.name));
                    hasher.update(asBytes(&loc.container));
                } else {
                    for (ty.fields.values()) |f| {
                        f.type.hash(hasher);
                    }
                }
            },
        }
    }

    pub fn toString(
        self: *const Type,
        allocator: Allocator,
        scope: *const LexicalScope,
        current_mod: usize,
        interner: *const Interner,
        module_interner: *const ModuleInterner,
    ) []const u8 {
        var res: std.ArrayList(u8) = .empty;
        var writer = res.writer(allocator);

        switch (self.*) {
            .int, .float, .bool, .str, .null, .void => return @tagName(self.*),
            .array => |ty| {
                writer.writeAll("[]") catch oom();
                writer.writeAll(ty.child.toString(allocator, scope, current_mod, interner, module_interner)) catch oom();
            },
            .function => |ty| {
                writer.writeAll("fn(") catch oom();
                for (ty.params.values(), 0..) |p, i| {
                    writer.writeAll(p.type.toString(allocator, scope, current_mod, interner, module_interner)) catch oom();
                    if (i != ty.params.count() - 1) writer.writeAll(", ") catch oom();
                }
                writer.writeAll(") -> ") catch oom();
                writer.writeAll(ty.return_type.toString(allocator, scope, current_mod, interner, module_interner)) catch oom();
            },
            .module => |interned| {
                const name = interner.getKey(interned).?;
                writer.print("module: {s}", .{name}) catch oom();
            },
            .structure => |ty| {
                if (ty.loc) |loc| {
                    writer.print("{s}.{s}", .{ interner.getKey(loc.container).?, interner.getKey(loc.name).? }) catch oom();
                } else {
                    writer.writeAll("struct {") catch oom();

                    for (ty.fields.keys(), ty.fields.values(), 0..) |k, v, i| {
                        writer.print("{s}: {s}{s}", .{
                            interner.getKey(k).?,
                            v.type.toString(allocator, scope, current_mod, interner, module_interner),
                            if (i < ty.fields.count() - 1) ", " else "",
                        }) catch oom();
                    }
                    writer.writeAll("}") catch oom();
                }
            },
        }

        return res.toOwnedSlice(allocator) catch oom();
    }
};

pub const TypeInterner = struct {
    arena: std.heap.ArenaAllocator,
    interned: AutoHashMapUnmanaged(u64, *Type) = .{},
    cache: Cache,

    pub const Cache = CreateCache(&.{ .int, .float, .bool, .str, .null, .void });

    pub fn init(allocator: Allocator) TypeInterner {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator), .cache = undefined };
    }

    pub fn deinit(self: *TypeInterner) void {
        self.arena.deinit();
    }

    pub fn CreateCache(comptime types: []const Type) type {
        var fields: []const std.builtin.Type.StructField = &.{};

        inline for (types) |ty| {
            fields = fields ++ .{std.builtin.Type.StructField{
                .name = @tagName(ty),
                .type = *const Type,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = 1,
            }};
        }

        return @Type(.{ .@"struct" = .{
            .layout = .auto,
            .fields = fields,
            .decls = &.{},
            .is_tuple = false,
        } });
    }

    pub fn cacheFrequentTypes(self: *TypeInterner) void {
        const tags = std.meta.fields(Type);
        inline for (@typeInfo(Cache).@"struct".fields) |f| {
            inline for (tags) |tag| {
                if (comptime std.mem.eql(u8, f.name, tag.name)) {
                    @field(self.cache, f.name) = self.intern(@unionInit(Type, tag.name, {}));
                }
            }
        }
    }

    // TODO: use getOrPut
    pub fn intern(self: *TypeInterner, ty: Type) *Type {
        var hasher = std.hash.Wyhash.init(0);
        ty.hash(&hasher);
        const hash = hasher.final();

        if (self.interned.get(hash)) |interned| {
            return interned;
        }

        const new_type = self.arena.allocator().create(Type) catch oom();
        new_type.* = ty;
        self.interned.put(self.arena.allocator(), hash, new_type) catch oom();

        return new_type;
    }
};
