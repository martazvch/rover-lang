const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const LexicalScope = @import("LexicalScope.zig");
const oom = @import("../utils.zig").oom;

pub const Type = union(enum) {
    void,
    int,
    float,
    bool,
    str,
    null,
    array: *const Type,
    function: Function,
    module: InternerIdx,
    structure: Structure,

    pub const Function = struct {
        params: AutoArrayHashMapUnmanaged(InternerIdx, Parameter) = .{},
        return_type: *const Type,
        is_method: bool,

        pub fn proto(self: *const Function, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
            var res: AutoArrayHashMapUnmanaged(usize, bool) = .{};
            res.ensureTotalCapacity(allocator, self.params.count()) catch oom();

            var kv = self.params.iterator();
            while (kv.next()) |entry| {
                std.log.info("putting entry: {}", .{entry.value_ptr.default});
                res.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.default);
            }

            return res;
        }

        pub fn toBoundMethod(self: *const Function, self_interned: InternerIdx, allocator: Allocator) Function {
            var params = self.params.clone(allocator) catch oom();
            _ = params.orderedRemove(self_interned);

            std.log.info("To bound?", .{});
            std.log.info("Params after bound: {any}", .{params.values()});
            return .{
                .params = params,
                .return_type = self.return_type,
                .is_method = false,
            };
        }
    };

    pub const Parameter = struct {
        type: *const Type,
        default: bool = false,
        captured: bool = false,
    };

    pub const Structure = struct {
        name: InternerIdx,
        functions: AutoArrayHashMapUnmanaged(usize, Field) = .{},
        fields: AutoArrayHashMapUnmanaged(usize, Field) = .{},
        default_value_fields: usize = 0,

        pub const Field = struct {
            /// Field's type
            type: *const Type,
            /// Has a default value
            default: bool = false,
        };

        pub fn proto(self: *const Structure, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
            var res: AutoArrayHashMapUnmanaged(usize, bool) = .{};
            res.ensureTotalCapacity(allocator, self.fields.count() - self.default_value_fields) catch oom();

            var kv = self.fields.iterator();
            while (kv.next()) |entry| {
                if (!entry.value_ptr.default) {
                    res.putAssumeCapacity(entry.key_ptr.*, false);
                }
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

    // TODO: maybe name + kind + scope index is enough?
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

        // TODO: we could use only name but relative to the base of the project to avoid
        // name collision between modules
        switch (self) {
            .void, .int, .float, .bool, .str, .null => {},
            .array => |ty| ty.hash(hasher),
            .function => |ty| {
                for (ty.params.values()) |param| {
                    param.type.hash(hasher);
                    // TODO: modify that, we can't share the default values, we have to intern type in another way
                    // cf. previous todo
                    // hasher.update(asBytes(&@intFromBool(param.default)));
                }
                ty.return_type.hash(hasher);
            },
            .module => |interned| hasher.update(asBytes(&interned)),
            .structure => |ty| {
                hasher.update(asBytes(&ty.name));
                for (ty.fields.values()) |f| {
                    f.type.hash(hasher);
                }
                // TODO: I think there is an infinite loop because first param is a struct
                // and interning the struct interns the method, ...

                // for (ty.functions.values()) |f| {
                //     f.type.hash(hasher);
                // }
            },
        }
    }

    pub fn toString(self: *const Type, allocator: Allocator, interner: *const Interner) []const u8 {
        var res: std.ArrayListUnmanaged(u8) = .{};
        var writer = res.writer(allocator);

        switch (self.*) {
            .int, .float, .bool, .str, .null, .void => return @tagName(self.*),
            .array => |ty| {
                writer.writeAll("[]") catch oom();
                writer.writeAll(ty.toString(allocator, interner)) catch oom();
            },
            .function => |ty| {
                writer.writeAll("fn (") catch oom();
                for (ty.params.values(), 0..) |p, i| {
                    writer.writeAll(p.type.toString(allocator, interner)) catch oom();
                    if (i != ty.params.count() - 1) writer.writeAll(", ") catch oom();
                }
                writer.writeAll(") -> ") catch oom();
                writer.writeAll(ty.return_type.toString(allocator, interner)) catch oom();
            },
            .module => |interned| {
                const name = interner.getKey(interned).?;
                writer.print("module: {s}", .{name}) catch oom();
            },
            .structure => |ty| return interner.getKey(ty.name).?,
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
