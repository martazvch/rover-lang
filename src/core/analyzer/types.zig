const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayMap = std.AutoArrayHashMapUnmanaged;
const Map = std.AutoHashMapUnmanaged;

const InternerIdx = Interner.Index;

const misc = @import("misc");
const Interner = misc.Interner;
const Set = misc.Set;
const oom = misc.oom;

pub const MapNameType = ArrayMap(InternerIdx, *const Type);

pub const Type = union(enum) {
    never,
    void,
    int,
    float,
    bool,
    str,
    null,
    array: Array,
    @"enum": Enum,
    function: Function,
    module: InternerIdx,
    optional: *const Type,
    structure: Structure,
    @"union": Union,

    pub const Array = struct {
        child: *const Type,

        pub fn depth(self: *const Array) usize {
            var res: usize = 1;
            var child = self.child;

            while (child.* == .array) : (res += 1) {
                child = child.array.child;
            }

            return res;
        }

        pub fn getChildAt(self: *const Array, at: usize) ?*const Type {
            var child = self.child;
            if (at == 0) return child;

            var dep: usize = 1;
            while (child.* == .array) : (dep += 1) {
                child = child.array.child;
                if (dep == at) return child;
            }

            return null;
        }

        pub fn depthAndChild(self: *const Array) struct { usize, *const Type } {
            var dep: usize = 1;
            var child = self.child;

            while (child.* == .array) : (dep += 1) {
                child = child.array.child;
            }

            return .{ dep, child };
        }
    };

    pub const Loc = struct { name: InternerIdx, container: InternerIdx };

    pub const Enum = struct {
        loc: ?Loc,
        tags: Tags,
        functions: MapNameType,

        pub const empty: Enum = .{ .loc = null, .tags = .empty };
        pub const Tags = MapNameType;

        pub const Proto = ArrayMap(InternerIdx, bool);

        pub fn proto(self: *const Enum, allocator: Allocator) Proto {
            var res: Proto = .empty;
            res.ensureTotalCapacity(allocator, self.tags.count()) catch oom();

            for (self.tags.keys()) |tag| {
                res.putAssumeCapacity(tag, false);
            }

            return res;
        }
    };

    pub const Function = struct {
        loc: ?Loc,
        params: ParamsMap,
        return_type: *const Type,
        kind: Kind,

        pub const Kind = enum { normal, method, bound, native, native_method };
        pub const Parameter = struct { type: *const Type, default: bool, captured: bool };
        pub const ParamsMap = ArrayMap(InternerIdx, Parameter);
        pub const Proto = ArrayMap(InternerIdx, struct { done: bool = false, default: bool = false });

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

                res.putAssumeCapacity(entry.key_ptr.*, .{ .default = entry.value_ptr.default });
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
        fields: FieldsMap,
        functions: MapNameType,

        pub const FieldsMap = ArrayMap(InternerIdx, Field);
        pub const Field = struct {
            type: *const Type,
            default: bool,
        };
        pub const Proto = ArrayMap(InternerIdx, struct { done: bool = false, default: bool = false });

        pub fn proto(self: *const Structure, allocator: Allocator) Proto {
            var res: Proto = .empty;
            res.ensureTotalCapacity(allocator, self.fields.count()) catch oom();

            var kv = self.fields.iterator();
            while (kv.next()) |entry| {
                res.putAssumeCapacity(entry.key_ptr.*, .{ .default = entry.value_ptr.default });
            }

            return res;
        }
    };

    pub const Union = struct {
        types: []const *const Type,

        pub const Proto = Map(*const Type, bool);

        pub fn proto(self: *const Union, allocator: Allocator) Proto {
            var res: Proto = .empty;
            // TODO: protect the cast?
            res.ensureTotalCapacity(allocator, @intCast(self.types.len)) catch oom();

            for (self.types) |ty| {
                res.putAssumeCapacity(ty, false);
            }

            return res;
        }

        /// Checks wether a type is contained in the union
        pub fn contains(self: *const Union, other: *const Type) bool {
            for (self.types) |ty| {
                if (other == ty) return true;
            }
            return false;
        }

        /// Checks if an union is a subset a the union
        pub fn containsSubset(self: *const Union, other: *const Union) bool {
            for (other.types) |sub| {
                if (!self.contains(sub)) return false;
            }
            return true;
        }
    };

    pub fn is(self: *const Type, tag: std.meta.Tag(Type)) bool {
        return std.meta.activeTag(self.*) == tag;
    }

    /// Returns the union tag if the type matches, otherwise null
    pub fn as(self: *const Type, comptime tag: std.meta.Tag(Type)) ?@FieldType(Type, @tagName(tag)) {
        return if (self.is(tag)) @field(self, @tagName(tag)) else null;
    }

    pub fn isNumeric(self: *const Type) bool {
        return self.is(.int) or self.is(.float);
    }

    pub fn isSymbol(self: *const Type) bool {
        return self.is(.function);
    }

    // TODO: values in unions can be heap...
    pub fn isHeap(self: *const Type) bool {
        return switch (self.*) {
            .void, .int, .float, .bool, .str, .null, .function, .optional, .@"union" => false,
            else => true,
        };
    }

    pub fn hash(self: Type, allocator: Allocator, hasher: anytype) void {
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
            .never, .void, .int, .float, .bool, .str, .null => {},
            .array => |ty| ty.child.hash(allocator, hasher),
            .@"enum" => |ty| {
                if (ty.loc) |loc| {
                    hasher.update(asBytes(&loc.name));
                    hasher.update(asBytes(&loc.container));
                } else {
                    for (ty.tags.values()) |tag| {
                        tag.hash(allocator, hasher);
                    }
                }
            },
            .function => |ty| {
                if (ty.loc) |loc| {
                    hasher.update(asBytes(&loc.name));
                    hasher.update(asBytes(&loc.container));
                } else {
                    for (ty.params.values()) |param| {
                        param.type.hash(allocator, hasher);
                    }
                    ty.return_type.hash(allocator, hasher);
                }
            },
            .module => |interned| hasher.update(asBytes(&interned)),
            .optional => |child| child.hash(allocator, hasher),
            .structure => |ty| {
                if (ty.loc) |loc| {
                    hasher.update(asBytes(&loc.name));
                    hasher.update(asBytes(&loc.container));
                } else {
                    for (ty.fields.values()) |f| {
                        f.type.hash(allocator, hasher);
                    }
                }
            },
            .@"union" => |u| {
                var set = Set(*const Type).fromSlice(allocator, u.types) catch oom();
                defer set.deinit(allocator);

                // We sort as we want equality between for ex: int|bool and bool|int
                set.sort(struct {
                    fn lessThan(_: void, a: *const Type, b: *const Type) bool {
                        return @intFromPtr(a) < @intFromPtr(b);
                    }
                }.lessThan);

                for (set.values()) |ty| ty.hash(allocator, hasher);
            },
        }
    }

    pub fn toString(self: *const Type, allocator: Allocator, interner: *const Interner, mod_name: InternerIdx) []const u8 {
        var res: std.ArrayList(u8) = .empty;
        var writer = res.writer(allocator);

        switch (self.*) {
            .never, .int, .float, .bool, .str, .null, .void => return @tagName(self.*),
            .array => |ty| {
                writer.writeAll("[]") catch oom();
                writer.writeAll(ty.child.toString(allocator, interner, mod_name)) catch oom();
            },
            .@"enum" => |ty| {
                if (ty.loc) |loc| {
                    // If symbol is defnined in current mod/file, don't repeat the module
                    if (loc.container == mod_name) {
                        writer.print("{s}", .{interner.getKey(loc.name).?}) catch oom();
                    } else {
                        writer.print("{s}.{s}", .{ interner.getKey(loc.container).?, interner.getKey(loc.name).? }) catch oom();
                    }
                } else {
                    writer.writeAll("enum {") catch oom();

                    for (ty.tags.keys(), ty.tags.values(), 0..) |k, v, i| {
                        writer.print("{s}: {s}{s}", .{
                            interner.getKey(k).?,
                            v.toString(allocator, interner, mod_name),
                            if (i < ty.tags.count() - 1) ", " else "",
                        }) catch oom();
                    }
                    writer.writeAll("}") catch oom();
                }
            },
            .function => |ty| {
                writer.writeAll("fn(") catch oom();
                for (ty.params.values(), 0..) |p, i| {
                    writer.writeAll(p.type.toString(allocator, interner, mod_name)) catch oom();
                    if (i != ty.params.count() - 1) writer.writeAll(", ") catch oom();
                }
                writer.writeAll(") -> ") catch oom();
                writer.writeAll(ty.return_type.toString(allocator, interner, mod_name)) catch oom();
            },
            .module => |interned| {
                const name = interner.getKey(interned).?;
                writer.print("module: {s}", .{name}) catch oom();
            },
            .optional => |opt| {
                writer.print("?{s}", .{opt.toString(allocator, interner, mod_name)}) catch oom();
            },
            .structure => |ty| {
                if (ty.loc) |loc| {
                    // If symbol is defnined in current mod/file, don't repeat the module
                    if (loc.container == mod_name) {
                        writer.print("{s}", .{interner.getKey(loc.name).?}) catch oom();
                    } else {
                        writer.print("{s}.{s}", .{ interner.getKey(loc.container).?, interner.getKey(loc.name).? }) catch oom();
                    }
                } else {
                    writer.writeAll("struct {") catch oom();

                    for (ty.fields.keys(), ty.fields.values(), 0..) |k, v, i| {
                        writer.print("{s}: {s}{s}", .{
                            interner.getKey(k).?,
                            v.type.toString(allocator, interner, mod_name),
                            if (i < ty.fields.count() - 1) ", " else "",
                        }) catch oom();
                    }
                    writer.writeAll("}") catch oom();
                }
            },
            .@"union" => |u| {
                for (u.types, 0..) |ty, i| {
                    writer.writeAll(ty.toString(allocator, interner, mod_name)) catch oom();
                    if (i < u.types.len - 1) writer.writeAll("|") catch oom();
                }
            },
        }

        return res.toOwnedSlice(allocator) catch oom();
    }
};

pub const TypeId = u16;
pub const TypeIds = Set(*const Type);

pub const TypeInterner = struct {
    arena: std.heap.ArenaAllocator,
    interned: Map(u64, *Type),
    ids: Set(*const Type),
    cache: Cache,

    const CacheList: []const Type = &.{ .float, .int, .bool, .str, .null, .void, .never };
    pub const Cache = CreateCache(CacheList);

    pub fn init(allocator: Allocator) TypeInterner {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .interned = .empty,
            .ids = .empty,
            .cache = undefined,
        };
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

    pub fn getCached(self: *const TypeInterner, comptime ty: Type) *const Type {
        if (!@hasField(Cache, @tagName(ty))) {
            @compileError("Trying to get a non-cached type in interner");
        }

        return @field(self.cache, @tagName(ty));
    }

    pub fn intern(self: *TypeInterner, ty: Type) *Type {
        var hasher = std.hash.Wyhash.init(0);
        ty.hash(self.arena.allocator(), &hasher);
        const hash = hasher.final();

        if (self.interned.get(hash)) |interned| {
            return interned;
        }

        const new_type = self.arena.allocator().create(Type) catch oom();
        new_type.* = ty;
        self.interned.put(self.arena.allocator(), hash, new_type) catch oom();

        self.ids.add(self.arena.allocator(), new_type) catch oom();

        return new_type;
    }

    /// Gets type's id
    // TODO: protect cast
    pub fn typeId(self: *const TypeInterner, ty: *const Type) TypeId {
        // Safe unwrap because in `intern` we add every new type in `ids`
        return @intCast(self.ids.getIndex(ty).?);
    }
};

test "inline union" {
    const expect = std.testing.expect;

    var ti: TypeInterner = .init(std.testing.allocator);
    defer ti.deinit();
    ti.cacheFrequentTypes();

    const union1 = &[_]*const Type{ ti.intern(.int), ti.intern(.bool) };
    const union2 = &[_]*const Type{ ti.intern(.bool), ti.intern(.int) };

    try expect(ti.intern(.{ .@"union" = .{ .types = union1 } }) == ti.intern(.{ .@"union" = .{ .types = union1 } }));
    try expect(ti.intern(.{ .@"union" = .{ .types = union2 } }) == ti.intern(.{ .@"union" = .{ .types = union2 } }));

    try expect(ti.intern(.{ .@"union" = .{ .types = union1 } }) == ti.intern(.{ .@"union" = .{ .types = union2 } }));
    try expect(ti.intern(.{ .@"union" = .{ .types = union2 } }) == ti.intern(.{ .@"union" = .{ .types = union1 } }));
}
