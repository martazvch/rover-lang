const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const oom = @import("../utils.zig").oom;
const BA = @import("builtins_analyzer.zig");
const BuiltinAnalyzer = BA.BuiltinAnalyzer;
const FnDeclaration = BA.FnDeclaration;
const builtin_init = BA.init;
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;

allocator: Allocator,
declared: AutoHashMapUnmanaged(usize, Type) = .{},
type_infos: ArrayListUnmanaged(TypeInfo) = .{},
array_cache: AutoHashMapUnmanaged(Type, u32) = .{},
natives: BuiltinAnalyzer = builtin_init(),

const Self = @This();
pub const Error = error{too_many_types};

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator };
}

pub fn init_builtins(self: *Self, interner: *Interner) void {
    self.declared.put(self.allocator, interner.intern("void"), .void) catch oom();
    self.declared.put(self.allocator, interner.intern("null"), .null) catch oom();
    self.declared.put(self.allocator, interner.intern("bool"), .bool) catch oom();
    self.declared.put(self.allocator, interner.intern("float"), .float) catch oom();
    self.declared.put(self.allocator, interner.intern("int"), .int) catch oom();
    self.declared.put(self.allocator, interner.intern("str"), .str) catch oom();
}

pub fn deinit(self: *Self) void {
    self.declared.deinit(self.allocator);
    self.type_infos.deinit(self.allocator);
    self.array_cache.deinit(self.allocator);
}

/// Adds information about a type. Requires the kind and extra info, the value (aka
/// index in information array) is computed in the function.
/// Returns the complete type
pub fn reserveInfo(self: *Self) Error!TypeSys.Value {
    const count = self.type_infos.items.len;
    self.type_infos.append(self.allocator, undefined) catch oom();

    return if (count == std.math.maxInt(TypeSys.Value))
        error.too_many_types
    else
        @intCast(count);
}

/// Set type information at a specific index in list (index gave by *reserveInfo* method)
pub fn setInfo(self: *Self, index: usize, info: TypeInfo) void {
    self.type_infos.items[index] = info;
}

pub fn createInfo(self: *Self, info: TypeInfo) Error!void {
    const index = try self.reserveInfo();
    self.setInfo(index, info);
}

/// Adds a type linked associated with the name
pub fn addType(self: *Self, name: usize, typ: Type) void {
    self.declared.put(self.allocator, name, typ) catch oom();
}

/// Get type informations of a function
pub fn getFnTypeInfos(self: *const Self, index: usize) FnInfo {
    return self.type_infos.items[index].func;
}

/// Get type informations of a structure
pub fn getStructTypeInfos(self: *const Self, index: usize) StructInfo {
    return self.type_infos.items[index].@"struct";
}

/// If type is a function, returns the same type but without any default values and without self
/// Used when assigning a function to a variable, we an't keep reference to default values and
/// if it's a method, it's transformed into a bounded method, so no `self` anymore
pub fn createBoundedFnType(self: *Self, allocator: Allocator, typ: Type) !Type {
    if (!typ.is(.function)) return typ;

    const infos = self.getFnTypeInfos(typ.getValue());
    const new_infos = infos.toBounded(allocator);
    const index = try self.reserveInfo();
    self.setInfo(index, .{ .func = new_infos });

    return Type.create(.function, index);
}

/// If an array of `child` type as already been declared, return a type with the
/// index as `Value`, otherwise create it
pub fn getOrCreateArray(self: *Self, child: Type) Error!Type {
    if (self.array_cache.get(child)) |cached_index| {
        return Type.create(.array, @intCast(cached_index));
    }

    const index = try self.reserveInfo();
    const info = TypeInfo{ .array = .{ .child = child } };
    self.setInfo(index, info);
    self.array_cache.put(self.allocator, child, index) catch oom();

    return Type.create(.array, index);
}

pub fn getArrayDimAndChildType(self: *const Self, array: Type) struct { usize, Type } {
    var dim: usize = 1;
    var child = self.type_infos.items[array.getValue()].array.child;

    while (child.is(.array)) : (dim += 1) {
        child = self.type_infos.items[child.getValue()].array.child;
    }

    return .{ dim, child };
}

/// Checks if the type has already been declared
pub fn isDeclared(self: *const Self, typ: usize) bool {
    return self.declared.get(typ) != null;
}

/// Use natives function whose informations are gathered at compile time. Import the
/// informations among other declared types
pub fn importNative(self: *Self, name: []const u8) ?std.StaticStringMap(FnDeclaration) {
    return self.natives.declarations.get(name);
}

/// Used only in error mode, no need for performance. If used in performance path
pub fn idx(self: *const Self, typ: Type) usize {
    var iter = self.declared.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.* == typ) {
            return entry.key_ptr.*;
        }
    }
    unreachable;
}

// Custom types
pub const TypeInfo = union(enum) {
    array: ArrayInfo,
    func: FnInfo,
    @"struct": StructInfo,

    /// Sets the module reference if it has been imported
    pub fn setModule(self: *TypeInfo, module_index: usize, type_index: usize) void {
        switch (self.*) {
            inline else => |*t| t.module = .{
                .import_index = module_index,
                .type_index = type_index,
            },
        }
    }
};

pub const ArrayInfo = struct {
    child: Type,
    module: ?ModuleRef = null,
};

pub const FnInfo = struct {
    params: AutoArrayHashMapUnmanaged(usize, ParamInfo),
    return_type: Type,
    kind: Kind,
    module: ?ModuleRef = null,

    pub const Kind = enum { function, method, anonymus };

    pub const ParamInfo = struct {
        /// Order of declaration
        index: usize,
        /// Field's type
        type: Type,
        /// Has a default value
        default: bool = false,
    };

    pub fn proto(self: *const FnInfo, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
        var res = AutoArrayHashMapUnmanaged(usize, bool){};
        res.ensureTotalCapacity(allocator, self.params.count()) catch oom();

        var kv = self.params.iterator();
        while (kv.next()) |entry| {
            res.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.default);
        }

        return res;
    }

    pub fn toBounded(self: *const FnInfo, allocator: Allocator) FnInfo {
        const method_offset = @intFromBool(self.kind == .method);
        var params = AutoArrayHashMapUnmanaged(usize, ParamInfo){};
        params.ensureTotalCapacity(allocator, self.params.count() - method_offset) catch oom();

        for (self.params.keys()[method_offset..], self.params.values()[method_offset..]) |k, v| {
            var p = v;
            p.default = false;
            params.putAssumeCapacity(k, p);
        }

        return .{
            .params = params,
            .return_type = self.return_type,
            .kind = .function,
            .module = self.module,
        };
    }
};

pub const StructInfo = struct {
    functions: AutoHashMapUnmanaged(usize, MemberInfo),
    fields: AutoArrayHashMapUnmanaged(usize, MemberInfo),
    default_value_fields: usize,
    module: ?ModuleRef = null,

    pub const MemberInfo = struct {
        /// Order of declaration
        index: usize,
        /// Field's type
        type: Type,
        /// Has a default value
        default: bool = false,
    };

    pub fn proto(self: *const StructInfo, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
        var res = AutoArrayHashMapUnmanaged(usize, bool){};
        res.ensureTotalCapacity(allocator, self.fields.count()) catch oom();

        var kv = self.fields.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.default) {
                res.putAssumeCapacity(entry.key_ptr.*, false);
            }
        }

        return res;
    }
};

pub const ModuleRef = struct {
    /// Modules from which it has been imported
    import_index: usize = 0,
    /// Index in type manager
    type_index: usize = 0,
};

pub const Symbols = AutoArrayHashMapUnmanaged(usize, StructInfo.MemberInfo);
