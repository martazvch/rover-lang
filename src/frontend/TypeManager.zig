const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const BA = @import("builtins_analyzer.zig");
const BuiltinAnalyzer = BA.BuiltinAnalyzer;
const FnDeclaration = BA.FnDeclaration;
const builtin_init = BA.init;
const Interner = @import("../Interner.zig");
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;
const TypeInfo = TypeSys.TypeInfo;
const oom = @import("../utils.zig").oom;

allocator: Allocator,
declared: AutoHashMapUnmanaged(usize, Type) = .{},
type_infos: ArrayListUnmanaged(TypeInfo) = .{},
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
    self.declared.put(self.allocator, interner.intern("Self"), .self) catch oom();
}

pub fn deinit(self: *Self) void {
    self.declared.deinit(self.allocator);
    self.type_infos.deinit(self.allocator);
}

/// Adds information about a type. Requires the kind and extra info, the value (aka
/// index in information array) is computed in the function.
/// Returns the complete type
pub fn reserveInfo(self: *Self) !TypeSys.Value {
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

/// Adds a type linked associated with the name
pub fn addType(self: *Self, name: usize, typ: Type) void {
    self.declared.put(self.allocator, name, typ) catch oom();
}

/// Declares a new type built with `kind` and `extra` parameters and add the informations
pub fn declare(self: *Self, name: usize, kind: TypeSys.Kind, extra: TypeSys.Extra, info: TypeInfo) Self.Error!TypeSys.Type {
    const count = self.type_infos.items.len;

    if (count == std.math.maxInt(TypeSys.Value)) return error.too_many_types;

    const typ: Type = .create(kind, extra, @intCast(count));
    self.type_infos.append(self.allocator, info) catch oom();
    self.addType(name, typ);

    return typ;
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
