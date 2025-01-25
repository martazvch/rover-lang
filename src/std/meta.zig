const Value = @import("../runtime/values.zig").Value;
const TypeSys = @import("../frontend/type_system.zig");
const Type = TypeSys.Type;

pub const NativeFn = *const fn ([]const Value) Value;

pub const NativeFnMeta = struct {
    params: []const Type = &.{},
    return_type: Type,
    function: NativeFn,
};

pub const ModuleMeta = struct {
    name: []const u8,
};
