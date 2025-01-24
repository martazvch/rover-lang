const Value = @import("../../src/runtime/values.zig").Value;
const TypeSys = @import("../src/frontend/type_system.zig");
const Type = TypeSys.Type;

pub const NativeFnMeta = struct {
    params: []const Type,
    return_type: Type,
    function: *const fn ([]const Value) Value,
};
