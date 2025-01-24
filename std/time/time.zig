const std = @import("std");
const Value = @import("../../src/runtime/values.zig").Value;
const TypeSys = @import("../../src/frontend/type_system.zig");
const NativeFnMeta = @import("../meta.zig").NativeFnMeta;

// Clock
pub const NativeClock = NativeFnMeta{
    .params = .{},
    .return_type = TypeSys.Float,
    .function = clock_native,
};

pub fn clock_native(_: []const Value) Value {
    return .{ .Float = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0 };
}

// Test
pub const NativeTest = NativeFnMeta{
    .params = .{ TypeSys.Int, TypeSys.Float },
    .return_type = TypeSys.Bool,
    .function = test_native,
};

pub fn test_native(values: []const Value) Value {
    std.debug.print(values[0].Int.print);
    std.debug.print(values[1].Float.print);

    return Value.bool_(true);
}
