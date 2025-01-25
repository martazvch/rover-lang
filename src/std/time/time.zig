const std = @import("std");
const Value = @import("../../runtime/values.zig").Value;
const TypeSys = @import("../../frontend/type_system.zig");
const Meta = @import("../meta.zig");
const NativeFnMeta = Meta.NativeFnMeta;
const NativeFn = Meta.NativeFn;

// pub fn Gen(
//     comptime params: []const TypeSys.Type,
//     comptime return_type: TypeSys.Type,
//     comptime function: *const fn ([]const Value) Value,
// ) NativeFnMeta {
//     return .{
//         .params = params,
//         .return_type = return_type,
//         .function = function,
//     };
// }

// Clock
pub const clock = NativeFnMeta{
    .params = &.{},
    .return_type = TypeSys.Float,
    .function = _clock,
};

// pub const NativeClock = Gen(&.{}, TypeSys.Float, clock_native);

pub fn _clock(_: []const Value) Value {
    return .{ .Float = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0 };
}

// Test
pub const pprint = NativeFnMeta{
    .params = &.{ TypeSys.Int, TypeSys.Float },
    .return_type = TypeSys.Bool,
    .function = _print,
};

// pub const NativeTest = Gen(&.{ TypeSys.Int, TypeSys.Float }, TypeSys.Bool, test_native);

pub fn _print(values: []const Value) Value {
    std.debug.print("{any}", .{values[0].Int});
    std.debug.print("{any}", .{values[1].Float});

    return Value.bool_(true);
}
