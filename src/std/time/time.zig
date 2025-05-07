const std = @import("std");
const Value = @import("../../runtime/values.zig").Value;
const Meta = @import("../meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

pub const meta = ModuleMeta{
    .name = "time",
};

// Clock
pub const clock = NativeFnMeta{
    .params = &.{},
    .return_type = .float,
    .function = _clock,
};

pub fn _clock(_: []const Value) Value {
    return .{ .float = @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0 };
}
