const std = @import("std");
const Value = @import("../../../runtime/values.zig").Value;
const Meta = @import("../../meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

pub const meta = ModuleMeta{
    .name = "_test2",
};

// Compares if float is greater than int with extra args
pub const _gt_f2i = NativeFnMeta{
    .params = &.{ .int, .str, .float },
    .return_type = .bool,
    .function = __gt_f2i,
};

pub fn __gt_f2i(values: []const Value) Value {
    return Value.bool_(values[2].Float > @as(f64, @floatFromInt(values[0].Int)));
}

// Compares if int is greater than float
pub const _gt_i2f = NativeFnMeta{
    .params = &.{ .int, .float },
    .return_type = .bool,
    .function = __gt_i2f,
};

pub fn __gt_i2f(values: []const Value) Value {
    return Value.bool_(values[0].Int > @as(i64, @intFromFloat(values[1].Float)));
}
