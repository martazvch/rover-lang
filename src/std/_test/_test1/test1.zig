const std = @import("std");
const Value = @import("../../../runtime/values.zig").Value;
const Meta = @import("../../meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

pub const meta = ModuleMeta{
    .name = "_test1",
};

// Casts float to int
pub const _cast_i2f = NativeFnMeta{
    .params = &.{.int},
    .return_type = .float,
    .function = __cast_i2f,
};

pub fn __cast_i2f(values: []const Value) Value {
    return Value.makeFloat(@as(f64, @floatFromInt(values[0].int)));
}

// Casts int to float with useless extra arg
pub const _cast_f2i = NativeFnMeta{
    .params = &.{ .bool, .float },
    .return_type = .int,
    .function = __cast_f2i,
};

pub fn __cast_f2i(values: []const Value) Value {
    return Value.makeInt(@as(i64, @intFromFloat(values[1].float)));
}
