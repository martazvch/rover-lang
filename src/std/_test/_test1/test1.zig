const std = @import("std");
const Value = @import("../../../runtime/values.zig").Value;
const TypeSys = @import("../../../frontend/type_system.zig");
const Meta = @import("../../meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

pub const meta = ModuleMeta{
    .name = "_test1",
};

// Casts float to int
pub const _cast_i2f = NativeFnMeta{
    .params = &.{TypeSys.Int},
    .return_type = TypeSys.Float,
    .function = __cast_i2f,
};

pub fn __cast_i2f(values: []const Value) Value {
    return Value.float(@as(f64, @floatFromInt(values[0].Int)));
}

// Casts int to float with useless extra arg
pub const _cast_f2i = NativeFnMeta{
    .params = &.{ TypeSys.Bool, TypeSys.Float },
    .return_type = TypeSys.Int,
    .function = __cast_f2i,
};

pub fn __cast_f2i(values: []const Value) Value {
    return Value.int(@as(i64, @intFromFloat(values[1].Float)));
}
