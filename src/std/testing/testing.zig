const std = @import("std");
const Value = @import("../../runtime/values.zig").Value;
const TypeSys = @import("../../frontend/type_system.zig");
const Meta = @import("../meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

pub const meta = ModuleMeta{
    .name = "testing",
};

// Assert
pub const assert = NativeFnMeta{
    .params = &.{TypeSys.Bool},
    .return_type = TypeSys.Bool,
    .function = _assert,
};

pub fn _assert(values: []const Value) Value {
    return values[0];
}

// Equal ints
pub const eql_int = NativeFnMeta{
    .params = &.{ TypeSys.Int, TypeSys.Int },
    .return_type = TypeSys.Bool,
    .function = _eql_int,
};

pub fn _eql_int(values: []const Value) Value {
    return Value.bool_(values[0].Int == values[0].Int);
}
