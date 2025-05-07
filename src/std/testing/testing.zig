const std = @import("std");
const Value = @import("../../runtime/values.zig").Value;
const Meta = @import("../meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

pub const meta = ModuleMeta{
    .name = "testing",
};

// Assert
pub const assert = NativeFnMeta{
    .params = &.{.bool},
    .return_type = .bool,
    .function = _assert,
};

pub fn _assert(values: []const Value) Value {
    return values[0];
}

// Equal ints
pub const eql_int = NativeFnMeta{
    .params = &.{ .int, .int },
    .return_type = .bool,
    .function = _eql_int,
};

pub fn _eql_int(values: []const Value) Value {
    return Value.makeBool(values[0].int == values[0].int);
}
