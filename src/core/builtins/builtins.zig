const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;

pub const int: ffi.FnMeta = .{
    .params = &.{
        .{ .type = .{ .@"union" = &.{ .float, .int } } },
    },
    .return_type = .int,
    .function = ffi.makeNative(_int),
};

fn _int(value: Value) i64 {
    return switch (value) {
        .int => |i| i,
        .float => @intFromFloat(value.float),
        else => unreachable,
    };
}

pub const float: ffi.FnMeta = .{
    .params = &.{
        .{ .type = .{ .@"union" = &.{ .float, .int } } },
    },
    .return_type = .float,
    .function = ffi.makeNative(_float),
};

fn _float(value: Value) f64 {
    return switch (value) {
        .int => @floatFromInt(value.int),
        .float => |f| f,
        else => unreachable,
    };
}
