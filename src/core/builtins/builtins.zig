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
        .int => value.int,
        .float => @intFromFloat(value.float),
        else => unreachable,
    };
}
