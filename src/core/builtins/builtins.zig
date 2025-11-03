const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;

pub const module: ffi.ZigModule = .{
    .is_module = false,
    .functions = &.{
        .init("int", "", &.{}, int),
        .init("float", "", &.{}, float),
    },
};

fn int(value: union(enum) { float: f64, int: i64 }) i64 {
    return switch (value) {
        .int => |i| i,
        .float => @intFromFloat(value.float),
    };
}

fn float(value: union(enum) { float: f64, int: i64 }) f64 {
    return switch (value) {
        .int => @floatFromInt(value.int),
        .float => |f| f,
    };
}
