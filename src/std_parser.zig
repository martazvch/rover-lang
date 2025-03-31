const std = @import("std");
const rover_std = @import("rover-std");
const NativeWrap = @import("native.zig").NativeWrap;
const FnWrapper = @import("native.zig").FnWrapper;

// TMP:
const Value = @import("runtime/values.zig").Value;

pub const Container = struct {
    constants: []const f64,
    functions: []const FnWrapper,
};

pub fn add(a: i64, b: i64) i64 {
    return a + b;
}

pub fn parse() void {
    const stack: []const Value = &.{
        .{ .Int = 5 },
        .{ .Int = 1 },
    };

    const wrapped: NativeWrap(add) = .{};
    std.debug.print("Called: {}\n", .{wrapped.call(stack)});

    const wrapped_add: NativeWrap(add) = .{};

    const container: Container = .{
        .constants = &.{},
        .functions = &.{
            wrapped_add.to_wrap(),
        },
    };

    std.debug.print("Called opaque: {}\n", .{container.functions[0].call(stack)});
}
