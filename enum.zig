const std = @import("std");

const E = enum { a, b };
const U = union(enum) { a, b: i64 };

pub fn main() !void {
    const e = E.a;
    std.log.debug("{}", .{e == E.a});

    const u = U{ .b = 1 };
    std.log.debug("{}", .{u == U.b});
    std.log.debug("{}", .{U.b == U.b});

    const v = U{ .b = 1 };
    _ = v; // autofix
    // std.log.debug("{}", .{u == v});
    std.log.debug("{}", .{U.a == U.a});
}
