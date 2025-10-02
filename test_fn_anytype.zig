const std = @import("std");

fn add(v1: anytype, v2: i64) void {
    _ = v2; // autofix
    _ = v1; // autofix
}

pub fn main() !void {
    inline for (@typeInfo(@TypeOf(add)).@"fn".params) |p| {
        @compileLog(p);
    }
}
