const std = @import("std");

pub fn oom() noreturn {
    std.debug.print("out of memory", .{});
    std.process.exit(1);
}
