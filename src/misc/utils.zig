const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

pub fn oom() noreturn {
    std.debug.print("out of memory\n", .{});
    std.process.exit(1);
}
