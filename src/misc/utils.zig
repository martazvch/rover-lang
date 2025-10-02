const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

pub fn oom() noreturn {
    std.debug.print("out of memory\n", .{});
    std.process.exit(1);
}

pub fn uniques(comptime T: type, allocator: Allocator, slice: []const T) []const T {
    var res: AutoArrayHashMapUnmanaged(T, void) = .empty;
    res.ensureTotalCapacity(allocator, slice.len) catch oom();

    for (slice) |elem| {
        if (res.contains(elem)) continue;
        res.putAssumeCapacity(elem, {});
    }

    return res.keys();
}
