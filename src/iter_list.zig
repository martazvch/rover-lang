const std = @import("std");
const ArrayList = std.ArrayList;

/// Unsafe iterator of any collection
pub fn IterList(comptime T: type) type {
    return struct {
        items: []const T,
        current: usize,

        const Self = @This();

        pub fn init(items: []const T) Self {
            return .{ .items = items, .current = 0 };
        }

        pub fn next(self: *Self) T {
            self.current += 1;
            return self.items[self.current - 1];
        }
    };
}
