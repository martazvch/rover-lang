/// Unsafe iterator. Dosen't return a nullable value, assumes
/// that the precise item count is known by the user
pub fn UnsafeIter(comptime Item: type) type {
    return struct {
        items: []const Item,
        index: usize,

        const Self = @This();

        pub fn init(items: []const Item) Self {
            return .{ .items = items, .index = 0 };
        }

        pub fn next(self: *Self) Item {
            defer self.index += 1;
            return self.items[self.index];
        }
    };
}

pub const UnsafeIterStr = UnsafeIter([]const u8);
