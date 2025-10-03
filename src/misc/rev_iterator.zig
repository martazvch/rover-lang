pub fn RevIterator(T: type) type {
    return struct {
        items: []T,
        index: usize,

        pub fn init(items: []T) @This() {
            return .{ .items = items, .index = items.len };
        }

        pub fn next(self: *@This()) ?*T {
            if (self.index > 0) {
                self.index -= 1;
                return &self.items[self.index];
            }
            return null;
        }
    };
}

test RevIterator {
    const expect = @import("std").testing.expect;

    var items: [5]i64 = .{ 1, 2, 3, 4, 5 };
    const rev: [5]i64 = .{ 5, 4, 3, 2, 1 };

    var i: usize = 0;
    var it = RevIterator(i64).init(&items);

    while (it.next()) |item| : (i += 1) {
        try expect(item.* == rev[i]);
    }
}
