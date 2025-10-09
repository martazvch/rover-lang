const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayMap = std.AutoArrayHashMapUnmanaged;

pub fn Set(comptime T: type) type {
    return struct {
        set: ArrayMap(T, void),

        const Self = @This();

        pub const empty: Self = .{ .set = .empty };

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.set.deinit(allocator);
        }

        pub fn fromSlice(allocator: Allocator, slice: []const T) Allocator.Error!Self {
            var set: ArrayMap(T, void) = .empty;
            try set.ensureTotalCapacity(allocator, slice.len);

            for (slice) |elem| {
                if (set.contains(elem)) continue;
                set.putAssumeCapacity(elem, {});
            }

            return .{ .set = set };
        }

        pub fn add(self: *Self, allocator: Allocator, item: T) Allocator.Error!void {
            const gop = try self.set.getOrPut(allocator, item);
            if (gop.found_existing) return;
            gop.key_ptr.* = item;
        }

        pub fn addSlice(self: *Self, allocator: Allocator, items: []const T) Allocator.Error!void {
            for (items) |item| {
                try self.add(allocator, item);
            }
        }

        pub fn has(self: *const Self, item: T) bool {
            return self.set.contains(item);
        }

        /// Removes the `item` if found while keeping order in the set
        pub fn remove(self: *Self, item: T) bool {
            return self.set.orderedRemove(item);
        }

        pub fn values(self: *const Self) []T {
            return self.set.keys();
        }

        /// Returns the owned slice of the elements in set and invalidate pointer
        pub fn toOwned(self: *Self) []const T {
            defer self.* = undefined;
            return self.set.move().keys();
        }

        pub fn count(self: *const Self) usize {
            return self.set.count();
        }

        pub fn sort(self: *Self, comptime lessThan: fn (void, T, T) bool) void {
            std.sort.heap(T, self.values(), {}, lessThan);
        }

        pub fn getIndex(self: *const Self, key: T) ?usize {
            return self.set.getIndex(key);
        }
    };
}

test Set {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var set: Set(i64) = .empty;
    defer set.deinit(allocator);

    try set.add(allocator, 4);
    try set.add(allocator, 6);
    try set.add(allocator, 4);
    try set.add(allocator, 1);
    try set.add(allocator, 6);

    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 4, 6, 1 }));
}

test "from slice" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var set: Set(i64) = try .fromSlice(allocator, &.{ 4, 6, 4, 1, 6 });
    defer set.deinit(allocator);

    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 4, 6, 1 }));
}

test "add slice" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var set: Set(i64) = .empty;
    defer set.deinit(allocator);

    try set.addSlice(allocator, &.{ 4, 6, 4, 1, 6 });

    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 4, 6, 1 }));
}

test "sort" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var set: Set(i64) = try .fromSlice(allocator, &.{ 4, 6, 4, 1, 6 });
    defer set.deinit(allocator);

    const sortFn = struct {
        fn lessThan(_: void, a: i64, b: i64) bool {
            return a < b;
        }
    }.lessThan;

    set.sort(sortFn);
    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 1, 4, 6 }));
}

test "remove" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var set: Set(i64) = try .fromSlice(allocator, &.{ 4, 6, 4, 1, 6 });
    defer set.deinit(allocator);

    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 4, 6, 1 }));
    try expect(set.remove(6));
    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 4, 1 }));

    const sortFn = struct {
        fn lessThan(_: void, a: i64, b: i64) bool {
            return a < b;
        }
    }.lessThan;

    set.sort(sortFn);
    try expect(std.mem.eql(i64, set.values(), &[_]i64{ 1, 4 }));
    try expect(set.remove(4));
    try expect(std.mem.eql(i64, set.values(), &[_]i64{1}));
}
