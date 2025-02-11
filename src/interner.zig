const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;

pub const Interner = struct {
    map: StringHashMap(usize),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .map = StringHashMap(usize).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }

    pub fn intern(self: *Self, str: []const u8) !usize {
        const entry = try self.map.getOrPut(str);

        if (!entry.found_existing) {
            entry.value_ptr.* = self.map.count() - 1;
        }

        return entry.value_ptr.*;
    }

    pub fn get_key(self: *const Self, index: usize) ?[]const u8 {
        var it = self.map.iterator();

        while (it.next()) |entry| {
            if (entry.value_ptr.* == index) return entry.key_ptr.*;
        }

        return null;
    }
};

test "intern" {
    const testing = @import("std").testing;
    const expect = testing.expect;

    var interner = Interner.init(testing.allocator);
    defer interner.deinit();

    try expect(try interner.intern("first") == 0);
    try expect(try interner.intern("second") == 1);
    try expect(try interner.intern("first") == 0);
    try expect(try interner.intern("third") == 2);
    try expect(try interner.intern("third") == 2);
    try expect(try interner.intern("third") == 2);
    try expect(try interner.intern("first") == 0);
}
