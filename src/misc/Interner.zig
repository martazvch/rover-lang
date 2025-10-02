const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const oom = @import("utils.zig").oom;

map: StringHashMap(usize),
refs: ArrayList([]const u8),

const Self = @This();
pub const Index = usize;

pub fn init(allocator: Allocator) Self {
    return .{
        .map = StringHashMap(usize).init(allocator),
        .refs = .empty,
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    for (self.refs.items) |ref| {
        allocator.free(ref);
    }
    self.refs.deinit(allocator);
    self.map.deinit();
}

pub fn intern(self: *Self, str: []const u8) Index {
    const entry = self.map.getOrPut(str) catch oom();

    if (!entry.found_existing) {
        entry.value_ptr.* = self.map.count() - 1;
    }

    return entry.value_ptr.*;
}

/// Duplicates the string to store a reference to it
pub fn internKeepRef(self: *Self, allocator: Allocator, str: []const u8) Index {
    self.refs.append(allocator, allocator.dupe(u8, str) catch oom()) catch oom();
    const entry = self.map.getOrPut(self.refs.getLast()) catch oom();

    if (!entry.found_existing) {
        entry.value_ptr.* = self.map.count() - 1;
    }

    return entry.value_ptr.*;
}

pub fn getKey(self: *const Self, index: Index) ?[]const u8 {
    var it = self.map.iterator();

    while (it.next()) |entry| {
        if (entry.value_ptr.* == index) return entry.key_ptr.*;
    }

    return null;
}

test "intern" {
    const testing = @import("std").testing;
    const expect = testing.expect;
    const allocator = testing.allocator;

    var interner = Self.init(allocator);
    defer interner.deinit(allocator);

    try expect(interner.intern("first") == 0);
    try expect(interner.intern("second") == 1);
    try expect(interner.intern("first") == 0);
    try expect(interner.intern("third") == 2);
    try expect(interner.intern("third") == 2);
    try expect(interner.intern("third") == 2);
    try expect(interner.intern("first") == 0);
}
