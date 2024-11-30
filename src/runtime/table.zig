const std = @import("std");
const Allocator = std.mem.Allocator;
const ObjString = @import("obj.zig").ObjString;
const Value = @import("values.zig").Value;

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

pub const Table = struct {
    count: usize,
    entries: []Entry,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .count = 0,
            .entries = &.{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.entries);
    }

    /// Returns if the inserted key is new
    pub fn set(self: *Self, key: *ObjString, value: Value) Allocator.Error!bool {
        // Encodes a 75%
        if (4 * (self.count + 1) > 3 * self.entries.len) {
            try self.adjust_capacity();
        }

        const entry = Table.find_entry(self.entries, key);
        const is_new = entry.key == null;

        // Don't increment if it was a tombstone
        if (is_new and entry.value == .Null) self.count += 1;

        entry.key = key;
        entry.value = value;

        return is_new;
    }

    pub fn get(self: *const Self, key: *const ObjString) ?Value {
        if (self.count == 0) return null;

        const entry = Table.find_entry(self.entries, key);

        if (entry.key == null) return null;
        return entry.value;
    }

    /// Returns true if it was deleted
    // We don't decrement self.count to take into account for grow
    pub fn delete(self: *Self, key: *const ObjString) bool {
        if (self.count == 0) return false;

        const entry = Table.find_entry(self.entries, key);
        if (entry.key == null) return false;

        // Place a tombstone
        entry.key = null;
        entry.value = Value.bool_(true);
        return true;
    }

    fn find_entry(entries: []Entry, key: *const ObjString) *Entry {
        var index = key.hash % entries.len;
        var tombstone: ?*Entry = null;

        while (true) : (index = (index + 1) % entries.len) {
            const entry = &entries[index];

            if (entry.key == null) {
                if (entry.value == .Null) {
                    return if (tombstone) |t| t else entry;
                } else {
                    // Tombstone
                    if (tombstone == null) tombstone = entry;
                }
            } else if (entry.key == key) {
                return entry;
            }
        }
    }

    pub fn find_string(self: *const Self, str: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;

        var index = hash % self.entries.len;

        while (true) : (index = (index + 1) % self.entries.len) {
            const entry = self.entries[index];

            if (entry.key) |k| {
                if (k.chars.len == str.len and k.hash == hash and std.mem.eql(u8, k.chars, str)) {
                    return k;
                }
            } else {
                // We found a non-tombstone empty entry
                if (entry.value == .Null) return null;
            }
        }
    }

    fn adjust_capacity(self: *Self) Allocator.Error!void {
        const new_capa = self.grow();
        const entries_grown = try self.allocator.alloc(Entry, new_capa);

        // PERF: is it mandatory? At least maybe only the key? If Entry has default values?
        for (entries_grown) |*e| {
            e.key = null;
            e.value = Value.null_();
        }

        // Reset count because we are going to take into account only non-tombstone
        self.count = 0;

        for (self.entries) |*e| {
            if (e.key) |k| {
                const dest = Table.find_entry(entries_grown, k);
                dest.key = k;
                dest.value = e.value;
                self.count += 1;
            }
        }

        self.allocator.free(self.entries);
        self.entries = entries_grown;
    }

    fn grow(self: *Self) usize {
        if (self.entries.len < 8) {
            return 8;
        } else {
            return self.entries.len * 2;
        }
    }
};

test "set" {
    const Vm = @import("vm.zig").Vm;
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    // Alloc with Vm's allocator to mimic real case
    // otherwise segfault while we free it at Vm's deinit
    const str = try vm.allocator.alloc(u8, 4);
    @memcpy(str, "mars");

    const key = try ObjString.take(&vm, str);
    const val = Value.int(42);

    try std.testing.expect(try table.set(key, val));
    try std.testing.expect(!try table.set(key, val));
}

test "grow" {
    const Vm = @import("vm.zig").Vm;
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const val = Value.int(42);

    for (0..9) |i| {
        const str = try vm.allocator.alloc(u8, i);
        @memset(str, 'a');
        const key = try ObjString.take(&vm, str);
        // We check that each new entry is unique
        try std.testing.expect(try table.set(key, val));
    }

    try std.testing.expect(table.count == 9);
    try std.testing.expect(table.entries.len == 16);
}

test "get" {
    const Vm = @import("vm.zig").Vm;
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const str = try vm.allocator.alloc(u8, 4);
    @memcpy(str, "mars");

    const key = try ObjString.take(&vm, str);
    const val = Value.int(42);
    _ = try table.set(key, val);

    var entry = table.get(key);
    try std.testing.expectEqual(entry.?.Int, 42);

    var save_key: *ObjString = undefined;

    // After grow
    for (0..9) |i| {
        const str1 = try vm.allocator.alloc(u8, i);
        @memset(str1, 'a');
        const key1 = try ObjString.take(&vm, str1);
        const val1 = Value.int(@intCast(i));
        _ = try table.set(key1, val1);
        save_key = key1;
    }

    // Old entry before grow
    entry = table.get(key);
    try std.testing.expectEqual(entry.?.Int, 42);

    entry = table.get(save_key);
    try std.testing.expectEqual(entry.?.Int, 8);
}

test "delete" {
    const Vm = @import("vm.zig").Vm;
    const allocator = std.testing.allocator;

    var vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const str = try vm.allocator.alloc(u8, 4);
    @memcpy(str, "mars");

    const key = try ObjString.take(&vm, str);
    const val = Value.int(42);
    _ = try table.set(key, val);

    const entry = table.get(key);
    try std.testing.expectEqual(entry.?.Int, 42);

    try std.testing.expect(table.delete(key));
    try std.testing.expectEqual(table.get(key), null);
    try std.testing.expect(!table.delete(key));
}
