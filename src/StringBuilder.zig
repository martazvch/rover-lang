const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const oom = @import("utils.zig").oom;

const Self = @This();

string: ArrayListUnmanaged([]const u8),

pub const empty: Self = .{ .string = .empty };

pub fn deinit(self: *Self, allocator: Allocator) void {
    for (self.string.items) |s| {
        allocator.free(s);
    }
    self.string.deinit(allocator);
}

/// Duplicates the string to own it
pub fn append(self: *Self, allocator: Allocator, s: []const u8) void {
    self.string.append(allocator, allocator.dupe(u8, s) catch oom()) catch oom();
}

/// Duplicates the string to own it
pub fn appendSlice(self: *Self, allocator: Allocator, s: []const []const u8) void {
    self.string.appendSlice(allocator, allocator.dupe(u8, s) catch oom()) catch oom();
}

pub fn pop(self: *Self) ?[]const u8 {
    return self.string.pop();
}

pub fn popMany(self: *Self, count: usize) void {
    for (0..count) |_| _ = self.string.pop();
}

pub fn render(self: *const Self, buf: []u8) []const u8 {
    var fbs = std.io.fixedBufferStream(buf);
    const writer = fbs.writer();

    for (self.string.items) |s| {
        writer.writeAll(s) catch oom();
    }

    return fbs.getWritten();
}

/// Caller owns the memory
pub fn renderAlloc(self: *const Self, allocator: Allocator) []const u8 {
    var path: ArrayListUnmanaged(u8) = .empty;

    for (self.string.items) |s| {
        path.appendSlice(allocator, s) catch oom();
    }

    return path.toOwnedSlice(allocator) catch oom();
}

pub fn renderWithSep(self: *const Self, buf: []u8, sep: []const u8) []const u8 {
    var fbs = std.io.fixedBufferStream(buf);
    const writer = fbs.writer();

    for (self.string.items, 0..) |s, i| {
        if (i != 0) writer.writeAll(sep) catch oom();
        writer.writeAll(s) catch oom();
    }

    return fbs.getWritten();
}

pub fn renderWithSepAlloc(self: *const Self, allocator: Allocator, sep: []const u8) []const u8 {
    var buf: ArrayListUnmanaged(u8) = .empty;

    for (self.string.items, 0..) |s, i| {
        if (i != 0) buf.appendSlice(allocator, sep) catch oom();
        buf.appendSlice(allocator, s) catch oom();
    }

    return buf.toOwnedSlice(allocator) catch oom();
}

/// Get current count of string chunks
pub fn len(self: *const Self) usize {
    return self.string.items.len;
}

/// Shrinks the number of string chunks
pub fn shrink(self: *Self, allocator: Allocator, length: usize) void {
    for (self.string.items[length..]) |chunk| {
        allocator.free(chunk);
    }
    self.string.shrinkRetainingCapacity(length);
}
