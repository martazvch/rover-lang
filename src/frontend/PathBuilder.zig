const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const oom = @import("../utils.zig").oom;

const Self = @This();
const Chunks = ArrayListUnmanaged([]const u8);

chunks: Chunks,
allocator: Allocator,

pub fn init(allocator: Allocator, folder: []const u8) Self {
    var chunks: Chunks = .{};
    chunks.append(allocator, folder) catch oom();

    return .{ .allocator = allocator, .chunks = chunks };
}

pub fn deinit(self: *Self) void {
    for (self.chunks.items) |chunk| {
        self.allocator.free(chunk);
    }
    self.chunks.deinit(self.allocator);
}

pub fn base(self: *const Self) []const u8 {
    return self.chunks.items[0];
}

/// Duplicates the string to own it
pub fn cd(self: *Self, folder: []const u8) void {
    self.chunks.append(self.allocator, self.allocator.dupe(u8, folder) catch oom()) catch oom();
}

pub fn up(self: *Self) void {
    if (self.chunks.items.len > 1) {
        _ = self.chunks.pop();
    }
}

pub fn fullPath(self: *const Self, buf: []u8) []const u8 {
    var fbs = std.io.fixedBufferStream(buf);
    const writer = fbs.writer();

    for (self.chunks.items, 0..) |chunk, i| {
        if (i != 0) writer.writeAll(std.fs.path.sep_str) catch oom();
        writer.writeAll(chunk) catch oom();
    }

    return fbs.getWritten();
}

/// Caller owns the memory
pub fn fullPathAlloc(self: *const Self, allocator: Allocator) ![]const u8 {
    var path: ArrayListUnmanaged(u8) = .{};
    defer if (path.capacity != 0) path.deinit(allocator);

    for (self.chunks.items, 0..) |chunk, i| {
        if (i != 0) try path.appendSlice(allocator, std.fs.path.sep_str);
        try path.appendSlice(allocator, chunk);
    }

    return try path.toOwnedSlice(allocator);
}
