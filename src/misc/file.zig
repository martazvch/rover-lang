const std = @import("std");
const Allocator = std.mem.Allocator;

/// Reads a file if it exists and return a buffer with content. Caller owns the memory
pub fn read(allocator: Allocator, file_path: []const u8) ![:0]const u8 {
    const file = std.fs.cwd().openFile(file_path, .{ .mode = .read_only }) catch |err| {
        // TODO: Rover error
        std.debug.print("Error: {}, unable to open file at: {s}\n", .{ err, file_path });
        std.process.exit(0);
    };
    defer file.close();

    // The file has a new line inserted by default
    const size = try file.getEndPos();
    const buf = try allocator.allocSentinel(u8, size, 0);
    // defer allocator.free(buf);
    _ = try file.readAll(buf);

    return buf;
}
