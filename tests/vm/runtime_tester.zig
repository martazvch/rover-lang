const std = @import("std");
const allocator = std.testing.allocator;

pub fn main() !void {}

test "poupi" {
    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "tests", "vm",
    });
    defer allocator.free(path);

    var base_dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer base_dir.close();

    var base_walker = try base_dir.walk(allocator);
    defer base_walker.deinit();

    while (try base_walker.next()) |*folder| {
        if (std.mem.endsWith(u8, folder.path, ".rv")) {
            const path_to_exe = try std.fs.path.join(allocator, &[_][]const u8{
                "zig-out", "bin", "rover-lang.exe",
            });
            defer allocator.free(path_to_exe);

            const path_to_file = try std.fs.path.join(allocator, &[_][]const u8{
                "tests", "vm", "binop", "scalar.rv",
            });
            defer allocator.free(path_to_file);

            var buf: [1024]u8 = undefined;
            std.debug.print("CWD: {s}\n", .{try std.process.getCwd(&buf)});

            const res = try std.process.Child.run(.{
                .allocator = allocator,
                .cwd_dir = base_dir,
                .argv = &[_][]const u8{
                    path_to_exe,
                    "-f",
                    path_to_file,
                },
            });
            defer allocator.free(res.stdout);
            defer allocator.free(res.stderr);

            std.debug.print("STDOUT: {s}\n", .{res.stdout});
            std.debug.print("STDERR: {s}\n", .{res.stderr});
        }
    }
}
