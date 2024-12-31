const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");
const expect = testing.expect;
const allocator = std.testing.allocator;

pub fn main() !void {}

test "runtime" {
    var cwd = std.fs.cwd();

    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "tests", "vm",
    });
    defer allocator.free(path);

    var test_dir = try cwd.openDir(path, .{ .iterate = true });
    defer test_dir.close();

    var base_walker = try test_dir.walk(allocator);
    defer base_walker.deinit();

    while (try base_walker.next()) |*item| {
        if (std.mem.endsWith(u8, item.path, ".rv")) {
            const exe_name = if (builtin.os.tag == .windows) "rover-lang.exe" else "rover-lang";

            const path_to_exe = try std.fs.path.join(allocator, &[_][]const u8{
                "zig-out", "bin", exe_name,
            });
            defer allocator.free(path_to_exe);

            const file_path = try std.fs.path.join(allocator, &[_][]const u8{
                "tests", "vm", item.path,
            });
            defer allocator.free(file_path);

            const res = try std.process.Child.run(.{
                .allocator = allocator,
                .cwd_dir = cwd,
                .argv = &[_][]const u8{
                    path_to_exe,
                    "-f",
                    file_path,
                },
            });
            defer allocator.free(res.stdout);
            defer allocator.free(res.stderr);

            var got_expects = std.mem.splitScalar(u8, res.stdout, '\n');
            var got_errors = std.mem.splitScalar(u8, res.stderr, '\n');
            _ = &got_errors;

            // Read file
            const file = try test_dir.openFile(item.path, .{ .mode = .read_only });
            defer file.close();

            const size = try file.getEndPos();
            const content = try allocator.alloc(u8, size);
            defer allocator.free(content);

            _ = try file.readAll(content);
            var lines = std.mem.splitScalar(u8, content, '\n');

            var expects = std.ArrayList(u8).init(allocator);
            defer expects.deinit();

            var i: usize = 0;

            while (lines.next()) |line| : (i += 1) {
                const trimmed = std.mem.trimRight(u8, line, "\r");

                if (std.mem.containsAtLeast(u8, trimmed, 1, "expect:")) {
                    var split = std.mem.splitSequence(u8, trimmed, "expect: ");
                    _ = split.next();
                    const exp = split.next().?;

                    const got = got_expects.next().?;
                    expect(std.mem.eql(u8, got, exp)) catch |e| {
                        std.debug.print("Error in file: {s} line: {}\n", .{ item.path, i });
                        std.debug.print("expect:\n{s}\n", .{exp});
                        std.debug.print("got:\n{s}\n", .{got});
                        std.debug.print("\nStderr: {s}\n", .{res.stderr});
                        return e;
                    };
                }
            }
        }
    }
}
