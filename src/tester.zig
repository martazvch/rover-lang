const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const allocator = testing.allocator;

pub fn GenericTester(
    foldername: []const u8,
    comptime run_test_fn: fn ([:0]const u8, []const u8, []const u8) anyerror!void,
) type {
    return struct {
        const Section = enum { Code, Expect, Err, None };

        pub fn test_all() !void {
            const path = try std.fs.path.join(allocator, &[_][]const u8{
                "tests", foldername,
            });
            defer allocator.free(path);

            var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
            defer cwd.close();

            var walker = try cwd.walk(allocator);
            defer walker.deinit();

            while (try walker.next()) |*entry| {
                if (std.mem.endsWith(u8, entry.basename, ".test")) {
                    try test_file(&cwd, entry.path);
                }
            }
        }

        fn test_file(dir: *std.fs.Dir, file_path: []const u8) !void {
            const file = try dir.openFile(file_path, .{ .mode = .read_only });
            defer file.close();

            const size = try file.getEndPos();
            const content = try allocator.alloc(u8, size + 1);
            defer allocator.free(content);

            _ = try file.readAll(content);
            content[size] = 0;

            var lines = std.mem.splitScalar(u8, content, '\n');

            var code = std.ArrayList(u8).init(allocator);
            defer code.deinit();
            var expects = std.ArrayList(u8).init(allocator);
            defer expects.deinit();
            var errors = std.ArrayList(u8).init(allocator);
            defer errors.deinit();

            var section: Section = .None;
            var test_count: usize = 0;

            while (lines.next()) |line| {
                if (line.len == 0 or std.mem.eql(u8, line, "\r")) continue;
                // If after removing the \r there is only spaces, we skip it
                if (std.mem.trimRight(u8, std.mem.trimRight(u8, line, "\r"), " ").len == 0) continue;

                if (std.mem.startsWith(u8, line, "code")) {
                    section = .Code;
                    continue;
                } else if (std.mem.startsWith(u8, line, "expect")) {
                    section = .Expect;
                    continue;
                } else if (std.mem.startsWith(u8, line, "error")) {
                    section = .Err;
                    continue;
                } else if (std.mem.startsWith(u8, line, "==")) {
                    try code.append(0);

                    run_test_fn(code.items[0 .. code.items.len - 1 :0], expects.items, errors.items) catch |e| {
                        print("Error in test {} in file {s}\n\n", .{ test_count, file_path });
                        return e;
                    };

                    code.clearRetainingCapacity();
                    expects.clearRetainingCapacity();
                    section = .None;
                    test_count += 1;
                    continue;
                }

                var writer = switch (section) {
                    .Code => code.writer(),
                    .Expect => expects.writer(),
                    .Err => errors.writer(),
                    .None => continue,
                };

                _ = try writer.write(std.mem.trimRight(u8, line, "\r"));
                _ = try writer.write("\n");
            }
        }

        pub fn run_test(source: [:0]const u8, exp: []const u8, errors: []const u8) anyerror!void {
            return run_test_fn(source, exp, errors);
        }
    };
}
