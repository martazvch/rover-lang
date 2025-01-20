const std = @import("std");
const print = std.debug.print;
const expect = testing.expect;
const assert = std.debug.assert;
const eql = std.mem.eql;
const fields = std.meta.fields;
const testing = std.testing;
const allocator = testing.allocator;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const test_config = @import("test_config");

const RED = "\x1b[31m";
const NORMAL = "\x1b[0m";

fn colorize_dif(str1: []const u8, str2: []const u8) ![]const u8 {
    var res = std.ArrayList(u8).init(allocator);

    const State = enum {
        normal,
        until_nl1,
        until_nl2,
    };

    var i: usize = 0;
    var j: usize = 0;
    dif: switch (State.normal) {
        .normal => {
            if (i >= str1.len) break :dif;
            if (j >= str2.len) break :dif;

            if (str1[i] != str2[j]) {
                try res.appendSlice(RED);
                try res.append(str2[j]);
                i += 1;
                j += 1;
                continue :dif .until_nl1;
            }

            try res.append(str2[j]);
            i += 1;
            j += 1;
            continue :dif .normal;
        },
        .until_nl1 => {
            if (i >= str1.len) break :dif;

            if (str1[i] == '\n') {
                i += 1;
                continue :dif .until_nl2;
            }

            i += 1;
            continue :dif .until_nl1;
        },
        .until_nl2 => {
            if (j >= str2.len) break :dif;

            if (str2[j] == '\n') {
                try res.appendSlice(NORMAL);
                try res.append(str2[j]);
                j += 1;
                continue :dif .normal;
            }

            try res.append(str2[j]);
            j += 1;
            continue :dif .until_nl2;
        },
    }

    return res.toOwnedSlice();
}

pub fn GenTestData(comptime Report: type) type {
    assert(@typeInfo(Report) == .@"union");

    return struct {
        expect: []const u8,
        reports: []const Report,
    };
}

pub const Config = struct {
    ignores: ArrayList([]const u8),
    modes: ArrayList([]const u8),

    pub fn init() Config {
        return .{
            .ignores = ArrayList([]const u8).init(allocator),
            .modes = ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Config) void {
        self.ignores.deinit();
        self.modes.deinit();
    }
};

pub fn GenericTester(
    foldername: []const u8,
    comptime Report: type,
    comptime get_test_data: fn ([:0]const u8, Allocator, ?Config) anyerror!GenTestData(Report),
) type {
    return struct {
        const Section = enum { Code, Config, Expect, Err, None };

        pub fn run() !void {
            if (eql(u8, test_config.file, "")) {
                const path = try std.fs.path.join(allocator, &[_][]const u8{
                    "tests", foldername,
                });
                defer allocator.free(path);

                var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
                defer cwd.close();

                var walker = try cwd.walk(allocator);
                defer walker.deinit();

                while (try walker.next()) |*entry| {
                    if (std.mem.endsWith(u8, entry.basename, ".rvt")) {
                        try test_file(&cwd, entry.path);
                    }
                }
            } else {
                const last_slash = std.mem.lastIndexOfScalar(u8, test_config.file, '/').?;
                const dir = test_config.file[0..last_slash];
                const filename = test_config.file[last_slash + 1 .. test_config.file.len];

                // Weird behavior, all tests are imported even if filtered in main...
                // We filter it by hand here. Maybe importing them via other testers do this
                if (!std.mem.eql(u8, foldername, @tagName(test_config.stage))) return;

                var cwd = try std.fs.cwd().openDir(dir, .{});
                defer cwd.close();

                try test_file(&cwd, filename);
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

            var config_text = std.ArrayList(u8).init(allocator);
            defer config_text.deinit();
            var code = std.ArrayList(u8).init(allocator);
            defer code.deinit();
            var expects = std.ArrayList(u8).init(allocator);
            defer expects.deinit();
            var errors = std.ArrayList(u8).init(allocator);
            defer errors.deinit();

            var config: ?Config = null;

            var section: Section = .None;
            var test_count: usize = 0;

            while (lines.next()) |line| {
                if (line.len == 0 or std.mem.eql(u8, line, "\r")) continue;
                // If after removing the \r there is only spaces, we skip it
                if (std.mem.trimRight(u8, std.mem.trimRight(u8, line, "\r"), " ").len == 0) continue;

                if (std.mem.startsWith(u8, line, "config")) {
                    section = .Config;
                    continue;
                } else if (std.mem.startsWith(u8, line, "code")) {
                    // First code block, we build the config
                    if (section == .Config) {
                        config = try build_config(config_text.items);
                    }

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

                    run_test(
                        code.items[0 .. code.items.len - 1 :0],
                        expects.items,
                        errors.items,
                        config,
                    ) catch |e| {
                        print("Error in test {} in file {s}\n\n", .{ test_count + 1, file_path });
                        if (config) |*conf| conf.deinit();
                        return e;
                    };

                    code.clearRetainingCapacity();
                    expects.clearRetainingCapacity();
                    errors.clearRetainingCapacity();
                    section = .None;
                    test_count += 1;
                    continue;
                }

                var writer = switch (section) {
                    .Code => code.writer(),
                    .Config => config_text.writer(),
                    .Expect => expects.writer(),
                    .Err => errors.writer(),
                    .None => continue,
                };

                _ = try writer.write(std.mem.trimRight(u8, line, "\r"));
                _ = try writer.write("\n");
            }

            if (config) |*conf| conf.deinit();
        }

        fn build_config(config_text: []const u8) !Config {
            var config = Config.init();

            var split_conf = std.mem.splitScalar(u8, config_text, '\n');

            while (split_conf.next()) |conf| {
                var split = std.mem.splitScalar(u8, conf, ' ');
                const kind = split.next().?;

                if (std.mem.eql(u8, kind, "ignore")) {
                    try config.ignores.append(split.next().?);
                } else if (std.mem.eql(u8, kind, "mode")) {
                    try config.modes.append(split.next().?);
                }
            }

            return config;
        }

        pub fn run_test(source: [:0]const u8, exp: []const u8, errors: []const u8, config: ?Config) !void {
            const test_data = try get_test_data(source, allocator, config);
            defer allocator.free(test_data.expect);
            defer allocator.free(test_data.reports);

            if (exp.len > 0) {
                expect(eql(u8, test_data.expect, exp)) catch |e| {
                    const color_dif = try colorize_dif(exp, test_data.expect);
                    defer allocator.free(color_dif);

                    print("expect:\n{s}\n", .{exp});
                    print("got:\n{s}\n", .{color_dif});
                    return e;
                };
            } else if (errors.len > 0) {
                var all = std.mem.splitScalar(u8, errors, '\n');

                var i: usize = 0;
                while (all.next()) |err| : (i += 1) {
                    if (err.len == 0) continue;

                    var line = std.mem.splitScalar(u8, err, ',');
                    // There is at least one element, the error name
                    const err_name = line.next().?;

                    expect(i < test_data.reports.len) catch |e| {
                        print("assertion {}, expect to find a {}th report but only {} were generated\n", .{ i, i + 1, test_data.reports.len });
                        return e;
                    };
                    const report = test_data.reports[i];

                    const got_name = @tagName(report);
                    expect(eql(u8, err_name, got_name)) catch |e| {
                        print("assertion {}, expect error: {s}, got {s}\n", .{ i, err_name, got_name });
                        return e;
                    };

                    // We look for all the fields
                    inline for (fields(Report)) |field| {
                        // If we find the current and that it's a non-void (assuming it's a struct)
                        if (field.type != void and eql(u8, field.name, @tagName(std.meta.activeTag(report)))) {
                            const fv = @field(report, field.name);

                            // We check all the fields of the structure to test them
                            inline for (fields(field.type)) |subf| {
                                const subv = @field(fv, subf.name);

                                if (line.next()) |extra| {
                                    expect(eql(u8, std.mem.trimLeft(u8, extra, " "), subv)) catch |e| {
                                        print("assertion {}, expect error extra arg: {s}, got {s}\n", .{ i, extra, subv });
                                        return e;
                                    };
                                } else {
                                    print("expect extra argument '{s}' for report '{s}'\n", .{ subf.name, field.name });
                                    return error.TestUnexpectedResult;
                                }
                            }
                        }
                    }
                }

                // i - 1 because end of while loop executed the continuation expression
                if (i - 1 != test_data.reports.len) {
                    print(
                        "Error: incoherent number of errors/warnings, execution had {} and test file had {}\n",
                        .{ test_data.reports.len, i - 1 },
                    );
                    print("Execution:\n{any}\nTest file:\n{s}\n", .{ test_data.reports, errors });
                    return error.TestUnexpectedResult;
                }
            } else {
                print("Error, no expect and no erros in test\n", .{});
                return error.TestUnexpectedResult;
            }
        }
    };
}
