const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
// const allocator = std.testing.allocator;
// const test_config = @import("test_config");
const eql = std.mem.eql;
const fields = std.meta.fields;
const clap = @import("clap");

const Stage = enum { all, parser, analyzer, compiler, vm };
const Diagnostic = struct {
    err_name: []const u8 = undefined,
    file_name: []const u8 = undefined,
    line: ?usize = null,
    expect: ?[]const u8 = null,
    got: ?[]const u8 = null,
    diff: ?[]const u8 = null,

    pub fn display(self: Diagnostic, verbose: bool) void {
        print("    {s}", .{self.file_name});

        if (self.line) |line|
            print(" line: {}\n", .{line})
        else
            print("\n", .{});

        if (verbose) {
            print("ACQUI\n", .{});
            // if (self.expect) |txt| print("expect:\n{s}\n", .{txt});
            // if (self.got) |txt| print("got:\n{s}\n", .{txt});
            if (self.diff) |txt| print("{s}\n", .{txt});
        }
    }

    pub fn deinit(self: *Diagnostic, allocator: Allocator) void {
        allocator.free(self.file_name);
        if (self.expect) |exp| allocator.free(exp);
        if (self.got) |got| allocator.free(got);
        if (self.diff) |diff| allocator.free(diff);
    }
};

const Tester = struct {
    allocator: Allocator,
    diags: ArrayList(Diagnostic),
    exe_path: []const u8,
    show_diff: bool,

    const Self = @This();

    pub fn init(allocator: Allocator, exe_path: []const u8, show_diff: bool) Self {
        return .{
            .allocator = allocator,
            .diags = ArrayList(Diagnostic).init(allocator),
            .exe_path = exe_path,
            .show_diff = show_diff,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.diags.items) |*diag|
            diag.deinit(self.allocator);

        self.diags.deinit();
    }

    fn run(self: *Self, stage: Stage) !void {
        switch (stage) {
            .all => {
                try self.run_stage_tests(.parser);
                self.report(.parser);
                try self.run_stage_tests(.analyzer);
                self.report(.analyzer);
                try self.run_stage_tests(.compiler);
                self.report(.compiler);
                try self.run_vm_tests();
            },
            .vm => try self.run_vm_tests(),
            else => |s| try self.run_stage_tests(s),
        }

        self.report(stage);
    }

    fn report(self: *const Self, stage: Stage) void {
        if (self.diags.items.len > 0) {
            print("Error testing {s}:\n", .{@tagName(stage)});

            for (self.diags.items) |diag| {
                print("SHOW_DIFF: {}\n", .{self.show_diff});
                diag.display(self.show_diff);
            }
        }
    }

    fn run_vm_tests(self: *Self) !void {
        const cwd = std.fs.cwd();

        const path = try std.fs.path.join(self.allocator, &[_][]const u8{
            "tests", "vm",
        });
        defer self.allocator.free(path);

        var test_dir = try cwd.openDir(path, .{ .iterate = true });
        defer test_dir.close();

        var base_walker = try test_dir.walk(self.allocator);
        defer base_walker.deinit();

        while (try base_walker.next()) |*item| {
            if (std.mem.endsWith(u8, item.path, ".rv")) {
                const file_path = try std.fs.path.join(self.allocator, &[_][]const u8{
                    "tests", "vm", item.path,
                });
                defer self.allocator.free(file_path);

                const res = std.process.Child.run(.{
                    .allocator = self.allocator,
                    .cwd_dir = cwd,
                    .argv = &[_][]const u8{
                        self.exe_path,
                        file_path,
                    },
                }) catch |e| {
                    try self.diags.append(.{
                        .file_name = try self.allocator.dupe(u8, item.path),
                        .err_name = @errorName(e),
                    });
                    continue;
                };

                defer self.allocator.free(res.stdout);
                defer self.allocator.free(res.stderr);

                var got_expects = std.mem.splitScalar(u8, res.stdout, '\n');

                // Read file
                const file = try test_dir.openFile(item.path, .{ .mode = .read_only });
                defer file.close();

                const size = try file.getEndPos();
                const content = try self.allocator.alloc(u8, size);
                defer self.allocator.free(content);

                _ = try file.readAll(content);
                var lines = std.mem.splitScalar(u8, content, '\n');

                var i: usize = 0;

                while (lines.next()) |line| : (i += 1) {
                    const trimmed = std.mem.trimRight(u8, line, "\r");

                    if (std.mem.containsAtLeast(u8, trimmed, 1, "expect:")) {
                        var split = std.mem.splitSequence(u8, trimmed, "expect: ");
                        _ = split.next();
                        const exp = split.next().?;

                        const got = got_expects.next().?;
                        std.testing.expect(std.mem.eql(u8, got, exp)) catch |e| {
                            try self.diags.append(.{
                                .file_name = try self.allocator.dupe(u8, item.path),
                                .err_name = @errorName(e),
                                .line = i,
                                .expect = try self.allocator.dupe(u8, exp),
                                .got = try self.allocator.dupe(u8, got),
                            });
                            continue;
                        };
                    }
                }
            }
        }
    }

    fn run_stage_tests(self: *Self, stage: Stage) !void {
        const path = try std.fs.path.join(self.allocator, &[_][]const u8{
            "tests", @tagName(stage),
        });
        defer self.allocator.free(path);

        var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
        defer cwd.close();

        var walker = try cwd.walk(self.allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (std.mem.endsWith(u8, entry.basename, ".rvt")) {
                self.test_file(&cwd, stage, entry) catch continue;
            }
        }
    }

    const Section = enum { Code, Config, Expect, Err, None };

    fn stage_to_opt(stage: Stage) []const u8 {
        return switch (stage) {
            .parser => "--print-ast",
            .analyzer => "--print-ir",
            .compiler => "--test-bytecode",
            else => unreachable,
        };
    }

    fn stage_extra_opt(stage: Stage) []const u8 {
        return if (stage == .analyzer) "-s" else "";
    }

    fn test_file(self: *Self, dir: *std.fs.Dir, stage: Stage, entry: std.fs.Dir.Walker.Entry) !void {
        const file = try dir.openFile(entry.path, .{ .mode = .read_only });
        defer file.close();

        const size = try file.getEndPos();
        const content = try self.allocator.alloc(u8, size);
        defer self.allocator.free(content);

        _ = try file.readAll(content);
        var lines = std.mem.splitScalar(u8, content, '\n');

        var config_text = std.ArrayList(u8).init(self.allocator);
        defer config_text.deinit();
        var code = std.ArrayList(u8).init(self.allocator);
        defer code.deinit();
        var expects = std.ArrayList(u8).init(self.allocator);
        defer expects.deinit();
        var errors = std.ArrayList(u8).init(self.allocator);
        defer errors.deinit();

        var section: Section = .None;
        var test_count: usize = 0;

        while (lines.next()) |line| {
            if (line.len == 0 or eql(u8, line, "\r")) continue;

            // Skip comments
            if (std.mem.startsWith(u8, std.mem.trimLeft(u8, line, " "), "//")) continue;

            // If after removing the \r there is only spaces, we skip it
            if (std.mem.trimRight(u8, std.mem.trimRight(u8, line, "\r"), " ").len == 0) continue;

            if (std.mem.startsWith(u8, line, "code")) {
                section = .Code;
                continue;
            } else if (std.mem.startsWith(u8, line, "config")) {
                section = .Config;
                continue;
            } else if (std.mem.startsWith(u8, line, "expect")) {
                section = .Expect;
                continue;
            } else if (std.mem.startsWith(u8, line, "error")) {
                section = .Err;
                continue;
            } else if (std.mem.startsWith(u8, line, "==")) {
                // try code.append(0);

                const tmp_file = try dir.createFile("tmp.rv", .{});
                // try tmp_file.writeAll(code.items[0 .. code.items.len - 1 :0]);
                try tmp_file.writeAll(code.items[0 .. code.items.len - 1]);
                tmp_file.close();
                defer dir.deleteFile("tmp.rv") catch unreachable;

                const exp = if (section == .Expect) expects.items else errors.items;

                self.run_test(
                    dir,
                    stage,
                    std.mem.trimRight(u8, exp, "\n"),
                    std.mem.trimRight(u8, config_text.items, "\n"),
                ) catch {
                    self.diags.items[self.diags.items.len - 1].file_name = try self.allocator.dupe(u8, entry.basename);
                };

                code.clearRetainingCapacity();
                expects.clearRetainingCapacity();
                errors.clearRetainingCapacity();
                config_text.clearRetainingCapacity();
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
    }

    pub fn run_test(
        self: *Self,
        dir: *std.fs.Dir,
        stage: Stage,
        exp: []const u8,
        config: []const u8,
    ) !void {
        const argv = if (std.mem.eql(u8, config, "static-analyzis"))
            &[_][]const u8{
                self.exe_path,
                "tmp.rv",
                stage_to_opt(stage),
                "-s",
            }
        else
            &[_][]const u8{
                self.exe_path,
                "tmp.rv",
                stage_to_opt(stage),
            };

        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const path = try std.os.getFdPath(dir.fd, &buf);

        const res = std.process.Child.run(.{
            .allocator = self.allocator,
            .cwd = path,
            .argv = argv,
        }) catch |e| {
            try self.diags.append(.{ .err_name = @errorName(e) });
            return e;
        };
        defer self.allocator.free(res.stdout);
        defer self.allocator.free(res.stderr);

        const got = try clean_text(self.allocator, res.stdout);
        defer self.allocator.free(got);

        std.testing.expect(eql(u8, std.mem.trimRight(u8, got, "\n"), exp)) catch |e| {
            try self.diags.append(.{ .diff = try colorize_dif(self.allocator, exp, got) });
            return e;
        };
    }

    fn clean_text(allocator: Allocator, text: []const u8) ![]const u8 {
        var res = std.ArrayList(u8).init(allocator);
        var lines = std.mem.splitScalar(u8, text, '\n');

        while (lines.next()) |line| {
            // Skip comments
            if (std.mem.startsWith(u8, std.mem.trimLeft(u8, line, " "), "//")) continue;

            // If after removing the \r there is only spaces, we skip it
            if (std.mem.trimRight(u8, std.mem.trimRight(u8, line, "\r"), " ").len == 0) continue;

            try res.appendSlice(line);
            try res.append('\n');
        }

        return res.toOwnedSlice();
    }

    const RED = "\x1b[31m";
    const NORMAL = "\x1b[0m";

    fn colorize_dif(allocator: Allocator, str1: []const u8, str2: []const u8) ![]const u8 {
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
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit
        \\--stage <STAGE>        Which stage to test [default: all]
        \\--file <FILE>          File to test
        \\--diff                 Show colored diff of expected/got
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
        .STAGE = clap.parsers.enumeration(Stage),
    };

    var clap_diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &clap_diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        clap_diag.report(std.io.getStdErr().writer(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});

    const diff = if (res.args.diff == 1) true else false;
    print("DIF: {}\n", .{diff});

    const tester_dir = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(tester_dir);

    const exe_path = try std.fs.path.join(allocator, &[_][]const u8{
        tester_dir, if (builtin.os.tag == .windows) "rover.exe" else "rover",
    });
    defer allocator.free(exe_path);

    var tester = Tester.init(allocator, exe_path, diff);
    defer tester.deinit();
    try tester.run(res.args.stage orelse .all);
}
