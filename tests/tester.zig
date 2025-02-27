const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
// const allocator = std.testing.allocator;
// const test_config = @import("test_config");
const eql = std.mem.eql;
const fields = std.meta.fields;
const clap = @import("clap");

const Stage = enum { all, parser, analyzer, compiler, vm };
const Diagnostic = struct {
    err_name: []const u8,
    file_name: []const u8,
    line: ?usize,
    expect: ?[]const u8,
    got: ?[]const u8,
    diff: ?[]const u8,

    const TestErr = error{err};

    pub fn display(self: Diagnostic, diff: bool) TestErr {
        print("Error in file: {s}", .{self.file_name});

        if (self.line) |line|
            print(" line: {}\n", .{line})
        else
            print("\n", .{});

        if (diff) {
            if (self.expect) |txt| print("expect:\n{s}\n", .{txt});
            if (self.got) |txt| print("got:\n{s}\n", .{txt});
        }

        return error.err;
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

    const tester_dir = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(tester_dir);
    print("ExeDir: {s}\n", .{tester_dir});

    const exe_path = try std.fs.path.join(allocator, &[_][]const u8{
        tester_dir, if (builtin.os.tag == .windows) "rover.exe" else "rover",
    });
    defer allocator.free(exe_path);

    var diag: Diagnostic = undefined;
    switch (res.args.stage orelse .all) {
        .all => {
            // try run_stage_tests(allocator, &diag, exe_path, .parser);
            // try run_stage_tests(allocator, &diag, exe_path, .analyzer);
            // try run_stage_tests(allocator, &diag, exe_path, .compiler);
            // try run_vm_tests(allocator, &diag, exe_path);
        },
        .vm => run_vm_tests(allocator, &diag, exe_path) catch return diag.display(diff),
        else => |s| {
            print("Else case: {s}\n", .{@tagName(s)});
            // try run_stage_tests(allocator, &diag, exe_path, s);
        },
    }
}

fn run_vm_tests(allocator: Allocator, diag: *Diagnostic, exe: []const u8) !void {
    const cwd = std.fs.cwd();

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
            const path_to_exe = try std.fs.path.join(allocator, &[_][]const u8{
                "zig-out", "bin", exe,
            });
            defer allocator.free(path_to_exe);

            const file_path = try std.fs.path.join(allocator, &[_][]const u8{
                "tests", "vm", item.path,
            });
            defer allocator.free(file_path);

            const res = std.process.Child.run(.{
                .allocator = allocator,
                .cwd_dir = cwd,
                .argv = &[_][]const u8{
                    path_to_exe,
                    file_path,
                },
            }) catch |e| {
                diag.err_name = @errorName(e);
                diag.file_name = item.path;

                return e;
            };
            defer allocator.free(res.stdout);
            defer allocator.free(res.stderr);

            var got_expects = std.mem.splitScalar(u8, res.stdout, '\n');

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
                    std.testing.expect(std.mem.eql(u8, got, exp)) catch |e| {
                        diag.line = i;
                        diag.expect = exp;
                        diag.got = got;

                        // print("Error in file: {s} line: {}\n", .{ item.path, i });
                        // print("expect:\n{s}\n", .{exp});
                        // print("got:\n{s}\n", .{got});
                        // print("\nStderr: {s}\n", .{res.stderr});
                        return e;
                    };
                }
            }
        }
    }
}

fn run_stage_tests(allocator: Allocator, diag: *Diagnostic, exe_path: []const u8, stage: Stage) !void {
    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "tests", @tagName(stage),
    });
    defer allocator.free(path);

    var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer cwd.close();

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    print("Opening files in dir: {s}\n", .{try std.os.getFdPath(cwd.fd, &buf)});

    var walker = try cwd.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |*entry| {
        if (std.mem.endsWith(u8, entry.basename, ".rvt")) {
            try test_file(allocator, diag, exe_path, &cwd, stage, entry.path);
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

fn test_file(
    allocator: Allocator,
    diag: *Diagnostic,
    exe_path: []const u8,
    dir: *std.fs.Dir,
    stage: Stage,
    file_path: []const u8,
) !void {
    _ = diag;
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
            try code.append(0);

            const tmp_file = try dir.createFile("tmp.rv", .{});
            try tmp_file.writeAll(code.items[0 .. code.items.len - 1 :0]);
            tmp_file.close();
            defer dir.deleteFile("tmp.rv") catch unreachable;

            const exp = if (section == .Expect) expects.items else errors.items;

            run_test(
                allocator,
                exe_path,
                dir,
                stage,
                std.mem.trimRight(u8, exp, "\n"),
                std.mem.trimRight(u8, config_text.items, "\n"),
            ) catch |e| {
                print("Error in test {} in file {s}\n", .{ test_count + 1, file_path });
                print("Error tag: {s}\n\n", .{@errorName(e)});
                return e;
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
    allocator: Allocator,
    diag: *Diagnostic,
    exe_path: []const u8,
    dir: *std.fs.Dir,
    stage: Stage,
    exp: []const u8,
    config: []const u8,
) !void {
    _ = diag;
    const argv = if (std.mem.eql(u8, config, "static-analyzis"))
        &[_][]const u8{
            exe_path,
            "tmp.rv",
            stage_to_opt(stage),
            "-s",
        }
    else
        &[_][]const u8{
            exe_path,
            "tmp.rv",
            stage_to_opt(stage),
        };

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try std.os.getFdPath(dir.fd, &buf);

    const res = std.process.Child.run(.{
        .allocator = allocator,
        .cwd = path,
        .argv = argv,
    }) catch |e| {
        print("Process failure, error tag: {s}\n", .{@errorName(e)});
        return e;
    };
    defer allocator.free(res.stdout);
    defer allocator.free(res.stderr);

    if (res.stderr.len > 0) std.debug.print("Process error executing file: {s}\n", .{res.stderr});

    const got = try clean_text(allocator, res.stdout);
    defer allocator.free(got);

    std.testing.expect(eql(u8, std.mem.trimRight(u8, got, "\n"), exp)) catch |e| {
        const color_dif = try colorize_dif(allocator, exp, got);
        defer allocator.free(color_dif);

        print("expect:\n{s}\n\n", .{exp});
        print("got:\n{s}\n", .{color_dif});
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
