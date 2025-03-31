const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const eql = std.mem.eql;
const builtin = @import("builtin");

const clap = @import("clap");

const Stage = enum { all, parser, analyzer, compiler, vm };

pub fn main() !u8 {
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
        \\-v, --verbose          Show additional data about tests
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
        .STAGE = clap.parsers.enumeration(Stage),
    };

    var clap_diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &clap_diag,
        .allocator = allocator,
    }) catch |err| {
        clap_diag.report(std.io.getStdErr().writer(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});
        return 0;
    }

    const diff = if (res.args.verbose == 1) true else false;

    const tester_dir = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(tester_dir);

    const exe_path = try std.fs.path.join(allocator, &[_][]const u8{
        tester_dir, if (builtin.os.tag == .windows) "rover.exe" else "rover",
    });
    defer allocator.free(exe_path);

    var tester = Tester.init(allocator, exe_path, diff);
    defer tester.deinit();
    const success = try tester.run(res.args.stage orelse .all);

    return if (success) 0 else 1;
}

const Diagnostic = struct {
    category: []const u8,
    err_name: []const u8 = undefined,
    file_name: []const u8 = undefined,
    diff: []const u8 = undefined,

    pub fn display(self: Diagnostic, verbose: bool) void {
        print("  {s}\n", .{self.file_name});

        if (verbose)
            print("{s}\n", .{self.diff});
    }

    pub fn deinit(self: *Diagnostic, allocator: Allocator) void {
        allocator.free(self.file_name);
        allocator.free(self.diff);
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
        self.clear_diags();
        self.diags.deinit();
    }

    fn clear_diags(self: *Self) void {
        for (self.diags.items) |*diag|
            diag.deinit(self.allocator);

        self.diags.clearRetainingCapacity();
    }

    fn run(self: *Self, stage: Stage) !bool {
        var success = false;

        switch (stage) {
            .all => {
                // TODO: for loop
                try self.run_stage(.parser);
                success = self.report(.parser);
                self.clear_diags();

                try self.run_stage(.analyzer);
                success = self.report(.analyzer) or success;
                self.clear_diags();

                try self.run_stage(.compiler);
                success = self.report(.compiler) or success;

                try self.run_stage(.vm);
                success = self.report(.vm);
            },
            else => {
                try self.run_stage(stage);
                success = self.report(stage);
            },
        }

        return success;
    }

    fn report(self: *const Self, stage: Stage) bool {
        if (self.diags.items.len > 0) {
            print("Error in stage {s}:\n", .{@tagName(stage)});

            for (self.diags.items) |diag| {
                print("    category {s:<10} ", .{diag.category});
                diag.display(self.show_diff);
            }

            return false;
        }

        return true;
    }

    fn run_stage(self: *Self, stage: Stage) !void {
        const categories = switch (stage) {
            .analyzer => &[_][]const u8{ "errors", "features", "warnings" },
            .parser => &[_][]const u8{ "errors", "features" },
            else => &[_][]const u8{"features"},
        };

        for (categories) |category| {
            const path = try std.fs.path.join(self.allocator, &[_][]const u8{ "tests", @tagName(stage), category });
            defer self.allocator.free(path);

            var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
            defer cwd.close();

            var walker = try cwd.walk(self.allocator);
            defer walker.deinit();

            while (try walker.next()) |entry| {
                if (std.mem.endsWith(u8, entry.basename, ".rv")) {
                    self.test_file(&cwd, stage, entry, category) catch continue;
                }
            }
        }
    }

    fn test_file(self: *Self, dir: *std.fs.Dir, stage: Stage, entry: std.fs.Dir.Walker.Entry, category: []const u8) !void {
        var no_ext = std.mem.splitScalar(u8, entry.basename, '.');
        const name = no_ext.next().?;
        var output_file = try self.allocator.alloc(u8, name.len + 4);
        defer self.allocator.free(output_file);
        @memcpy(output_file[0..name.len], name);
        @memcpy(output_file[name.len..], ".out");

        // std.debug.print("Entry path: {s}\n", .{entry.path});
        // std.debug.print("Output file: {s}\n", .{output_file});
        // const output = std.fs.path.join(allocator, entry.path, entry.basename);

        const argv = if (stage == .vm)
            &[_][]const u8{ self.exe_path, entry.basename }
        else if (eql(u8, category, "warnings"))
            &[_][]const u8{ self.exe_path, entry.basename, stage_to_opt(stage), "-s" }
        else
            &[_][]const u8{ self.exe_path, entry.basename, stage_to_opt(stage) };

        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const path = try std.os.getFdPath(dir.fd, &buf);
        // std.debug.print("Exe path: {s}\n", .{self.exe_path});
        // std.debug.print("Cwd: {s}\n", .{path});
        // std.debug.print("Argv: {s}\n", .{argv});

        const res = std.process.Child.run(.{
            .allocator = self.allocator,
            .cwd = path,
            .argv = argv,
        }) catch |e| {
            print("Error launching rover process: {s}, argv: {s}\n", .{ @errorName(e), argv });
            return e;
        };
        defer self.allocator.free(res.stdout);
        defer self.allocator.free(res.stderr);

        const got = try self.clean_text(res.stdout);
        defer self.allocator.free(got);

        const file = dir.openFile(output_file, .{ .mode = .read_only }) catch |err| {
            print("Error: {s}, unable to open file at: {s}\n", .{ @errorName(err), output_file });
            std.process.exit(0);
        };
        defer file.close();

        // The file has a new line inserted by default
        const size = try file.getEndPos();
        const expect: []u8 = try self.allocator.alloc(u8, size);
        defer self.allocator.free(expect);

        _ = try file.readAll(expect);
        const clean_expect = try self.clean_text(expect);
        defer self.allocator.free(clean_expect);

        std.testing.expect(eql(u8, std.mem.trimRight(u8, got, "\n"), std.mem.trimRight(u8, clean_expect, "\n"))) catch |e| {
            // std.debug.print("Expect: --{s}--\n", .{clean_expect});
            // std.debug.print("Got: --{s}--\n", .{got});
            try self.diags.append(.{
                .category = category,
                .diff = try self.colorized_dif(clean_expect, got),
                .file_name = try self.allocator.dupe(u8, entry.basename),
            });
            return e;
        };
    }

    fn clean_text(self: *const Self, text: []const u8) ![]const u8 {
        var res = std.ArrayList(u8).init(self.allocator);
        var lines = std.mem.splitScalar(u8, text, '\n');

        while (lines.next()) |line| {
            // Skip comments
            if (std.mem.startsWith(u8, std.mem.trimLeft(u8, line, " "), "//")) continue;

            // If after removing the \r there is only spaces, we skip it
            const trimmed = std.mem.trimRight(u8, line, "\r");
            if (std.mem.trimRight(u8, trimmed, " ").len == 0) continue;

            try res.appendSlice(trimmed);
            try res.append('\n');
        }

        return res.toOwnedSlice();
    }

    fn colorized_dif(self: *Self, expect: []const u8, got: []const u8) ![]const u8 {
        const RED = "\x1b[31m";
        const NORMAL = "\x1b[0m";

        var err = false;
        var res = std.ArrayList(u8).init(self.allocator);

        // If we got nothing, we put everything in red
        if (got.len == 0)
            try res.appendSlice(RED);

        for (expect, 0..) |c, i| {
            if (i < got.len and c != got[i] and !err) {
                err = true;
                try res.appendSlice(RED);
            }

            try res.append(c);
        }
        try res.appendSlice(NORMAL);

        return res.toOwnedSlice();
    }
};

fn stage_to_opt(stage: Stage) []const u8 {
    return switch (stage) {
        .parser => "--print-ast",
        .analyzer => "--print-ir",
        .compiler => "--print-bytecode",
        else => unreachable,
    };
}
