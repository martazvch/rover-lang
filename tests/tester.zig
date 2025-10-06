const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const print = std.debug.print;
const eql = std.mem.eql;
const builtin = @import("builtin");

const clap = @import("clap");

const Stage = enum {
    all,
    parser,
    analyzer,
    compiler,
    vm,
    standalone,

    pub fn toOpt(self: Stage) []const u8 {
        return switch (self) {
            .parser => "--print-ast",
            .analyzer => "--print-ir",
            .compiler => "--print-bytecode",
            else => unreachable,
        };
    }
};
const Config = struct {
    stage: Stage = .all,
    show_diff: bool = false,
    show_got: bool = false,
    file: ?[]const u8 = null,
};

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }
    const allocator = gpa.allocator();
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit
        \\-f, --file <FILE>      File to test (without '.rv' extension)
        \\-s, --stage <STAGE>    Which stage to test [default: all]
        \\-d, --diff             Shows a colored diff
        \\-g, --got              Prints what we got
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
        clap_diag.reportToFile(std.fs.File.stderr(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.helpToFile(std.fs.File.stderr(), clap.Help, &params, .{});
        return 0;
    }

    var config: Config = .{};
    config.show_diff = if (res.args.diff == 1) true else false;
    config.show_got = if (res.args.got == 1) true else false;
    config.stage = res.args.stage orelse .all;
    config.file = res.args.file;

    const tester_dir = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(tester_dir);

    const exe_path = try std.fs.path.join(allocator, &[_][]const u8{
        tester_dir, if (builtin.os.tag == .windows) "rover.exe" else "rover",
    });
    defer allocator.free(exe_path);

    var tester = Tester.init(allocator, exe_path, config);
    defer tester.deinit();
    const success = try tester.run();

    return if (success) 0 else 1;
}

const Diagnostic = struct {
    category: []const u8,
    err_name: []const u8 = undefined,
    file_name: []const u8 = undefined,
    diff: []const u8 = undefined,
    got: []const u8 = undefined,

    pub fn display(self: Diagnostic, show_diff: bool, show_got: bool) void {
        print("  {s}\n", .{self.file_name});

        if (show_diff) print("{s}\n", .{self.diff});
        if (show_got) print("{s}\n", .{self.got});
    }

    pub fn deinit(self: *Diagnostic, allocator: Allocator) void {
        allocator.free(self.file_name);
        allocator.free(self.diff);
        allocator.free(self.got);
    }
};

const Tester = struct {
    allocator: Allocator,
    diags: ArrayList(Diagnostic),
    exe_path: []const u8,
    config: Config,

    const Self = @This();

    pub fn init(allocator: Allocator, exe_path: []const u8, config: Config) Self {
        return .{
            .allocator = allocator,
            .diags = .empty,
            .exe_path = exe_path,
            .config = config,
        };
    }

    pub fn deinit(self: *Self) void {
        self.clearDiags();
        self.diags.deinit(self.allocator);
    }

    fn clearDiags(self: *Self) void {
        for (self.diags.items) |*diag| {
            diag.deinit(self.allocator);
        }
        self.diags.clearRetainingCapacity();
    }

    fn run(self: *Self) !bool {
        var success = false;

        switch (self.config.stage) {
            .all => {
                try self.runStage(.parser);
                success = self.report(.parser);
                self.clearDiags();

                try self.runStage(.analyzer);
                success = self.report(.analyzer) and success;
                self.clearDiags();

                try self.runStage(.compiler);
                success = self.report(.compiler) and success;
                self.clearDiags();

                try self.runStage(.vm);
                success = self.report(.vm) and success;
                self.clearDiags();

                try self.runStandalone();
                success = self.report(.standalone) and success;
            },
            .standalone => {
                try self.runStandalone();
                success = self.report(.standalone) and success;
            },
            else => |s| {
                try self.runStage(s);
                success = self.report(s);
            },
        }

        return success;
    }

    fn report(self: *const Self, stage: Stage) bool {
        if (self.diags.items.len > 0) {
            print("Error in stage {t}:\n", .{stage});

            for (self.diags.items) |diag| {
                print("    category {s:<10} ", .{diag.category});
                diag.display(self.config.show_diff, self.config.show_got);
            }

            return false;
        }

        return true;
    }

    fn runStandalone(self: *Self) !void {
        const path = try std.fs.path.join(self.allocator, &.{ "tests", "standalones" });
        defer self.allocator.free(path);

        var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
        defer cwd.close();

        var walker = try cwd.walk(self.allocator);
        defer walker.deinit();
        var specific_tested = false;

        while (try walker.next()) |entry| {
            if (entry.kind != .directory) continue;

            // If specific file asked
            if (self.config.file) |f| {
                if (!std.mem.eql(u8, entry.basename, f)) {
                    continue;
                }
                specific_tested = true;
            }
            cwd = try cwd.openDir(entry.path, .{});
            defer cwd = cwd.openDir("..", .{}) catch unreachable;

            var buf: [1024]u8 = undefined;
            self.testFile(&cwd, .standalone, try std.fmt.bufPrint(&buf, "{s}.rv", .{entry.basename}), "features") catch continue;
        }

        if (self.config.file != null and !specific_tested) {
            print("Failed to test specific standalone: '{s}'\n", .{self.config.file.?});
        }
    }

    fn runStage(self: *Self, stage: Stage) !void {
        const categories = switch (stage) {
            .analyzer => &[_][]const u8{ "errors", "features", "warnings" },
            .parser => &[_][]const u8{ "errors", "features" },
            else => &[_][]const u8{"features"},
        };

        for (categories) |category| {
            const path = try std.fs.path.join(self.allocator, &.{ "tests", @tagName(stage), category });
            defer self.allocator.free(path);

            var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
            defer cwd.close();

            var walker = try cwd.walk(self.allocator);
            defer walker.deinit();
            var specific_tested = false;

            while (try walker.next()) |entry| {
                // File ending with .rv and not a child of current directory
                if (std.mem.endsWith(u8, entry.basename, ".rv") and entry.dir.fd == cwd.fd) {
                    // If specific file asked
                    if (self.config.file) |f| {
                        // Check without extension
                        if (!std.mem.eql(u8, entry.basename[0 .. entry.basename.len - 3], f)) {
                            continue;
                        }
                        specific_tested = true;
                    }
                    self.testFile(&cwd, stage, entry.basename, category) catch continue;
                }
            }

            if (self.config.file != null and !specific_tested) {
                print("Failed to test specific file: '{s}' in category: {s}\n", .{ self.config.file.?, category });
            }
        }
    }

    fn testFile(self: *Self, dir: *std.fs.Dir, stage: Stage, file_name: []const u8, category: []const u8) !void {
        var no_ext = std.mem.splitScalar(u8, file_name, '.');
        const name = no_ext.next().?;
        var output_file = self.allocator.alloc(u8, name.len + 4) catch oom();
        defer self.allocator.free(output_file);
        @memcpy(output_file[0..name.len], name);
        @memcpy(output_file[name.len..], ".out");

        const argv = if (stage == .vm or stage == .standalone)
            &[_][]const u8{ self.exe_path, file_name }
        else if (eql(u8, category, "warnings"))
            &[_][]const u8{ self.exe_path, file_name, stage.toOpt(), "-s" }
        else
            &[_][]const u8{ self.exe_path, file_name, stage.toOpt() };

        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const path = try std.os.getFdPath(dir.fd, &buf);

        const res = std.process.Child.run(.{
            .allocator = self.allocator,
            .cwd = path,
            .argv = argv,
        }) catch |e| {
            print("Error launching rover process: {t}, with args:\n", .{e});
            for (argv) |arg| print("\t{s}\n", .{arg});
            return e;
        };
        defer self.allocator.free(res.stdout);
        defer self.allocator.free(res.stderr);

        const got = self.cleanText(if (eql(u8, category, "features")) res.stdout else res.stderr) catch oom();
        defer self.allocator.free(got);

        const file = dir.openFile(output_file, .{ .mode = .read_only }) catch |err| {
            print("Error: {t}, unable to open file at: {s}\n", .{ err, output_file });
            std.process.exit(0);
        };
        defer file.close();

        // The file has a new line inserted by default
        const size = try file.getEndPos();
        const expect: []u8 = self.allocator.alloc(u8, size) catch oom();
        defer self.allocator.free(expect);

        _ = try file.readAll(expect);
        const clean_expect = self.cleanText(expect) catch oom();
        defer self.allocator.free(clean_expect);

        std.testing.expect(eql(u8, std.mem.trimRight(u8, got, "\n"), std.mem.trimRight(u8, clean_expect, "\n"))) catch |e| {
            try self.diags.append(self.allocator, .{
                .category = category,
                .file_name = self.allocator.dupe(u8, file_name) catch oom(),
                .diff = self.colorizedDiff(clean_expect, got) catch oom(),
                .got = self.allocator.dupe(u8, std.mem.trimRight(u8, got, "\n")) catch oom(),
            });
            return e;
        };
    }

    fn cleanText(self: *const Self, text: []const u8) Allocator.Error![]const u8 {
        var res: ArrayList(u8) = .empty;
        var lines = std.mem.splitScalar(u8, text, '\n');

        while (lines.next()) |line| {
            // Skip comments
            if (std.mem.startsWith(u8, std.mem.trimLeft(u8, line, " "), "//")) continue;

            // If after removing the \r there is only spaces, we skip it
            const trimmed = std.mem.trimRight(u8, line, "\r");
            if (std.mem.trimRight(u8, trimmed, " ").len == 0) continue;

            try res.appendSlice(self.allocator, trimmed);
            try res.append(self.allocator, '\n');
        }

        return res.toOwnedSlice(self.allocator);
    }

    fn colorizedDiff(self: *Self, expect: []const u8, got: []const u8) Allocator.Error![]const u8 {
        const RED = "\x1b[31m";
        const NORMAL = "\x1b[0m";

        var err = false;
        var res: ArrayList(u8) = .empty;

        // If we got nothing, we put everything in red
        if (got.len == 0)
            try res.appendSlice(self.allocator, RED);

        for (expect, 0..) |c, i| {
            if (i < got.len and c != got[i] and !err) {
                err = true;
                try res.appendSlice(self.allocator, RED);
            }

            try res.append(self.allocator, c);
        }
        try res.appendSlice(self.allocator, NORMAL);

        return res.toOwnedSlice(self.allocator);
    }
};

pub fn oom() noreturn {
    std.debug.print("out of memory\n", .{});
    std.process.exit(1);
}
