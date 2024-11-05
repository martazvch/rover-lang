const std = @import("std");
const builtin = @import("builtin");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const clap = @import("clap");
const Lexer = @import("lexer.zig").Lexer;
const TokenKind = @import("lexer.zig").TokenKind;
const Report = @import("reporter.zig").Report;
const ReportLevel = Report.Level;
const Reporter = @import("reporter.zig").Reporter;

pub fn main() !void {
    if (builtin.os.tag == .windows) {
        // NOTE: it changes for the whole execution, so some characters won't
        // be printed out correctly. Maybe change just before / after printing
        // with 'GetConsoleOutputCP' and changing back to it after
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-f, --file <FILE>       Path to the file to execute
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    // Initialize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        // Report useful error and exit
        diag.report(std.io.getStdErr().writer(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0) return clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});

    if (res.args.file) |f| {
        try run_file(allocator, f);
    } else {
        try repl(allocator);
    }
}

fn run_file(allocator: Allocator, filename: []const u8) !void {
    const file = std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch |err| {
        var buf: [500]u8 = undefined;
        _ = try std.fmt.bufPrint(&buf, "Error: {}, unable to open file at: {s}\n", .{ err, filename });
        print("{s}", .{buf});
        std.process.exit(0);
    };
    defer file.close();

    const size = try file.getEndPos();

    const buf = try allocator.alloc(u8, size);
    defer allocator.free(buf);

    _ = try file.readAll(buf);
}

fn repl(allocator: Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    var lexer = Lexer.new();

    _ = try stdout.write("\t\tGlyph language REPL\n");

    while (true) {
        _ = try stdout.write("\n> ");

        input.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);
        // const trimmed = std.mem.trimRight(u8, input.items, "\r");

        try input.append('\n');

        lexer.init(input.items);

        while (true) {
            const tk = lexer.next();

            if (tk.kind == .Eof) break;

            if (tk.kind == .Error) {
                const reporter = Reporter.init(input.items);
                const report = Report.new(
                    ReportLevel.Error,
                    "unterminated string",
                    tk.start,
                    tk.lexeme.len,
                    "close the opening one",
                );
                try reporter.report_all(&[1]Report{report});
            }
        }
    }
}
