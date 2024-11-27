const std = @import("std");
const builtin = @import("builtin");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const clap = @import("clap");
const Pipeline = @import("pipeline.zig").Pipeline;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit
        \\-f, --file <FILE>      Path to the file to execute
        \\-s, --static-analysis Statically checks the file without running it (shows warnings)
        \\--print-ast            Prints the AST
        \\--print-bytecode       Prints the compiled bytecode
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

    const print_ast = if (res.args.@"print-ast" == 1) true else false;
    const print_bytecode = if (res.args.@"print-bytecode" == 1) true else false;
    const static_analysis = if (res.args.@"static-analysis" == 1) true else false;

    if (res.args.file) |f| {
        try run_file(allocator, f, print_ast, print_bytecode, static_analysis);
    } else {
        try repl(allocator, print_ast, print_bytecode, static_analysis);
    }
}

fn run_file(
    allocator: Allocator,
    filename: []const u8,
    print_ast: bool,
    print_bytecode: bool,
    static_analysis: bool,
) !void {
    const file = std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch |err| {
        var buf: [500]u8 = undefined;
        _ = try std.fmt.bufPrint(&buf, "Error: {}, unable to open file at: {s}\n", .{ err, filename });
        print("{s}", .{buf});
        std.process.exit(0);
    };
    defer file.close();

    const size = try file.getEndPos();
    const buf = try allocator.alloc(u8, size + 1);
    defer allocator.free(buf);

    _ = try file.readAll(buf);
    buf[size] = 0;
    const zt = buf[0.. :0];

    var pipeline = Pipeline.init(allocator, .{
        .print_ast = print_ast,
        .print_bytecode = print_bytecode,
        .static_analysis = static_analysis,
    });
    defer pipeline.deinit();
    try pipeline.run(filename, zt);
}

fn repl(
    allocator: Allocator,
    print_ast: bool,
    print_bytecode: bool,
    static_analysis: bool,
) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    var pipeline = Pipeline.init(allocator, .{
        .print_ast = print_ast,
        .print_bytecode = print_bytecode,
        .static_analysis = static_analysis,
    });
    defer pipeline.deinit();

    _ = try stdout.write("\t\tRover language REPL\n");

    while (true) {
        _ = try stdout.write("\n> ");

        input.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);
        // const trimmed = std.mem.trimRight(u8, input.items, "\r");

        pipeline.reinit_frontend();

        try input.append(0);
        const zt = input.items[0 .. input.items.len - 1 :0];
        try pipeline.run("stdin", zt);
    }
}

test {
    _ = @import("frontend/lexer.zig");
    _ = @import("frontend/parser.zig");
}
