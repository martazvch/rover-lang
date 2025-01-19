const std = @import("std");
const builtin = @import("builtin");
const test_config = @import("test_config");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const clap = @import("clap");
const run = @import("pipeline.zig").run;
const ReplPipeline = @import("pipeline.zig").ReplPipeline;

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
        \\-s, --static-analyzis  Statically checks the file without running it (shows warnings)
        \\--print-ast            Prints the AST
        \\--print-bytecode       Prints the compiled bytecode
        \\--print-analyzed-ast   Prints the extra infos on AST
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
    const static_analyzis = if (res.args.@"static-analyzis" == 1) true else false;
    const print_analyzed_ast = if (res.args.@"print-analyzed-ast" == 1) true else false;

    if (res.args.file) |f| {
        try run_file(allocator, f, print_ast, print_bytecode, static_analyzis, print_analyzed_ast);
    } else {
        // try repl(allocator, print_ast, print_bytecode, static_analyzis, print_analyzed_ast);
    }
}

fn run_file(
    allocator: Allocator,
    filename: []const u8,
    print_ast: bool,
    print_bytecode: bool,
    static_analyzis: bool,
    print_analyzed_ast: bool,
) !void {
    const file = std.fs.cwd().openFile(filename, .{ .mode = .read_only }) catch |err| {
        var buf: [500]u8 = undefined;
        _ = try std.fmt.bufPrint(&buf, "Error: {}, unable to open file at: {s}\n", .{ err, filename });
        print("{s}", .{buf});
        std.process.exit(0);
    };
    defer file.close();

    // The file has a new line inserted by default
    const size = try file.getEndPos();
    const buf = try allocator.alloc(u8, size + 1);
    defer allocator.free(buf);

    _ = try file.readAll(buf);
    buf[size] = 0;
    const zt = buf[0..size :0];

    try run(
        allocator,
        .{
            .print_ast = print_ast,
            .print_bytecode = print_bytecode,
            .static_analyzis = static_analyzis,
            .print_analyzed_ast = print_analyzed_ast,
        },
        filename,
        zt,
    );
}

fn repl(
    allocator: Allocator,
    print_ast: bool,
    print_bytecode: bool,
    static_analyzis: bool,
    print_analyzed_ast: bool,
) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    // Keep all prompts because all of the pipeline uses []const u8 wich point
    // to the user input
    var prompts = std.ArrayList([]const u8).init(allocator);
    defer prompts.deinit();

    var input = std.ArrayList(u8).init(allocator);

    var pipeline = try ReplPipeline.new(allocator, .{
        .print_ast = print_ast,
        .print_bytecode = print_bytecode,
        .static_analyzis = static_analyzis,
        .print_analyzed_ast = print_analyzed_ast,
    });
    try pipeline.init();
    defer pipeline.deinit();

    _ = try stdout.write("\t\tRover language REPL\n");

    while (true) {
        _ = try stdout.write("\n> ");

        input.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);

        // For ease of use of the parser
        try input.append('\n');
        try input.append(0);

        const zt = input.items[0 .. input.items.len - 1 :0];
        try prompts.append(try input.toOwnedSlice());

        try pipeline.run("stdin", zt);
    }
}

test {
    const stage = test_config.stage;

    if (stage == .parser or stage == .all) {
        _ = @import("frontend/parser.zig");
    }

    if (stage == .analyzer or stage == .all) {
        _ = @import("frontend/analyzer.zig");
    }

    if (stage == .compiler or stage == .all) {
        _ = @import("backend/compiler.zig");
    }

    if (stage == .vm or stage == .all) {
        _ = @import("runtime/vm.zig");
    }

    _ = @import("frontend/lexer.zig");
    _ = @import("runtime/table.zig");
    _ = @import("frontend/type_system.zig");
}
