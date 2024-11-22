const std = @import("std");
const builtin = @import("builtin");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const clap = @import("clap");
const Reporter = @import("reporter.zig").Reporter;
const Parser = @import("frontend/parser.zig").Parser;
const AstPrinter = @import("frontend/ast_print.zig").AstPrinter;
const Compiler = @import("backend/compiler.zig").Compiler;
const Disassembler = @import("backend/disassembler.zig").Disassembler;
const Vm = @import("runtime/vm.zig").Vm;
const Analyzer = @import("frontend/analyzer.zig").Analyzer;

pub fn main() !void {
    // if (builtin.os.tag == .windows) {
    //     // NOTE: it changes for the whole execution, so some characters won't
    //     // be printed out correctly. Maybe change just before / after printing
    //     // with 'GetConsoleOutputCP' and changing back to it after
    //     _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    // }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit
        \\-f, --file <FILE>      Path to the file to execute
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

    if (res.args.file) |f| {
        try run_file(allocator, f, print_ast, print_bytecode);
    } else {
        try repl(allocator, print_ast, print_bytecode);
    }
}

fn run_file(
    allocator: Allocator,
    filename: []const u8,
    print_ast: bool,
    print_bytecode: bool,
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

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();

    try parser.parse(zt);

    if (parser.errs.items.len > 0) {
        var reporter = Reporter.init(zt, parser.tokens.items);
        try reporter.report_all(filename, parser.errs.items);
        return;
    }

    if (print_ast) {
        var ast_printer = AstPrinter.init(allocator);
        defer ast_printer.deinit();

        try ast_printer.parse_ast(buf, parser.stmts.items);
        ast_printer.display();
    }

    var analyzer = Analyzer.init(allocator);
    defer analyzer.deinit();
    try analyzer.analyze(parser.stmts.items);

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();
    try compiler.compile(parser.stmts.items);

    if (print_bytecode) {
        var dis = Disassembler.init(&compiler.chunk);
        try dis.dis_chunk("main");
    }

    var vm = Vm.new(allocator, &compiler.chunk);
    vm.init();
    defer vm.deinit();
    try vm.run();
}

fn repl(allocator: Allocator, print_ast: bool, print_bytecode: bool) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input = std.ArrayList(u8).init(allocator);
    defer input.deinit();

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();

    var ast_printer = AstPrinter.init(allocator);
    defer ast_printer.deinit();

    _ = try stdout.write("\t\tRover language REPL\n");

    while (true) {
        _ = try stdout.write("\n> ");

        input.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);
        // const trimmed = std.mem.trimRight(u8, input.items, "\r");

        parser.reinit();

        try input.append(0);
        const zt = input.items[0 .. input.items.len - 1 :0];
        try parser.parse(zt);

        if (parser.errs.items.len > 0) {
            var reporter = Reporter.init(zt, parser.tokens.items);
            try reporter.report_all("stdin", parser.errs.items);
            continue;
        }

        if (print_ast) {
            ast_printer.reinit();
            try ast_printer.parse_ast(input.items, parser.stmts.items);
            ast_printer.display();
        }

        var compiler = Compiler.init(allocator);
        defer compiler.deinit();
        try compiler.compile(parser.stmts.items);

        if (print_bytecode) {
            var dis = Disassembler.init(&compiler.chunk);
            try dis.dis_chunk("main");
        }

        var vm = Vm.new(allocator, &compiler.chunk);
        vm.init();
        defer vm.deinit();
        try vm.run();
    }
}

test {
    _ = @import("frontend/lexer.zig");
    _ = @import("frontend/parser.zig");
}
