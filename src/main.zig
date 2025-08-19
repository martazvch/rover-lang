const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const clap = @import("clap");

const oom = @import("utils.zig").oom;
const Pipeline = @import("Pipeline.zig");
const Repl = @import("runtime/Repl.zig");
const Vm = @import("runtime/Vm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        std.debug.assert(status == .ok);
    }
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit
        \\<FILE>                 Path to the file to execute
        \\-s, --static-analyzis  Statically checks the file without running it (shows warnings)
        \\--print-ast            Prints the AST
        \\--print-bytecode       Prints the compiled bytecode
        \\--print-ir             Prints the extra infos on AST
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0) return clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});

    const config: Vm.Config = .{
        .print_ast = if (res.args.@"print-ast" == 1) true else false,
        .print_bytecode = if (res.args.@"print-bytecode" == 1) true else false,
        .static_analyzis = if (res.args.@"static-analyzis" == 1) true else false,
        .print_ir = if (res.args.@"print-ir" == 1) true else false,
        .embedded = if (res.positionals[0] == null) true else false,
    };

    if (res.positionals[0]) |f| {
        const file = std.fs.cwd().openFile(f, .{ .mode = .read_only }) catch |err| {
            // TODO: Rover error
            print("Error: {}, unable to open file at: {s}\n", .{ err, f });
            std.process.exit(0);
        };
        defer file.close();

        // The file has a new line inserted by default
        const size = try file.getEndPos();
        const buf = try allocator.allocSentinel(u8, size, 0);
        defer allocator.free(buf);
        _ = try file.readAll(buf);

        var vm: Vm = undefined;
        vm.init(allocator, config);
        defer vm.deinit();

        var ctx: Pipeline.Context = .new(allocator, config);

        var pipeline: Pipeline = undefined;
        defer pipeline.deinit();
        pipeline.init(&vm, &ctx);

        var module = pipeline.run(f, buf) catch |e| switch (e) {
            error.ExitOnPrint => return,
            else => return e,
        };
        defer module.compiled.deinit(vm.allocator);

        try vm.run(module.compiled);
    } else {
        var repl: Repl = undefined;
        defer repl.deinit(allocator);
        repl.init(allocator, config);
        try repl.run();
    }
}
