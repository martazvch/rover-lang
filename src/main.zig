const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const clap = @import("clap");

const oom = @import("misc").oom;
const State = @import("core/pipeline/State.zig");
const rover = @import("commands/rover.zig");
const compile = @import("commands/compile.zig");

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const allocator, const is_debug = gpa: {
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        std.debug.assert(debug_allocator.deinit() == .ok);
    };

    var iter = try std.process.ArgIterator.initWithAllocator(allocator);
    defer iter.deinit();

    _ = iter.next();

    const Commands = enum { compile, fmt };

    const parsers = comptime .{
        .FILE = clap.parsers.string,
        .command = clap.parsers.enumeration(Commands),
    };
    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit
        \\<FILE>                 Path to the file to execute
        \\<command>              Command to execute (compile | fmt)
        \\-s, --static-analyzis  Statically checks the file without running it (shows warnings)
        \\--print-ast            Prints the AST
        \\--print-bytecode       Prints the compiled bytecode
        \\--print-ir             Prints the extra infos on AST
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, parsers, &iter, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        diag.reportToFile(std.fs.File.stderr(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0) return clap.helpToFile(std.fs.File.stderr(), clap.Help, &params, .{});

    const config: State.Config = .{
        .print_ast = res.args.@"print-ast" == 1,
        .print_bytecode = res.args.@"print-bytecode" == 1,
        .static_analyzis = res.args.@"static-analyzis" == 1,
        .print_ir = res.args.@"print-ir" == 1,
        .embedded = res.positionals[0] == null,
    };

    const first_arg = res.positionals[0] orelse {
        std.debug.print("Error: Expected a command or file.\n", .{});
        return;
    };

    const command: ?Commands = std.meta.stringToEnum(Commands, first_arg);

    if (command) |cmd| {
        switch (cmd) {
            .compile => try compile.run(allocator, &iter),
            .fmt => {},
        }
    } else if (res.positionals[0]) |f| {
        try rover.run(allocator, f, config);
    } else {
        // var repl: Repl = undefined;
        // defer repl.deinit(allocator);
        // repl.init(allocator, config);
        // try repl.run();
    }
}

test {
    _ = @import("core/parser/Lexer.zig");
    _ = @import("core/analyzer/types.zig");
}
