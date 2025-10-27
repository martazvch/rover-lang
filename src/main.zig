const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const clarg = @import("clarg");
const Arg = clarg.Arg;

const oom = @import("misc").oom;
const State = @import("core/pipeline/State.zig");
const rover = @import("commands/rover.zig");
const compile = @import("commands/compile.zig");

const Args = struct {
    file: Arg(.string) = .{ .desc = "Path to the file to run", .positional = true },
    print_ast: Arg(bool) = .{ .desc = "Prints the AST" },
    print_ir: Arg(bool) = .{ .desc = "Prints the IR" },
    print_bytecode: Arg(bool) = .{ .desc = "Prints the compiled bytecode" },
    static_analyzis: Arg(bool) = .{ .desc = "Statically checks the file without running it (shows warnings)", .short = 's' },

    compile: Arg(compile.Args) = .{ .desc = "Compiles to a native executable (WIP)", .short = 'c' },

    help: Arg(bool) = .{ .desc = "Prints this help and exit", .short = 'h' },

    pub const description: []const u8 =
        \\Interpreter for Rover language. You can either run a file with the available options
        \\or run a command.
        \\If no arguments are provided, runs the REPL.
    ;
};

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

    var diag: clarg.Diag = undefined;
    const parsed = clarg.parse("rover", Args, &iter, &diag, .{}) catch |e| {
        try diag.reportToFile(.stderr());
        return e;
    };

    if (parsed.help) {
        return clarg.helpToFile(Args, .stderr());
    }

    const config: State.Config = .{
        .print_ast = parsed.print_ast,
        .print_bytecode = parsed.print_bytecode,
        .static_analyzis = parsed.static_analyzis,
        .print_ir = parsed.print_ir,
        .embedded = parsed.file == null,
    };

    if (parsed.compile) |cmd| {
        try compile.run(allocator, cmd);
    } else if (parsed.file) |f| {
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
