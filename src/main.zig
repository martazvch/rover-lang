const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const clap = @import("clap");

const oom = @import("misc").oom;
const Pipeline = @import("Pipeline.zig");
const Repl = @import("commands/repl/Repl.zig");
const Vm = @import("core/runtime/Vm.zig");

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
        .allocator = allocator,
    }) catch |err| {
        diag.reportToFile(std.fs.File.stderr(), err) catch {};
        std.process.exit(0);
    };
    defer res.deinit();

    if (res.args.help != 0) return clap.helpToFile(std.fs.File.stderr(), clap.Help, &params, .{});

    const config: Vm.Config = .{
        .print_ast = res.args.@"print-ast" == 1,
        .print_bytecode = res.args.@"print-bytecode" == 1,
        .static_analyzis = res.args.@"static-analyzis" == 1,
        .print_ir = res.args.@"print-ir" == 1,
        .embedded = res.positionals[0] == null,
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

        var arena = std.heap.ArenaAllocator.init(allocator);
        const arena_alloc = arena.allocator();
        defer arena.deinit();

        var ctx: Pipeline.Context = .new(arena_alloc, config);
        var pipeline: Pipeline = .init(arena_alloc, &vm, &ctx);

        const module = pipeline.run(f, ".", buf) catch |e| switch (e) {
            error.ExitOnPrint => return,
            else => return e,
        };

        try vm.run(module, ctx.module_interner.compiled.values());
    } else {
        // var repl: Repl = undefined;
        // defer repl.deinit(allocator);
        // repl.init(allocator, config);
        // try repl.run();
    }
}

test {
    _ = @import("core/analyzer/types.zig");
}
