const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const clap = @import("clap");

const Vm = @import("runtime/Vm.zig");
const oom = @import("utils.zig").oom;

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

    var vm: Vm = .empty;
    vm.init(allocator, config);
    defer vm.deinit();

    if (res.positionals[0]) |f| {
        const file = std.fs.cwd().openFile(f, .{ .mode = .read_only }) catch |err| {
            var buf: [500]u8 = undefined;
            _ = try std.fmt.bufPrint(&buf, "Error: {}, unable to open file at: {s}\n", .{ err, f });
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

        try vm.run(f, zt);
    } else {
        try vm.runRepl();
    }
}
