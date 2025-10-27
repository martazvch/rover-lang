const std = @import("std");
const Allocator = std.mem.Allocator;

const clarg = @import("clarg");
const Arg = clarg.Arg;
const Ast = @import("../core/parser/Ast.zig");
const Pipeline = @import("../core/pipeline/Pipeline.zig");
const Vm = @import("../core/runtime/Vm.zig");
const State = @import("../core/pipeline/State.zig");

const file = @import("misc").file;
const oom = @import("misc").oom;

indent_level: usize,
output: std.ArrayList(u8),
writer: std.ArrayList(u8).Writer,
ast: Ast,

const Self = @This();
const Error = std.ArrayList(u8).Writer.Error;
const spaces: []const u8 = " " ** 1024;
const INDENT_SIZE = 4;

pub const Args = struct {
    file: Arg(.string) = .{ .desc = "Path to the file to compile", .positional = true },
    out: Arg("out.e") = .{ .desc = "Output's name", .short = 'o' },
    help: Arg(bool) = .{ .desc = "Prints this help and exit", .short = 'h' },

    pub const description: []const u8 = "Compiles to a native executable";
};

pub fn run(allocator: Allocator, args: clarg.ParsedArgs(Args)) !void {
    var transpiler: Self = .{ .output = .empty, .writer = undefined, .ast = undefined, .indent_level = 0 };
    try transpiler.runPipeline(allocator, args);
}

pub fn runPipeline(self: *Self, allocator: Allocator, args: anytype) !void {
    if (args.help) return clarg.helpToFile(Args, .stderr());

    const file_path = args.file orelse {
        std.debug.print("Error: Expected as file to compile.\n", .{});
        return;
    };

    const file_content = try file.read(allocator, file_path);
    defer allocator.free(file_content);

    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    var state: State = .new(arena_alloc, .{});
    state.registerNatives(arena_alloc, @import("../core/builtins/builtins.zig"));

    var vm: Vm = undefined;
    vm.init(allocator, &state);
    defer vm.deinit();

    var pipeline: Pipeline = .init(arena_alloc, &vm, &state);

    _ = pipeline.run(file_path, ".", file_content) catch |e| switch (e) {
        error.ExitOnPrint => return,
        else => return e,
    };

    self.ast = try pipeline.parse(file_path, file_content);
    try self.compile(allocator, "out.zig");
}

fn compile(self: *Self, allocator: Allocator, output: []const u8) !void {
    _ = output; // autofix
    try self.output.appendSlice(allocator, "pub fn main() !void {");
    defer {
        self.indent_level = 0;
        self.output.appendSlice(allocator, "}") catch oom();
    }

    for (self.ast.nodes) |node| {
        try self.transpileNode(node);
    }

    std.log.debug("Transpiled:\n{s}", .{self.output.items});
}

fn transpileNode(self: *Self, node: Ast.Node) !void {
    switch (node) {
        .var_decl => |n| {
            if (n.typ) |ty| {
                _ = ty; // autofix
            } else {
                @panic("Variable must have an explicit type for transpilation");
            }
        },
        .expr => |expr| try self.transpileExpr(expr.*),
        else => unreachable,
    }
}

fn transpileExpr(self: *Self, expr: Ast.Expr) !void {
    _ = self; // autofix
    switch (expr) {
        .literal => |e| {
            _ = e; // autofix
            //
        },
        else => unreachable,
    }
}

fn transpileType(self: *Self, ty: *Ast.Type) []const u8 {
    switch (ty.*) {
        .scalar => |t| {
            const eql = std.mem.eql;
            const text = self.ast.toSource(t);

            return if (eql(u8, text, "int"))
                "i64"
            else if (eql(u8, text, "float"))
                "f64"
            else
                text;
        },
        else => @panic("Type not yet supported"),
    }
}

fn indent(self: *Self) !void {
    std.debug.assert(self.indent_level * 2 < 1024);
    try self.output.appendSlice(self.allocator, spaces[0 .. self.indent_level * INDENT_SIZE]);
}
