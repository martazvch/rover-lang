const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Instruction = @import("rir.zig").Instruction;
const Ast = @import("ast.zig");
const Node = @import("ast.zig").Node;
const Token = @import("lexer.zig").Token;
const Span = @import("lexer.zig").Span;
const Interner = @import("../interner.zig").Interner;

pub const RirRenderer = struct {
    source: []const u8,
    instructions: []const Instruction,
    interner: *const Interner,
    indent_level: u8 = 0,
    tree: std.ArrayList(u8),

    const indent_size: u8 = 4;
    const spaces: [1024]u8 = [_]u8{' '} ** 1024;

    const Error = Allocator.Error || std.fmt.BufPrintError;
    const Self = @This();

    pub fn init(
        allocator: Allocator,
        source: []const u8,
        instructions: []const Instruction,
        interner: *const Interner,
    ) Self {
        return .{
            .source = source,
            .instructions = instructions,
            .interner = interner,
            .indent_level = 0,
            .tree = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tree.deinit();
    }

    pub fn display(self: *const Self) void {
        print("\n--- IR ---\n{s}", .{self.tree.items});
    }

    pub fn parse_ir(self: *Self) !void {
        for (self.instructions) |instr| {
            try switch (instr) {
                .Assignment => |i| self.assignment(i),
                .Binop => |i| self.binop(i),
                .Block => |i| self.block(i),
                .Bool => |i| self.bool_instr(i),
                .CastToFloat => try self.tree.appendSlice("[Cast to float]\n"),
                .Discard => try self.tree.appendSlice("[Discard]\n"),
                .Float => |i| self.float_instr(i),
                .Identifier => |i| self.identifier(i),
                .Int => |i| self.int_instr(i),
                .Null => try self.tree.appendSlice("[Null]\n"),
                .Print => try self.tree.appendSlice("[Print]\n"),
                .String => |i| self.string_instr(i),
                .Unary => |i| self.unary(i),
                .VarDecl => |i| self.var_decl(i),
            };
        }
    }

    fn assignment(self: *Self, variable: Instruction.Variable) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Assignment variable index: {}, scope: {s}]\n", .{
            variable.index, @tagName(variable.scope),
        });
    }

    fn binop(self: *Self, data: Instruction.BinopData) Error!void {
        var writer = self.tree.writer();
        try writer.print(
            "[Binop type: {s}, cast: {s}]\n",
            .{ @tagName(data.tag), @tagName(data.cast) },
        );
    }

    fn block(self: *Self, data: Instruction.BlockData) Error!void {
        var writer = self.tree.writer();
        try writer.print(
            "[Block pop count: {}, is_expr: {}]\n",
            .{ data.pop_count, data.is_expr },
        );
    }

    fn bool_instr(self: *Self, value: bool) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Bool {}]\n", .{value});
    }

    fn float_instr(self: *Self, value: f64) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Float {d}]\n", .{value});
    }

    fn identifier(self: *Self, variable: Instruction.Variable) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Variable index: {}, scope: {s}]\n", .{
            variable.index, @tagName(variable.scope),
        });
    }

    fn int_instr(self: *Self, value: i64) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Int {}]\n", .{value});
    }

    fn string_instr(self: *Self, index: usize) Error!void {
        var writer = self.tree.writer();
        try writer.print("[String {s}]\n", .{self.interner.get_key(index).?});
    }

    fn string_mul(self: *Self, side: Instruction.Side) Error!void {
        var writer = self.tree.writer();
        try writer.print("[String mul {s}]\n", .{@tagName(side)});
    }

    fn unary(self: *Self, tag: Instruction.UnaryTag) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Unary {s}]\n", .{@tagName(tag)});
    }

    fn var_decl(self: *Self, variable: Instruction.Variable) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Declare variable index: {}, scope: {s}]\n", .{
            variable.index, @tagName(variable.scope),
        });
    }
};
