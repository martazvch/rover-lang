const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Instruction = @import("rir.zig").Instruction;
const Ast = @import("ast.zig");
const Node = @import("ast.zig").Node;
const Token = @import("lexer.zig").Token;
const Span = @import("lexer.zig").Span;
const Interner = @import("../interner.zig").Interner;

const Labels = struct { depth: usize, msg: []const u8 };

pub const RirRenderer = struct {
    source: []const u8,
    instructions: []const Instruction,
    interner: *const Interner,
    indent_level: u8 = 0,
    tree: ArrayList(u8),
    instr_idx: usize,

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
            .tree = ArrayList(u8).init(allocator),
            .instr_idx = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tree.deinit();
    }

    pub fn display(self: *const Self) void {
        print("\n--- IR ---\n{s}", .{self.tree.items});
    }

    fn indent(self: *Self) !void {
        try self.tree.appendSlice(Self.spaces[0 .. self.indent_level * Self.indent_size]);
    }

    fn add_label(self: *Self, msg: []const u8) Error!void {
        try self.labels.append(.{ .depth = self.depth, .msg = msg });
    }

    pub fn parse_ir(self: *Self) !void {
        while (self.instr_idx < self.instructions.len) {
            try self.parse_instr(self.instr_idx);
        }
    }

    fn parse_instr(self: *Self, index: usize) !void {
        try switch (self.instructions[index]) {
            .Assignment => |i| self.assignment(i),
            .Binop => |i| self.binop(i),
            .Block => |i| self.block(i),
            .Bool => |i| self.bool_instr(i),
            .CastToFloat => try self.tree.appendSlice("[Cast to float]\n"),
            .Discard => try self.discard(),
            .Float => |i| self.float_instr(i),
            .Identifier => |i| self.identifier(i),
            .Int => |i| self.int_instr(i),
            .Null => try self.tree.appendSlice("[Null]\n"),
            .Print => try self.tree.appendSlice("[Print]\n"),
            .String => |i| self.string_instr(i),
            .Unary => |i| self.unary(i),
            .VarDecl => |i| self.var_decl(i),
            .While => self.while_instr(),
        };
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
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Bool {}]\n", .{value});
        self.instr_idx += 1;
    }

    fn discard(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Discard]\n");
        self.indent_level += 1;
        self.instr_idx += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn float_instr(self: *Self, value: f64) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Float {d}]\n", .{value});
        self.instr_idx += 1;
    }

    fn identifier(self: *Self, variable: Instruction.Variable) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Variable index: {}, scope: {s}]\n", .{
            variable.index, @tagName(variable.scope),
        });
        self.instr_idx += 1;
    }

    fn int_instr(self: *Self, value: i64) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Int {}]\n", .{value});
        self.instr_idx += 1;
    }

    fn string_instr(self: *Self, index: usize) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[String {s}]\n", .{self.interner.get_key(index).?});
        self.instr_idx += 1;
    }

    fn unary(self: *Self, tag: Instruction.UnaryTag) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        self.instr_idx += 1;
        try writer.print("[Unary {s}]\n", .{@tagName(tag)});
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn var_decl(self: *Self, variable: Instruction.Variable) Error!void {
        var writer = self.tree.writer();
        try writer.print("[Declare variable index: {}, scope: {s}]\n", .{
            variable.index, @tagName(variable.scope),
        });
    }

    fn while_instr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[While]\n");
        self.instr_idx += 1;
        try self.indent();
        try self.tree.appendSlice("condition\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("body\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }
};
