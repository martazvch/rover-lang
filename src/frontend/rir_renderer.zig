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
    // instructions: []const Instruction,
    instr_tags: []const Instruction.Tag,
    instr_data: []const Instruction.Data,
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
        // instructions: []const Instruction,
        instr_tags: []const Instruction.Tag,
        instr_data: []const Instruction.Data,
        interner: *const Interner,
    ) Self {
        return .{
            .source = source,
            // .instructions = instructions,
            .instr_tags = instr_tags,
            .instr_data = instr_data,
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

    pub fn parse_ir(self: *Self) !void {
        while (self.instr_idx < self.instr_tags.len) {
            try self.parse_instr(self.instr_idx);
        }
    }

    fn parse_instr(self: *Self, index: usize) !void {
        // try switch (self.instructions[index]) {
        try switch (self.instr_tags[index]) {
            .Assignment => self.assignment(index),
            .Binop => self.binop(index),
            .Block => self.block(index),
            .Bool => self.bool_instr(index),
            .Cast => self.cast(index),
            .Discard => try self.discard(),
            .Float => self.float_instr(index),
            .FnDecl => self.fn_declaration(index),
            .Identifier => self.identifier(index),
            .Int => self.int_instr(index),
            .Null => {
                try self.indent();
                try self.tree.appendSlice("[Null]\n");
                self.instr_idx += 1;
            },
            .Print => {
                try self.indent();
                try self.tree.appendSlice("[Print]\n");
                self.instr_idx += 1;
                self.indent_level += 1;
                try self.parse_instr(self.instr_idx);
                self.indent_level -= 1;
            },
            .Sentinel => unreachable,
            .String => self.string_instr(index),
            .Unary => self.unary(index),
            .VarDecl => self.var_decl(index),
            .While => self.while_instr(),
            // .Assignment => |i| self.assignment(i),
            // .Binop => |i| self.binop(i),
            // .Block => |i| self.block(i),
            // .Bool => |i| self.bool_instr(i),
            // .CastToFloat => try self.tree.appendSlice("[Cast to float]\n"),
            // .Discard => try self.discard(),
            // .Float => |i| self.float_instr(i),
            // .Identifier => |i| self.identifier(i),
            // .Int => |i| self.int_instr(i),
            // .Null => try self.tree.appendSlice("[Null]\n"),
            // .Print => try self.tree.appendSlice("[Print]\n"),
            // .String => |i| self.string_instr(i),
            // .Unary => |i| self.unary(i),
            // .VarDecl => |i| self.var_decl(i),
            // .While => self.while_instr(),
        };
    }

    fn assignment(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Variable;

        var writer = self.tree.writer();
        try self.indent();
        try writer.print("[Assignment variable index: {}, scope: {s}]\n", .{
            data.index, @tagName(data.scope),
        });

        self.instr_idx += 1;
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn binop(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Binop;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print(
            "[Binop type: {s}, cast: {s}]\n",
            .{ @tagName(data.op), @tagName(data.cast) },
        );

        self.instr_idx += 1;
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn block(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Block;

        var writer = self.tree.writer();
        try self.indent();
        try writer.print(
            "[Block pop count: {}, is_expr: {}]\n",
            .{ data.pop_count, data.is_expr },
        );

        self.instr_idx += 1;
        self.indent_level += 1;

        while (self.instr_tags[self.instr_idx] != .Sentinel) {
            try self.parse_instr(self.instr_idx);
        }

        // Skips the sentinel
        self.instr_idx += 1;

        self.indent_level -= 1;
    }

    fn bool_instr(self: *Self, instr: usize) Error!void {
        const value = self.instr_data[instr].Bool;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Bool {}]\n", .{value});
        self.instr_idx += 1;
    }

    fn cast(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].CastTo;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Cast to {s}]\n", .{@tagName(data)});
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

    fn float_instr(self: *Self, instr: usize) Error!void {
        const value = self.instr_data[instr].Float;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Float {d}]\n", .{value});
        self.instr_idx += 1;
    }

    fn fn_declaration(self: *Self, intr: usize) Error!void {

    }

    fn identifier(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Variable;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Variable index: {}, scope: {s}]\n", .{
            data.index, @tagName(data.scope),
        });
        self.instr_idx += 1;
    }

    fn int_instr(self: *Self, instr: usize) Error!void {
        const value = self.instr_data[instr].Int;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Int {}]\n", .{value});
        self.instr_idx += 1;
    }

    fn string_instr(self: *Self, instr: usize) Error!void {
        const index = self.instr_data[instr].Id;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("[String {s}]\n", .{self.interner.get_key(index).?});
        self.instr_idx += 1;
    }

    fn unary(self: *Self, instr: usize) Error!void {
        const op = self.instr_data[instr].Unary;
        var writer = self.tree.writer();

        try self.indent();
        self.instr_idx += 1;
        try writer.print("[Unary {s}]\n", .{@tagName(op)});
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn var_decl(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Variable;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("[Declare variable index: {}, scope: {s}]\n", .{
            data.index, @tagName(data.scope),
        });

        self.indent_level += 1;
        self.instr_idx += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn while_instr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[While]\n");
        self.instr_idx += 1;
        try self.indent();
        try self.tree.appendSlice("condition:\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("body:\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }
};
