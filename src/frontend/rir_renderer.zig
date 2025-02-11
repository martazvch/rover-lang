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

    fn indent(self: *Self) !void {
        try self.tree.appendSlice(Self.spaces[0 .. self.indent_level * Self.indent_size]);
    }

    pub fn parse_ir(self: *Self) !void {
        for (self.instructions) |instr| {
            try switch (instr) {
                .Bool => |i| self.bool_instr(i),
                .Float => |i| self.float_instr(i),
                .Int => |i| self.int_instr(i),
                .Null => self.null_instr(),
                .String => |i| self.string_instr(i),
            };
        }
    }

    fn bool_instr(self: *Self, value: bool) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Bool {s}]\n", .{if (value) "true" else "false"});
    }

    fn float_instr(self: *Self, value: f64) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Float {}]\n", .{value});
    }

    fn int_instr(self: *Self, value: i64) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Int {}]\n", .{value});
    }

    fn null_instr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Null]\n");
    }

    fn string_instr(self: *Self, index: usize) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[String {s}]\n", .{self.interner.get_key(index).?});
    }
};
