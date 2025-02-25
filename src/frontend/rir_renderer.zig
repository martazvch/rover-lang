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
const AnalyzerReport = @import("analyzer.zig").Analyzer.AnalyzerReport;

const Labels = struct { depth: usize, msg: []const u8 };

pub const RirRenderer = struct {
    source: []const u8,
    instr_tags: []const Instruction.Tag,
    instr_data: []const Instruction.Data,
    errs: []const AnalyzerReport,
    warns: []const AnalyzerReport,
    interner: *const Interner,
    static_analyzis: bool,
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
        instr_tags: []const Instruction.Tag,
        instr_data: []const Instruction.Data,
        errs: []const AnalyzerReport,
        warns: []const AnalyzerReport,
        interner: *const Interner,
        static_analyzis: bool,
    ) Self {
        return .{
            .source = source,
            .instr_tags = instr_tags,
            .instr_data = instr_data,
            .errs = errs,
            .warns = warns,
            .interner = interner,
            .static_analyzis = static_analyzis,
            .indent_level = 0,
            .tree = ArrayList(u8).init(allocator),
            .instr_idx = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tree.deinit();
    }

    pub fn display(self: *const Self) !void {
        var stdout = std.io.getStdOut().writer();
        try stdout.writeAll(self.tree.items);
    }

    fn indent(self: *Self) !void {
        try self.tree.appendSlice(Self.spaces[0 .. self.indent_level * Self.indent_size]);
    }

    pub fn parse_ir(self: *Self) !void {
        if (self.errs.len > 0)
            try self.parse_errs()
        else if (self.static_analyzis and self.warns.len > 0)
            try self.parse_errs()
        else while (self.instr_idx < self.instr_tags.len)
            try self.parse_instr(self.instr_idx);
    }

    fn parse_errs(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();
        for (self.errs, 0..) |err, i| {
            try err.to_str(stdout);

            if (self.warns.len > 0 or i < self.errs.len - 1)
                try stdout.writeAll("\n");
        }

        for (self.warns, 0..) |warn, i| {
            try warn.to_str(stdout);

            if (i < self.warns.len - 1) try stdout.writeAll("\n");
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
            .Discard => self.discard(),
            .Float => self.float_instr(index),
            .FnCall => self.fn_call(index),
            .FnDecl => self.fn_declaration(index),
            .FnName => unreachable,
            .Identifier => self.identifier(index),
            .If => self.if_instr(index),
            .Imported => unreachable,
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
            .Return => self.return_instr(index),
            .String => self.string_instr(index),
            .Unary => self.unary(index),
            .Use => self.use(index),
            .VarDecl => self.var_decl(index),
            .While => self.while_instr(),
        };
    }

    fn assignment(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Assignment;

        var writer = self.tree.writer();
        try self.indent();
        try writer.print("[Assignment index: {}, scope: {s}]\n", .{
            data.variable.index, @tagName(data.variable.scope),
        });

        self.instr_idx += 1;
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);

        if (data.cast) try self.parse_instr(self.instr_idx);
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

        for (0..data.length) |_| {
            try self.parse_instr(self.instr_idx);
        }

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

    fn fn_call(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].FnCall;
        self.instr_idx += 1;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Fn call arity: {}, builtin: {}]\n", .{
            data.arity, data.builtin,
        });
        self.indent_level += 1;

        // Variable
        try self.parse_instr(self.instr_idx);

        if (data.arity > 0) {
            try self.indent();
            try self.tree.appendSlice("- args:\n");

            for (0..data.arity) |_| {
                try self.parse_instr(self.instr_idx);

                if (self.instr_idx < self.instr_tags.len and self.instr_tags[self.instr_idx] == .Cast)
                    try self.parse_instr(self.instr_idx);
            }
        }

        self.indent_level -= 1;
    }

    fn fn_declaration(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].FnDecl;
        self.instr_idx += 1;

        const fn_name = self.interner.get_key(self.instr_data[self.instr_idx].Id).?;
        self.instr_idx += 1;
        const fn_var = self.instr_data[self.instr_idx].Variable;
        self.instr_idx += 1;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print(
            "[Fn declaration {s}, index: {}, scope: {s}, return kind: {s}]\n",
            .{ fn_name, fn_var.index, @tagName(fn_var.scope), @tagName(data.return_kind) },
        );

        self.indent_level += 1;
        for (0..data.body_len) |_| {
            try self.parse_instr(self.instr_idx);
        }
        self.indent_level -= 1;
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

    fn if_instr(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].If;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[If cast: {s}, has else: {}]\n", .{
            @tagName(data.cast),
            data.has_else,
        });
        self.instr_idx += 1;

        try self.indent();
        try self.tree.appendSlice("- condition:\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;

        try self.indent();
        try self.tree.appendSlice("- then:\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;

        if (data.has_else) {
            try self.indent();
            try self.tree.appendSlice("- else:\n");
            self.indent_level += 1;
            try self.parse_instr(self.instr_idx);
            self.indent_level -= 1;
        }
    }

    fn int_instr(self: *Self, instr: usize) Error!void {
        const value = self.instr_data[instr].Int;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Int {}]\n", .{value});
        self.instr_idx += 1;
    }

    fn return_instr(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Return;
        self.instr_idx += 1;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Return expr: {}]\n", .{data});

        if (data) {
            self.indent_level += 1;
            try self.parse_instr(self.instr_idx);
            self.indent_level -= 1;
        }
    }

    fn string_instr(self: *Self, instr: usize) Error!void {
        const index = self.instr_data[instr].Id;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("[String {s}]\n", .{self.interner.get_key(index).?});
        self.instr_idx += 1;
    }

    fn unary(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].Unary;
        var writer = self.tree.writer();

        try self.indent();
        self.instr_idx += 1;
        try writer.print("[Unary {s}]\n", .{@tagName(data.op)});
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn use(self: *Self, instr: usize) Error!void {
        const count = self.instr_data[instr].Use;
        var writer = self.tree.writer();

        // NOTE: For now, skips the first 'Null' placed by the analyzer
        // Needs a rework
        self.instr_idx += 1;

        try self.indent();
        try writer.print("[Use count: {}]\n", .{count});
        self.instr_idx += 1;

        for (0..count) |_| {
            self.instr_idx += 1;
        }
    }

    fn var_decl(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].VarDecl;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("[Variable declaration index: {}, scope: {s}]\n", .{
            data.variable.index, @tagName(data.variable.scope),
        });

        self.indent_level += 1;
        self.instr_idx += 1;
        try self.parse_instr(self.instr_idx);

        if (data.cast) try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn while_instr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[While]\n");
        self.instr_idx += 1;
        try self.indent();
        try self.tree.appendSlice("- condition:\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("- body:\n");
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }
};
