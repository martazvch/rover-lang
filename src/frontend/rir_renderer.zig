const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const Interner = @import("../Interner.zig");
const AnalyzerReport = @import("Analyzer.zig").AnalyzerReport;
const Ast = @import("Ast.zig");
const Instruction = @import("rir.zig").Instruction;
const Node = @import("Ast.zig").Node;
const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

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
        from: usize,
        instructions: std.MultiArrayList(Instruction),
        errs: []const AnalyzerReport,
        warns: []const AnalyzerReport,
        interner: *const Interner,
        static_analyzis: bool,
    ) Self {
        return .{
            .source = source,
            .instr_tags = instructions.items(.tag)[from..],
            .instr_data = instructions.items(.data)[from..],
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

        for (self.errs) |err| {
            try err.to_str(stdout);
            try stdout.writeAll("\n");
        }

        for (self.warns) |warn| {
            try warn.to_str(stdout);
            try stdout.writeAll("\n");
        }
    }

    const Tag = enum {
        data,
        tag,

        pub fn Type(tag: Tag) type {
            return switch (tag) {
                .data => Instruction.Data,
                .tag => Instruction.Tag,
            };
        }
    };

    fn next(self: *Self, comptime tag: Tag) tag.Type() {
        defer self.instr_idx += 1;

        return switch (tag) {
            .data => self.instr_data[self.instr_idx],
            .tag => self.instr_tag[self.instr_idx],
        };
    }

    fn parse_instr(self: *Self, index: usize) !void {
        try switch (self.instr_tags[index]) {
            .assignment => self.assignment(index),
            .binop => self.binop(index),
            .block => self.block(index),
            .bool => self.bool_instr(index),
            .cast => self.cast(index),
            .discard => self.discard(),
            .float => self.float_instr(index),
            .call => self.fn_call(index),
            .field => self.get_field(index),
            .fn_decl => self.fn_declaration(index),
            .name => unreachable,
            .identifier => self.identifier(index, false),
            .identifier_id => self.identifier(index, true),
            .@"if" => self.if_instr(index),
            .imported => unreachable,
            .int => self.int_instr(),
            .multiple_var_decl => self.multiple_var_decl(),
            .null => {
                try self.indent();
                try self.tree.appendSlice("[Null]\n");
                self.instr_idx += 1;
            },
            .print => {
                try self.indent();
                try self.tree.appendSlice("[Print]\n");
                self.instr_idx += 1;
                self.indent_level += 1;
                try self.parse_instr(self.instr_idx);
                self.indent_level -= 1;
            },
            .@"return" => self.return_instr(),
            .string => self.string_instr(),
            .struct_decl => self.struct_decl(),
            .struct_literal => self.struct_literal(),
            .unary => self.unary(index),
            .use => self.use(index),
            .var_decl => self.var_decl(index),
            .@"while" => self.while_instr(),
        };
    }

    fn assignment(self: *Self, instr: usize) Error!void {
        const assign_data = self.instr_data[instr].assignment;
        self.instr_idx += 1;

        const data = if (self.instr_tags[instr + 1] == .identifier_id)
            self.instr_data[self.instr_data[instr + 1].id].var_decl.variable
        else if (self.instr_tags[instr + 1] == .field) {
            return self.field_assignment(assign_data);
        } else self.instr_data[instr + 1].variable;

        var writer = self.tree.writer();
        try self.indent();
        try writer.print("[Assignment index: {}, scope: {s}]\n", .{
            data.index, @tagName(data.scope),
        });

        self.instr_idx += 1;
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);

        if (assign_data.cast) try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn field_assignment(self: *Self, assign_data: Instruction.Assignment) Error!void {
        var writer = self.tree.writer();
        try self.indent();
        try writer.writeAll("[Field assignment]\n");

        try self.get_field(self.instr_idx);

        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);

        if (assign_data.cast) try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn binop(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].binop;
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
        const data = self.instr_data[instr].block;

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
        const value = self.instr_data[instr].bool;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Bool {}]\n", .{value});
        self.instr_idx += 1;
    }

    fn cast(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].cast_to;

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
        const value = self.instr_data[instr].float;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Float {d}]\n", .{value});
        self.instr_idx += 1;
    }

    fn fn_call(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].call;
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

                if (self.instr_idx < self.instr_tags.len and self.instr_tags[self.instr_idx] == .cast)
                    try self.parse_instr(self.instr_idx);
            }
        }

        self.indent_level -= 1;
    }

    fn get_field(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].field;
        self.instr_idx += 1;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Field access {}]\n", .{data});
        self.indent_level += 1;

        // Variable
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn fn_declaration(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].fn_decl;
        self.instr_idx += 1;

        const fn_name = self.interner.getKey(self.instr_data[self.instr_idx].id).?;
        self.instr_idx += 1;
        const fn_var = self.instr_data[self.instr_idx].var_decl.variable;
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

    fn identifier(self: *Self, instr: usize, is_id: bool) Error!void {
        const data = if (is_id)
            self.instr_data[self.instr_data[instr].id].var_decl.variable
        else
            self.instr_data[instr].variable;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Variable index: {}, scope: {s}]\n", .{
            data.index, @tagName(data.scope),
        });
        self.instr_idx += 1;
    }

    fn if_instr(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].@"if";

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

    fn int_instr(self: *Self) Error!void {
        const value = self.next(.data).int;

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Int {}]\n", .{value});
    }

    fn multiple_var_decl(self: *Self) Error!void {
        const count = self.next(.data).id;

        for (0..count) |_| {
            try self.parse_instr(self.instr_idx);
        }
    }

    fn return_instr(self: *Self) Error!void {
        const data = self.next(.data).@"return";

        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Return expr: {}, cast: {}]\n", .{ data.value, data.cast });

        if (data.value) {
            self.indent_level += 1;
            try self.parse_instr(self.instr_idx);
            if (data.cast) try self.parse_instr(self.instr_idx);
            self.indent_level -= 1;
        }
    }

    fn string_instr(self: *Self) Error!void {
        const index = self.next(.data).id;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("[String {s}]\n", .{self.interner.getKey(index).?});
    }

    fn struct_decl(self: *Self) Error!void {
        var writer = self.tree.writer();
        const data = self.next(.data).struct_decl;
        const name = self.next(.data).id;
        const struct_var = self.next(.data).var_decl.variable;

        try self.indent();
        try writer.print("[Structure declaration {s}, index: {}, scope: {s}]\n", .{
            self.interner.getKey(name).?,
            struct_var.index,
            @tagName(struct_var.scope),
        });

        self.indent_level += 1;

        for (0..data.default_fields) |_| {
            try self.indent();
            const field_idx = self.next(.data).field;
            try writer.print("[field {} default value\n", .{field_idx});

            self.indent_level += 1;
            try self.parse_instr(self.instr_idx);
            self.indent_level -= 1;
            try self.indent();
            try self.tree.appendSlice("]\n");
        }

        for (0..data.func_count) |_| {
            try self.parse_instr(self.instr_idx);
        }

        self.indent_level -= 1;
    }

    fn struct_literal(self: *Self) Error!void {
        const data = self.next(.data).struct_literal;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print(
            "[Structure literal, index: {}, scope: {s}]\n",
            .{ data.variable.index, @tagName(data.variable.scope) },
        );
        self.indent_level += 1;

        for (0..data.arity) |_| {
            const save = self.instr_idx;
            const field_data = self.next(.data).field;
            self.instr_idx = field_data;
            self.indent_level += 1;
            try self.parse_instr(self.instr_idx);
            self.indent_level -= 1;
            self.instr_idx = save + 1;
        }

        self.instr_idx = data.end;

        self.indent_level -= 1;
    }

    fn unary(self: *Self, instr: usize) Error!void {
        const data = self.instr_data[instr].unary;
        var writer = self.tree.writer();

        try self.indent();
        self.instr_idx += 1;
        try writer.print("[Unary {s}]\n", .{@tagName(data.op)});
        self.indent_level += 1;
        try self.parse_instr(self.instr_idx);
        self.indent_level -= 1;
    }

    fn use(self: *Self, instr: usize) Error!void {
        const count = self.instr_data[instr].use;
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
        const data = self.instr_data[instr].var_decl;
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("[Variable declaration index: {}, scope: {s}]\n", .{
            data.variable.index,
            @tagName(data.variable.scope),
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
