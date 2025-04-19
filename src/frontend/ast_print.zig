const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("ast.zig");
const Node = @import("ast.zig").Node;
const ParserReport = @import("parser.zig").Parser.ParserReport;
const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

pub const AstPrinter = struct {
    allocator: Allocator,
    source: [:0]const u8,
    token_tags: []const Token.Tag,
    token_spans: []const Span,
    node_tags: []const Node.Tag,
    node_mains: []const Ast.TokenIndex,
    node_data: []const usize,
    errs: []const ParserReport,
    node_idx: usize,
    indent_level: u8 = 0,
    tree: std.ArrayList(u8),

    const indent_size: u8 = 4;
    const spaces: [1024]u8 = [_]u8{' '} ** 1024;

    const Error = Allocator.Error || std.fmt.BufPrintError;
    const Self = @This();

    pub fn init(
        allocator: Allocator,
        source: [:0]const u8,
        token_tags: []const Token.Tag,
        token_spans: []const Span,
        node_tags: []const Node.Tag,
        node_mains: []const Ast.TokenIndex,
        node_data: []const usize,
        errs: []const ParserReport,
    ) Self {
        return .{
            .allocator = allocator,
            .source = source,
            .token_tags = token_tags,
            .token_spans = token_spans,
            .node_tags = node_tags,
            .node_mains = node_mains,
            .node_data = node_data,
            .errs = errs,
            .node_idx = 0,
            .indent_level = 0,
            .tree = std.ArrayList(u8).init(allocator),
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

    fn source_from_tk(self: *const Self, index: Ast.TokenIndex) []const u8 {
        const span = self.token_spans[self.node_mains[index]];
        return self.source[span.start..span.end];
    }

    pub fn parse_ast(self: *Self) !void {
        if (self.errs.len > 0)
            try self.parse_errs()
        else while (self.node_idx < self.node_data.len)
            try self.parse_node(self.node_idx);
    }

    fn parse_errs(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();
        for (self.errs) |err| {
            try err.to_str(stdout);
            try stdout.writeAll("\n");
        }
    }

    fn parse_node(self: *Self, index: Node.Index) !void {
        try switch (self.node_tags[index]) {
            .Add, .@"and", .Div, .Mul, .@"or", .Sub, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => self.binop_expr(),
            .Assignment => self.assignment(),
            .Block => self.block_expr(),
            .Bool => self.literal("Bool literal"),
            .call => self.fn_call(),
            .count => unreachable,
            .Discard => self.discard(),
            .Empty => unreachable,
            .field => self.field(),
            .Float => self.literal("Float literal"),
            .FnDecl => self.fn_decl(),
            .Grouping => self.grouping(),
            .Identifier => self.literal("Identifier"),
            .@"if" => self.if_expr(),
            .Int => self.literal("Int literal"),
            .MultiVarDecl => self.multi_var_decl(),
            .null => self.null_(),
            .Parameter => self.parameter(),
            .print => self.print_stmt(),
            .@"return" => self.return_expr(),
            .self => unreachable,
            .string => self.literal("String literal"),
            .StructDecl => self.structure(),
            .struct_literal => self.struct_literal(),
            .Type => unreachable,
            .Unary => self.unary_expr(),
            .use => self.use_stmt(),
            .VarDecl => self.var_decl(),
            .@"while" => self.while_stmt(),
        };
    }

    fn assignment(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Assignment\n");
        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("assigne:\n");
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        try self.indent();
        try self.tree.appendSlice("value:\n");
        try self.parse_node(self.node_idx);

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn binop_expr(self: *Self) Error!void {
        try self.indent();

        var writer = self.tree.writer();
        try writer.print("[Binop {s}]\n", .{switch (self.node_tags[self.node_idx]) {
            .Add => "+",
            .@"and" => "and",
            .Div => "/",
            .Eq => "==",
            .Ge => ">=",
            .Gt => ">",
            .Le => "<=",
            .Lt => "<",
            .Mul => "*",
            .Ne => "!=",
            .@"or" => "or",
            .Sub => "-",
            else => unreachable,
        }});

        self.indent_level += 1;
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        try self.parse_node(self.node_idx);

        self.indent_level -= 1;
    }

    fn block_expr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Block]\n");

        self.indent_level += 1;

        const count = self.node_data[self.node_idx];
        self.node_idx += 1;

        for (0..count) |_| {
            try self.parse_node(self.node_idx);
        }

        self.indent_level -= 1;
    }

    fn discard(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Discard\n");
        self.indent_level += 1;
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn fn_call(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Fn call\n");
        const arity = self.node_data[self.node_idx];

        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("callee:\n");
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        try self.indent();
        try self.tree.appendSlice("args:\n");

        for (0..arity) |_| {
            try self.parse_node(self.node_idx);
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn fn_decl(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();

        const name = self.node_idx;
        const arity = self.node_data[self.node_idx];
        self.node_idx += 1;

        const save_writer = self.tree;
        self.tree = std.ArrayList(u8).init(self.allocator);

        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("params:\n");
        self.indent_level += 1;

        for (0..arity) |i| {
            if (i == 0 and self.node_tags[self.node_idx] == .self) {
                try self.indent();
                try self.tree.appendSlice("self\n");
                self.node_idx += 1;
            } else try self.parse_node(self.node_idx);
        }

        self.indent_level -= 1;

        const res = try self.tree.toOwnedSlice();
        self.tree = save_writer;

        try writer.print(
            "[Fn declaration {s}, type {s}, arity {}\n",
            .{
                self.source_from_tk(name),
                self.get_type(self.node_idx),
                arity,
            },
        );

        try self.tree.appendSlice(res);

        try self.indent();
        try self.tree.appendSlice("body:\n");
        self.indent_level += 1;
        try self.parse_node(self.node_idx);
        self.indent_level -= 2;

        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn parameter(self: *Self) Error!void {
        try self.indent();
        const writer = self.tree.writer();

        try writer.print("{s}, ", .{self.source_from_tk(self.node_idx)});

        self.node_idx += 1;
        try writer.print("type {s}\n", .{self.get_type(self.node_idx)});
    }

    fn grouping(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Grouping]\n");

        self.indent_level += 1;
        self.node_idx += 1;

        if (self.node_tags[self.node_idx] != .Empty)
            try self.parse_node(self.node_idx)
        else
            self.node_idx += 1;

        self.indent_level -= 1;
    }

    fn if_expr(self: *Self) Error!void {
        try self.indent();

        try self.tree.appendSlice("[If\n");
        self.indent_level += 1;
        try self.indent();

        try self.tree.appendSlice("condition:\n");
        self.node_idx += 1;
        try self.parse_node(self.node_idx);

        try self.indent();
        try self.tree.appendSlice("then body:\n");
        try self.parse_node(self.node_idx);

        try self.indent();
        try self.tree.appendSlice("else body:\n");

        if (self.node_tags[self.node_idx] != .Empty) {
            try self.parse_node(self.node_idx);
        } else {
            self.node_idx += 1;
            try self.indent();
            try self.tree.appendSlice("none\n");
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn literal(self: *Self, text: []const u8) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[{s} {s}]\n", .{ text, self.source_from_tk(self.node_idx) });
        self.node_idx += 1;
    }

    fn field(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Member {s} of struct {s}]\n", .{
            self.source_from_tk(self.node_idx + 2),
            self.source_from_tk(self.node_idx + 1),
        });
        self.node_idx += 3;
    }

    fn multi_var_decl(self: *Self) Error!void {
        const count = self.node_mains[self.node_idx];
        self.node_idx += 1;

        for (0..count) |_| {
            try self.parse_node(self.node_idx);
        }
    }

    fn null_(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Null literal]\n");
        self.node_idx += 1;
    }

    fn print_stmt(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Print]\n");
        self.indent_level += 1;
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        self.indent_level -= 1;
    }

    fn return_expr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Return");

        self.node_idx += 1;

        if (self.node_tags[self.node_idx] != .Empty) {
            try self.tree.appendSlice("\n");
            self.indent_level += 1;
            try self.parse_node(self.node_idx);
            self.indent_level -= 1;
            try self.indent();
        } else self.node_idx += 1;

        try self.tree.appendSlice("]\n");
    }

    fn structure(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();

        // Advance to type
        try writer.print(
            "[Structure declaration {s}\n",
            .{self.source_from_tk(self.node_idx)},
        );

        self.node_idx += 1;
        self.indent_level += 1;

        // Fields
        const fields_count: usize = self.node_data[self.node_idx];
        self.node_idx += 1;

        if (fields_count > 0) {
            try self.indent();
            try self.tree.appendSlice("fields:\n");
            self.indent_level += 1;

            for (0..fields_count) |_| {
                try self.indent();
                try writer.print("{s}", .{self.source_from_tk(self.node_idx)});
                self.node_idx += 1;

                // Type
                if (self.node_tags[self.node_idx] != .Empty) {
                    try writer.print(", type: {s}", .{self.get_type(self.node_idx)});
                } else self.node_idx += 1;

                // Value
                if (self.node_tags[self.node_idx] != .Empty) {
                    try writer.writeAll(", value:\n");
                    self.indent_level += 1;
                    try self.parse_node(self.node_idx);
                    self.indent_level -= 1;
                } else self.node_idx += 1;

                try self.tree.appendSlice("\n");
            }

            self.indent_level -= 1;
        }

        const func_count: usize = self.node_data[self.node_idx];
        self.node_idx += 1;

        for (0..func_count) |_| {
            try self.fn_decl();
        }

        self.indent_level -= 1;
        try self.tree.appendSlice("]\n");
    }

    fn struct_literal(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        const arity = self.node_data[self.node_idx];

        self.node_idx += 1;
        try writer.print("[Structure {s} literal{s}", .{
            self.source_from_tk(self.node_idx),
            if (arity > 0) "\n" else "",
        });

        self.indent_level += 1;
        self.node_idx += 1;

        for (0..arity) |_| {
            try self.field_init();
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn field_init(self: *Self) !void {
        var writer = self.tree.writer();

        try self.indent();
        try writer.print("field: {s}", .{self.source_from_tk(self.node_idx)});
        self.node_idx += 1;

        if (self.node_tags[self.node_idx] == .Empty) {
            self.node_idx += 1;
            try self.tree.appendSlice(", shorthand");
        } else {
            try self.tree.appendSlice("\n");
            self.indent_level += 1;
            _ = try self.parse_node(self.node_idx);
            self.indent_level -= 1;
        }

        try self.tree.appendSlice("\n");
    }

    fn unary_expr(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Unary {s}]\n", .{self.source_from_tk(self.node_idx)});

        self.indent_level += 1;
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        self.indent_level -= 1;
    }

    fn var_decl(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();

        // Advance to type
        self.node_idx += 1;
        try writer.print(
            "[Var declaration {s}, type {s}, value\n",
            .{
                self.source_from_tk(self.node_idx - 1),
                self.get_type(self.node_idx),
            },
        );

        self.indent_level += 1;

        if (self.node_tags[self.node_idx] != .Empty) {
            try self.parse_node(self.node_idx);
        } else {
            try self.indent();
            try self.tree.appendSlice("none\n");
            self.node_idx += 1;
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn get_type(self: *Self, index: Node.Index) []const u8 {
        if (self.node_tags[index] == .Empty) {
            self.node_idx += 1;
            return "void";
        } else if (self.token_tags[self.node_mains[index]] == .nf)
            return self.get_fn_type(index) catch @panic("oom");

        defer self.node_idx += 1;

        return switch (self.node_tags[index]) {
            .Type => return self.source_from_tk(index),
            else => unreachable,
        };
    }

    fn get_fn_type(self: *Self, index: Node.Index) ![]const u8 {
        var res: std.ArrayListUnmanaged(u8) = .{};
        var writer = res.writer(self.allocator);
        try writer.writeAll("fn(");
        const arity = self.node_data[index];
        self.node_idx += 1;

        for (0..arity) |i| {
            try writer.print("{s}{s}", .{
                self.get_type(self.node_idx),
                if (i < arity - 1) ", " else "",
            });
        }
        try writer.print(") -> {s}", .{self.get_type(self.node_idx)});

        return try res.toOwnedSlice(self.allocator);
    }

    fn use_stmt(self: *Self) !void {
        try self.indent();
        try self.tree.appendSlice("[Use ");
        var writer = self.tree.writer();

        const count = self.node_data[self.node_idx];

        for (0..count) |i| {
            self.node_idx += 1;
            try writer.print("{s}", .{self.source_from_tk(self.node_idx)});

            if (i < count - 1) {
                try writer.print(" ", .{});
            }
        }

        self.node_idx += 1;
        try writer.print("]\n", .{});
    }

    fn while_stmt(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[While\n");
        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("condition:\n");
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        try self.indent();
        try self.tree.appendSlice("body:\n");
        try self.parse_node(self.node_idx);

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }
};
