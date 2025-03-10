const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("ast.zig");
const Node = @import("ast.zig").Node;
const Token = @import("lexer.zig").Token;
const Span = @import("lexer.zig").Span;
const ParserReport = @import("parser.zig").Parser.ParserReport;

pub const AstPrinter = struct {
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
            .Add, .And, .Div, .Mul, .Or, .Sub, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => self.binop_expr(),
            .Assignment => self.assignment(),
            .Block => self.block_expr(),
            .Bool => self.literal("Bool literal"),
            .Discard => self.discard(),
            .Empty => unreachable,
            .Float => self.literal("Float literal"),
            .FnCall => self.fn_call(),
            .FnDecl => self.fn_decl(),
            .Grouping => self.grouping(),
            .Identifier => self.literal("Identifier"),
            .If => self.if_expr(),
            .Int => self.literal("Int literal"),
            .Link => {
                const start = self.node_idx;
                self.node_idx = self.node_data[index];
                try self.parse_node(self.node_idx);
                // Past the link
                self.node_idx = start + 1;
            },
            .MultiVarDecl => self.multi_var_decl(),
            .Null => self.null_(),
            .Parameter => self.parameter(),
            .Print => self.print_stmt(),
            .Return => self.return_expr(),
            .String => self.literal("String literal"),
            .Type => unreachable,
            .Unary => self.unary_expr(),
            .Use => self.use_stmt(),
            .VarDecl => self.var_decl(),
            .While => self.while_stmt(),
            .FnCallEnd, .FnDeclEnd => unreachable,
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
            .And => "and",
            .Div => "/",
            .Eq => "==",
            .Ge => ">=",
            .Gt => ">",
            .Le => "<=",
            .Lt => "<",
            .Mul => "*",
            .Ne => "!=",
            .Or => "or",
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
        // Skips the FnCallEnd
        self.node_idx += 1;
    }

    fn fn_decl(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();

        const name = self.node_idx;
        const arity = self.node_data[self.node_idx];

        self.node_idx += 1;
        // x2 because each param is composed of 2 nodes: Param + Type
        const return_idx = self.node_idx + arity * 2;

        try writer.print(
            "[Fn declaration {s}, type {s}, arity {}\n",
            .{
                self.source_from_tk(name),
                self.get_type(return_idx),
                arity,
            },
        );

        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("params:\n");
        self.indent_level += 1;

        for (0..arity) |_| {
            try self.parse_node(self.node_idx);
            // We manually increment because the type parsing
            // doesn't do it
            self.node_idx += 1;
        }

        // Skips the return type
        self.node_idx += 1;
        self.indent_level -= 1;

        try self.indent();
        try self.tree.appendSlice("body:\n");
        self.indent_level += 1;
        try self.parse_node(self.node_idx);
        self.indent_level -= 1;

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
        // Skips the FnDeclEnd
        self.node_idx += 1;
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

    fn unary_expr(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Unary {s}]\n", .{self.source_from_tk(self.node_idx)});

        self.indent_level += 1;
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        self.indent_level -= 1;
    }

    fn link_to_empty(self: *const Self, index: Node.Index) bool {
        if (self.node_tags[index] == .Link)
            return self.node_tags[self.node_data[index]] == .Empty;

        return false;
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
        self.node_idx += 1;

        if (self.node_tags[self.node_idx] != .Empty and !self.link_to_empty(self.node_idx)) {
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
        if (self.node_tags[index] == .Empty) return "void";

        return switch (self.node_tags[index]) {
            .Link => return self.get_type(self.node_data[index]),
            .Type => {
                return self.source_from_tk(index);
            },
            else => unreachable,
        };
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
