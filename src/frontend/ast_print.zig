const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Ast = @import("ast.zig");
const Node = Ast.Node;
const NullNode = Ast.NullNode;
// const Expr = Ast.Expr;
// const Stmt = Ast.Stmt;
const Token = @import("lexer.zig").Token;
const Span = @import("lexer.zig").Span;

pub const AstPrinter = struct {
    source: [:0]const u8,
    token_tags: []const Token.Tag,
    token_spans: []const Span,
    node_tags: []const Node.Tag,
    node_mains: []const Ast.TokenIndex,
    node_data: []const usize,
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
    ) Self {
        return .{
            .source = source,
            .token_tags = token_tags,
            .token_spans = token_spans,
            .node_tags = node_tags,
            .node_mains = node_mains,
            .node_data = node_data,
            .node_idx = 0,
            .indent_level = 0,
            .tree = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tree.deinit();
    }

    pub fn display(self: *const Self) void {
        print("\n--- AST ---\n{s}", .{self.tree.items});
    }

    fn indent(self: *Self) !void {
        try self.tree.appendSlice(Self.spaces[0 .. self.indent_level * Self.indent_size]);
    }

    fn source_from_tk(self: *const Self, index: Ast.TokenIndex) []const u8 {
        const span = self.token_spans[self.node_mains[index]];
        return self.source[span.start..span.end];
    }

    pub fn parse_ast(self: *Self) !void {
        while (self.node_idx < self.node_data.len) {
            try self.parse_node(self.node_idx);
        }
    }

    fn parse_node(self: *Self, index: Node.Index) !void {
        try switch (self.node_tags[index]) {
            .Add, .And, .Div, .Mul, .Or, .Sub => self.binop_expr(),
            .Assignment => self.assignment(),
            .Block => self.block_expr(),
            .Bool => self.literal("Bool literal"),
            .Data => unreachable,
            .Discard => self.discard(),
            .Empty => unreachable,
            .Float => self.literal("Float literal"),
            .FnDecl => self.fn_decl(),
            .Grouping => self.grouping(),
            .Identifier => self.literal("Identifier"),
            .If => self.if_expr(),
            .Int => self.literal("Int literal"),
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
            .Mul => "*",
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

        self.node_idx += 1;
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

    fn fn_decl(self: *Self) Error!void {
        try self.indent();
        var writer = self.tree.writer();

        self.node_idx += 1;
        const arity = self.node_data[self.node_idx];

        self.node_idx += 1;
        // x2 because each param is composed of 2 nodes: Param + Type
        const return_idx = self.node_idx + arity * 2;

        try writer.print(
            "[Fn declaration {s}, type {s}, arity {}\n",
            .{
                self.source_from_tk(return_idx),
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
        try self.parse_node(self.node_idx);
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
        }

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

        // const type_name = if (stmt.type_) |t| self.print_type(t) else "void";
        // const written = try std.fmt.bufPrint(
        //     &buf,
        //     "[Var declaration {s}, type {s}, value\n",
        //     .{ stmt.name.text, type_name },
        // );
        // try self.tree.appendSlice(written);
        //
        // self.indent_level += 1;
        //
        // if (stmt.value) |v| {
        //     try self.expression(v);
        // } else {
        //     try self.indent();
        //     try self.tree.appendSlice("none\n");
        // }
        //
        // self.indent_level -= 1;
        // try self.indent();
        // try self.tree.appendSlice("]\n");
    }

    fn get_type(self: *Self, index: Node.Index) []const u8 {
        if (self.node_tags[index] == .Empty) return "void";

        return switch (self.node_tags[index]) {
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

        self.node_idx += 1;
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
        // try self.expression(stmt.condition);
        // try self.parse_node(self.node_data[index].lhs);
        self.node_idx += 1;
        try self.parse_node(self.node_idx);
        try self.indent();
        try self.tree.appendSlice("body:\n");
        try self.parse_node(self.node_idx);
        // try self.parse_node(self.node_data[index].rhs);
        // try self.statement(stmt.body);

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    // fn statement(self: *Self, stmt: *const Ast.Stmt) !void {
    //     try switch (stmt.*) {
    //         .Assignment => |*s| self.assignment(s),
    //         .Discard => |*s| self.discard(s),
    //         .FnDecl => |*s| self.fn_decl(s),
    //         .Print => |*s| self.print_stmt(s),
    //         .Use => |*s| self.use_stmt(s),
    //         .VarDecl => |*s| self.var_decl(s),
    //         .While => |*s| self.while_stmt(s),
    //         .Expr => |s| self.expression(s),
    //     };
    // }

    // fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Assignment\n");
    //     self.indent_level += 1;
    //     try self.indent();
    //     try self.tree.appendSlice("assigne:\n");
    //     try self.expression(stmt.assigne);
    //     try self.indent();
    //     try self.tree.appendSlice("value:\n");
    //     try self.expression(stmt.value);
    //
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn discard(self: *Self, stmt: *const Ast.Discard) !void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Discard\n");
    //     self.indent_level += 1;
    //     try self.expression(stmt.expr);
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn fn_decl(self: *Self, stmt: *const Ast.FnDecl) !void {
    //     try self.indent();
    //
    //     const return_type = if (stmt.return_type) |rt|
    //         self.print_type(rt)
    //     else
    //         "void";
    //
    //     var buf: [100]u8 = undefined;
    //     var written = try std.fmt.bufPrint(
    //         &buf,
    //         "[Fn declaration {s}, type {s}, arity {}\n",
    //         .{ stmt.name.text, return_type, stmt.arity },
    //     );
    //     try self.tree.appendSlice(written);
    //     self.indent_level += 1;
    //     try self.indent();
    //     try self.tree.appendSlice("params:\n");
    //     self.indent_level += 1;
    //
    //     for (0..stmt.arity) |i| {
    //         try self.indent();
    //         written = try std.fmt.bufPrint(
    //             &buf,
    //             "{s}, type {s}\n",
    //             .{ stmt.params[i].name.text, self.print_type(stmt.params[i].type_) },
    //         );
    //         try self.tree.appendSlice(written);
    //     }
    //     self.indent_level -= 1;
    //
    //     try self.indent();
    //     try self.tree.appendSlice("body:\n");
    //     self.indent_level += 1;
    //     try self.block_expr(&stmt.body);
    //     self.indent_level -= 1;
    //
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn print_stmt(self: *Self, stmt: *const Ast.Print) !void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Print]\n");
    //     self.indent_level += 1;
    //     try self.expression(stmt.expr);
    //     self.indent_level -= 1;
    // }

    // fn use_stmt(self: *Self, stmt: *const Ast.Use) !void {
    //     try self.indent();
    //     var writer = self.tree.writer();
    //     try writer.print("[Use ", .{});
    //
    //     for (stmt.module, 0..) |m, i| {
    //         try writer.print("{s}", .{m.text});
    //
    //         if (i < stmt.module.len - 1) {
    //             try writer.print(" ", .{});
    //         }
    //     }
    //     try writer.print("]\n", .{});
    // }

    // fn var_decl(self: *Self, stmt: *const Ast.VarDecl) !void {
    //     try self.indent();
    //     var buf: [100]u8 = undefined;
    //
    //     const type_name = if (stmt.type_) |t| self.print_type(t) else "void";
    //     const written = try std.fmt.bufPrint(
    //         &buf,
    //         "[Var declaration {s}, type {s}, value\n",
    //         .{ stmt.name.text, type_name },
    //     );
    //     try self.tree.appendSlice(written);
    //
    //     self.indent_level += 1;
    //
    //     if (stmt.value) |v| {
    //         try self.expression(v);
    //     } else {
    //         try self.indent();
    //         try self.tree.appendSlice("none\n");
    //     }
    //
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn while_stmt(self: *Self, stmt: *const Ast.While) Error!void {
    //     try self.indent();
    //     try self.tree.appendSlice("[While\n");
    //     self.indent_level += 1;
    //     try self.indent();
    //     try self.tree.appendSlice("condition:\n");
    //     try self.expression(stmt.condition);
    //     try self.indent();
    //     try self.tree.appendSlice("body:\n");
    //     try self.statement(stmt.body);
    //
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn expression(self: *Self, expr: *const Expr) Error!void {
    //     try switch (expr.*) {
    //         .Block => |*e| self.block_expr(e),
    //         .BinOp => |*e| self.binop_expr(e),
    //         .BoolLit => |*e| self.bool_expr(e),
    //         .FnCall => |*e| self.fn_call(e),
    //         .FloatLit => |*e| self.float_expr(e),
    //         .Grouping => |*e| self.grouping_expr(e),
    //         .Identifier => |*e| self.ident_expr(e),
    //         .If => |*e| self.if_expr(e),
    //         .IntLit => |*e| self.int_expr(e),
    //         .NullLit => self.null_expr(),
    //         .Return => |*e| self.return_expr(e),
    //         .StringLit => |*e| self.string_expr(e),
    //         .Unary => |*e| self.unary_expr(e),
    //     };
    // }

    // fn block_expr(self: *Self, expr: *const Ast.Block) Error!void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Block]\n");
    //
    //     self.indent_level += 1;
    //
    //     for (expr.stmts) |*s| try self.statement(s);
    //
    //     self.indent_level -= 1;
    // }

    // fn float_expr(self: *Self, expr: *const Ast.FloatLit) Error!void {
    //     try self.indent();
    //     var buf: [100]u8 = undefined;
    //     const written = try std.fmt.bufPrint(&buf, "[Float literal {d}]\n", .{expr.value});
    //     try self.tree.appendSlice(written);
    // }

    // fn fn_call(self: *Self, expr: *const Ast.FnCall) Error!void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Fn call\n");
    //     self.indent_level += 1;
    //
    //     try self.indent();
    //     try self.tree.appendSlice("callee:\n");
    //     try self.expression(expr.callee);
    //     try self.indent();
    //     try self.tree.appendSlice("args:\n");
    //
    //     for (0..expr.arity) |i| {
    //         try self.expression(expr.args[i]);
    //     }
    //
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn grouping_expr(self: *Self, expr: *const Ast.Grouping) Error!void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Grouping]\n");
    //
    //     self.indent_level += 1;
    //     try self.expression(expr.expr);
    //     self.indent_level -= 1;
    // }

    // fn ident_expr(self: *Self, expr: *const Ast.Identifier) Error!void {
    //     try self.indent();
    //     var buf: [100]u8 = undefined;
    //     const written = try std.fmt.bufPrint(&buf, "[Identifier {s}]\n", .{expr.name});
    //     try self.tree.appendSlice(written);
    // }

    // fn if_expr(self: *Self, expr: *const Ast.If) Error!void {
    //     try self.indent();
    //
    //     try self.tree.appendSlice("[If\n");
    //     self.indent_level += 1;
    //     try self.indent();
    //     try self.tree.appendSlice("condition:\n");
    //     try self.expression(expr.condition);
    //     try self.indent();
    //     try self.tree.appendSlice("then body:\n");
    //     try self.statement(&expr.then_body);
    //     try self.indent();
    //     try self.tree.appendSlice("else body:\n");
    //     if (expr.else_body) |*body| {
    //         try self.statement(body);
    //     } else {
    //         try self.indent();
    //         try self.tree.appendSlice("none\n");
    //     }
    //
    //     self.indent_level -= 1;
    //     try self.indent();
    //     try self.tree.appendSlice("]\n");
    // }

    // fn int_expr(self: *Self, expr: *const Ast.IntLit) Error!void {
    //     try self.indent();
    //     var buf: [100]u8 = undefined;
    //     const written = try std.fmt.bufPrint(&buf, "[Int literal {}]\n", .{expr.value});
    //     try self.tree.appendSlice(written);
    // }

    // fn null_expr(self: *Self) Error!void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Null literal]\n");
    // }

    // fn return_expr(self: *Self, expr: *const Ast.Return) Error!void {
    //     try self.indent();
    //     try self.tree.appendSlice("[Return");
    //
    //     if (expr.expr) |e| {
    //         self.indent_level += 1;
    //         try self.expression(e);
    //         self.indent_level -= 1;
    //     }
    //
    //     try self.tree.appendSlice("]\n");
    // }

    // fn string_expr(self: *Self, expr: *const Ast.StringLit) Error!void {
    //     try self.indent();
    //     var buf: [100]u8 = undefined;
    //     const written = try std.fmt.bufPrint(&buf, "[String literal {s}]\n", .{expr.value});
    //     try self.tree.appendSlice(written);
    // }

    // fn unary_expr(self: *Self, expr: *const Ast.Unary) Error!void {
    //     try self.indent();
    //
    //     var buf: [100]u8 = undefined;
    //     const written = try std.fmt.bufPrint(&buf, "[Unary {s}]\n", .{expr.op.symbol()});
    //     try self.tree.appendSlice(written);
    //
    //     self.indent_level += 1;
    //     try self.expression(expr.rhs);
    //     self.indent_level -= 1;
    // }
};
