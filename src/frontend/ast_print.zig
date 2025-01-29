const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Ast = @import("ast.zig");
const Expr = Ast.Expr;
const Stmt = Ast.Stmt;

pub const AstPrinter = struct {
    source: []const u8,
    indent_level: u8 = 0,
    tree: std.ArrayList(u8),

    const indent_size: u8 = 4;
    const spaces: [1024]u8 = [_]u8{' '} ** 1024;

    const Error = Allocator.Error || std.fmt.BufPrintError;
    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .source = undefined,
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

    pub fn parse_ast(self: *Self, source: []const u8, stmts: []const Stmt) !void {
        self.source = source;

        for (stmts) |*stmt| {
            try self.statement(stmt);
        }
    }

    fn print_type(self: *Self, type_: Ast.Type) []const u8 {
        _ = self;
        return switch (type_) {
            .Entity => |t| t.text,
            .Function => |_| "",
        };
    }

    fn statement(self: *Self, stmt: *const Ast.Stmt) !void {
        try switch (stmt.*) {
            .Assignment => |*s| self.assignment(s),
            .Discard => |*s| self.discard(s),
            .FnDecl => |*s| self.fn_decl(s),
            .Print => |*s| self.print_stmt(s),
            .Use => |*s| self.use_stmt(s),
            .VarDecl => |*s| self.var_decl(s),
            .While => |*s| self.while_stmt(s),
            .Expr => |s| self.expression(s),
        };
    }

    fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
        try self.indent();
        try self.tree.appendSlice("[Assignment\n");
        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("assigne:\n");
        try self.expression(stmt.assigne);
        try self.indent();
        try self.tree.appendSlice("value:\n");
        try self.expression(stmt.value);

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn discard(self: *Self, stmt: *const Ast.Discard) !void {
        try self.indent();
        try self.tree.appendSlice("[Discard\n");
        self.indent_level += 1;
        try self.expression(stmt.expr);
        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn fn_decl(self: *Self, stmt: *const Ast.FnDecl) !void {
        try self.indent();

        const return_type = if (stmt.return_type) |rt|
            self.print_type(rt)
        else
            "void";

        var buf: [100]u8 = undefined;
        var written = try std.fmt.bufPrint(
            &buf,
            "[Fn declaration {s}, type {s}, arity {}\n",
            .{ stmt.name.text, return_type, stmt.arity },
        );
        try self.tree.appendSlice(written);
        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("params:\n");
        self.indent_level += 1;

        for (0..stmt.arity) |i| {
            try self.indent();
            written = try std.fmt.bufPrint(
                &buf,
                "{s}, type {s}\n",
                .{ stmt.params[i].name.text, self.print_type(stmt.params[i].type_) },
            );
            try self.tree.appendSlice(written);
        }
        self.indent_level -= 1;

        try self.indent();
        try self.tree.appendSlice("body:\n");
        self.indent_level += 1;
        try self.block_expr(&stmt.body);
        self.indent_level -= 1;

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn print_stmt(self: *Self, stmt: *const Ast.Print) !void {
        try self.indent();
        try self.tree.appendSlice("[Print]\n");
        self.indent_level += 1;
        try self.expression(stmt.expr);
        self.indent_level -= 1;
    }

    fn use_stmt(self: *Self, stmt: *const Ast.Use) !void {
        try self.indent();
        var writer = self.tree.writer();
        try writer.print("[Use ", .{});

        for (stmt.module, 0..) |m, i| {
            try writer.print("{s}", .{m.text});

            if (i < stmt.module.len - 1) {
                try writer.print(" ", .{});
            }
        }
        try writer.print("]\n", .{});
    }

    fn var_decl(self: *Self, stmt: *const Ast.VarDecl) !void {
        try self.indent();
        var buf: [100]u8 = undefined;

        const type_name = if (stmt.type_) |t| self.print_type(t) else "void";
        const written = try std.fmt.bufPrint(
            &buf,
            "[Var declaration {s}, type {s}, value\n",
            .{ stmt.name.text, type_name },
        );
        try self.tree.appendSlice(written);

        self.indent_level += 1;

        if (stmt.value) |v| {
            try self.expression(v);
        } else {
            try self.indent();
            try self.tree.appendSlice("none\n");
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn while_stmt(self: *Self, stmt: *const Ast.While) Error!void {
        try self.indent();
        try self.tree.appendSlice("[While\n");
        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("condition:\n");
        try self.expression(stmt.condition);
        try self.indent();
        try self.tree.appendSlice("body:\n");
        try self.statement(stmt.body);

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn expression(self: *Self, expr: *const Expr) Error!void {
        try switch (expr.*) {
            .Block => |*e| self.block_expr(e),
            .BinOp => |*e| self.binop_expr(e),
            .BoolLit => |*e| self.bool_expr(e),
            .FnCall => |*e| self.fn_call(e),
            .FloatLit => |*e| self.float_expr(e),
            .Grouping => |*e| self.grouping_expr(e),
            .Identifier => |*e| self.ident_expr(e),
            .If => |*e| self.if_expr(e),
            .IntLit => |*e| self.int_expr(e),
            .NullLit => self.null_expr(),
            .Return => |*e| self.return_expr(e),
            .StringLit => |*e| self.string_expr(e),
            .Unary => |*e| self.unary_expr(e),
        };
    }

    fn block_expr(self: *Self, expr: *const Ast.Block) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Block]\n");

        self.indent_level += 1;

        for (expr.stmts) |*s| try self.statement(s);

        self.indent_level -= 1;
    }

    fn binop_expr(self: *Self, expr: *const Ast.BinOp) Error!void {
        try self.indent();

        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Binop {s}]\n", .{expr.op.symbol()});
        try self.tree.appendSlice(written);

        self.indent_level += 1;
        try self.expression(expr.lhs);
        try self.expression(expr.rhs);
        self.indent_level -= 1;
    }

    fn bool_expr(self: *Self, expr: *const Ast.BoolLit) Error!void {
        try self.indent();
        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Bool literal {}]\n", .{expr.value});
        try self.tree.appendSlice(written);
    }

    fn float_expr(self: *Self, expr: *const Ast.FloatLit) Error!void {
        try self.indent();
        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Float literal {d}]\n", .{expr.value});
        try self.tree.appendSlice(written);
    }

    fn fn_call(self: *Self, expr: *const Ast.FnCall) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Fn call\n");
        self.indent_level += 1;

        try self.indent();
        try self.tree.appendSlice("callee:\n");
        try self.expression(expr.callee);
        try self.indent();
        try self.tree.appendSlice("args:\n");

        for (0..expr.arity) |i| {
            try self.expression(expr.args[i]);
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn grouping_expr(self: *Self, expr: *const Ast.Grouping) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Grouping]\n");

        self.indent_level += 1;
        try self.expression(expr.expr);
        self.indent_level -= 1;
    }

    fn ident_expr(self: *Self, expr: *const Ast.Identifier) Error!void {
        try self.indent();
        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Identifier {s}]\n", .{expr.name});
        try self.tree.appendSlice(written);
    }

    fn if_expr(self: *Self, expr: *const Ast.If) Error!void {
        try self.indent();

        try self.tree.appendSlice("[If\n");
        self.indent_level += 1;
        try self.indent();
        try self.tree.appendSlice("condition:\n");
        try self.expression(expr.condition);
        try self.indent();
        try self.tree.appendSlice("then body:\n");
        try self.statement(&expr.then_body);
        try self.indent();
        try self.tree.appendSlice("else body:\n");
        if (expr.else_body) |*body| {
            try self.statement(body);
        } else {
            try self.indent();
            try self.tree.appendSlice("none\n");
        }

        self.indent_level -= 1;
        try self.indent();
        try self.tree.appendSlice("]\n");
    }

    fn int_expr(self: *Self, expr: *const Ast.IntLit) Error!void {
        try self.indent();
        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Int literal {}]\n", .{expr.value});
        try self.tree.appendSlice(written);
    }

    fn null_expr(self: *Self) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Null literal]\n");
    }

    fn return_expr(self: *Self, expr: *const Ast.Return) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Return");

        if (expr.expr) |e| {
            self.indent_level += 1;
            try self.expression(e);
            self.indent_level -= 1;
        }

        try self.tree.appendSlice("]\n");
    }

    fn string_expr(self: *Self, expr: *const Ast.StringLit) Error!void {
        try self.indent();
        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[String literal {s}]\n", .{expr.value});
        try self.tree.appendSlice(written);
    }

    fn unary_expr(self: *Self, expr: *const Ast.Unary) Error!void {
        try self.indent();

        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Unary {s}]\n", .{expr.op.symbol()});
        try self.tree.appendSlice(written);

        self.indent_level += 1;
        try self.expression(expr.rhs);
        self.indent_level -= 1;
    }
};
