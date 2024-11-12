const std = @import("std");
const print = std.debug.print;
const Ast = @import("ast.zig");
const Expr = Ast.Expr;
const Stmt = Ast.Stmt;
//
// fn Visitor(
//     comptime Ctx: type,
//     comptime Res: type,
// ) type {
//     return struct {
//         vtable: *const VTable,
//
//         const VTable = struct {
//             int_expr: *const fn (*Ctx, Ast.IntLit) Res,
//             unary_expr: *const fn (*Ctx, Ast.Unary) Res,
//             binop_expr: *const fn (*Ctx, Ast.BinOp) Res,
//         };
//         const Self = @This();
//
//         fn int_expr(self: *const Self, expr: Ast.IntLit) Res {
//             return self.vtable.int_expr(expr);
//         }
//
//         fn unary_expr(self: *const Self, expr: Ast.Unary) Res {
//             return self.vtable.unary_expr(expr);
//         }
//
//         fn binop_expr(self: *const Self, expr: Ast.BinOp) Res {
//             return self.vtable.binop_expr(expr);
//         }
//     };
// }
//
// const AstPrinterVisitor = Visitor(AstPrinter, void);

// pub fn visitor() AstPrinterVisitor {
//     return .{
//         .vtable = &.{
//             .int_expr = Self.int_expr,
//             .unary_expr = Self.unary_expr,
//             .binop_expr = Self.binop_expr,
//         },
//     };
// }

pub const AstPrinter = struct {
    indent_level: u8 = 0,

    const indent_size: u8 = 2;
    const spaces: [1024]u8 = [_]u8{' '} ** 1024;
    const Self = @This();

    pub fn print_ast(self: *Self, nodes: []const Stmt) void {
        for (nodes) |node| {
            switch (node) {
                .Expr => |e| self.print_expr(e),
                else => @panic("todo"),
            }
        }
    }

    fn print_expr(self: *Self, expr: *const Expr) void {
        switch (expr.*) {
            .IntLit => |*e| self.int_expr(e),
            .Unary => |*e| self.unary_expr(e),
            .BinOp => |*e| self.binop_expr(e),
        }
    }

    pub fn int_expr(self: *Self, expr: *const Ast.IntLit) void {
        self.indent();
        print("[Int literal {}]\n", .{expr.value});
    }

    pub fn unary_expr(self: *Self, expr: *const Ast.Unary) void {
        self.indent();
        print("[Unary {s}]\n", .{expr.op.lexeme});
        self.indent_level += 1;
        self.print_expr(expr.rhs);
        self.indent_level -= 1;
    }

    pub fn binop_expr(self: *Self, expr: *const Ast.BinOp) void {
        self.indent();
        print("[Binop {s}]\n", .{expr.op.lexeme});
        self.indent_level += 1;
        self.print_expr(expr.rhs);
        self.print_expr(expr.lhs);
        self.indent_level -= 1;
    }

    fn indent(self: *const Self) void {
        print("{s}", .{Self.spaces[0 .. self.indent_level * Self.indent_size]});
    }
};
