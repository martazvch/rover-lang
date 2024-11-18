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
    tree: std.ArrayList(u8),
    allocator: std.mem.Allocator,

    const indent_size: u8 = 4;
    const spaces: [1024]u8 = [_]u8{' '} ** 1024;

    const Error = std.mem.Allocator.Error || std.fmt.BufPrintError;
    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .indent_level = 0,
            .tree = std.ArrayList(u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn reinit(self: *Self) void {
        self.tree.clearRetainingCapacity();
    }

    pub fn deinit(self: *Self) void {
        self.tree.deinit();
    }

    pub fn parse_ast(self: *Self, nodes: []const Stmt) !void {
        for (nodes) |node| {
            try switch (node) {
                .Expr => |e| self.print_expr(e),
                else => @panic("todo"),
            };
        }
    }

    pub fn display(self: *const Self) void {
        print("{s}", .{self.tree.items});
    }

    fn print_expr(self: *Self, expr: *const Expr) Error!void {
        try switch (expr.*) {
            .BinOp => |*e| self.binop_expr(e),
            .Grouping => |*e| self.grouping_expr(e),
            .IntLit => |*e| self.int_expr(e),
            .Unary => |*e| self.unary_expr(e),
        };
    }

    fn binop_expr(self: *Self, expr: *const Ast.BinOp) Error!void {
        try self.indent();

        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Binop {s}]\n", .{expr.op.lexeme});
        try self.tree.appendSlice(written);

        self.indent_level += 1;
        try self.print_expr(expr.lhs);
        try self.print_expr(expr.rhs);
        self.indent_level -= 1;
    }

    fn grouping_expr(self: *Self, expr: *const Ast.Grouping) Error!void {
        try self.indent();
        try self.tree.appendSlice("[Grouping]\n");

        self.indent_level += 1;
        try self.print_expr(expr.expr);
        self.indent_level -= 1;
    }

    fn int_expr(self: *Self, expr: *const Ast.IntLit) Error!void {
        try self.indent();
        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Int literal {}]\n", .{expr.value});
        try self.tree.appendSlice(written);
    }

    fn unary_expr(self: *Self, expr: *const Ast.Unary) Error!void {
        try self.indent();

        var buf: [100]u8 = undefined;
        const written = try std.fmt.bufPrint(&buf, "[Unary {s}]\n", .{expr.op.lexeme});
        try self.tree.appendSlice(written);

        self.indent_level += 1;
        try self.print_expr(expr.rhs);
        self.indent_level -= 1;
    }

    fn indent(self: *Self) !void {
        try self.tree.appendSlice(Self.spaces[0 .. self.indent_level * Self.indent_size]);
    }
};
