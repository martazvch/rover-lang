const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;

pub const Analyzer = struct {
    types: StringHashMap(Type),
    errs: ArrayList(AnalyzerReport),

    const Type = enum {
        Int,
        Float,
        Bool,
    };

    const Error = error{Err} || Allocator.Error;
    const Self = @This();

    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .types = StringHashMap(Type).init(allocator),
            .errs = ArrayList(AnalyzerReport).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
        self.errs.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.types.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
    }

    pub fn analyze(self: *Self, stmts: []const Stmt) !void {
        for (stmts) |*stmt| {
            _ = try self.statement(stmt);
        }
    }

    fn statement(self: *Self, stmt: *const Stmt) !Type {
        return switch (stmt.*) {
            .VarDecl => unreachable,
            .Expr => |e| self.expression(e),
        };
    }

    fn expression(self: *Self, expr: *const Expr) !Type {
        return switch (expr.*) {
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .IntLit => .Int,
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) Error!Type {
        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.lhs);

        if (lhs != rhs) {
            return error.Err;
        } else return lhs;
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        return self.expression(expr.rhs);
    }
};
