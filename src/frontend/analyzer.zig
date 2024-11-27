const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Type = @import("../runtime/values.zig").Type;

pub const Analyzer = struct {
    types: StringHashMap(Type),
    errs: ArrayList(AnalyzerReport),
    warns: ArrayList(AnalyzerReport),
    analyzed_binops: ArrayList(AnalyzedBinOp),

    pub const AnalyzedBinOp = struct {
        type_: Type,
        cast: Cast = .None,

        const Cast = enum { Lhs, Rhs, None };
    };

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .types = StringHashMap(Type).init(allocator),
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .analyzed_binops = ArrayList(AnalyzedBinOp).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
        self.errs.deinit();
        self.warns.deinit();
        self.analyzed_binops.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.types.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
        self.warns.clearRetainingCapacity();
        self.analyzed_binops.clearRetainingCapacity();
    }

    pub fn analyze(self: *Self, stmts: []const Stmt) !void {
        for (stmts) |*stmt| {
            _ = self.statement(stmt) catch |e| {
                switch (e) {
                    // If it's our own errror, we continue
                    error.Err => continue,
                    else => return e,
                }
            };
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
            .FloatLit => .Float,
            .IntLit => .Int,
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) Error!Type {
        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        var analyzed: AnalyzedBinOp = .{ .type_ = lhs };

        switch (expr.op) {
            // Arithmetic binop
            .Plus, .Minus, .Star, .Slash => {
                switch (lhs) {
                    .Float => {
                        switch (rhs) {
                            .Float => {},
                            .Int => {
                                try self.warns.append(AnalyzerReport.warn(
                                    AnalyzerMsg.implicit_cast(.Rhs, lhs.str()),
                                    expr.rhs.span(),
                                ));

                                analyzed.cast = .Rhs;
                            },
                            else => {
                                try self.errs.append(AnalyzerReport.err(
                                    AnalyzerMsg.invalid_math_binop(rhs.str()),
                                    expr.rhs.span(),
                                ));

                                return error.Err;
                            },
                        }
                    },
                    .Int => {
                        switch (rhs) {
                            .Float => {
                                try self.warns.append(AnalyzerReport.warn(
                                    AnalyzerMsg.implicit_cast(.Lhs, rhs.str()),
                                    expr.lhs.span(),
                                ));

                                analyzed.type_ = .Float;
                                analyzed.cast = .Lhs;
                            },
                            .Int => {},
                            else => {
                                try self.errs.append(AnalyzerReport.err(
                                    AnalyzerMsg.invalid_math_binop(rhs.str()),
                                    expr.rhs.span(),
                                ));

                                return error.Err;
                            },
                        }
                    },
                    else => {
                        try self.errs.append(AnalyzerReport.err(
                            AnalyzerMsg.invalid_math_binop(lhs.str()),
                            expr.lhs.span(),
                        ));

                        return error.Err;
                    },
                }
            },
            // Logical binop
            .And, .Or => unreachable,
            else => unreachable,
        }

        try self.analyzed_binops.append(analyzed);

        return analyzed.type_;
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        const rhs = try self.expression(expr.rhs);

        if (rhs != .Int or rhs != .Float) {
            try self.errs.append(AnalyzerReport.err(
                .{ .InvalidMathBinop = .{ .found = rhs.str() } },
                expr.rhs.span(),
            ));

            return error.Err;
        }

        return rhs;
    }
};
