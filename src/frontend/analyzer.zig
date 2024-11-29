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
    binop_infos: ArrayList(BinopInfos),

    // We assume we only need to know the side because the cast will always
    // be from int to float
    pub const BinopInfos = struct {
        cast: Side = .None,
        res_type: Type,

        const Side = enum {
            Lhs,
            Rhs,
            None,
        };
    };

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .types = StringHashMap(Type).init(allocator),
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .binop_infos = ArrayList(BinopInfos).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
        self.errs.deinit();
        self.warns.deinit();
        self.binop_infos.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.types.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
        self.warns.clearRetainingCapacity();
        self.binop_infos.clearRetainingCapacity();
    }

    fn is_numeric(t: Type) bool {
        return (t == .Int or t == .Float);
    }

    pub fn analyze(self: *Self, stmts: []Stmt) !void {
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

    fn statement(self: *Self, stmt: *Stmt) !Type {
        return switch (stmt.*) {
            .VarDecl => unreachable,
            .Expr => |e| self.expression(e),
        };
    }

    fn expression(self: *Self, expr: *Expr) !Type {
        return switch (expr.*) {
            .BoolLit => .Bool,
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => .Float,
            .IntLit => .Int,
            .NullLit => .Null,
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *Ast.BinOp) Error!Type {
        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        var res: Type = lhs;
        var binop_infos: BinopInfos = .{ .res_type = lhs };

        switch (expr.op) {
            // Arithmetic binop
            .Plus, .Minus, .Star, .Slash => {
                if (!Analyzer.is_numeric(lhs)) {
                    try self.errs.append(AnalyzerReport.err(
                        AnalyzerMsg.invalid_arithmetic(lhs.str()),
                        expr.lhs.span(),
                    ));
                    return error.Err;
                }

                if (!Analyzer.is_numeric(rhs)) {
                    try self.errs.append(AnalyzerReport.err(
                        AnalyzerMsg.invalid_arithmetic(rhs.str()),
                        expr.rhs.span(),
                    ));
                    return error.Err;
                }

                switch (lhs) {
                    .Float => {
                        switch (rhs) {
                            .Float => {},
                            .Int => {
                                try self.warns.append(AnalyzerReport.warn(
                                    AnalyzerMsg.implicit_cast(.Rhs, lhs.str()),
                                    expr.rhs.span(),
                                ));

                                binop_infos.cast = .Rhs;
                            },
                            else => unreachable,
                        }
                    },
                    .Int => {
                        switch (rhs) {
                            .Float => {
                                try self.warns.append(AnalyzerReport.warn(
                                    AnalyzerMsg.implicit_cast(.Lhs, rhs.str()),
                                    expr.lhs.span(),
                                ));

                                res = .Float;
                                binop_infos.cast = .Lhs;
                                binop_infos.res_type = .Float;
                            },
                            .Int => {},
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .EqualEqual, .BangEqual => {
                // If different value types
                if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) {
                    // Check for implicit casts
                    if ((lhs == .Int and rhs == .Float) or (lhs == .Float and rhs == .Int)) {
                        if (lhs == .Int) {
                            binop_infos.cast = .Lhs;

                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.lhs.span()));
                        } else {
                            binop_infos.cast = .Rhs;

                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.rhs.span()));
                        }
                    } else {
                        try self.errs.append(AnalyzerReport.err(
                            AnalyzerMsg.invalid_cmp(lhs.str(), rhs.str()),
                            expr.span,
                        ));
                        return error.Err;
                    }
                } else {
                    // Check for unsafe float comparisons
                    if (lhs == .Float) {
                        try self.warns.append(AnalyzerReport.warn(.FloatEqual, expr.span));
                    }
                }

                res = .Bool;
            },

            .Greater, .GreaterEqual, .Less, .LessEqual => {
                if (!Analyzer.is_numeric(lhs)) {
                    try self.errs.append(AnalyzerReport.err(
                        AnalyzerMsg.invalid_arithmetic(lhs.str()),
                        expr.lhs.span(),
                    ));
                    return error.Err;
                }

                if (!Analyzer.is_numeric(rhs)) {
                    try self.errs.append(AnalyzerReport.err(
                        AnalyzerMsg.invalid_arithmetic(rhs.str()),
                        expr.rhs.span(),
                    ));
                    return error.Err;
                }

                switch (lhs) {
                    .Float => switch (rhs) {
                        .Float => try self.warns.append(AnalyzerReport.warn(.FloatEqual, expr.span)),
                        .Int => {
                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.rhs.span()));

                            binop_infos.cast = .Rhs;
                        },
                        else => unreachable,
                    },
                    .Int => switch (rhs) {
                        .Float => {
                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.lhs.span()));

                            binop_infos.cast = .Lhs;
                            binop_infos.res_type = .Float;
                        },
                        .Int => {},
                        else => unreachable,
                    },
                    else => unreachable,
                }

                res = .Bool;
            },

            // Logical binop
            .And, .Or => unreachable,
            else => unreachable,
        }

        try self.binop_infos.append(binop_infos);
        return res;
    }

    fn grouping(self: *Self, expr: *Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn unary(self: *Self, expr: *Ast.Unary) Error!Type {
        const rhs = try self.expression(expr.rhs);

        if (expr.op == .Not and rhs != .Bool) {
            try self.errs.append(AnalyzerReport.err(
                .{ .InvalidUnary = .{ .found = rhs.str() } },
                expr.rhs.span(),
            ));
        } else if (expr.op == .Minus and rhs != .Int and rhs != .Float) {
            try self.errs.append(AnalyzerReport.err(
                AnalyzerMsg.invalid_arithmetic(rhs.str()),
                expr.rhs.span(),
            ));

            return error.Err;
        }

        expr.type_ = rhs;

        return rhs;
    }
};

// Test
test Analyzer {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_analyzer.zig").get_test_data;

    const Tester = GenericTester("analyzer", AnalyzerMsg, get_test_data);
    try Tester.run();
}
