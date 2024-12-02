const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;

pub const Type = union(enum) {
    Bool,
    Float,
    Int,
    Null,
    Obj,
    Str,

    const Self = @This();

    pub fn str(self: Self) []const u8 {
        return switch (self) {
            .Bool => "bool",
            .Float => "float",
            .Int => "int",
            .Null => "null",
            .Obj => "object",
            .Str => "string",
        };
    }
};

pub const Analyzer = struct {
    types: StringHashMap(Type),
    errs: ArrayList(AnalyzerReport),
    warns: ArrayList(AnalyzerReport),
    binop_casts: ArrayList(BinopCast),

    // We assume we only need to know the side because the cast will always
    // be from int to float
    pub const BinopCast = enum {
        Lhs,
        Rhs,
        None,
    };

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .types = StringHashMap(Type).init(allocator),
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .binop_casts = ArrayList(BinopCast).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit();
        self.errs.deinit();
        self.warns.deinit();
        self.binop_casts.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.types.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
        self.warns.clearRetainingCapacity();
        self.binop_casts.clearRetainingCapacity();
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
            .Print => |*e| {
                _ = try self.expression(e.expr);
                return .Null;
            },
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
            .StringLit => .Str,
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *Ast.BinOp) Error!Type {
        // We reserve the slot because of recursion
        const id = self.binop_casts.items.len;
        try self.binop_casts.append(.None);

        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        expr.type_ = lhs;
        var res = lhs;

        if (expr.op == .Plus and lhs == .Str and rhs == .Str) {
            return .Str;
        } else if (expr.op == .Star) {
            if ((lhs == .Str and rhs == .Int) or (lhs == .Int and rhs == .Str)) {
                expr.type_ = .Str;

                // For string concatenation, we use the cast information to tell
                // on wich side is the integer (for the compiler)
                const cast: BinopCast = if (rhs == .Int) .Rhs else .Lhs;
                self.binop_casts.items[id] = cast;
                return .Str;
            }
        }

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

                                self.binop_casts.items[id] = .Rhs;
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

                                self.binop_casts.items[id] = .Lhs;
                                res = .Float;
                                expr.type_ = .Float;
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
                            self.binop_casts.items[id] = .Lhs;

                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.lhs.span()));
                        } else {
                            self.binop_casts.items[id] = .Rhs;

                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.rhs.span()));
                        }

                        expr.type_ = .Float;
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

                            self.binop_casts.items[id] = .Rhs;
                        },
                        else => unreachable,
                    },
                    .Int => switch (rhs) {
                        .Float => {
                            try self.warns.append(AnalyzerReport.warn(.FloatEqualCast, expr.lhs.span()));

                            self.binop_casts.items[id] = .Lhs;
                            expr.type_ = .Float;
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
