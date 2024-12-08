const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const activeTag = std.meta.activeTag;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Span = Ast.Span;
const AnalyzedAst = @import("analyzed_ast.zig");
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;

pub const Type = union(enum) {
    Bool,
    Float,
    Int,
    Null,
    Str,
    Struct: []const u8,

    const Self = @This();

    const types = std.StaticStringMap(Type).initComptime(.{
        .{ "bool", .Bool },
        .{ "float", .Float },
        .{ "int", .Int },
        .{ "null", .Null },
        .{ "str", .Str },
    });

    pub fn get_type(name: []const u8) Type {
        if (types.get(name)) |t| {
            return t;
        } else {
            return .{ .Struct = name };
        }
    }

    pub fn str(self: Self) []const u8 {
        return switch (self) {
            .Bool => "bool",
            .Float => "float",
            .Int => "int",
            .Null => "null",
            .Str => "string",
            .Struct => |t| t,
        };
    }
};

pub const AstExtra = struct {
    binops: ArrayList(AnalyzedAst.BinOp),
    unaries: ArrayList(AnalyzedAst.Unary),
    variables: ArrayList(AnalyzedAst.Variable),

    pub const Iter = struct {
        binops: UnsafeIter(AnalyzedAst.BinOp),
        unaries: UnsafeIter(AnalyzedAst.Unary),
        variables: UnsafeIter(AnalyzedAst.Variable),
    };

    pub fn init(allocator: Allocator) AstExtra {
        return .{
            .binops = ArrayList(AnalyzedAst.BinOp).init(allocator),
            .unaries = ArrayList(AnalyzedAst.Unary).init(allocator),
            .variables = ArrayList(AnalyzedAst.Variable).init(allocator),
        };
    }

    pub fn deinit(self: *AstExtra) void {
        self.binops.deinit();
        self.unaries.deinit();
        self.variables.deinit();
    }

    pub fn reinit(self: *AstExtra) void {
        self.binops.clearRetainingCapacity();
        self.unaries.clearRetainingCapacity();
        self.variables.clearRetainingCapacity();
    }

    pub fn as_iter(self: *AstExtra) Iter {
        return .{
            .binops = UnsafeIter(AnalyzedAst.BinOp).init(self.binops.items),
            .unaries = UnsafeIter(AnalyzedAst.Unary).init(self.unaries.items),
            .variables = UnsafeIter(AnalyzedAst.Variable).init(self.variables.items),
        };
    }
};

pub const Analyzer = struct {
    errs: ArrayList(AnalyzerReport),
    warns: ArrayList(AnalyzerReport),
    var_types: StringHashMap(Type),
    ast_extras: AstExtra,

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .var_types = StringHashMap(Type).init(allocator),
            .ast_extras = AstExtra.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
        self.warns.deinit();
        self.var_types.deinit();
        self.ast_extras.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.errs.clearRetainingCapacity();
        self.warns.clearRetainingCapacity();
        self.var_types.clearRetainingCapacity();
        self.ast_extras.reinit();
    }

    fn is_numeric(t: Type) bool {
        return (t == .Int or t == .Float);
    }

    fn err(self: *Self, kind: AnalyzerMsg, span: Span) !void {
        const report = AnalyzerReport.err(kind, span);
        try self.errs.append(report);
        return error.Err;
    }

    fn warn(self: *Self, kind: AnalyzerMsg, span: Span) !void {
        const report = AnalyzerReport.warn(kind, span);
        try self.warns.append(report);
    }

    pub fn analyze(self: *Self, stmts: []const Stmt) !void {
        for (stmts) |*stmt| {
            _ = self.statement(stmt) catch |e| {
                switch (e) {
                    // If it's our own error, we continue
                    error.Err => continue,
                    else => return e,
                }
            };
        }
    }

    fn statement(self: *Self, stmt: *const Stmt) !Type {
        return switch (stmt.*) {
            .Print => |*s| {
                _ = try self.expression(s.expr);
                return .Null;
            },
            .VarDecl => |*s| self.var_declaration(s),
            .Expr => |e| self.expression(e),
        };
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !Type {
        var final_type: Type = .Null;

        if (stmt.type_) |t| {
            final_type = t;
        }

        if (stmt.value) |v| {
            const value_type = try self.expression(v);

            // If no type declared, we infer the value type
            if (final_type == .Null) {
                final_type = value_type;
                // Else, we check for coherence
            } else if (activeTag(final_type) != activeTag(value_type)) {
                // One case in wich we can coerce; int -> float
                if (final_type == .Float and value_type == .Int) {} else {
                    try self.err(
                        .{ .InvalidVarDeclType = .{
                            .expect = final_type.str(),
                            .found = value_type.str(),
                        } },
                        v.span(),
                    );
                }
            }
        }

        try self.var_types.put(stmt.name, final_type);
        return .Null;
    }

    fn expression(self: *Self, expr: *const Expr) !Type {
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

    fn binop(self: *Self, expr: *const Ast.BinOp) Error!Type {
        // We reserve the slot because of recursion
        const id = self.ast_extras.binops.items.len;
        try self.ast_extras.binops.append(.{});
        var binop_extra = &self.ast_extras.binops.items[id];

        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        binop_extra.type_ = lhs;

        var res = lhs;

        // String operations
        if (expr.op == .Plus and lhs == .Str and rhs == .Str) {
            return .Str;
        } else if (expr.op == .Star) {
            if ((lhs == .Str and rhs == .Int) or (lhs == .Int and rhs == .Str)) {
                binop_extra.type_ = .Str;

                // For string concatenation, we use the cast information to tell
                // on wich side is the integer (for the compiler)
                binop_extra.cast = if (rhs == .Int) .Rhs else .Lhs;
                return .Str;
            }
        }

        switch (expr.op) {
            // Arithmetic binop
            .Plus, .Minus, .Star, .Slash => {
                if (!Analyzer.is_numeric(lhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(lhs.str()),
                        expr.lhs.span(),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(rhs.str()),
                        expr.rhs.span(),
                    );
                }

                switch (lhs) {
                    .Float => {
                        switch (rhs) {
                            .Float => {},
                            .Int => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast(.Rhs, lhs.str()),
                                    expr.rhs.span(),
                                );

                                binop_extra.cast = .Rhs;
                            },
                            else => unreachable,
                        }
                    },
                    .Int => {
                        switch (rhs) {
                            .Float => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast(.Lhs, rhs.str()),
                                    expr.lhs.span(),
                                );

                                binop_extra.type_ = .Float;
                                binop_extra.cast = .Lhs;
                                res = .Float;
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
                            binop_extra.cast = .Lhs;

                            try self.warn(.FloatEqualCast, expr.lhs.span());
                        } else {
                            binop_extra.cast = .Rhs;

                            try self.warn(.FloatEqualCast, expr.rhs.span());
                        }

                        binop_extra.type_ = .Float;
                    } else {
                        try self.err(
                            AnalyzerMsg.invalid_cmp(lhs.str(), rhs.str()),
                            expr.span,
                        );
                    }
                } else {
                    // Check for unsafe float comparisons
                    if (lhs == .Float) {
                        try self.warn(.FloatEqual, expr.span);
                    }
                }

                res = .Bool;
            },

            .Greater, .GreaterEqual, .Less, .LessEqual => {
                if (!Analyzer.is_numeric(lhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(lhs.str()),
                        expr.lhs.span(),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(rhs.str()),
                        expr.rhs.span(),
                    );
                }

                switch (lhs) {
                    .Float => switch (rhs) {
                        .Float => try self.warn(.FloatEqual, expr.span),
                        .Int => {
                            try self.warn(.FloatEqualCast, expr.rhs.span());

                            binop_extra.cast = .Rhs;
                        },
                        else => unreachable,
                    },
                    .Int => switch (rhs) {
                        .Float => {
                            try self.warn(.FloatEqualCast, expr.lhs.span());

                            binop_extra.cast = .Lhs;
                            binop_extra.type_ = .Float;
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

    fn grouping(self: *Self, expr: *const Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        const id = self.ast_extras.unaries.items.len;
        try self.ast_extras.unaries.append(undefined);
        var unary_extra = &self.ast_extras.unaries.items[id];

        const rhs = try self.expression(expr.rhs);

        if (expr.op == .Not and rhs != .Bool) {
            try self.err(
                .{ .InvalidUnary = .{ .found = rhs.str() } },
                expr.rhs.span(),
            );
        } else if (expr.op == .Minus and rhs != .Int and rhs != .Float) {
            try self.err(
                AnalyzerMsg.invalid_arithmetic(rhs.str()),
                expr.rhs.span(),
            );
        }

        unary_extra.type_ = rhs;

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
