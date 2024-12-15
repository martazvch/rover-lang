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
const AnalyzedStmt = AnalyzedAst.AnalyzedStmt;
const GenReport = @import("../reporter.zig").GenReport;
const Type = AnalyzedAst.Type;

const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;

const Unknown = AnalyzedAst.Unknown;
const Null = AnalyzedAst.Null;
const Int = AnalyzedAst.Int;
const Float = AnalyzedAst.Float;
const Bool = AnalyzedAst.Bool;
const Str = AnalyzedAst.Str;

pub const TypeManager = struct {
    declared: StringHashMap(Type),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .declared = StringHashMap(Type).init(allocator) };
    }

    pub fn init_builtins(self: *Self) !void {
        try self.declared.put("unknown", Unknown);
        try self.declared.put("null", Null);
        try self.declared.put("bool", Bool);
        try self.declared.put("float", Float);
        try self.declared.put("int", Int);
        try self.declared.put("str", Str);
    }

    pub fn deinit(self: *Self) void {
        self.declared.deinit();
    }

    // pub fn fetch_or_create(self: *Self, type_name: []const u8) !Type {
    //     const entry = try self.declared.getOrPut(type_name);
    //
    //     if (entry.found_existing) {
    //         return entry.value_ptr.*;
    //     } else {
    //         // Minus 1 because it just has been added
    //         const value = self.declared.count() - 1;
    //         entry.value_ptr.* = value;
    //         return value;
    //     }
    // }

    // NOTE:
    // Used only in error mode, no need for performance. If used in
    // performance path, maybe use a ArrayHashMap to retreive with
    // index (as type == index) but every thing else is slow?
    pub fn str(self: *const Self, type_: Type) []const u8 {
        var iter = self.declared.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.* == type_) {
                return entry.key_ptr.*;
            }
        }
        unreachable;
    }
};

pub const Analyzer = struct {
    source: []const u8,
    errs: ArrayList(AnalyzerReport),
    warns: ArrayList(AnalyzerReport),
    globals: StringHashMap(Variable),
    analyzed_stmts: ArrayList(AnalyzedStmt),
    type_manager: TypeManager,

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    // Representation of a variable. Index is the declaration order
    const Variable = struct {
        index: usize,
        type_: Type,
        initialized: bool = false,
    };
    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .source = undefined,
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .globals = StringHashMap(Variable).init(allocator),
            .analyzed_stmts = ArrayList(AnalyzedStmt).init(allocator),
            .type_manager = TypeManager.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
        self.warns.deinit();
        self.globals.deinit();
        self.analyzed_stmts.deinit();
        self.type_manager.deinit();
    }

    fn is_numeric(t: Type) bool {
        return t == Int or t == Float;
    }

    fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
        try self.errs.append(AnalyzerReport.err(kind, span));
        return error.Err;
    }

    fn warn(self: *Self, kind: AnalyzerMsg, span: Span) !void {
        try self.warns.append(AnalyzerReport.warn(kind, span));
    }

    fn span_to_source(self: *const Self, span: Span) []const u8 {
        return self.source[span.start..span.end];
    }

    pub fn analyze(self: *Self, stmts: []const Stmt, source: []const u8) !void {
        self.source = source;

        for (stmts) |*stmt| {
            self.statement(stmt) catch |e| {
                switch (e) {
                    // If it's our own error, we continue
                    error.Err => continue,
                    else => return e,
                }
            };
        }
    }

    fn statement(self: *Self, stmt: *const Stmt) !void {
        try switch (stmt.*) {
            .Assignment => |*s| _ = try self.assignment(s),
            .Print => |*s| _ = try self.expression(s.expr),
            .VarDecl => |*s| self.var_declaration(s),
            .Expr => |e| _ = try self.expression(e),
        };
    }

    fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
        const value_type = try self.expression(stmt.value);

        switch (stmt.assigne.*) {
            .Identifier => |*ident| {
                // Forward declaration to preserve order
                const idx = self.analyzed_stmts.items.len;
                try self.analyzed_stmts.append(.{ .Assignment = .{} });

                const assigne = try self.identifier(ident, false);

                // If type is unknown, we update it
                if (assigne.type_ == Unknown) {
                    assigne.type_ = value_type;
                } else if (assigne.type_ != value_type) {
                    // One case in wich we can coerce; int -> float
                    if (assigne.type_ == Float and value_type == Int) {
                        self.analyzed_stmts.items[idx].Assignment.cast = .Yes;
                    } else {
                        return self.err(
                            .{ .InvalidAssignType = .{
                                .expect = self.type_manager.str(assigne.type_),
                                .found = self.type_manager.str(value_type),
                            } },
                            ident.span,
                        );
                    }
                }

                assigne.initialized = true;
            },
            // Later, manage member, pointer, ...
            else => |*expr| return self.err(.InvalidAssignTarget, expr.span()),
        }
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
        // Name check
        // TODO: combine with identifier()
        if (self.globals.get(stmt.name.text)) |_| {
            return self.err(
                .{ .AlreadyDeclaredVar = .{ .name = stmt.name.text } },
                Span.from_source_slice(stmt.name),
            );
        }

        // Type check
        var final_type = Unknown;
        var variable: Variable = .{ .index = 0, .type_ = Unknown };

        // If a type was declared
        if (stmt.type_) |t| {
            final_type = self.type_manager.declared.get(t.text) orelse {
                return self.err(
                    .{ .UndeclaredType = .{ .found = t.text } },
                    Span.from_source_slice(t),
                );
            };
        }

        // Value type check
        if (stmt.value) |v| {
            const value_type = try self.expression(v);
            var assign_extra: AnalyzedAst.Assignment = .{};

            // If no type declared, we infer the value type
            if (final_type == Unknown) {
                final_type = value_type;
                // Else, we check for coherence
            } else if (final_type != value_type) {
                // One case in wich we can coerce; int -> float
                if (final_type == Float and value_type == Int) {
                    assign_extra.cast = .Yes;
                } else {
                    return self.err(
                        .{ .InvalidAssignType = .{
                            .expect = self.type_manager.str(final_type),
                            .found = self.type_manager.str(value_type),
                        } },
                        v.span(),
                    );
                }
            }

            variable.initialized = true;
            try self.analyzed_stmts.append(.{ .Assignment = assign_extra });
        }

        variable.type_ = final_type;

        const idx = self.globals.count();
        variable.index = idx;

        const var_decl_extra: AnalyzedAst.Variable = .{
            .scope = .Global,
            .index = idx,
        };

        try self.globals.put(stmt.name.text, variable);
        try self.analyzed_stmts.append(.{ .Variable = var_decl_extra });
    }

    fn expression(self: *Self, expr: *const Expr) !Type {
        return switch (expr.*) {
            .BoolLit => Bool,
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => Float,
            .Identifier => |*e| {
                const res = try self.identifier(e, true);
                return res.type_;
            },
            .IntLit => Int,
            .NullLit => Null,
            .StringLit => Str,
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) Error!Type {
        // We reserve the slot because of recursion
        const id = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var binop_extra: AnalyzedAst.BinOp = .{ .type_ = Null };

        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        binop_extra.type_ = lhs;
        var res = lhs;

        // String operations
        if (expr.op == .Plus and lhs == Str and rhs == Str) {
            self.analyzed_stmts.items[id] = .{ .Binop = binop_extra };
            return Str;
        } else if (expr.op == .Star) {
            if ((lhs == Str and rhs == Int) or (lhs == Int and rhs == Str)) {
                binop_extra.type_ = Str;

                // For string concatenation, we use the cast information to tell
                // on wich side is the integer (for the compiler)
                binop_extra.cast = if (rhs == Int) .Rhs else .Lhs;
                self.analyzed_stmts.items[id] = .{ .Binop = binop_extra };
                return Str;
            }
        }

        switch (expr.op) {
            // Arithmetic binop
            .Plus, .Minus, .Star, .Slash => {
                if (!Analyzer.is_numeric(lhs)) {
                    return self.err(
                        AnalyzerMsg.invalid_arithmetic(self.type_manager.str(lhs)),
                        expr.lhs.span(),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
                    return self.err(
                        AnalyzerMsg.invalid_arithmetic(self.type_manager.str(rhs)),
                        expr.rhs.span(),
                    );
                }

                switch (lhs) {
                    Float => {
                        switch (rhs) {
                            Float => {},
                            Int => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast(.Rhs, self.type_manager.str(lhs)),
                                    expr.rhs.span(),
                                );

                                binop_extra.cast = .Rhs;
                            },
                            else => unreachable,
                        }
                    },
                    Int => {
                        switch (rhs) {
                            Float => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast(.Lhs, self.type_manager.str(rhs)),
                                    expr.lhs.span(),
                                );

                                binop_extra.type_ = Float;
                                binop_extra.cast = .Lhs;
                                res = Float;
                            },
                            Int => {},
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .EqualEqual, .BangEqual => {
                // If different value types
                if (lhs != rhs) {
                    // Check for implicit casts
                    if ((lhs == Int and rhs == Float) or (lhs == Float and rhs == Int)) {
                        if (lhs == Int) {
                            binop_extra.cast = .Lhs;

                            try self.warn(.FloatEqualCast, expr.lhs.span());
                        } else {
                            binop_extra.cast = .Rhs;

                            try self.warn(.FloatEqualCast, expr.rhs.span());
                        }

                        binop_extra.type_ = Float;
                    } else {
                        return self.err(
                            AnalyzerMsg.invalid_cmp(
                                self.type_manager.str(lhs),
                                self.type_manager.str(rhs),
                            ),
                            expr.span,
                        );
                    }
                } else {
                    // Check for unsafe float comparisons
                    if (lhs == Float) {
                        try self.warn(.FloatEqual, expr.span);
                    }
                }

                res = Bool;
            },

            .Greater, .GreaterEqual, .Less, .LessEqual => {
                if (!Analyzer.is_numeric(lhs)) {
                    return self.err(
                        AnalyzerMsg.invalid_arithmetic(self.type_manager.str(lhs)),
                        expr.lhs.span(),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
                    return self.err(
                        AnalyzerMsg.invalid_arithmetic(self.type_manager.str(rhs)),
                        expr.rhs.span(),
                    );
                }

                switch (lhs) {
                    Float => switch (rhs) {
                        Float => try self.warn(.FloatEqual, expr.span),
                        Int => {
                            try self.warn(.FloatEqualCast, expr.rhs.span());

                            binop_extra.cast = .Rhs;
                        },
                        else => unreachable,
                    },
                    Int => switch (rhs) {
                        Float => {
                            try self.warn(.FloatEqualCast, expr.lhs.span());

                            binop_extra.cast = .Lhs;
                            binop_extra.type_ = Float;
                        },
                        Int => {},
                        else => unreachable,
                    },
                    else => unreachable,
                }

                res = Bool;
            },

            // Logical binop
            .And, .Or => unreachable,
            else => unreachable,
        }

        self.analyzed_stmts.items[id] = .{ .Binop = binop_extra };
        return res;
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn identifier(self: *Self, expr: *const Ast.Identifier, initialized: bool) Error!*Variable {
        if (self.globals.getPtr(expr.name)) |glob| {
            // Checks the initialization if asked
            if (initialized and !glob.initialized) {
                return self.err(.{ .UseUninitVar = .{ .name = expr.name } }, expr.span);
            }

            try self.analyzed_stmts.append(.{
                .Variable = .{ .scope = .Global, .index = glob.index },
            });

            return glob;
        } else {
            return self.err(
                .{ .UndeclaredVar = .{ .name = expr.name } },
                expr.span,
            );
        }
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        const id = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var unary_extra: AnalyzedAst.Unary = .{ .type_ = Null };

        const rhs = try self.expression(expr.rhs);

        if (expr.op == .Not and rhs != Bool) {
            return self.err(
                .{ .InvalidUnary = .{ .found = self.type_manager.str(rhs) } },
                expr.rhs.span(),
            );
        } else if (expr.op == .Minus and rhs != Int and rhs != Float) {
            return self.err(
                AnalyzerMsg.invalid_arithmetic(self.type_manager.str(rhs)),
                expr.rhs.span(),
            );
        }

        unary_extra.type_ = rhs;

        self.analyzed_stmts.items[id] = .{ .Unary = unary_extra };
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
