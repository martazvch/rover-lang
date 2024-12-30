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

const Void = AnalyzedAst.Void;
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
        try self.declared.put("void", Void);
        try self.declared.put("null", Null);
        try self.declared.put("bool", Bool);
        try self.declared.put("float", Float);
        try self.declared.put("int", Int);
        try self.declared.put("str", Str);
    }

    pub fn deinit(self: *Self) void {
        self.declared.deinit();
    }

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
    locals: ArrayList(Variable),
    scope_depth: usize,
    analyzed_stmts: ArrayList(AnalyzedStmt),
    type_manager: TypeManager,

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    // Representation of a variable. Index is the declaration order
    // NOTE: use depth: isize = -1 as uninit? Saves a bool in struct. On passerait
    // de 48 à 40 bits
    // Voir si possible de faire autrement que de stocker le nom des vars
    const Variable = struct {
        index: usize,
        type_: Type,
        depth: usize,
        name: []const u8,
        initialized: bool = false,
    };
    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .source = undefined,
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .globals = StringHashMap(Variable).init(allocator),
            .locals = ArrayList(Variable).init(allocator),
            .scope_depth = 0,
            .analyzed_stmts = ArrayList(AnalyzedStmt).init(allocator),
            .type_manager = TypeManager.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
        self.warns.deinit();
        self.globals.deinit();
        self.locals.deinit();
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

    fn var_in_scope(self: *const Self, name: []const u8) bool {
        if (self.scope_depth == 0) {
            if (self.globals.get(name)) |_| {
                return true;
            }
        } else {
            if (self.locals.items.len == 0) return false;

            var idx = self.locals.items.len;

            while (idx > 0) : (idx -= 1) {
                const local = self.locals.items[idx - 1];

                if (local.depth < self.scope_depth) break;

                // First condition might fail first avoiding string comparison
                if (local.name.len == name.len and std.mem.eql(u8, local.name, name)) {
                    return true;
                }
            }
        }

        return false;
    }

    pub fn analyze(self: *Self, stmts: []const Stmt, source: []const u8) !void {
        self.source = source;

        for (stmts) |*stmt| {
            const stmt_type = self.statement(stmt) catch |e| {
                switch (e) {
                    // If it's our own error, we continue
                    error.Err => continue,
                    else => return e,
                }
            };

            // If at this stage we have a type, it means that nobody
            // consumed it. It might be a standalone expression like:
            // 3+4
            if (stmt_type != Void) {
                self.err(.UnusedValue, stmt.Expr.span()) catch {};
            }
        }
    }

    fn statement(self: *Self, stmt: *const Stmt) !Type {
        var final: Type = Void;

        switch (stmt.*) {
            .Assignment => |*s| try self.assignment(s),
            .Discard => |*s| try self.discard(s),
            .Print => |*s| _ = try self.expression(s.expr),
            .VarDecl => |*s| try self.var_declaration(s),
            .While => |*s| try self.while_stmt(s),
            .Expr => |e| final = try self.expression(e),
        }

        return final;
    }

    fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
        const value_type = try self.expression(stmt.value);

        if (value_type == Void) {
            return self.err(.VoidAssignment, stmt.value.span());
        }

        switch (stmt.assigne.*) {
            .Identifier => |*ident| {
                // Forward declaration to preserve order
                const idx = self.analyzed_stmts.items.len;
                try self.analyzed_stmts.append(.{ .Assignment = .{} });

                const assigne = try self.identifier(ident, false);

                // If type is unknown, we update it
                if (assigne.type_ == Void) {
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

    fn discard(self: *Self, stmt: *const Ast.Discard) !void {
        const discarded = try self.expression(stmt.expr);

        if (discarded == Void) return self.err(.VoidDiscard, stmt.expr.span());
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
        // Name check
        if (self.var_in_scope(stmt.name.text)) {
            return self.err(
                .{ .AlreadyDeclaredVar = .{ .name = stmt.name.text } },
                Span.from_source_slice(stmt.name),
            );
        }

        // Type check
        var final_type = Void;
        var variable: Variable = .{
            .index = 0,
            .type_ = Void,
            .name = stmt.name.text,
            .depth = self.scope_depth,
        };

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

            // Void assignment check
            if (value_type == Void) {
                return self.err(.VoidAssignment, v.span());
            }

            var assign_extra: AnalyzedAst.Assignment = .{};

            // If no type declared, we infer the value type
            if (final_type == Void) {
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

        // Build the extra infos
        const extra: AnalyzedAst.Variable = if (self.scope_depth == 0) .{
            .scope = .Global,
            .index = self.globals.count(),
        } else .{
            .scope = .Local,
            .index = self.locals.items.len,
        };

        // Get the index
        variable.index = extra.index;

        // Add the variable to the correct data structure
        if (extra.scope == .Global) {
            try self.globals.put(stmt.name.text, variable);
        } else {
            try self.locals.append(variable);
        }

        try self.analyzed_stmts.append(.{ .Variable = extra });
    }

    fn while_stmt(self: *Self, stmt: *const Ast.While) !void {
        _ = self;
        _ = stmt;
    }

    fn expression(self: *Self, expr: *const Expr) !Type {
        return switch (expr.*) {
            .Block => |*e| self.block(e),
            .BoolLit => Bool,
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => Float,
            .Identifier => |*e| {
                const res = try self.identifier(e, true);
                return res.type_;
            },
            .If => |*e| self.if_expr(e),
            .IntLit => Int,
            .NullLit => Null,
            .StringLit => Str,
            .Unary => |*e| self.unary(e),
        };
    }

    fn block(self: *Self, expr: *const Ast.Block) Error!Type {
        const idx = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);

        self.scope_depth += 1;

        var final: Type = Void;

        for (expr.stmts, 0..) |*s, i| {
            final = try self.statement(s);

            if (final != Void and i != expr.stmts.len - 1) {
                return self.err(.UnusedValue, s.span());
            }
        }

        self.scope_depth -= 1;

        var pop_count: usize = 0;
        // Discards all the local variables
        if (self.locals.items.len > 0) {
            var i: usize = self.locals.items.len;

            while (i > 0 and self.locals.items[i - 1].depth > self.scope_depth) {
                i -= 1;
            }

            pop_count = self.locals.items.len - i;
            try self.locals.resize(i);
        }

        self.analyzed_stmts.items[idx] = .{ .Block = .{
            .pop_count = pop_count,
            .is_expr = if (final != Void) true else false,
        } };

        return final;
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn identifier(self: *Self, expr: *const Ast.Identifier, initialized: bool) Error!*Variable {
        // We first check in locals
        if (self.locals.items.len > 0) {
            var idx = self.locals.items.len;

            while (idx > 0) : (idx -= 1) {
                const local = &self.locals.items[idx - 1];

                if (std.mem.eql(u8, local.name, expr.name)) {
                    // Checks the initialization if asked
                    if (initialized and !local.initialized) {
                        return self.err(.{ .UseUninitVar = .{ .name = expr.name } }, expr.span);
                    }

                    try self.analyzed_stmts.append(.{
                        .Variable = .{ .scope = .Local, .index = local.index },
                    });

                    return local;
                }
            }
        }

        // We then check for global
        if (self.globals.getPtr(expr.name)) |glob| {
            if (initialized and !glob.initialized) {
                return self.err(.{ .UseUninitVar = .{ .name = expr.name } }, expr.span);
            }

            try self.analyzed_stmts.append(.{
                .Variable = .{ .scope = .Global, .index = glob.index },
            });

            return glob;
        }

        // Else, it's undeclared
        return self.err(
            .{ .UndeclaredVar = .{ .name = expr.name } },
            expr.span,
        );
    }

    // For nullable, use bitmasking!
    // like: first 5 digit for type id (should be enough) and the 6th one
    // is 1 for nullable and 0 for non-nullable for example
    // The 7th digit could tell if we exit scope. For example if the
    // else branch isn't the same type but is an explicit return, we
    // dont check if type match, we exit scope anyway
    fn if_expr(self: *Self, expr: *const Ast.If) Error!Type {
        // We reserve the slot because of recursion
        const idx = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var extra: AnalyzedAst.If = .{};

        const cond_type = try self.expression(expr.condition);
        if (cond_type != Bool) return self.err(
            .{ .NonBoolIfCond = .{ .found = self.type_manager.str(cond_type) } },
            expr.condition.span(),
        );

        const then_type = try self.statement(&expr.then_body);

        var else_type: Type = Void;
        if (expr.else_body) |*body| {
            else_type = try self.statement(body);
        } else if (then_type != Void) {
            // If there is no else body but the then returns a value
            // it's an error because not all paths return a value
            return self.err(
                .{ .MissingElseClause = .{ .if_type = self.type_manager.str(then_type) } },
                expr.span,
            );
        }

        if (then_type != else_type) {
            if (then_type == Int and else_type == Float) {
                extra.cast = .Then;

                try self.warn(
                    AnalyzerMsg.implicit_cast("then branch", "float"),
                    expr.then_body.span(),
                );
            } else if (then_type == Float and else_type == Int) {
                extra.cast = .Else;

                // Safe unsafe access, if there is a non void type
                // there is an else body
                try self.warn(
                    AnalyzerMsg.implicit_cast("else branch", "float"),
                    expr.else_body.?.span(),
                );
            } else {
                return self.err(
                    .{ .IncompatibleIfType = .{
                        .found1 = self.type_manager.str(then_type),
                        .found2 = self.type_manager.str(else_type),
                    } },
                    expr.span,
                );
            }
        }

        self.analyzed_stmts.items[idx] = .{ .If = extra };

        return then_type;
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        const idx = self.analyzed_stmts.items.len;
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

        self.analyzed_stmts.items[idx] = .{ .Unary = unary_extra };
        return rhs;
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) Error!Type {
        // We reserve the slot because of recursion
        const idx = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var binop_extra: AnalyzedAst.BinOp = .{ .type_ = Null };

        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        binop_extra.type_ = lhs;
        var res = lhs;

        // String operations
        if (expr.op == .Plus and lhs == Str and rhs == Str) {
            self.analyzed_stmts.items[idx] = .{ .Binop = binop_extra };
            return Str;
        } else if (expr.op == .Star) {
            if ((lhs == Str and rhs == Int) or (lhs == Int and rhs == Str)) {
                binop_extra.type_ = Str;

                // For string concatenation, we use the cast information to tell
                // on wich side is the integer (for the compiler)
                binop_extra.cast = if (rhs == Int) .Rhs else .Lhs;
                self.analyzed_stmts.items[idx] = .{ .Binop = binop_extra };
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
                                    AnalyzerMsg.implicit_cast("right hand side", self.type_manager.str(lhs)),
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
                                    AnalyzerMsg.implicit_cast("left hand side", self.type_manager.str(rhs)),
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
            .And, .Or => {
                if (lhs != Bool) return self.err(.{ .InvalidLogical = .{
                    .found = self.type_manager.str(lhs),
                } }, expr.lhs.span());

                if (rhs != Bool) return self.err(.{ .InvalidLogical = .{
                    .found = self.type_manager.str(rhs),
                } }, expr.rhs.span());
            },
            else => unreachable,
        }

        self.analyzed_stmts.items[idx] = .{ .Binop = binop_extra };
        return res;
    }
};

// Test
test Analyzer {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_analyzer.zig").get_test_data;

    const Tester = GenericTester("analyzer", AnalyzerMsg, get_test_data);
    try Tester.run();
}
