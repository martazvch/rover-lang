const std = @import("std");
const assert = std.debug.assert;
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
const Scope = AnalyzedAst.Scope;
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;
const TypeInfo = TypeSys.TypeInfo;
const SourceSlice = @import("../frontend/ast.zig").SourceSlice;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const GenReport = @import("../reporter.zig").GenReport;

// Re-export constants
const Void = TypeSys.Void;
const Null = TypeSys.Null;
const Int = TypeSys.Int;
const Float = TypeSys.Float;
const Bool = TypeSys.Bool;
const Str = TypeSys.Str;

pub const TypeManager = struct {
    declared: StringHashMap(Type),
    type_infos: ArrayList(TypeInfo),

    const Self = @This();
    const Error = error{TooManyTypes} || std.fmt.BufPrintError || Allocator.Error;

    pub fn init(allocator: Allocator) Self {
        return .{
            .declared = StringHashMap(Type).init(allocator),
            .type_infos = ArrayList(TypeInfo).init(allocator),
        };
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
        self.type_infos.deinit();
    }

    pub fn add_info(self: *Self, info: TypeInfo) !TypeSys.Value {
        try self.type_infos.append(info);
        const count = self.type_infos.items.len - 1;

        return if (count == std.math.maxInt(TypeSys.Value))
            error.TooManyTypes
        else
            @intCast(count);
    }

    /// Set type information at a specific index in list (index gave by *reserve_info* method)
    pub inline fn set_info(self: *Self, index: usize, info: TypeInfo) void {
        self.type_infos.items[index] = info;
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
    globals: ArrayList(Variable),
    locals: ArrayList(Variable),
    scope_depth: usize,
    analyzed_stmts: ArrayList(AnalyzedStmt),
    type_manager: TypeManager,
    main: ?*const Ast.FnDecl,

    const Self = @This();
    const Error = error{Err} || TypeManager.Error || Allocator.Error;

    // Representation of a variable. Index is the declaration order
    // NOTE: use depth: isize = -1 as uninit? Saves a bool in struct. On passerait
    // de 48 à 47 bits
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
            .globals = ArrayList(Variable).init(allocator),
            .locals = ArrayList(Variable).init(allocator),
            .scope_depth = 0,
            .analyzed_stmts = ArrayList(AnalyzedStmt).init(allocator),
            .type_manager = TypeManager.init(allocator),
            .main = null,
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

    /// Reserve a slot in analyzed statements and returns the index
    fn reserve_slot(self: *Self) !usize {
        try self.analyzed_stmts.append(undefined);
        return self.analyzed_stmts.items.len - 1;
    }

    /// Unincrement scope depth, discards all locals and return the number
    /// of discarded locals
    fn end_scope(self: *Self) !usize {
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

        return pop_count;
    }

    /// Checks if the variable name is in local or global scope
    fn ident_in_scope(self: *const Self, name: []const u8) bool {
        if (self.scope_depth > 0) {
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
        } else {
            if (self.globals.items.len == 0) return false;

            for (self.globals.items) |*glob| {
                if (glob.name.len == name.len and std.mem.eql(u8, glob.name, name)) {
                    return true;
                }
            }
        }

        return false;
    }

    /// Checks if an identifier already exists in current scope and if it's type exists
    /// Returns the type of the variable
    fn check_ident_and_type(self: *Self, ident: SourceSlice, type_: ?SourceSlice) !Type {
        // Name check
        if (self.ident_in_scope(ident.text)) {
            return self.err(
                .{ .AlreadyDeclared = .{ .name = ident.text } },
                Span.from_source_slice(ident),
            );
        }

        // If a type was declared
        return if (type_) |t| self.type_manager.declared.get(t.text) orelse {
            return self.err(
                .{ .UndeclaredType = .{ .found = t.text } },
                Span.from_source_slice(t),
            );
        } else Void;
    }

    /// Declares a variable either in globals or in locals based on current scope depth
    fn declare_variable(self: *Self, name: []const u8, type_: Type, initialized: bool) !AnalyzedAst.Variable {
        var variable: Variable = .{
            .index = 0,
            .name = name,
            .type_ = type_,
            .depth = self.scope_depth,
            .initialized = initialized,
        };

        // Add the variable to the correct data structure
        if (self.scope_depth == 0) {
            const index = self.globals.items.len;
            variable.index = index;

            try self.globals.append(variable);
            return .{ .index = index, .scope = .Global };
        } else {
            const index = self.locals.items.len;
            variable.index = index;

            try self.locals.append(variable);
            return .{ .index = index, .scope = .Local };
        }
    }

    pub fn analyze(self: *Self, stmts: []const Stmt, source: []const u8, repl: bool) !void {
        self.source = source;

        // HACK: needed while there is no call to main function at start of program
        try self.locals.append(.{ .name = "", .initialized = true, .index = 0, .type_ = TypeSys.Fn, .depth = 0 });

        for (stmts) |*stmt| {
            const stmt_type = self.statement(stmt) catch |e| {
                switch (e) {
                    // If it's our own error, we continue
                    error.Err => continue,
                    error.TooManyTypes => return self.err(.TooManyTypes, stmt.span()),
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

        // In REPL mode, no need for main function
        if (repl) return;

        if (self.main) |main| {
            self.fn_declaration(main) catch |e| switch (e) {
                error.Err => {},
                else => return e,
            };
        } else self.err(.NoMain, .{ .start = 0, .end = 0 }) catch {};
    }

    fn statement(self: *Self, stmt: *const Stmt) !Type {
        var final: Type = Void;

        switch (stmt.*) {
            .Assignment => |*s| try self.assignment(s),
            .Discard => |*s| try self.discard(s),
            .FnDecl => |*s| try self.fn_declaration(s),
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

    fn fn_declaration(self: *Self, stmt: *const Ast.FnDecl) !void {
        // If we find a main function in global scope, we save it to analyze last
        // If there is another global scoped main function, it's going to be analyzed
        // and when we analyze the first one there will be an error anyway
        if (self.main == null and self.scope_depth == 0 and std.mem.eql(u8, stmt.name.text, "main")) {
            self.main = stmt;
            return;
        }

        // TODO: Analyzer
        // [x] - if name == main && scope == 0 => save pointer to treat it last and return
        // [x] - call fn_declaration again with the pointer
        // [x] - if pointer is null at the end of all stmt -> error no main
        // [] - return the pointer to compiler so it only compares addresses to find it back
        //
        // TODO: Compiler
        // [] - compile every thing but end with the main
        //
        // VM will automatically call the last retruned function, the main
        //
        // NOTE: string comparison is slow, add a field in Ast node?

        const idx = try self.reserve_slot();

        // Check in current scope
        const return_type = try self.check_ident_and_type(stmt.name, stmt.return_type);

        const type_idx = try self.type_manager.add_info(undefined);
        const fn_type = TypeSys.create(TypeSys.Fn, type_idx);

        const fn_extra = try self.declare_variable(stmt.name.text, fn_type, true);
        self.scope_depth += 1;

        var params_type: [256]Type = undefined;

        for (0..stmt.arity) |i| {
            // Check on parameter
            const param_type = self.check_ident_and_type(
                stmt.params[i].name,
                stmt.params[i].type_,
            ) catch |e| switch (e) {
                error.Err => {
                    // We replace the error with a more explicit one for parameters
                    if (self.errs.items[self.errs.items.len - 1].report == .AlreadyDeclared) {
                        const name = stmt.params[i].name;

                        self.errs.items[self.errs.items.len - 1] = AnalyzerReport.err(
                            .{ .DuplicateParam = .{ .name = name.text } },
                            Span.from_source_slice(name),
                        );
                    }

                    return e;
                },
                else => return e,
            };

            if (param_type == Void) {
                return self.err(.VoidParam, Span.from_source_slice(stmt.params[i].name));
            }

            _ = try self.declare_variable(stmt.params[i].name.text, param_type, true);
            params_type[i] = param_type;
        }

        const result_type = try self.block(&stmt.body);

        // Here we close the function's parameters list, scope pops should
        // be equal to arity. In two part to allow optimization by removing the call
        const end = try self.end_scope();
        assert(end == stmt.arity);

        if (result_type != return_type) {
            return self.err(
                .{ .IncompatibleFnType = .{
                    .found = self.type_manager.str(result_type),
                    .expect = self.type_manager.str(return_type),
                } },
                stmt.body.span,
            );
        }

        // TODO: why one have a set method?
        self.type_manager.set_info(type_idx, .{ .Fn = .{
            .arity = stmt.arity,
            .params = params_type,
            .return_type = return_type,
        } });

        self.analyzed_stmts.items[idx] = .{ .FnDecl = .{
            .arity = stmt.arity,
            .variable = fn_extra,
        } };
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
        var checked_type = try self.check_ident_and_type(stmt.name, stmt.type_);

        var initialized = false;

        if (stmt.value) |v| {
            const value_type = try self.expression(v);

            // Void assignment check
            if (value_type == Void) {
                return self.err(.VoidAssignment, v.span());
            }

            var assign_extra: AnalyzedAst.Assignment = .{};

            // If no type declared, we infer the value type
            if (checked_type == Void) {
                checked_type = value_type;
                // Else, we check for coherence
            } else if (checked_type != value_type) {
                // One case in wich we can coerce; int -> float
                if (checked_type == Float and value_type == Int) {
                    assign_extra.cast = .Yes;
                } else {
                    return self.err(
                        .{ .InvalidAssignType = .{
                            .expect = self.type_manager.str(checked_type),
                            .found = self.type_manager.str(value_type),
                        } },
                        v.span(),
                    );
                }
            }

            initialized = true;
            try self.analyzed_stmts.append(.{ .Assignment = assign_extra });
        }

        const extra = try self.declare_variable(stmt.name.text, checked_type, initialized);
        try self.analyzed_stmts.append(.{ .Variable = extra });
    }

    fn while_stmt(self: *Self, stmt: *const Ast.While) Error!void {
        const cond_type = try self.expression(stmt.condition);

        if (cond_type != Bool) return self.err(
            .{ .NonBoolCond = .{
                .what = "while",
                .found = self.type_manager.str(cond_type),
            } },
            stmt.condition.span(),
        );

        const body_type = try self.statement(stmt.body);

        if (body_type != Void) return self.err(
            .{ .NonVoidWhile = .{
                .found = self.type_manager.str(body_type),
            } },
            stmt.body.span(),
        );
    }

    fn expression(self: *Self, expr: *const Expr) !Type {
        return switch (expr.*) {
            .Block => |*e| self.block(e),
            .BoolLit => Bool,
            .BinOp => |*e| self.binop(e),
            .FloatLit => Float,
            .FnCall => |*e| self.fn_call(e),
            .Grouping => |*e| self.grouping(e),
            .Identifier => |*e| {
                const res = try self.identifier(e, true);
                return res.type_;
            },
            .If => |*e| self.if_expr(e),
            .IntLit => Int,
            .NullLit => Null,
            .Return => |*e| self.return_expr(e),
            .StringLit => Str,
            .Unary => |*e| self.unary(e),
        };
    }

    fn block(self: *Self, expr: *const Ast.Block) Error!Type {
        const idx = try self.reserve_slot();

        self.scope_depth += 1;

        var final: Type = Void;

        for (expr.stmts, 0..) |*s, i| {
            final = try self.statement(s);

            if (final != Void and i != expr.stmts.len - 1) {
                return self.err(.UnusedValue, s.span());
            }
        }

        self.analyzed_stmts.items[idx] = .{ .Block = .{
            .pop_count = try self.end_scope(),
            .is_expr = if (final != Void) true else false,
        } };

        return final;
    }

    /// Checks if an expression if of a certain type kind and returns the associated value or error
    fn expect_type_kind(self: *Self, expr: *const Expr, kind: TypeSys.Kind) !TypeSys.Value {
        const expr_type = try self.expression(expr);

        return if (TypeSys.is(expr_type, kind))
            TypeSys.get_value(expr_type)
        else
            self.err(
                .{ .TypeMismatch = .{
                    .expect = TypeSys.str_kind(kind),
                    .found = TypeSys.str_kind(TypeSys.get_kind(expr_type)),
                } },
                expr.span(),
            );
    }

    fn fn_call(self: *Self, expr: *const Ast.FnCall) Error!Type {
        const type_value = try self.expect_type_kind(expr.callee, TypeSys.Fn);
        const type_info = self.type_manager.type_infos.items[type_value].Fn;

        if (type_info.arity != expr.arity) {
            return self.err(
                try AnalyzerMsg.wrong_args_count(type_info.arity, expr.arity),
                expr.span,
            );
        }

        for (0..expr.arity) |i| {
            const arg_type = try self.expression(expr.args[i]);

            if (arg_type != type_info.params[i]) {
                return self.err(
                    .{ .TypeMismatch = .{
                        .expect = self.type_manager.str(type_info.params[i]),
                        .found = self.type_manager.str(arg_type),
                    } },
                    expr.args[i].span(),
                );
            }
        }

        return type_info.return_type;
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

        for (self.globals.items) |*glob| {
            if (std.mem.eql(u8, glob.name, expr.name)) {
                if (initialized and !glob.initialized) {
                    return self.err(.{ .UseUninitVar = .{ .name = expr.name } }, expr.span);
                }

                try self.analyzed_stmts.append(.{
                    .Variable = .{ .scope = .Global, .index = glob.index },
                });

                return glob;
            }
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
        const idx = try self.reserve_slot();
        var extra: AnalyzedAst.If = .{};

        const cond_type = try self.expression(expr.condition);
        if (cond_type != Bool) return self.err(
            .{ .NonBoolCond = .{
                .what = "if",
                .found = self.type_manager.str(cond_type),
            } },
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

    fn return_expr(self: *Self, expr: *const Ast.Return) Error!Type {
        return if (expr.expr) |val| self.expression(val) else Void;
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        const idx = try self.reserve_slot();
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
        const idx = try self.reserve_slot();
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
