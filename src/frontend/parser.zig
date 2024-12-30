const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Span = Ast.Span;
const GenReport = @import("../reporter.zig").GenReport;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const Token = @import("lexer.zig").Token;
const SourceSlice = Ast.SourceSlice;

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

pub const Parser = struct {
    source: []const u8,
    stmts: ArrayList(Stmt),
    errs: ArrayList(ParserReport),
    arena: ArenaAllocator,
    allocator: Allocator,
    tokens: []const Token,
    token_id: usize,
    panic_mode: bool,

    const Self = @This();
    const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;

    pub const ParserReport = GenReport(ParserMsg);

    /// Initialize an instance of Parser. Use it as:
    /// ```
    /// var parser: Parser = undefined;
    /// parser.init(allocator);
    /// ```
    pub fn init(self: *Self, allocator: Allocator) void {
        self.arena = ArenaAllocator.init(allocator);
        self.allocator = self.arena.allocator();
        self.stmts = ArrayList(Stmt).init(self.allocator);
        self.errs = ArrayList(ParserReport).init(self.allocator);
        self.token_id = 0;
        self.panic_mode = false;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    /// Parses the token stream
    pub fn parse(self: *Self, source: [:0]const u8, tokens: []const Token) !void {
        self.source = source;
        self.tokens = tokens;

        self.skip_new_lines();

        while (!self.match(.Eof)) {
            const stmt = self.declaration() catch |e| switch (e) {
                // If it's our own error, we continue on parsing
                Error.err => {
                    // If last error was Eof, exit the parser
                    if (self.errs.items[self.errs.items.len - 1].report == .UnexpectedEof) {
                        return;
                    }

                    self.synchronize();
                    continue;
                },
                else => return e,
            };

            try self.stmts.append(stmt);

            // If EOF, exit
            if (self.match(.Eof)) break;

            // After each statements we expect a new line
            self.expect_or_err_at_tk(
                .NewLine,
                .ExpectNewLine,
                &self.prev(),
            ) catch |e| switch (e) {
                // If it's our own error, we just synchronize before resuming
                error.err => self.synchronize(),
                else => return e,
            };

            self.skip_new_lines();
        }
    }

    fn current(self: *const Self) Token {
        return self.tokens[self.token_id];
    }

    fn prev(self: *const Self) Token {
        return self.tokens[self.token_id - 1];
    }

    fn advance(self: *Self) void {
        self.token_id += 1;
    }

    /// Returns *true* if the current token is of the asked type and
    /// advance the `current` field to next one. Otherwise, returns *false*
    fn match(self: *Self, kind: Token.Kind) bool {
        if (!self.check(kind)) {
            return false;
        }

        self.advance();
        return true;
    }

    fn match_and_skip(self: *Self, kind: Token.Kind) bool {
        const res = self.match(kind);
        self.skip_new_lines();
        return res;
    }

    /// Checks if we currently are at a token
    fn check(self: *const Self, kind: Token.Kind) bool {
        return self.current().kind == kind;
    }

    fn skip_new_lines(self: *Self) void {
        while (self.check(.NewLine)) {
            self.advance();
        }
    }

    /// Expect a specific type, otherwise it's an error and we enter
    /// *panic* mode
    fn expect(self: *Self, kind: Token.Kind, error_kind: ParserMsg) !void {
        if (self.match(kind)) {
            return;
        }

        return self.error_at_current(error_kind);
    }

    /// Expecy a specific type, otherwise it's an error and we enter
    /// *panic* mode. Allow to pass a token to be marked as initial error
    /// (usefull for example when unclosed parenthesis, we give the first)
    fn expect_or_err_at_tk(
        self: *Self,
        kind: Token.Kind,
        error_kind: ParserMsg,
        tk: *const Token,
    ) !void {
        if (self.match(kind)) return;

        return self.error_at(tk, error_kind);
    }

    fn expect_or_err_at_span(
        self: *Self,
        kind: Token.Kind,
        error_kind: ParserMsg,
        span: Span,
    ) !void {
        if (self.match(kind)) return;

        return self.error_at_span(error_kind, span);
    }

    fn error_at_current(self: *Self, error_kind: ParserMsg) Error {
        return self.error_at(&self.current(), error_kind);
    }

    fn error_at_prev(self: *Self, error_kind: ParserMsg) Error {
        return self.error_at(&self.prev(), error_kind);
    }

    /// If error already encountered and in the same statement parsing,
    /// we exit, let synchronize and resume. It is likely that if we
    /// are already in panic mode, the following errors are just
    /// consequencies of actual bad statement
    fn error_at(self: *Self, token: *const Token, error_kind: ParserMsg) Error {
        if (self.panic_mode) return error.err;

        self.panic_mode = true;

        const report = ParserReport.err(error_kind, token.span);
        try self.errs.append(report);

        return error.err;
    }

    fn error_at_span(self: *Self, error_kind: ParserMsg, span: Span) !void {
        if (self.panic_mode) return;

        self.panic_mode = true;

        const report = ParserReport.err(error_kind, span);
        try self.errs.append(report);

        return error.err;
    }

    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (!self.check(.Eof)) {
            switch (self.current().kind) {
                .Fn, .For, .If, .LeftBrace, .Print, .Return, .Struct, .Var, .While => return,
                else => self.advance(),
            }
        }
    }

    fn declaration(self: *Self) !Stmt {
        if (self.match(.Var)) {
            return self.var_declaration(false);
        } else if (self.match(.Underscore)) {
            return self.discard();
        } else {
            return self.statement();
        }
    }

    fn var_declaration(self: *Self, is_const: bool) !Stmt {
        try self.expect(.Identifier, .{ .ExpectVarName = .{ .keyword = "var" } });
        const ident = self.prev();

        var type_: ?SourceSlice = null;

        // Type
        // NOTE: make builtin types basic identifier to treat the the same?
        if (self.match(.Colon)) {
            if (self.match(.FloatKw) or self.match(.IntKw) or self.match(.StrKw) or self.match(.Bool)) {
                type_ = SourceSlice.from_token(self.prev(), self.source);
            } else {
                try self.expect(.Identifier, .ExpectTypeName);
                type_ = SourceSlice.from_token(self.prev(), self.source);
            }
        }

        // If no ':' but we are at an identifier, maybe a typo
        if (self.check(.Identifier)) {
            return self.error_at_current(.ExpectColonBeforeType);
        }

        var value: ?*Expr = null;

        if (self.match(.Equal)) {
            value = try self.parse_precedence_expr(0);
        }

        return .{
            .VarDecl = .{
                .name = SourceSlice.from_token(ident, self.source),
                .is_const = is_const,
                .type_ = type_,
                .value = value,
            },
        };
    }

    fn discard(self: *Self) !Stmt {
        try self.expect(.Equal, .InvalidDiscard);

        return .{
            .Discard = .{
                .expr = try self.parse_precedence_expr(0),
            },
        };
    }

    fn statement(self: *Self) !Stmt {
        if (self.match(.Print)) {
            return self.print_stmt();
        } else {
            const assigne = try self.parse_precedence_expr(0);

            if (self.match(.Equal)) {
                return self.assignment(assigne);
            } else return .{ .Expr = assigne };
        }
    }

    fn print_stmt(self: *Self) !Stmt {
        return .{ .Print = .{ .expr = try self.parse_precedence_expr(0) } };
    }

    fn assignment(self: *Self, assigne: *const Expr) !Stmt {
        return .{ .Assignment = .{
            .assigne = assigne,
            .value = try self.parse_precedence_expr(0),
        } };
    }

    const Assoc = enum { Left, None };

    const Rule = struct { prec: i8, assoc: Assoc = .Left };

    const rules = std.enums.directEnumArrayDefault(Token.Kind, Rule, .{ .prec = -1 }, 0, .{
        .And = .{ .prec = 20 },
        .Or = .{ .prec = 20 },

        .EqualEqual = .{ .prec = 30, .assoc = .None },
        .BangEqual = .{ .prec = 30, .assoc = .None },

        .Greater = .{ .prec = 40, .assoc = .None },
        .GreaterEqual = .{ .prec = 40, .assoc = .None },
        .Less = .{ .prec = 40, .assoc = .None },
        .LessEqual = .{ .prec = 40, .assoc = .None },

        .Minus = .{ .prec = 60 },
        .Plus = .{ .prec = 60 },

        .Slash = .{ .prec = 70 },
        .Star = .{ .prec = 70 },
    });

    fn parse_precedence_expr(self: *Self, prec_min: i8) Error!*Expr {
        self.advance();
        var node = try self.parse_prefix_expr();

        var banned_prec: i8 = -1;

        while (true) {
            // We check the current before consuming it
            const next_rule = rules[@as(usize, @intFromEnum(self.current().kind))];

            if (next_rule.prec < prec_min) break;

            if (next_rule.prec == banned_prec) {
                return self.error_at_current(.ChainingCmpOp);
            }

            // Here, we can safely use it
            self.advance();
            const op = self.prev().kind;
            const rhs = try self.parse_precedence_expr(next_rule.prec + 1);

            const expr = try self.allocator.create(Expr);
            expr.* = .{ .BinOp = .{
                .lhs = node,
                .rhs = rhs,
                .op = op,
                .span = .{ .start = node.span().start, .end = rhs.span().end },
            } };

            node = expr;

            if (next_rule.assoc == .None) banned_prec = next_rule.prec;
        }

        return node;
    }

    /// Parses a prefix expression. If we hit a unary first, we recurse
    /// to form an expression
    fn parse_prefix_expr(self: *Self) Error!*Expr {
        const unary_expr = switch (self.prev().kind) {
            .LeftBrace => return self.block_expr(),
            .If => return self.if_expr(),
            .Minus, .Not => try self.allocator.create(Expr),
            else => return self.parse_primary_expr(),
        };

        const op = self.prev();
        self.advance();

        // Recursion appens here
        unary_expr.* = .{ .Unary = .{
            .op = op.kind,
            .rhs = try self.parse_prefix_expr(),
            .span = .{
                .start = op.span.start,
                .end = self.current().span.start,
            },
        } };

        return unary_expr;
    }

    fn block_expr(self: *Self) Error!*Expr {
        const openning_brace = self.prev();

        self.skip_new_lines();
        var span: Span = .{ .start = openning_brace.span.start, .end = 0 };
        const expr = try self.allocator.create(Expr);

        var stmts = ArrayList(Stmt).init(self.allocator);

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try stmts.append(try self.declaration());
            self.skip_new_lines();
        }

        try self.expect_or_err_at_tk(.RightBrace, .UnclosedBrace, &openning_brace);

        span.end = self.prev().span.end;

        expr.* = .{ .Block = .{
            .stmts = try stmts.toOwnedSlice(),
            .span = span,
        } };

        return expr;
    }

    fn if_expr(self: *Self) Error!*Expr {
        var span = self.prev().span;
        const condition = try self.parse_precedence_expr(0);

        self.skip_new_lines();

        // TODO: Warning for unnecessary 'do' if there is a block after
        const then_body: Ast.Stmt = if (self.match_and_skip(.LeftBrace))
            .{ .Expr = try self.block_expr() }
        else if (self.match_and_skip(.Do))
            try self.declaration()
        else
            return self.error_at_current(.ExpectBraceOrDoAfterIf);

        span.end = self.prev().span.end;
        self.skip_new_lines();
        var else_body: ?Stmt = null;

        // If we dosen't match an else, we go back one token to be able
        // to match the rule "after each statement there is a new line"
        // tested in the main caller
        if (self.match_and_skip(.Else)) {
            if (self.match_and_skip(.LeftBrace)) {
                else_body = .{ .Expr = try self.block_expr() };
            } else else_body = try self.declaration();

            span.end = self.prev().span.end;
        } else self.token_id -= 1;

        const expr = try self.allocator.create(Expr);

        expr.* = .{ .If = .{
            .condition = condition,
            .then_body = then_body,
            .else_body = else_body,
            .span = span,
        } };

        return expr;
    }

    fn parse_primary_expr(self: *Self) Error!*Expr {
        return switch (self.prev().kind) {
            .False => self.bool_(false),
            .Float => self.float(),
            .Identifier => self.identifier(),
            .Int => self.int(),
            .LeftParen => self.grouping(),
            .Null => self.null_(),
            .String => self.string(),
            .True => self.bool_(true),
            else => |k| {
                if (k == .Eof) {
                    return self.error_at_prev(.UnexpectedEof);
                } else {
                    const p = self.prev();
                    return self.error_at_prev(.{ .ExpectExpr = .{ .found = p.from_source(self.source) } });
                }
                unreachable;
            },
        };
    }

    fn grouping(self: *Self) Error!*Expr {
        const opening = self.prev();
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .Grouping = .{
            .expr = try self.parse_precedence_expr(0),
            .span = .{
                .start = opening.span.start,
                .end = self.current().span.start,
            },
        } };

        try self.expect_or_err_at_tk(.RightParen, .UnclosedParen, &opening);
        return expr;
    }

    fn bool_(self: *Self, value: bool) Error!*Expr {
        const p = self.prev();
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .BoolLit = .{
            .value = value,
            .span = .{
                .start = p.span.start,
                .end = p.span.end,
            },
        } };

        return expr;
    }

    fn float(self: *Self) Error!*Expr {
        const p = self.prev();
        const lexeme = p.from_source(self.source);
        const value = try std.fmt.parseFloat(f64, lexeme);
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .FloatLit = .{
            .value = value,
            .span = .{
                .start = p.span.start,
                .end = p.span.end,
            },
        } };

        return expr;
    }

    fn identifier(self: *Self) Error!*Expr {
        const p = self.prev();
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .Identifier = .{
            .name = p.from_source(self.source),
            .span = p.span,
        } };

        return expr;
    }

    fn int(self: *Self) Error!*Expr {
        const p = self.prev();
        const lexeme = p.from_source(self.source);
        const value = try std.fmt.parseInt(i64, lexeme, 10);
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .IntLit = .{
            .value = value,
            .span = .{
                .start = p.span.start,
                .end = p.span.end,
            },
        } };

        return expr;
    }

    fn null_(self: *Self) Error!*Expr {
        const p = self.prev();
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .NullLit = .{
            .span = .{
                .start = p.span.start,
                .end = p.span.end,
            },
        } };

        return expr;
    }

    fn string(self: *Self) Error!*Expr {
        const p = self.prev();
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .StringLit = .{
            .value = self.source[p.span.start + 1 .. p.span.end - 1],
            .span = .{
                .start = p.span.start,
                .end = p.span.end,
            },
        } };

        return expr;
    }
};

// Tests
test Parser {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_parser.zig").get_test_data;

    const Tester = GenericTester("parser", ParserMsg, get_test_data);
    try Tester.run();
}
