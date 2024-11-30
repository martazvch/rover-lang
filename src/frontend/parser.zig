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

    pub fn reinit(self: *Self) void {
        self.panic_mode = false;
        self.stmts.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
        self.token_id = 0;
    }

    /// Parses the while token stream
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
            self.skip_new_lines();
        }
    }

    fn current(self: *const Self) *const Token {
        return &self.tokens[self.token_id];
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

        try self.error_at_current(error_kind);
        return Error.err;
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

        try self.error_at(tk, error_kind);
        return error.err;
    }

    fn expect_or_err_at_span(
        self: *Self,
        kind: Token.Kind,
        error_kind: ParserMsg,
        span: Span,
    ) !void {
        if (self.match(kind)) return;

        try self.error_at_span(error_kind, span);
        return error.err;
    }

    fn error_at_current(self: *Self, error_kind: ParserMsg) Error!void {
        try self.error_at(&self.prev(), error_kind);
    }

    fn error_at_prev(self: *Self, error_kind: ParserMsg) !void {
        try self.error_at(&self.prev(), error_kind);
    }

    /// If error already encountered and in the same statement parsing,
    /// we exit, let synchronize and resume. It is likely that if we
    /// are already in panic mode, the following errors are just
    /// consequencies of actual bad statement
    fn error_at(self: *Self, token: *const Token, error_kind: ParserMsg) !void {
        if (self.panic_mode) return;

        self.panic_mode = true;

        const report = ParserReport.err(error_kind, token.span);
        try self.errs.append(report);
    }

    fn error_at_span(self: *Self, error_kind: ParserMsg, span: Span) !void {
        if (self.panic_mode) return;

        self.panic_mode = true;

        const report = ParserReport.err(error_kind, span);
        try self.errs.append(report);
    }

    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (!self.check(.Eof)) {
            switch (self.current().kind) {
                .Fn, .For, .If, .Print, .Return, .Struct, .Var, .While => return,
                else => self.advance(),
            }
        }
    }

    fn declaration(self: *Self) !Stmt {
        if (self.match(.Var)) {
            unreachable;
        } else {
            return self.statement();
        }
    }

    fn statement(self: *Self) !Stmt {
        if (self.match(.Print)) {
            return self.print_stmt();
        } else {
            return .{ .Expr = try self.parse_precedence_expr(0) };
        }
    }

    fn print_stmt(self: *Self) !Stmt {
        var span: Span = .{ .start = self.prev().span.start, .end = 0 };
        const expr = try self.parse_precedence_expr(0);
        span.end = expr.span().end;

        try self.expect_or_err_at_span(
            .NewLine,
            .ExpectNewLine,
            .{ .start = span.end, .end = span.end },
        );

        return .{ .Print = .{
            .expr = expr,
            .span = span,
        } };
    }

    const Assoc = enum { Left, None };

    const Rule = struct { prec: i8, assoc: Assoc = .Left };

    const rules = std.enums.directEnumArrayDefault(Token.Kind, Rule, .{ .prec = -1 }, 0, .{
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
                try self.error_at_current(.ChainingCmpOp);
                return error.err;
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

            // self.skip_new_lines();
        }

        return node;
    }

    /// Parses a prefix expression. If we hit a unary first, we recurse
    /// to form an expression
    fn parse_prefix_expr(self: *Self) Error!*Expr {
        const unary_expr = switch (self.prev().kind) {
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

    fn parse_primary_expr(self: *Self) Error!*Expr {
        return switch (self.prev().kind) {
            .False => self.bool_(false),
            .Float => self.float(),
            .Int => self.int(),
            .LeftParen => self.grouping(),
            .Null => self.null_(),
            .String => self.string(),
            .True => self.bool_(true),
            else => |k| {
                if (k == .Eof) {
                    try self.error_at_prev(.UnexpectedEof);
                } else {
                    const p = self.prev().span;
                    try self.error_at_prev(.{ .ExpectExpr = .{ .found = self.source[p.start..p.end] } });
                }
                return error.err;
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
        const lexeme = self.source[p.span.start..p.span.end];
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

    fn int(self: *Self) Error!*Expr {
        const previous = self.prev();
        const lexeme = self.source[previous.span.start..previous.span.end];
        const value = try std.fmt.parseInt(i64, lexeme, 10);
        const expr = try self.allocator.create(Expr);

        expr.* = .{ .IntLit = .{
            .value = value,
            .span = .{
                .start = previous.span.start,
                .end = previous.span.end,
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
