const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Report = @import("../reporter.zig").Report;
const Token = @import("lexer.zig").Token;
const TokenKind = @import("lexer.zig").TokenKind;
const Lexer = @import("lexer.zig").Lexer;
const ErrorKind = @import("../errors.zig").ErrKind;
const IterList = @import("../iter_list.zig").IterList;

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
    stmts: ArrayList(Stmt),
    errs: ArrayList(Report),
    errs_extra: ArrayList([]const u8),
    arena: ArenaAllocator,
    allocator: Allocator,
    lexer: Lexer,
    current: Token,
    previous: Token,
    panic_mode: bool,

    const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;
    const Self = @This();

    /// Initialize an instance of Parser. Use it as:
    /// `var parser: Parser = undefined;`
    /// `parser.init(allocator);`
    pub fn init(self: *Self, allocator: Allocator) void {
        self.arena = ArenaAllocator.init(allocator);
        self.allocator = self.arena.allocator();
        self.stmts = ArrayList(Stmt).init(self.allocator);
        self.errs = ArrayList(Report).init(self.allocator);
        self.errs_extra = ArrayList([]const u8).init(self.allocator);
        self.lexer = Lexer.new();
        self.current = Token.empty();
        self.previous = Token.empty();
        self.panic_mode = false;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.panic_mode = false;
        self.previous = Token.empty();
        self.current = Token.empty();
        self.stmts.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
    }

    pub fn parse(self: *Self, source: []const u8) !void {
        self.lexer.init(source);
        try self.advance();
        try self.skip_new_lines();

        while (!try self.match(.Eof)) {
            const stmt = self.declaration() catch |e| switch (e) {
                // If it's our own error, we continue on parsing
                Error.err => {
                    try self.synchronize();
                    continue;
                },
                else => return e,
            };
            try self.stmts.append(stmt);
            try self.skip_new_lines();
        }
    }

    fn advance(self: *Self) !void {
        self.previous = self.current;

        while (true) {
            self.current = self.lexer.next();

            if (self.check(.Error)) {
                try self.err_from_lexer();
            } else break;
        }
    }

    /// Returns *true* if the current token is of the asked type and
    /// advance the `current` field to next one. Otherwise, returns *false*
    fn match(self: *Self, kind: TokenKind) !bool {
        if (!self.check(kind)) {
            return false;
        }

        try self.advance();
        return true;
    }

    /// Checks if we currently are at a token
    fn check(self: *const Self, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    fn skip_new_lines(self: *Self) !void {
        while (self.check(.NewLine)) {
            try self.advance();
        }
    }

    /// Expect a specific type, otherwise it's an error and we enter
    /// *panic* mode
    fn expect(self: *Self, kind: TokenKind, error_kind: ErrorKind) !void {
        if (try self.match(kind)) {
            return;
        }

        try self.error_at_current(error_kind, null);
        return Error.err;
    }

    /// Expecy a specific type, otherwise it's an error and we enter
    /// *panic* mode. Allow to pass a token to be marked as initial error
    /// (usefull for example when unclosed parenthesis, we give the first)
    fn expect_or_err_at(self: *Self, kind: TokenKind, error_kind: ErrorKind, tk: *const Token) !void {
        if (try self.match(kind)) return;

        try self.error_at(tk, error_kind, null);
        return error.err;
    }

    fn error_at_current(self: *Self, error_kind: ErrorKind, msg: ?[]const u8) Error!void {
        try self.error_at(&self.current, error_kind, msg);
    }

    fn error_at_prev(self: *Self, error_kind: ErrorKind, msg: ?[]const u8) !void {
        try self.error_at(&self.previous, error_kind, msg);
    }

    /// Error coming from Error tokens. We use the *start* field to retreive the
    /// enum value of the error and set it to the real value of the lexer
    fn err_from_lexer(self: *Self) !void {
        const err_kind: ErrorKind = @enumFromInt(self.current.start);
        self.current.start = self.lexer.offset;
        try self.error_at_current(err_kind, null);
    }

    /// If error already encountered and in the same statement parsing,
    /// we exit, let synchronize and resume. It is likely that if we
    /// are already in panic mode, the following errors are just
    /// consequencies of actual bad statement
    fn error_at(self: *Self, token: *const Token, error_kind: ErrorKind, msg: ?[]const u8) !void {
        if (self.panic_mode) return;

        self.panic_mode = true;

        const report = Report.err_at_token(error_kind, token, msg);
        try self.errs.append(report);
    }

    fn synchronize(self: *Self) !void {
        self.panic_mode = false;

        while (!self.check(.Eof)) {
            switch (self.current.kind) {
                .Fn, .For, .If, .Print, .Return, .Struct, .Var, .While => return,
                else => try self.advance(),
            }
        }
    }

    fn declaration(self: *Self) !Stmt {
        if (self.check(.Var)) {
            unreachable;
        } else {
            return .{ .Expr = try self.parse_precedence_expr(0) };
        }
    }

    const Assoc = enum { Left, None };

    const Rule = struct { prec: i8, assoc: Assoc = .Left };

    const rules = std.enums.directEnumArrayDefault(TokenKind, Rule, .{ .prec = -1 }, 0, .{
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
        try self.advance();
        var node = try self.parse_prefix_expr();

        var banned_prec: i8 = -1;

        while (true) {
            // We check the current before consuming it
            const next_rule = rules[@as(usize, @intFromEnum(self.current.kind))];

            if (next_rule.prec < prec_min) break;

            if (next_rule.prec == banned_prec) {
                try self.error_at_current(.ChainingCmpOp, null);
                return error.err;
            }

            // Here, we can safely use it
            try self.advance();
            const op = self.previous;
            const rhs = try self.parse_precedence_expr(next_rule.prec + 1);

            const expr = try self.allocator.create(Expr);
            expr.* = .{ .BinOp = .{
                .lhs = node,
                .rhs = rhs,
                .op = op,
            } };

            node = expr;

            if (next_rule.assoc == .None) banned_prec = next_rule.prec;

            try self.skip_new_lines();
        }

        return node;
    }

    /// Parses a prefix expression. If we hit a unary first, we recurse
    /// to form an expression
    fn parse_prefix_expr(self: *Self) Error!*Expr {
        const unary_expr = switch (self.previous.kind) {
            .Minus => try self.allocator.create(Expr),
            else => return self.parse_primary_expr(),
        };

        const op = self.previous;
        try self.advance();

        // Recursion appens here
        unary_expr.* = .{ .Unary = .{
            .op = op,
            .rhs = try self.parse_prefix_expr(),
        } };

        return unary_expr;
    }

    fn parse_primary_expr(self: *Self) Error!*Expr {
        return switch (self.previous.kind) {
            .LeftParen => self.grouping(),
            .Int => self.int(),
            else => {
                if (self.previous.kind == .Eof) {
                    try self.error_at_prev(.UnexpectedEof, null);
                } else {
                    try self.errs_extra.append(self.previous.lexeme);
                    try self.error_at_prev(.ExpectExpr, null);
                }
                return error.err;
            },
        };
    }

    fn grouping(self: *Self) Error!*Expr {
        const opening = &self.previous;
        const expr = try self.allocator.create(Expr);
        expr.* = .{ .Grouping = .{ .expr = try self.parse_precedence_expr(0) } };
        try self.expect_or_err_at(.RightParen, .UnclosedParen, opening);
        return expr;
    }

    fn int(self: *Self) Error!*Expr {
        const value = try std.fmt.parseInt(i64, self.previous.lexeme, 10);
        const expr = try self.allocator.create(Expr);
        expr.* = .{ .IntLit = .{ .value = value } };

        return expr;
    }
};

// Tests
test Parser {
    const test_all = @import("test_parser.zig").test_all;
    try test_all();
}
