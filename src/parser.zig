const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Report = @import("reporter.zig").Report;
const Token = @import("lexer.zig").Token;
const TokenKind = @import("lexer.zig").TokenKind;
const Lexer = @import("lexer.zig").Lexer;
const ErrorKind = @import("errors.zig").ErrKind;

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
    allocator: Allocator,
    lexer: Lexer,
    current: Token,
    previous: Token,
    panic_mode: bool,

    const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;
    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .stmts = ArrayList(Stmt).init(allocator),
            .errs = ArrayList(Report).init(allocator),
            .allocator = allocator,
            .lexer = Lexer.new(),
            .current = Token.empty(),
            .previous = Token.empty(),
            .panic_mode = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stmts.deinit();
        self.errs.deinit();
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

    fn error_at_current(self: *Self, error_kind: ErrorKind, msg: ?[]const u8) !void {
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

        const report = Report.err(error_kind, token.start, token.start + token.lexeme.len, msg);
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

    const rules = std.enums.directEnumArrayDefault(TokenKind, Rule, .{ .prec = -1, .assoc = .Left }, 0, .{
        .LeftParen = .{ .prec = 30 },
        .Minus = .{ .prec = 60 },
        .Plus = .{ .prec = 60 },
        .Slash = .{ .prec = 70 },
        .Star = .{ .prec = 70 },
    });

    fn parse_precedence_expr(self: *Self, prec_min: i8) Error!*Expr {
        try self.advance();
        std.debug.print("begin prec: {any}\n", .{self.previous});
        var node = try self.parse_prefix_expr();

        var banned_prec: i8 = -1;

        while (true) {
            // We check the current before consuming it
            const next_rule = rules[@as(usize, @intFromEnum(self.current.kind))];
            std.debug.print("next rule: {}\n", .{next_rule.prec});

            if (next_rule.prec < prec_min) break;

            if (next_rule.prec == banned_prec) {
                try self.error_at_current(.ChainingCmpOp, null);
                return error.err;
            }

            std.debug.print("gonna recurs\n", .{});

            // Here, we can safely use it
            try self.advance();
            const op = self.previous;
            const rhs = try self.parse_precedence_expr(next_rule.prec);

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
            else => return try self.parse_primary_expr(),
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
            .LeftParen => try self.grouping(),
            .Int => try self.int(),

            else => {
                try self.error_at_current(.UnexpectedEof, null);
                return error.err;
            },
        };
    }

    fn grouping(self: *Self) Error!*Expr {
        const expr = try self.allocator.create(Expr);
        expr.* = .{ .Grouping = .{ .expr = try self.parse_precedence_expr(0) } };
        try self.expect(.RightParen, .UnclosedParen);
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
test "literals" {
    const test_file = @import("tests/parser/test_parser.zig").test_file;
    const file_name = "literals.test";
    try test_file(file_name);
}
