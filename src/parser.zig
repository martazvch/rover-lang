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

    const Error = error{err} || Allocator.Error;

    // const Rule = struct {
    //     prefix: ?*const fn (*Parser, bool) Error!void = null,
    //     infix: ?*const fn (*Parser, bool) Error!void = null,
    //     precedence: Precedence = .None,
    // };
    //
    // const rule_table = std.EnumArray(TokenKind, Rule).init(.{
    //     .And = Rule{ .infix = Self.and_, .precedence = .And },
    //     .Bang = Rule{ .prefix = Self.unary },
    //     .BangEqual = Rule{ .infix = Self.binary, .precedence = .Equality },
    //     .Colon = Rule{},
    //     .Comma = Rule{},
    //     .Dot = Rule{ .infix = Self.dot, .precedence = .Call },
    //     .DotDot = Rule{},
    //     .DotDotDot = Rule{ .prefix = Self.unzip },
    //     .DotStar = Rule{ .infix = Self.deref, .precedence = .Call },
    //     .DotBang = Rule{ .infix = Self.unsafe_access, .precedence = .Call },
    //     .DotQuestionMark = Rule{ .infix = Self.safe_access, .precedence = .Call },
    //     .Else = Rule{},
    //     .Eof = Rule{},
    //     .Equal = Rule{},
    //     .EqualEqual = Rule{ .infix = Self.binary, .precedence = .Equality },
    //     .Error = Rule{},
    //     .False = Rule{ .prefix = Self.identifier },
    //     .Float = Rule{ .prefix = Self.float },
    //     .Fn = Rule{},
    //     .For = Rule{},
    //     .Greater = Rule{ .infix = Self.binary, .precedence = .Comparison },
    //     .GreaterEqual = Rule{ .infix = Self.binary, .precedence = .Comparison },
    //     .Identifier = Rule{ .prefix = Self.identifier },
    //     .If = Rule{ .prefix = Self.if_ },
    //     .In = Rule{},
    //     .Int = Rule{ .prefix = Self.int },
    //     .LeftBrace = Rule{},
    //     .LeftParen = Rule{ .prefix = Self.grouping, .infix = Self.call, .precedence = .Call },
    //     .Less = Rule{ .infix = Self.binary, .precedence = .Comparison },
    //     .LessEqual = Rule{ .infix = Self.binary, .precedence = .Comparison },
    //     .Minus = Rule{ .prefix = Self.unary, .infix = Self.binary, .precedence = .Term },
    //     .NewLine = Rule{},
    //     .Null = Rule{ .prefix = Self.identifier },
    //     .Or = Rule{ .infix = Self.or_, .precedence = .Or },
    //     .Plus = Rule{ .infix = Self.binary, .precedence = .Term },
    //     .Print = Rule{},
    //     .Return = Rule{},
    //     .RightParen = Rule{},
    //     .RightBrace = Rule{},
    //     .Slash = Rule{ .infix = Self.binary, .precedence = .Factor },
    //     .Star = Rule{ .infix = Self.binary, .precedence = .Factor },
    //     .String = Rule{ .prefix = Self.string },
    //     .Struct = Rule{},
    //     .Self = Rule{ .prefix = Self.self_ },
    //     .True = Rule{ .prefix = Self.identifier },
    //     .Var = Rule{},
    //     .While = Rule{},
    // });

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

        while (!try self.match(.Eof)) {
            try self.skip_new_lines();
            const stmt = self.declaration() catch |e| switch (e) {
                Error.err => {
                    try self.synchronize();
                    continue;
                },
                else => return e,
            };
            try self.stmts.append(stmt);
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
        if (self.match(kind)) {
            self.advance();
            return;
        }

        try self.error_at_current(error_kind, null);
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

    // fn get_rule(kind: TokenKind) *const Rule {
    //     return rule_table.getPtrConst(kind);
    // }

    // fn parse_precedence(self: *Self, precedence: Precedence) !void {
    //     self.advance();
    //     const prefix_rule = get_rule(self.previous.kind).prefix orelse {
    //         self.error_at_prev(.ExpectExpr, null);
    //         return;
    //     };
    //
    //     // We can assign only if already in assignment or expr_stmt
    //     const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
    //     try prefix_rule(self, can_assign);
    //
    //     while (@intFromEnum(precedence) <= @intFromEnum(get_rule(self.current.kind).precedence)) {
    //         self.advance();
    //
    //         // Here we can safely use unreachable because if the token is unknown,
    //         // the lexer will tell us before. All the tokens we use here are validated
    //         // by the lexer so we are going to find a rule
    //         const infix_rule = get_rule(self.parser.previous.kind).infix orelse unreachable;
    //
    //         try infix_rule(self, can_assign);
    //     }
    //
    //     // If we went through all the loop and that we tried to assign with
    //     // a wrong target, nobody will have consumed the '='. It means the
    //     // assignment target was invalid
    //     if (can_assign and self.match(.Equal)) {
    //         self.error_at_prev(.InvalidAssignTarget, null);
    //     }
    // }

    const Assoc = enum { Left, None };

    const Rule = struct { prec: i8, assoc: Assoc = .Left };

    const rules = std.enums.directEnumArrayDefault(TokenKind, Rule, .{ .prec = -1, .assoc = .Left }, 0, .{
        .Plus = .{ .prec = 60 },
    });

    var banned_prec: i8 = -1;

    fn parse_precedence_expr(self: *Self, prec_min: i8) !*Expr {
        try self.advance();
        var node = try self.parse_prefix_expr();

        while (true) {
            try self.advance();

            const next_rule = rules[@as(usize, @intFromEnum(self.previous.kind))];

            if (next_rule.prec < prec_min) break;

            if (next_rule.prec == banned_prec) {
                try self.error_at_prev(.ChainingCmpOp, null);
            }

            const rhs = try self.parse_precedence_expr(prec_min + 1);

            const expr = try self.allocator.create(Expr);
            expr.* = .{ .BinOp = .{
                .lhs = node,
                .rhs = rhs,
                .op = self.previous,
            } };

            node = expr;

            if (next_rule.assoc == .None) banned_prec = next_rule.prec;

            try self.skip_new_lines();
        }

        return node;
    }

    // fn assemble_expr(self: *Self, rhs: *Expr, lhs: *Expr, op: *Token) *Expr {
    //     switch (op.kind) {
    //         .Plus => {
    //             var expr = try self.allocator.create(Expr.BinOp);
    //             expr.rhs = rhs;
    //             expr.op = op.kind;
    //             expr.lhs = lhs;
    //             return expr;
    //         },
    //         else => unreachable,
    //     }
    // }

    fn parse_prefix_expr(self: *Self) !*Expr {
        const unary_expr = switch (self.previous.kind) {
            .Minus => try self.allocator.create(Expr),
            else => return try self.parse_primary_expr(),
        };

        const op = self.previous;
        try self.advance();

        unary_expr.* = .{ .Unary = .{
            .op = op,
            .rhs = try self.parse_prefix_expr(),
        } };

        return unary_expr;
    }

    fn parse_primary_expr(self: *Self) !*Expr {
        return switch (self.previous.kind) {
            .Int => try self.int(),
            else => {
                try self.error_at_current(.UnexpectedEof, null);
                return error.err;
            },
        };
    }

    fn int(self: *Self) !*Expr {
        const value = try std.fmt.parseInt(i64, self.previous.lexeme, 10);
        const expr = try self.allocator.create(Expr);
        expr.* = .{ .IntLit = .{ .value = value } };

        return expr;
    }

    //     fn expression(self: *Self) !Expr {
    //         _ = self;
    //         return Expr{ .IntLit = .{ .value = 0 } };
    //     }
    //
    //     fn if_(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn call(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn unzip(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn safe_access(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn unsafe_access(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn deref(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn dotdotdot(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn dot(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn and_(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn or_(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn self_(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn grouping(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn unary(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn binary(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn identifier(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn string(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn float(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn int(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
    //
    //     fn uint(self: *Self, _: bool) Error!Expr {
    //         _ = self;
    //     }
};
