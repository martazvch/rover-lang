const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const GenReport = @import("../reporter.zig").GenReport;
const LexerMsg = @import("lexer_msg.zig").LexerMsg;

pub const Token = struct {
    kind: Kind,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    const keywords = std.StaticStringMap(Kind).initComptime(.{
        .{ "and", .And },
        .{ "as", .As },
        .{ "bool", .Bool },
        .{ "else", .Else },
        .{ "error", .Error },
        .{ "false", .False },
        .{ "float", .FloatKw },
        .{ "fn", .Fn },
        .{ "for", .For },
        .{ "if", .If },
        .{ "ifnull", .IfNull },
        .{ "in", .In },
        .{ "int", .IntKw },
        .{ "not", .Not },
        .{ "null", .Null },
        .{ "or", .Or },
        .{ "print", .Print },
        .{ "return", .Return },
        .{ "self", .Self },
        .{ "struct", .Struct },
        .{ "true", .True },
        .{ "var", .Var },
        .{ "while", .While },
    });

    pub const Kind = enum {
        And,
        As,
        Bang,
        BangEqual,
        Bool,
        Colon,
        Comma,
        Dot,
        DotBang,
        DotDot,
        DotDotDot,
        DotQuestionMark,
        DotStar,
        Else,
        Eof,
        Equal,
        EqualEqual,
        Error,
        False,
        Float,
        FloatKw,
        Fn,
        For,
        Greater,
        GreaterEqual,
        Identifier,
        If,
        IfNull,
        In,
        Int,
        IntKw,
        LeftBrace,
        LeftParen,
        Less,
        LessEqual,
        Minus,
        Modulo,
        NewLine,
        Not,
        Null,
        Or,
        Plus,
        Print,
        QuestionMark,
        Return,
        RightBrace,
        RightParen,
        Self,
        Slash,
        Star,
        String,
        Struct,
        True,
        UintKw,
        Var,
        While,

        UnterminatedStr,
        UnexpectedChar,

        pub fn symbol(self: Kind) []const u8 {
            return switch (self) {
                .And => "and",
                .As => "as",
                .Bang => "!",
                .BangEqual => "!=",
                .Bool => "boolean value",
                .Colon => ":",
                .Comma => ",",
                .Dot => ".",
                .DotBang => ".!",
                .DotDot => "..",
                .DotDotDot => "...",
                .DotQuestionMark => ".?",
                .DotStar => ".*",
                .Else => "else",
                .Eof => "eof",
                .Equal => "=",
                .EqualEqual => "==",
                .Error => "error",
                .False => "false",
                .Float => "float value",
                .FloatKw => "float",
                .Fn => "fn",
                .For => "for",
                .Greater => ">",
                .GreaterEqual => ">=",
                .Identifier => "identifier",
                .If => "if",
                .IfNull => "ifnull",
                .In => "in",
                .Int => "int value",
                .IntKw => "int",
                .LeftBrace => "{",
                .LeftParen => "(",
                .Less => "<",
                .LessEqual => "<=",
                .Minus => "-",
                .Modulo => "%",
                .NewLine => "newline",
                .Not => "not",
                .Null => "null",
                .Or => "or",
                .Plus => "+",
                .Print => "print",
                .QuestionMark => "?",
                .Return => "return",
                .RightBrace => "}",
                .RightParen => ")",
                .Self => "self",
                .Slash => "/",
                .Star => "*",
                .String => "string value",
                .Struct => "struct",
                .True => "true",
                .UintKw => "uint",
                .Var => "var",
                .While => "while",

                .UnexpectedChar, .UnterminatedStr => unreachable,
            };
        }
    };

    pub fn from_source(self: *const Token, source: []const u8) []const u8 {
        return source[self.loc.start..self.loc.end];
    }

    pub fn get_keyword(ident: []const u8) ?Kind {
        return keywords.get(ident);
    }

    pub fn empty() Token {
        return .{ .kind = .Null, .loc = Loc{ .start = 0, .end = 0 } };
    }
};

pub const Lexer = struct {
    source: [:0]const u8,
    index: usize,
    tokens: ArrayList(Token),
    errs: ArrayList(LexerReport),

    const Self = @This();
    pub const LexerReport = GenReport(LexerMsg);

    const State = enum {
        Bang,
        Comment,
        Dot,
        DotDot,
        Equal,
        Float,
        Greater,
        Identifier,
        Int,
        Invalid,
        Less,
        Slash,
        Start,
        String,
    };

    pub fn init(allocator: Allocator) Self {
        return .{
            .source = undefined,
            .index = 0,
            .tokens = ArrayList(Token).init(allocator),
            .errs = ArrayList(LexerReport).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.errs.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.index = 0;
        self.tokens.clearRetainingCapacity();
        self.errs.clearRetainingCapacity();
    }

    pub fn lex(self: *Self, source: [:0]const u8) !void {
        self.source = source;

        while (true) {
            const tk = self.next();

            try switch (tk.kind) {
                .UnterminatedStr => self.error_at(.UnterminatedStr, &tk),
                .UnexpectedChar => self.error_at(.UnexpectedChar, &tk),
                else => self.tokens.append(tk),
            };

            if (tk.kind == .Eof) break;
        }
    }

    fn error_at(self: *Self, kind: LexerMsg, token: *const Token) !void {
        const report = LexerReport.err(kind, token.loc.start, token.loc.end, null);
        try self.errs.append(report);
    }

    pub fn next(self: *Self) Token {
        var res = Token{
            .kind = undefined,

            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        state: switch (State.Start) {
            .Start => {
                switch (self.source[self.index]) {
                    'a'...'z', 'A'...'Z', '_' => {
                        res.kind = .Identifier;
                        continue :state .Identifier;
                    },
                    ' ', '\t', '\r' => {
                        self.index += 1;
                        res.loc.start = self.index;
                        continue :state .Start;
                    },
                    '(' => {
                        res.kind = .LeftParen;
                        self.index += 1;
                    },
                    ')' => {
                        res.kind = .RightParen;
                        self.index += 1;
                    },
                    '{' => {
                        res.kind = .LeftBrace;
                        self.index += 1;
                    },
                    '}' => {
                        res.kind = .RightBrace;
                        self.index += 1;
                    },
                    ':' => {
                        res.kind = .Colon;
                        self.index += 1;
                    },
                    ',' => {
                        res.kind = .Comma;
                        self.index += 1;
                    },
                    '+' => {
                        res.kind = .Plus;
                        self.index += 1;
                    },
                    '-' => {
                        res.kind = .Minus;
                        self.index += 1;
                    },
                    '*' => {
                        res.kind = .Star;
                        self.index += 1;
                    },
                    '/' => continue :state .Slash,
                    '\n' => {
                        res.kind = .NewLine;
                        self.index += 1;
                    },
                    '<' => continue :state .Less,
                    '>' => continue :state .Greater,
                    '!' => continue :state .Bang,
                    '=' => continue :state .Equal,
                    '.' => continue :state .Dot,
                    '"' => {
                        res.kind = .String;
                        continue :state .String;
                    },
                    '1'...'9' => {
                        res.kind = .Int;
                        self.index += 1;
                        continue :state .Int;
                    },
                    0 => {
                        if (self.index == self.source.len) {
                            return .{
                                .kind = .Eof,
                                .loc = .{ .start = self.index, .end = self.index },
                            };
                        } else continue :state .Invalid;
                    },
                    else => {
                        res.kind = .UnexpectedChar;
                        self.index += 1;
                    },
                }
            },
            .Bang => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.kind = .BangEqual;
                        self.index += 1;
                    },
                    else => res.kind = .Bang,
                }
            },
            .Comment => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '\n' => continue :state .Start,
                    else => continue :state .Comment,
                }
            },
            .Dot => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '.' => continue :state .DotDot,
                    '*' => {
                        res.kind = .DotStar;
                        self.index += 1;
                    },
                    '?' => {
                        res.kind = .DotQuestionMark;
                        self.index += 1;
                    },
                    '!' => {
                        res.kind = .DotBang;
                        self.index += 1;
                    },
                    else => res.kind = .Dot,
                }
            },
            .DotDot => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '.' => {
                        res.kind = .DotDotDot;
                        self.index += 1;
                    },
                    else => res.kind = .DotDot,
                }
            },
            .Equal => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.kind = .EqualEqual;
                        self.index += 1;
                    },
                    else => res.kind = .Equal,
                }
            },
            .Float => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '0'...'9' => continue :state .Float,
                    else => res.kind = .Float,
                }
            },
            .Greater => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.kind = .GreaterEqual;
                        self.index += 1;
                    },
                    else => res.kind = .Greater,
                }
            },
            .Identifier => {
                self.index += 1;

                switch (self.source[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .Identifier,
                    else => {
                        const ident = self.source[res.loc.start..self.index];

                        if (Token.get_keyword(ident)) |kw| {
                            res.kind = kw;
                        }
                    },
                }
            },
            .Int => {
                switch (self.source[self.index]) {
                    '0'...'9' => {
                        self.index += 1;
                        continue :state .Int;
                    },
                    '.' => continue :state .Float,
                    else => {},
                }
            },
            .Invalid => {
                self.index += 1;

                switch (self.source[self.index]) {
                    0 => {
                        if (self.index == self.source.len) {
                            res.kind = .Eof;
                        } else continue :state .Invalid;
                    },
                    ' ' => res.kind = .Error,
                    else => continue :state .Invalid,
                }
            },
            .Less => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.kind = .LessEqual;
                        self.index += 1;
                    },
                    else => res.kind = .Less,
                }
            },
            .Slash => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '/' => continue :state .Comment,
                    else => res.kind = .Slash,
                }
            },
            .String => {
                self.index += 1;

                switch (self.source[self.index]) {
                    0 => {
                        if (self.index == self.source.len) {
                            // For error reporting, one byte length
                            return .{
                                .kind = .UnterminatedStr,
                                .loc = .{
                                    .start = res.loc.start,
                                    .end = res.loc.start + 1,
                                },
                            };
                        }
                    },
                    '"' => self.index += 1,
                    else => continue :state .String,
                }
            },
        }

        res.loc.end = self.index;
        return res;
    }
};

// ------------
//  Tests
// ------------
const expect = std.testing.expect;

test "ident and strings" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("foo bar variable  truth");

    const res = [_]Token{
        .{ .kind = .Identifier, .loc = .{ .start = 0, .end = 3 } },
        .{ .kind = .Identifier, .loc = .{ .start = 4, .end = 7 } },
        .{ .kind = .Identifier, .loc = .{ .start = 8, .end = 16 } },
        .{ .kind = .Identifier, .loc = .{ .start = 18, .end = 23 } },
    };

    for (0..res.len) |i| {
        const tk = lexer.tokens.items[i];
        try expect(tk.kind == res[i].kind);
        try expect(tk.loc.start == res[i].loc.start);
        try expect(tk.loc.end == res[i].loc.end);
    }
}

test "numbers" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("123 45.6 7.");

    const res = [3]Token{
        .{ .kind = .Int, .loc = .{ .start = 0, .end = 3 } },
        .{ .kind = .Float, .loc = .{ .start = 4, .end = 8 } },
        .{ .kind = .Float, .loc = .{ .start = 9, .end = 11 } },
    };

    for (0..res.len) |i| {
        const tk = lexer.tokens.items[i];
        try expect(tk.kind == res[i].kind);
        try expect(tk.loc.start == res[i].loc.start);
        try expect(tk.loc.end == res[i].loc.end);
    }
}

test "tokens" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("(){}.:,=!< ><= >= !=+-*/");

    const res = [_]Token.Kind{
        .LeftParen,    .RightParen, .LeftBrace, .RightBrace, .Dot,     .Colon,
        .Comma,        .Equal,      .Bang,      .Less,       .Greater, .LessEqual,
        .GreaterEqual, .BangEqual,  .Plus,      .Minus,      .Star,    .Slash,
    };

    for (0..res.len) |i| {
        const tk = lexer.tokens.items[i];
        try expect(tk.kind == res[i]);
    }
}

test "keywords" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("and else false for fn if in null or print return self struct true var while not");

    const res = [_]Token.Kind{
        .And,    .Else, .False,  .For,  .Fn,  .If,    .In,  .Null, .Or, .Print,
        .Return, .Self, .Struct, .True, .Var, .While, .Not, .Eof,
    };

    for (0..res.len) |i| {
        const tk = lexer.tokens.items[i];
        try expect(tk.kind == res[i]);
    }
}

test "unterminated string" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("\"blabla bli blop");

    const err = lexer.errs.items[0];
    try expect(err.report == .UnterminatedStr);
}

test "dot" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex(". .. ... .! .? .* ....!");

    const res = [_]Token.Kind{
        .Dot,     .DotDot,    .DotDotDot, .DotBang, .DotQuestionMark,
        .DotStar, .DotDotDot, .DotBang,
    };

    for (0..res.len) |i| {
        const tk = lexer.tokens.items[i];
        try expect(tk.kind == res[i]);
    }
}
