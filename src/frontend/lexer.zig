const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const GenReport = @import("../reporter.zig").GenReport;
const LexerMsg = @import("lexer_msg.zig").LexerMsg;

pub const Span = struct {
    start: usize,
    end: usize,

    pub fn text(self: *const Span, source: []const u8) []const u8 {
        return source[self.start..self.end];
    }
};

pub const Token = struct {
    tag: Tag,
    span: Span,

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .And },
        .{ "as", .As },
        .{ "bool", .Bool },
        .{ "do", .Do },
        .{ "else", .@"else" },
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
        .{ "str", .StrKw },
        .{ "struct", .Struct },
        .{ "true", .True },
        .{ "use", .Use },
        .{ "var", .Var },
        .{ "while", .While },
    });

    pub const Tag = enum {
        And,
        As,
        bang,
        BangEqual,
        Bool,
        Colon,
        Comma,
        Do,
        Dot,
        DotBang,
        DotDot,
        DotDotDot,
        DotQuestionMark,
        DotStar,
        @"else",
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
        minus,
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
        SmallArrow,
        Star,
        StrKw,
        String,
        Struct,
        True,
        Underscore,
        Use,
        Var,
        While,

        LeadingZeros,
        UnterminatedStr,
        UnexpectedChar,

        // Only used by AST printer for binop and unary nodes
        pub fn symbol(self: Tag) []const u8 {
            return switch (self) {
                .And => "and",
                // .As => "as",
                .bang => "!",
                .BangEqual => "!=",
                // .Bool => "bool",
                // .Colon => ":",
                // .Comma => ",",
                // .Do => "do",
                // .Dot => ".",
                // .DotBang => ".!",
                // .DotDot => "..",
                // .DotDotDot => "...",
                // .DotQuestionMark => ".?",
                // .DotStar => ".*",
                // .@"else" => "else",
                // .Eof => "eof",
                // .Equal => "=",
                // .EqualEqual => "==",
                // .Error => "error",
                // .False => "false",
                // .Float => "float value",
                // .FloatKw => "float",
                // .Fn => "fn",
                // .For => "for",
                .Greater => ">",
                .GreaterEqual => ">=",
                // .Identifier => "identifier",
                // .If => "if",
                // .IfNull => "ifnull",
                // .In => "in",
                // .Int => "int value",
                // .IntKw => "int",
                // .LeftBrace => "{",
                // .LeftParen => "(",
                .Less => "<",
                .LessEqual => "<=",
                .minus => "-",
                // .Modulo => "%",
                // .NewLine => "newline",
                .Not => "not",
                // .Null => "null",
                .Or => "or",
                .Plus => "+",
                // .Print => "print",
                // .QuestionMark => "?",
                // .Return => "return",
                // .RightBrace => "}",
                // .RightParen => ")",
                // .Self => "self",
                .Slash => "/",
                // .SmallArrow => "->",
                .Star => "*",
                // .StrKw => "str",
                // .String => "string value",
                // .Struct => "struct",
                // .True => "true",
                // .Underscore => "_",
                // .Use => "use",
                // .Var => "var",
                // .While => "while",

                // .LeadingZeros, .UnexpectedChar, .UnterminatedStr => unreachable,
                else => unreachable,
            };
        }
    };

    pub fn from_source(self: *const Token, source: []const u8) []const u8 {
        return source[self.span.start..self.span.end];
    }

    pub fn get_keyword(ident: []const u8) ?Tag {
        return keywords.get(ident);
    }
};

pub const Lexer = struct {
    source: [:0]const u8,
    index: usize,
    tokens: std.MultiArrayList(Token),
    errs: ArrayList(LexerReport),
    allocator: Allocator,

    const Self = @This();
    pub const LexerReport = GenReport(LexerMsg);

    const State = enum {
        bang,
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
            .tokens = .{},
            .errs = .init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit(self.allocator);
        self.errs.deinit();
    }

    pub fn lex(self: *Self, source: [:0]const u8) !void {
        self.source = source;

        while (true) {
            const tk = self.next();

            // TODO: redo this part. As we lex every thing at once, use arraylist for
            // errors like parser, analyzer, ...? Or use compitme to associate both sides
            try switch (tk.tag) {
                .LeadingZeros => self.error_at(.LeadingZeros, &tk),
                .UnterminatedStr => self.error_at(.UnterminatedStr, &tk),
                .UnexpectedChar => self.error_at(.UnexpectedChar, &tk),
                else => self.tokens.append(self.allocator, tk),
            };

            if (tk.tag == .Eof) break;
        }
    }

    fn error_at(self: *Self, tag: LexerMsg, token: *const Token) !void {
        const report = LexerReport.err(tag, token.span);
        try self.errs.append(report);
    }

    pub fn next(self: *Self) Token {
        var res = Token{
            .tag = undefined,
            .span = .{
                .start = self.index,
                .end = undefined,
            },
        };

        state: switch (State.Start) {
            .Start => {
                switch (self.source[self.index]) {
                    'a'...'z', 'A'...'Z' => {
                        res.tag = .Identifier;
                        continue :state .Identifier;
                    },
                    ' ', '\t', '\r' => {
                        self.index += 1;
                        res.span.start = self.index;
                        continue :state .Start;
                    },
                    '(' => {
                        res.tag = .LeftParen;
                        self.index += 1;
                    },
                    ')' => {
                        res.tag = .RightParen;
                        self.index += 1;
                    },
                    '{' => {
                        res.tag = .LeftBrace;
                        self.index += 1;
                    },
                    '}' => {
                        res.tag = .RightBrace;
                        self.index += 1;
                    },
                    ':' => {
                        res.tag = .Colon;
                        self.index += 1;
                    },
                    ',' => {
                        res.tag = .Comma;
                        self.index += 1;
                    },
                    '+' => {
                        res.tag = .Plus;
                        self.index += 1;
                    },
                    '-' => {
                        self.index += 1;

                        if (self.source[self.index] == '>') {
                            self.index += 1;
                            res.tag = .SmallArrow;
                        } else res.tag = .minus;
                    },
                    '*' => {
                        res.tag = .Star;
                        self.index += 1;
                    },
                    '/' => continue :state .Slash,
                    '\n' => {
                        res.tag = .NewLine;
                        self.index += 1;
                    },
                    '<' => continue :state .Less,
                    '>' => continue :state .Greater,
                    '!' => continue :state .bang,
                    '=' => continue :state .Equal,
                    '.' => continue :state .Dot,
                    '"' => {
                        res.tag = .String;
                        continue :state .String;
                    },
                    '0' => {
                        if (self.source[self.index + 1] == '.') {
                            self.index += 1;
                            continue :state .Float;
                        } else {
                            self.index += 1;
                            switch (self.source[self.index]) {
                                '0'...'9' => return .{
                                    .tag = .LeadingZeros,
                                    .span = .{
                                        .start = self.index - 1,
                                        .end = self.index,
                                    },
                                },
                                else => res.tag = .Int,
                            }
                        }
                    },
                    '1'...'9' => {
                        res.tag = .Int;
                        self.index += 1;
                        continue :state .Int;
                    },
                    '_' => {
                        self.index += 1;
                        switch (self.source[self.index]) {
                            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                                res.tag = .Identifier;
                                continue :state .Identifier;
                            },
                            else => res.tag = .Underscore,
                        }
                    },
                    0 => {
                        if (self.index == self.source.len) {
                            return .{
                                .tag = .Eof,
                                .span = .{ .start = self.index, .end = self.index },
                            };
                        } else continue :state .Invalid;
                    },
                    else => {
                        res.tag = .UnexpectedChar;
                        self.index += 1;
                    },
                }
            },
            .bang => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .BangEqual;
                        self.index += 1;
                    },
                    else => res.tag = .bang,
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
                    '0'...'9' => continue :state .Float,
                    '.' => continue :state .DotDot,
                    '*' => {
                        res.tag = .DotStar;
                        self.index += 1;
                    },
                    '?' => {
                        res.tag = .DotQuestionMark;
                        self.index += 1;
                    },
                    '!' => {
                        res.tag = .DotBang;
                        self.index += 1;
                    },
                    else => res.tag = .Dot,
                }
            },
            .DotDot => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '.' => {
                        res.tag = .DotDotDot;
                        self.index += 1;
                    },
                    else => res.tag = .DotDot,
                }
            },
            .Equal => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .EqualEqual;
                        self.index += 1;
                    },
                    else => res.tag = .Equal,
                }
            },
            .Float => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '0'...'9' => continue :state .Float,
                    else => res.tag = .Float,
                }
            },
            .Greater => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .GreaterEqual;
                        self.index += 1;
                    },
                    else => res.tag = .Greater,
                }
            },
            .Identifier => {
                self.index += 1;

                switch (self.source[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .Identifier,
                    else => {
                        const ident = self.source[res.span.start..self.index];

                        if (Token.get_keyword(ident)) |kw| {
                            res.tag = kw;
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
                            res.tag = .Eof;
                        } else continue :state .Invalid;
                    },
                    ' ' => res.tag = .Error,
                    else => continue :state .Invalid,
                }
            },
            .Less => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .LessEqual;
                        self.index += 1;
                    },
                    else => res.tag = .Less,
                }
            },
            .Slash => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '/' => continue :state .Comment,
                    else => res.tag = .Slash,
                }
            },
            .String => {
                self.index += 1;

                switch (self.source[self.index]) {
                    0 => {
                        if (self.index == self.source.len) {
                            // For error reporting, one byte length
                            return .{
                                .tag = .UnterminatedStr,
                                .span = .{
                                    .start = res.span.start,
                                    .end = res.span.start + 1,
                                },
                            };
                        }
                    },
                    '"' => self.index += 1,
                    else => continue :state .String,
                }
            },
        }

        res.span.end = self.index;
        return res;
    }
};

// ------------
//  Tests
// ------------
test "ident and strings" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("foo bar variable  truth");

    const res = [_]Token{
        .{ .tag = .Identifier, .span = .{ .start = 0, .end = 3 } },
        .{ .tag = .Identifier, .span = .{ .start = 4, .end = 7 } },
        .{ .tag = .Identifier, .span = .{ .start = 8, .end = 16 } },
        .{ .tag = .Identifier, .span = .{ .start = 18, .end = 23 } },
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        const span = lexer.tokens.items(.span)[i];

        try expect(tag == res[i].tag);
        try expect(span.start == res[i].span.start);
        try expect(span.end == res[i].span.end);
    }
}

test "numbers" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("123 45.6 7. .86");

    const res = [_]Token{
        .{ .tag = .Int, .span = .{ .start = 0, .end = 3 } },
        .{ .tag = .Float, .span = .{ .start = 4, .end = 8 } },
        .{ .tag = .Float, .span = .{ .start = 9, .end = 11 } },
        .{ .tag = .Float, .span = .{ .start = 12, .end = 15 } },
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        const span = lexer.tokens.items(.span)[i];

        try expect(tag == res[i].tag);
        try expect(span.start == res[i].span.start);
        try expect(span.end == res[i].span.end);
    }
}

test "tokens" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("(){}.:,=!< ><= >= !=+-*/");

    const res = [_]Token.Tag{
        .LeftParen,    .RightParen, .LeftBrace, .RightBrace, .Dot,     .Colon,
        .Comma,        .Equal,      .bang,      .Less,       .Greater, .LessEqual,
        .GreaterEqual, .BangEqual,  .Plus,      .minus,      .Star,    .Slash,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "keywords" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex(
        \\\and else false for fn if in null or print return 
        \\\self struct true var while not int float str do use
    );

    const res = [_]Token.Tag{
        .And,    .@"else", .False, .For,    .Fn,   .If,  .In,    .Null, .Or,    .Print,
        .Return, .NewLine, .Self,  .Struct, .True, .Var, .While, .Not,  .IntKw, .FloatKw,
        .StrKw,  .Do,      .Use,   .Eof,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "unterminated string" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("\"blabla bli blop");

    const err = lexer.errs.items[0];
    try expect(err.report == .UnterminatedStr);
}

test "leading zeros" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("var e = 01\n var b = 00002");

    try expect(lexer.errs.items[0].report == .LeadingZeros);
    try expect(lexer.errs.items[1].report == .LeadingZeros);
}

test "underscore" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("var _under   _=1   var _1art   var ___yo");

    const res = [_]Token.Tag{
        .Var, .Identifier, .Underscore, .Equal,      .Int,
        .Var, .Identifier, .Var,        .Identifier, .Eof,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "arrow" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("- > -5> >- -< ->");

    const res = [_]Token.Tag{
        .minus, .Greater, .minus,      .Int, .Greater, .Greater, .minus,
        .minus, .Less,    .SmallArrow,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "dot" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex(". .. ... .! .? .* ....!");

    const res = [_]Token.Tag{
        .Dot,     .DotDot,    .DotDotDot, .DotBang, .DotQuestionMark,
        .DotStar, .DotDotDot, .DotBang,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}
