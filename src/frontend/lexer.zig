const std = @import("std");
const Allocator = std.mem.Allocator;

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

        // Errors
        UnterminatedStr,
        UnexpectedChar,

        //         pub fn symbol(self: Kind) []const u8 {
        //             switch (self) {
        //         .And => ,
        //         .As => ,
        //         .Bang => ,
        //         .BangEqual => ,
        //         .Bool => ,
        //         .Colon => ,
        //         .Comma => ,
        //         .Dot => ,
        //         .DotBang => ,
        // .        DotDot => ,
        //         .DotDotDot => ,
        //         .DotQuestionMark => ,
        //         .DotStar => ,
        //         .Else => ,
        //         .Eof => ,
        //         .Equal => ,
        //         .EqualEqual => ,
        //         .Error => ,
        //         .False => ,
        //         .Float => ,
        //         .FloatKw => ,
        //         .Fn => ,
        //         .For => ,
        //         .Greater => ,
        //         .GreaterEqual => ,
        //         .Identifier => ,
        //         .If => ,
        //         .IfNull => ,
        //         .In => ,
        //         .Int => ,
        //         .IntKw => ,
        //         .LeftBrace => ,
        //         .LeftParen => ,
        //         .Less => ,
        //         .LessEqual => ,
        //         .Minus => ,
        //         .Modulo => ,
        //         .NewLine => ,
        //         .Not => ,
        //         .Null => ,
        //         .Or => ,
        //         .Plus => ,
        //         .Print => ,
        //         .QuestionMark => ,
        //         .Return => ,
        //         .RightBrace => ,
        //         .RightParen => ,
        //         .Self => ,
        //         .Slash => ,
        //         .Star => ,
        //         .String => ,
        //         .Struct => ,
        //         .True => ,
        //         .UintKw => ,
        //         .Var => ,
        //         .While => ,
        // .        UnterminatedStr => ,
        //         .UnexpectedChar => ,
        //
        //
        //             }
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

    const Self = @This();

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

    pub fn init(source: [:0]const u8) Self {
        return .{
            .source = source,
            .index = 0,
        };
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
    var lexer = Lexer.init("foo bar variable  truth");

    const res = [_]Token{
        .{ .kind = .Identifier, .loc = .{ .start = 0, .end = 3 } },
        .{ .kind = .Identifier, .loc = .{ .start = 4, .end = 7 } },
        .{ .kind = .Identifier, .loc = .{ .start = 8, .end = 16 } },
        .{ .kind = .Identifier, .loc = .{ .start = 18, .end = 23 } },
    };

    for (0..res.len) |i| {
        const tk = lexer.next();
        try expect(tk.kind == res[i].kind);
        try expect(tk.loc.start == res[i].loc.start);
        try expect(tk.loc.end == res[i].loc.end);
    }
}

test "numbers" {
    var lexer = Lexer.init("123 45.6 7.");

    const res = [3]Token{
        .{ .kind = .Int, .loc = .{ .start = 0, .end = 3 } },
        .{ .kind = .Float, .loc = .{ .start = 4, .end = 8 } },
        .{ .kind = .Float, .loc = .{ .start = 9, .end = 11 } },
    };

    for (0..res.len) |i| {
        const tk = lexer.next();
        try expect(tk.kind == res[i].kind);
        try expect(tk.loc.start == res[i].loc.start);
        try expect(tk.loc.end == res[i].loc.end);
    }
}

test "tokens" {
    var lexer = Lexer.init("(){}.:,=!< ><= >= !=+-*/");

    const res = [_]Token.Kind{
        .LeftParen,    .RightParen, .LeftBrace, .RightBrace, .Dot,     .Colon,
        .Comma,        .Equal,      .Bang,      .Less,       .Greater, .LessEqual,
        .GreaterEqual, .BangEqual,  .Plus,      .Minus,      .Star,    .Slash,
    };

    for (0..res.len) |i| {
        const tk = lexer.next();
        try expect(tk.kind == res[i]);
    }
}

test "keywords" {
    var lexer = Lexer.init("and else false for fn if in null or print return self struct true var while not");

    const res = [_]Token.Kind{
        .And,    .Else, .False,  .For,  .Fn,  .If,    .In,  .Null, .Or, .Print,
        .Return, .Self, .Struct, .True, .Var, .While, .Not, .Eof,
    };

    for (0..res.len) |i| {
        const tk = lexer.next();
        try expect(tk.kind == res[i]);
    }
}

test "unterminated string" {
    var lexer = Lexer.init("\"blabla bli blop");

    const res: Token.Kind = .UnterminatedStr;
    const tk = lexer.next();
    try expect(tk.kind == res);
}

test "dot" {
    var lexer = Lexer.init(". .. ... .! .? .* ....!");

    const res = [_]Token.Kind{
        .Dot,     .DotDot,    .DotDotDot, .DotBang, .DotQuestionMark,
        .DotStar, .DotDotDot, .DotBang,
    };

    for (0..res.len) |i| {
        const tk = lexer.next();
        try expect(tk.kind == res[i]);
    }
}
