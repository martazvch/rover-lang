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
        .{ "and", .@"and" },
        .{ "as", .as },
        .{ "bool", .bool },
        .{ "do", .do },
        .{ "else", .@"else" },
        .{ "error", .@"error" },
        .{ "false", .false },
        .{ "float", .floatKw },
        .{ "fn", .@"fn" },
        .{ "for", .@"for" },
        .{ "if", .@"if" },
        .{ "ifnull", .ifNull },
        .{ "in", .in },
        .{ "int", .intKw },
        .{ "not", .not },
        .{ "null", .null },
        .{ "or", .@"or" },
        .{ "print", .print },
        .{ "return", .@"return" },
        .{ "self", .self },
        .{ "str", .strKw },
        .{ "struct", .@"struct" },
        .{ "true", .true },
        .{ "use", .use },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
    });

    pub const Tag = enum {
        @"and",
        as,
        bang,
        bangEqual,
        bool,
        colon,
        comma,
        do,
        dot,
        dotBang,
        dotDot,
        dotDotDot,
        dotQuestionMark,
        dotStar,
        @"else",
        eof,
        equal,
        equalEqual,
        @"error",
        false,
        float,
        floatKw,
        @"fn",
        @"for",
        greater,
        greaterEqual,
        identifier,
        @"if",
        ifNull,
        in,
        int,
        intKw,
        leftBrace,
        leftParen,
        less,
        lessEqual,
        minus,
        modulo,
        newLine,
        not,
        null,
        @"or",
        plus,
        print,
        questionMark,
        @"return",
        rightBrace,
        rightParen,
        self,
        slash,
        smallArrow,
        star,
        strKw,
        string,
        @"struct",
        true,
        underscore,
        use,
        @"var",
        @"while",

        leadingZeros,
        unterminatedStr,
        unexpectedChar,
    };

    pub fn getKeyword(ident: []const u8) ?Tag {
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
        comment,
        dot,
        dotDot,
        equal,
        float,
        greater,
        identifier,
        int,
        invalid,
        less,
        slash,
        start,
        string,
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
                .leadingZeros => self.errorAt(.leadingZeros, &tk),
                .unterminatedStr => self.errorAt(.unterminatedStr, &tk),
                .unexpectedChar => self.errorAt(.unexpectedChar, &tk),
                else => self.tokens.append(self.allocator, tk),
            };

            if (tk.tag == .eof) break;
        }
    }

    fn errorAt(self: *Self, tag: LexerMsg, token: *const Token) !void {
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

        state: switch (State.start) {
            .start => {
                switch (self.source[self.index]) {
                    'a'...'z', 'A'...'Z' => {
                        res.tag = .identifier;
                        continue :state .identifier;
                    },
                    ' ', '\t', '\r' => {
                        self.index += 1;
                        res.span.start = self.index;
                        continue :state .start;
                    },
                    '(' => {
                        res.tag = .leftParen;
                        self.index += 1;
                    },
                    ')' => {
                        res.tag = .rightParen;
                        self.index += 1;
                    },
                    '{' => {
                        res.tag = .leftBrace;
                        self.index += 1;
                    },
                    '}' => {
                        res.tag = .rightBrace;
                        self.index += 1;
                    },
                    ':' => {
                        res.tag = .colon;
                        self.index += 1;
                    },
                    ',' => {
                        res.tag = .comma;
                        self.index += 1;
                    },
                    '+' => {
                        res.tag = .plus;
                        self.index += 1;
                    },
                    '-' => {
                        self.index += 1;

                        if (self.source[self.index] == '>') {
                            self.index += 1;
                            res.tag = .smallArrow;
                        } else res.tag = .minus;
                    },
                    '*' => {
                        res.tag = .star;
                        self.index += 1;
                    },
                    '/' => continue :state .slash,
                    '\n' => {
                        res.tag = .newLine;
                        self.index += 1;
                    },
                    '<' => continue :state .less,
                    '>' => continue :state .greater,
                    '!' => continue :state .bang,
                    '=' => continue :state .equal,
                    '.' => continue :state .dot,
                    '"' => {
                        res.tag = .string;
                        continue :state .string;
                    },
                    '0' => {
                        if (self.source[self.index + 1] == '.') {
                            self.index += 1;
                            continue :state .float;
                        } else {
                            self.index += 1;
                            switch (self.source[self.index]) {
                                '0'...'9' => return .{
                                    .tag = .leadingZeros,
                                    .span = .{
                                        .start = self.index - 1,
                                        .end = self.index,
                                    },
                                },
                                else => res.tag = .int,
                            }
                        }
                    },
                    '1'...'9' => {
                        res.tag = .int;
                        self.index += 1;
                        continue :state .int;
                    },
                    '_' => {
                        self.index += 1;
                        switch (self.source[self.index]) {
                            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                                res.tag = .identifier;
                                continue :state .identifier;
                            },
                            else => res.tag = .underscore,
                        }
                    },
                    0 => {
                        if (self.index == self.source.len) {
                            return .{
                                .tag = .eof,
                                .span = .{ .start = self.index, .end = self.index },
                            };
                        } else continue :state .invalid;
                    },
                    else => {
                        res.tag = .unexpectedChar;
                        self.index += 1;
                    },
                }
            },
            .bang => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .bangEqual;
                        self.index += 1;
                    },
                    else => res.tag = .bang,
                }
            },
            .comment => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '\n' => continue :state .start,
                    else => continue :state .comment,
                }
            },
            .dot => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '0'...'9' => continue :state .float,
                    '.' => continue :state .dotDot,
                    '*' => {
                        res.tag = .dotStar;
                        self.index += 1;
                    },
                    '?' => {
                        res.tag = .dotQuestionMark;
                        self.index += 1;
                    },
                    '!' => {
                        res.tag = .dotBang;
                        self.index += 1;
                    },
                    else => res.tag = .dot,
                }
            },
            .dotDot => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '.' => {
                        res.tag = .dotDotDot;
                        self.index += 1;
                    },
                    else => res.tag = .dotDot,
                }
            },
            .equal => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .equalEqual;
                        self.index += 1;
                    },
                    else => res.tag = .equal,
                }
            },
            .float => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '0'...'9' => continue :state .float,
                    else => res.tag = .float,
                }
            },
            .greater => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .greaterEqual;
                        self.index += 1;
                    },
                    else => res.tag = .greater,
                }
            },
            .identifier => {
                self.index += 1;

                switch (self.source[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const ident = self.source[res.span.start..self.index];

                        if (Token.getKeyword(ident)) |kw| {
                            res.tag = kw;
                        }
                    },
                }
            },
            .int => {
                switch (self.source[self.index]) {
                    '0'...'9' => {
                        self.index += 1;
                        continue :state .int;
                    },
                    '.' => continue :state .float,
                    else => {},
                }
            },
            .invalid => {
                self.index += 1;

                switch (self.source[self.index]) {
                    0 => {
                        if (self.index == self.source.len) {
                            res.tag = .eof;
                        } else continue :state .invalid;
                    },
                    ' ' => res.tag = .@"error",
                    else => continue :state .invalid,
                }
            },
            .less => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '=' => {
                        res.tag = .lessEqual;
                        self.index += 1;
                    },
                    else => res.tag = .less,
                }
            },
            .slash => {
                self.index += 1;

                switch (self.source[self.index]) {
                    '/' => continue :state .comment,
                    else => res.tag = .slash,
                }
            },
            .string => {
                self.index += 1;

                switch (self.source[self.index]) {
                    0 => {
                        if (self.index == self.source.len) {
                            // For error reporting, one byte length
                            return .{
                                .tag = .unterminatedStr,
                                .span = .{
                                    .start = res.span.start,
                                    .end = res.span.start + 1,
                                },
                            };
                        }
                    },
                    '"' => self.index += 1,
                    else => continue :state .string,
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
        .{ .tag = .identifier, .span = .{ .start = 0, .end = 3 } },
        .{ .tag = .identifier, .span = .{ .start = 4, .end = 7 } },
        .{ .tag = .identifier, .span = .{ .start = 8, .end = 16 } },
        .{ .tag = .identifier, .span = .{ .start = 18, .end = 23 } },
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
        .{ .tag = .int, .span = .{ .start = 0, .end = 3 } },
        .{ .tag = .float, .span = .{ .start = 4, .end = 8 } },
        .{ .tag = .float, .span = .{ .start = 9, .end = 11 } },
        .{ .tag = .float, .span = .{ .start = 12, .end = 15 } },
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
        .leftParen,    .rightParen, .leftBrace, .rightBrace, .dot,     .colon,
        .comma,        .equal,      .bang,      .less,       .greater, .lessEqual,
        .greaterEqual, .bangEqual,  .plus,      .minus,      .star,    .slash,
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
        .@"and",    .@"else", .false, .@"for",    .@"fn", .@"if",  .in,       .null, .@"or", .print,
        .@"return", .newLine, .self,  .@"struct", .true,  .@"var", .@"while", .not,  .intKw, .floatKw,
        .strKw,     .do,      .use,   .eof,
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
    try expect(err.report == .unterminatedStr);
}

test "leading zeros" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("var e = 01\n var b = 00002");

    try expect(lexer.errs.items[0].report == .leadingZeros);
    try expect(lexer.errs.items[1].report == .leadingZeros);
}

test "underscore" {
    var lexer = Lexer.init(std.testing.allocator);
    defer lexer.deinit();
    try lexer.lex("var _under   _=1   var _1art   var ___yo");

    const res = [_]Token.Tag{
        .@"var", .identifier, .underscore, .equal,      .int,
        .@"var", .identifier, .@"var",     .identifier, .eof,
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
        .minus, .greater, .minus,      .int, .greater, .greater, .minus,
        .minus, .less,    .smallArrow,
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
        .dot,     .dotDot,    .dotDotDot, .dotBang, .dotQuestionMark,
        .dotStar, .dotDotDot, .dotBang,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}
