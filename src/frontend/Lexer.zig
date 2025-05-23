const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const GenReport = @import("../reporter.zig").GenReport;
const LexerMsg = @import("lexer_msg.zig").LexerMsg;
const oom = @import("../utils.zig").oom;

source: [:0]const u8,
index: usize,
tokens: std.MultiArrayList(Token),
errs: ArrayList(LexerReport),
allocator: Allocator,

const Self = @This();
pub const LexerReport = GenReport(LexerMsg);

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
        .{ "float", .float_kw },
        .{ "fn", .@"fn" },
        .{ "for", .@"for" },
        .{ "if", .@"if" },
        .{ "ifnull", .if_null },
        .{ "in", .in },
        .{ "int", .int_kw },
        .{ "not", .not },
        .{ "null", .null },
        .{ "or", .@"or" },
        .{ "print", .print },
        .{ "return", .@"return" },
        .{ "self", .self },
        .{ "str", .str_kw },
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
        bang_equal,
        bool,
        colon,
        comma,
        do,
        dot,
        dot_bang,
        dot_dot,
        dot_dot_dot,
        dot_question_mark,
        dot_star,
        @"else",
        eof,
        equal,
        equal_equal,
        @"error",
        false,
        float,
        float_kw,
        @"fn",
        @"for",
        greater,
        greater_equal,
        identifier,
        @"if",
        if_null,
        in,
        int,
        int_kw,
        left_brace,
        left_paren,
        less,
        less_equal,
        minus,
        minus_equal,
        modulo,
        new_line,
        not,
        null,
        @"or",
        plus,
        plus_equal,
        print,
        question_mark,
        @"return",
        right_brace,
        right_paren,
        self,
        slash,
        slash_equal,
        small_arrow,
        star,
        star_equal,
        str_kw,
        string,
        @"struct",
        true,
        underscore,
        use,
        @"var",
        @"while",

        leading_zeroes,
        unterminated_str,
        unexpected_char,
    };

    pub fn getKeyword(ident: []const u8) ?Tag {
        return keywords.get(ident);
    }
};

const State = enum {
    bang,
    comment,
    dot,
    dot_dot,
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

pub fn lex(self: *Self, source: [:0]const u8) void {
    self.source = source;

    while (true) {
        const tk = self.next();

        // TODO: redo this part. As we lex every thing at once, use arraylist for
        // errors like parser, analyzer, ...? Or use compitme to associate both sides
        switch (tk.tag) {
            .leading_zeroes => self.errorAt(.leading_zeroes, &tk),
            .unterminated_str => self.errorAt(.unterminated_str, &tk),
            .unexpected_char => self.errorAt(.unexpected_char, &tk),
            else => self.tokens.append(self.allocator, tk) catch oom(),
        }

        if (tk.tag == .eof) break;
    }
}

fn errorAt(self: *Self, tag: LexerMsg, token: *const Token) void {
    const report = LexerReport.err(tag, token.span);
    self.errs.append(report) catch oom();
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
                    res.tag = .left_paren;
                    self.index += 1;
                },
                ')' => {
                    res.tag = .right_paren;
                    self.index += 1;
                },
                '{' => {
                    res.tag = .left_brace;
                    self.index += 1;
                },
                '}' => {
                    res.tag = .right_brace;
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
                    self.index += 1;
                    if (self.source[self.index] == '=') {
                        self.index += 1;
                        res.tag = .plus_equal;
                    } else res.tag = .plus;
                },
                '-' => {
                    self.index += 1;

                    switch (self.source[self.index]) {
                        '>' => {
                            self.index += 1;
                            res.tag = .small_arrow;
                        },
                        '=' => {
                            self.index += 1;
                            res.tag = .minus_equal;
                        },
                        else => res.tag = .minus,
                    }
                },
                '*' => {
                    self.index += 1;

                    if (self.source[self.index] == '=') {
                        self.index += 1;
                        res.tag = .star_equal;
                    } else res.tag = .star;
                },
                '/' => continue :state .slash,
                '\n' => {
                    res.tag = .new_line;
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
                                .tag = .leading_zeroes,
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
                    res.tag = .unexpected_char;
                    self.index += 1;
                },
            }
        },
        .bang => {
            self.index += 1;

            switch (self.source[self.index]) {
                '=' => {
                    res.tag = .bang_equal;
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
                '.' => continue :state .dot_dot,
                '*' => {
                    res.tag = .dot_star;
                    self.index += 1;
                },
                '?' => {
                    res.tag = .dot_question_mark;
                    self.index += 1;
                },
                '!' => {
                    res.tag = .dot_bang;
                    self.index += 1;
                },
                else => res.tag = .dot,
            }
        },
        .dot_dot => {
            self.index += 1;

            switch (self.source[self.index]) {
                '.' => {
                    res.tag = .dot_dot_dot;
                    self.index += 1;
                },
                else => res.tag = .dot_dot,
            }
        },
        .equal => {
            self.index += 1;

            switch (self.source[self.index]) {
                '=' => {
                    res.tag = .equal_equal;
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
                    res.tag = .greater_equal;
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
                    res.tag = .less_equal;
                    self.index += 1;
                },
                else => res.tag = .less,
            }
        },
        .slash => {
            self.index += 1;

            switch (self.source[self.index]) {
                '/' => continue :state .comment,
                '=' => {
                    self.index += 1;
                    res.tag = .slash_equal;
                },
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
                            .tag = .unterminated_str,
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

// ------------
//  Tests
// ------------
test "ident and strings" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("foo bar variable  truth");

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
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("123 45.6 7. .86");

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
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("(){}.:,=!< ><= >= !=+-*/ += -= *= /=");

    const res = [_]Token.Tag{
        .left_paren,    .right_paren, .left_brace, .right_brace, .dot,     .colon,
        .comma,         .equal,       .bang,       .less,        .greater, .less_equal,
        .greater_equal, .bang_equal,  .plus,       .minus,       .star,    .slash,
        .plus_equal,    .minus_equal, .star_equal, .slash_equal,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "keywords" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex(
        \\\and else false for fn if in null or print return 
        \\\self struct true var while not int float str do use
    );

    const res = [_]Token.Tag{
        .@"and",    .@"else",  .false, .@"for",    .@"fn", .@"if",  .in,       .null, .@"or",  .print,
        .@"return", .new_line, .self,  .@"struct", .true,  .@"var", .@"while", .not,  .int_kw, .float_kw,
        .str_kw,    .do,       .use,   .eof,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "unterminated string" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("\"blabla bli blop");

    const err = lexer.errs.items[0];
    try expect(err.report == .unterminated_str);
}

test "leading zeros" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("var e = 01\n var b = 00002");

    try expect(lexer.errs.items[0].report == .leading_zeroes);
    try expect(lexer.errs.items[1].report == .leading_zeroes);
}

test "underscore" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("var _under   _=1   var _1art   var ___yo");

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
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex("- > -5> >- -< ->");

    const res = [_]Token.Tag{
        .minus, .greater, .minus,       .int, .greater, .greater, .minus,
        .minus, .less,    .small_arrow,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}

test "dot" {
    var lexer = Self.init(std.testing.allocator);
    defer lexer.deinit();
    lexer.lex(". .. ... .! .? .* ....!");

    const res = [_]Token.Tag{
        .dot,      .dot_dot,     .dot_dot_dot, .dot_bang, .dot_question_mark,
        .dot_star, .dot_dot_dot, .dot_bang,
    };

    for (0..res.len) |i| {
        const tag = lexer.tokens.items(.tag)[i];
        try expect(tag == res[i]);
    }
}
