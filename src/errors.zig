pub const ErrorInfo = struct {
    msg: []const u8,
    hint: []const u8,
    help: ?[]const u8 = null,
};

pub const ErrKind = enum {
    // Lexer
    UnterminatedStr,
    UnexpectedChar,

    // Parser
    ChainingCmpOp,
    UnclosedParen,
    UnexpectedEof,

    // Compiler
    TooManyConst,

    pub fn get_infos(self: *const ErrKind) ErrorInfo {
        return switch (self.*) {
            // Lexer
            .UnterminatedStr => .{
                .msg = "unterminated string",
                .hint = "here",
                .help = "close the opening quote",
            },
            .UnexpectedChar => .{
                .msg = "unexpected character",
                .hint = "here",
            },

            // Parser
            .ChainingCmpOp => .{
                .msg = "chaining comparison operators",
                .hint = "this one is not allowed",
                .help = "split your comparison with 'and' and 'or' operators",
            },
            .UnclosedParen => .{
                .msg = "unclosed parenthesis",
                .hint = "here",
                .help = "close the opening parenthesis",
            },
            .UnexpectedEof => .{
                .msg = "unexpected end of file",
                .hint = "",
            },

            // Compiler
            .TooManyConst => .{
                .msg = "too many constant in this chunk (max 256)",
                .hint = "this one",
                .help = "try to split your code into smaller chunks",
            },
        };
    }
};
