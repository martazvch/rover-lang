pub const ErrorInfo = struct {
    msg: []const u8,
    hint: []const u8,
    help: []const u8,
};

pub const ErrKind = enum {
    // Lexer
    UnterminatedStr,

    // Parser
    BinopInvalidOp,
};

pub fn error_infos(kind: ErrKind) ErrorInfo {
    return switch (kind) {
        .UnterminatedStr => .{
            .msg = "unterminated string",
            .hint = "here",
            .help = "close the opening quote",
        },
        .BinopInvalidOp => .{
            .msg = "invalid binary operator",
            .hint = "here",
            .help = "valid ones are: +, -, *, /, %",
        },
    };
}
