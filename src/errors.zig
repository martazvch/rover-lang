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
    // ExpectExpr,
    // InvalidAssignTarget,
    // BinopInvalidOp,
    ChainingCmpOp,
};

pub fn error_infos(kind: ErrKind) ErrorInfo {
    return switch (kind) {
        .UnterminatedStr => .{
            .msg = "unterminated string",
            .hint = "here",
            .help = "close the opening quote",
        },
        .UnexpectedChar => .{
            .msg = "unexpected character",
            .hint = "here",
        },
        .ChainingCmpOp => .{
            .msg = "chaining comparison operators",
            .hint = "this one is not allowed",
            .help = "split your comparison with 'and' and 'or' operators",
        },
        // .ExpectExpr => .{
        //     .msg = "expect expression",
        //     .hint = "not an expression",
        //     .help = "line must start with a valid statement or expression",
        // },
        // .InvalidAssignTarget => .{
        //     .msg = "invalid assignment target",
        //     .hint = "left hand side of expression",
        //     .help = "assignemnts can only be done on variables and structure fields",
        // },
        // .BinopInvalidOp => .{
        //     .msg = "invalid binary operator",
        //     .hint = "here",
        //     .help = "valid ones are: +, -, *, /, %",
        // },
    };
}
