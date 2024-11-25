pub const ParserMsg = enum {
    ChainingCmpOp,
    ExpectExpr,
    UnclosedParen,
    UnexpectedEof,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !usize {
        return switch (self) {
            .ChainingCmpOp => writer.write("chaining comparison operators"),
            .ExpectExpr => writer.write("expected expression, found "),
            .UnclosedParen => writer.write("unclosed parenthesis"),
            .UnexpectedEof => writer.write("unexpected end of file"),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !usize {
        return switch (self) {
            .ExpectExpr, .UnclosedParen, .UnexpectedEof => writer.write("here"),
            .ChainingCmpOp => writer.write("this one is not allowed"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !usize {
        return switch (self) {
            .ChainingCmpOp => writer.write("split your comparison with 'and' and 'or' operators"),
            .UnclosedParen => writer.write("close the opening parenthesis"),
            else => 0,
        };
    }
};
