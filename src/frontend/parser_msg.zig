pub const ParserMsg = union(enum) {
    ChainingCmpOp,
    ExpectExpr: struct { found: []const u8 },
    ExpectNewLine,
    UnclosedParen,
    UnexpectedEof,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        // Trick for handling both !usize and !void return type
        try switch (self) {
            .ChainingCmpOp => writer.print("chaining comparison operators", .{}),
            .ExpectExpr => |e| try writer.print("expected expression, found \"{s}\"", .{e.found}),
            .ExpectNewLine => writer.print("expect new line after statement", .{}),
            .UnclosedParen => writer.print("unclosed parenthesis", .{}),
            .UnexpectedEof => writer.print("unexpected end of file", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .ExpectExpr, .ExpectNewLine, .UnclosedParen, .UnexpectedEof => writer.print("here", .{}),
            .ChainingCmpOp => writer.print("this one is not allowed", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("split your comparison with 'and' and 'or' operators", .{}),
            .UnclosedParen => writer.print("close the opening parenthesis", .{}),
            else => {},
        };
    }
};
