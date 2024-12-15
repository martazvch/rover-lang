pub const ParserMsg = union(enum) {
    ChainingCmpOp,
    ExpectColonBeforeType,
    ExpectExpr: struct { found: []const u8 },
    ExpectNewLine,
    ExpectTypeName,
    ExpectVarName: struct { keyword: []const u8 },
    UnclosedParen,
    UnexpectedEof,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("chaining comparison operators", .{}),
            .ExpectColonBeforeType => writer.print("invalid variable type declaration", .{}),
            .ExpectExpr => |e| try writer.print("expected expression, found \"{s}\"", .{e.found}),
            .ExpectNewLine => writer.print("expect new line after statement", .{}),
            .ExpectTypeName => writer.print("expect type name after ':'", .{}),
            .ExpectVarName => |e| writer.print("expect variable name after '{s}' keyword", .{e.keyword}),
            .UnclosedParen => writer.print("unclosed parenthesis", .{}),
            .UnexpectedEof => writer.print("unexpected end of file", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .ExpectColonBeforeType => writer.print("before this identifier", .{}),
            .ExpectExpr, .ExpectNewLine, .UnclosedParen, .UnexpectedEof => writer.print("here", .{}),
            .ExpectTypeName, .ExpectVarName => writer.print("this is not an identifier", .{}),
            .ChainingCmpOp => writer.print("this one is not allowed", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("split your comparison with 'and' and 'or' operators", .{}),
            .ExpectColonBeforeType => writer.print("add ':' bofre type name", .{}),
            .ExpectTypeName, .ExpectVarName => writer.print("define an identifier", .{}),
            .UnclosedParen => writer.print("close the opening parenthesis", .{}),
            else => {},
        };
    }
};
