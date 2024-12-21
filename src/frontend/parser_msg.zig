pub const ParserMsg = union(enum) {
    ChainingCmpOp,
    ExpectColonBeforeType,
    ExpectExpr: struct { found: []const u8 },
    ExpectNewLine,
    ExpectTypeName,
    ExpectVarName: struct { keyword: []const u8 },
    InvalidDiscard,
    ExpectBraceAfterElse,
    ExpectBraceAfterIf,
    UnclosedBrace,
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
            .InvalidDiscard => writer.print("invalid discard expression", .{}),
            .ExpectBraceAfterElse => writer.print("expect opening brace after 'else'", .{}),
            .ExpectBraceAfterIf => writer.print("expect opening brace after 'if' condition", .{}),
            .UnclosedBrace => writer.print("unclosed brace", .{}),
            .UnclosedParen => writer.print("unclosed parenthesis", .{}),
            .UnexpectedEof => writer.print("unexpected end of file", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("this one is not allowed", .{}),
            .ExpectColonBeforeType => writer.print("before this identifier", .{}),
            .ExpectExpr, .ExpectNewLine, .UnexpectedEof => writer.print("here", .{}),
            .ExpectTypeName, .ExpectVarName => writer.print("this is not an identifier", .{}),
            .InvalidDiscard => writer.print("expect an assignment to '_'", .{}),
            .ExpectBraceAfterElse, .ExpectBraceAfterIf => writer.print("expect to be here", .{}),
            .UnclosedBrace => writer.print("this opening brace", .{}),
            .UnclosedParen => writer.print("this opening parenthesis", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("split your comparison with 'and' and 'or' operators", .{}),
            .ExpectColonBeforeType => writer.print("add ':' bofre type name", .{}),
            .ExpectTypeName, .ExpectVarName => writer.print("define an identifier", .{}),
            .InvalidDiscard => writer.print("add '=' token: _ = call()", .{}),
            .ExpectBraceAfterElse,
            .ExpectBraceAfterIf,
            => writer.print("add an opening brace '{{' before expression", .{}),
            .UnclosedBrace => writer.print("close the opening brace", .{}),
            .UnclosedParen => writer.print("close the opening parenthesis", .{}),
            else => {},
        };
    }
};
