pub const ParserMsg = union(enum) {
    ChainingCmpOp,
    ExpectArrowBeforeFnType,
    ExpectBraceBeforeFnBody,
    ExpectBraceOrDo: struct { what: []const u8 },
    ExpectColonBeforeType,
    ExpectExpr: struct { found: []const u8 },
    ExpectNewLine,
    ExpectFnName,
    ExpectParenAfterFnArgs,
    ExpectParenAfterFnName,
    ExpectParenAfterFnParams,
    ExpectTypeName,
    ExpectName: struct { kind: []const u8 },
    InvalidDiscard,
    MissingFnParamType,
    TooManyFnArgs: struct { what: []const u8 },
    UnclosedBrace,
    UnclosedParen,
    UnexpectedEof,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("chaining comparison operators", .{}),
            .ExpectArrowBeforeFnType => writer.print("expect arrow '->' before function type", .{}),
            .ExpectBraceOrDo => |e| writer.print("expect opening brace or 'do' after '{s}' condition", .{e.what}),
            .ExpectBraceBeforeFnBody => writer.print("expect opening brace before function's body", .{}),
            .ExpectColonBeforeType => writer.print("invalid variable type declaration", .{}),
            .ExpectExpr => |e| writer.print("expected expression, found \"{s}\"", .{e.found}),
            .ExpectFnName => writer.print("expect function name after 'fn' keyword", .{}),
            .ExpectNewLine => writer.print("expect new line after statement", .{}),
            .ExpectParenAfterFnArgs => writer.print("expect closing parenthesis ')' after function's arguments", .{}),
            .ExpectParenAfterFnName => writer.print("expect opening parenthesis '(' after function's name", .{}),
            .ExpectParenAfterFnParams => writer.print("expect closing parenthesis ')' after function's parameters", .{}),
            .ExpectTypeName => writer.print("expect type name", .{}),
            .ExpectName => |e| writer.print("expect {s} name ", .{e.kind}),
            .InvalidDiscard => writer.print("invalid discard expression", .{}),
            .MissingFnParamType => writer.print("missing function's parameter's type", .{}),
            .TooManyFnArgs => |e| writer.print("functions can't have more than 255 {s}", .{e.what}),
            .UnclosedBrace => writer.print("unclosed brace", .{}),
            .UnclosedParen => writer.print("unclosed parenthesis", .{}),
            .UnexpectedEof => writer.print("unexpected end of file", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("this one is not allowed", .{}),
            .ExpectArrowBeforeFnType,
            .ExpectBraceBeforeFnBody,
            .ExpectBraceOrDo,
            .ExpectParenAfterFnArgs,
            .ExpectParenAfterFnName,
            .ExpectParenAfterFnParams,
            .MissingFnParamType,
            => writer.print("expect to be here", .{}),
            .ExpectColonBeforeType => writer.print("before this identifier", .{}),
            .ExpectExpr, .ExpectNewLine, .UnexpectedEof => writer.print("here", .{}),
            .ExpectFnName, .ExpectTypeName, .ExpectName => writer.print("this is not an identifier", .{}),
            .InvalidDiscard => writer.print("expect an assignment to '_'", .{}),
            .TooManyFnArgs => |e| writer.print("this is the 256th {s}", .{e.what}),
            .UnclosedBrace => writer.print("this opening brace", .{}),
            .UnclosedParen => writer.print("this opening parenthesis", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("split your comparison with 'and' and 'or' operators", .{}),
            .ExpectArrowBeforeFnType => writer.print("add an arrow '->' between function's arguments list and type", .{}),
            .ExpectBraceBeforeFnBody => writer.print("add an opening brace '{{'", .{}),
            .ExpectBraceOrDo => writer.print("add an opening brace '{{' or 'do' keyword before statement", .{}),
            .ExpectParenAfterFnArgs => writer.print("add an closing parenthesis ')' after function call", .{}),
            .ExpectParenAfterFnName => writer.print(
                "add an opening parenthesis '(' between function's name and arguments list",
                .{},
            ),
            .ExpectParenAfterFnParams => writer.print(
                "add an closing parenthesis ')' between function's parameters and return type",
                .{},
            ),
            .ExpectColonBeforeType => writer.print("add ':' bofre type name", .{}),
            .ExpectFnName, .ExpectTypeName, .ExpectName => writer.print("define an identifier", .{}),
            .InvalidDiscard => writer.print("add '=' token: _ = call()", .{}),
            .MissingFnParamType => writer.print("function's paramters' types are mandatory, add it after ':'", .{}),
            .TooManyFnArgs => |e| writer.print(
                "split your function into multiple small ones or pass your {s} in structures / arrays",
                .{e.what},
            ),
            .UnclosedBrace => writer.print("close the opening brace", .{}),
            .UnclosedParen => writer.print("close the opening parenthesis", .{}),
            else => {},
        };
    }
};
