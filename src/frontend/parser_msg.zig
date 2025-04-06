pub const ParserMsg = union(enum) {
    ChainingCmpOp,
    ExpectArrowBeforeFnType,
    ExpectBraceBeforeFnBody,
    ExpectBraceAfterStructBody,
    ExpectBraceBeforeStructBody,
    ExpectBraceOrDo: struct { what: []const u8 },
    ExpectColonBeforeType,
    ExpectExpr: struct { found: []const u8 },
    ExpectFnName,
    ExpectName: struct { kind: []const u8 },
    ExpectNewLine,
    ExpectParenAfterFnArgs,
    ExpectParenAfterFnName,
    ExpectParenAfterFnParams,
    ExpectStructName,
    ExpectTypeName,
    InvalidDiscard,
    MissingFnParamType,
    TooManyFnArgs: struct { what: []const u8 },
    UnclosedBrace,
    UnclosedParen,
    WrongValueCountVarDecl: struct { expect: usize },

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("chaining comparison operators", .{}),
            .ExpectArrowBeforeFnType => writer.print("expect arrow '->' before function type", .{}),
            .ExpectBraceOrDo => |e| writer.print("expect opening brace or 'do' after '{s}' condition", .{e.what}),
            .ExpectBraceBeforeFnBody => writer.print("expect opening brace before function's body", .{}),
            .ExpectBraceAfterStructBody => writer.print("expect opening brace after structure's body", .{}),
            .ExpectBraceBeforeStructBody => writer.print("expect opening brace before structure's body", .{}),
            .ExpectColonBeforeType => writer.print("invalid variable type declaration", .{}),
            .ExpectExpr => |e| writer.print("expected expression, found \"{s}\"", .{e.found}),
            .ExpectFnName => writer.print("expect function name after 'fn' keyword", .{}),
            .ExpectName => |e| writer.print("expect {s} name ", .{e.kind}),
            .ExpectNewLine => writer.print("expect new line after statement", .{}),
            .ExpectParenAfterFnArgs => writer.print("expect closing parenthesis ')' after function's arguments", .{}),
            .ExpectParenAfterFnName => writer.print("expect opening parenthesis '(' after function's name", .{}),
            .ExpectParenAfterFnParams => writer.print("expect closing parenthesis ')' after function's parameters", .{}),
            .ExpectStructName => writer.print("expect structure name", .{}),
            .ExpectTypeName => writer.print("expect type name", .{}),
            .InvalidDiscard => writer.print("invalid discard expression", .{}),
            .MissingFnParamType => writer.print("missing function's parameter's type", .{}),
            .TooManyFnArgs => |e| writer.print("functions can't have more than 255 {s}", .{e.what}),
            .UnclosedBrace => writer.print("unclosed brace", .{}),
            .UnclosedParen => writer.print("unclosed parenthesis", .{}),
            .WrongValueCountVarDecl => |e| writer.print(
                "value count mismatch variable count, expect {} values",
                .{e.expect},
            ),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("this one is not allowed", .{}),
            .ExpectArrowBeforeFnType,
            .ExpectBraceBeforeFnBody,
            .ExpectBraceAfterStructBody,
            .ExpectBraceBeforeStructBody,
            .ExpectBraceOrDo,
            .ExpectParenAfterFnArgs,
            .ExpectParenAfterFnName,
            .ExpectParenAfterFnParams,
            .ExpectStructName,
            .MissingFnParamType,
            => writer.print("expect to be here", .{}),
            .ExpectColonBeforeType => writer.print("before this identifier", .{}),
            .ExpectExpr, .ExpectNewLine => writer.print("here", .{}),
            .ExpectFnName, .ExpectTypeName, .ExpectName => writer.print("this is not an identifier", .{}),
            .InvalidDiscard => writer.print("expect an assignment to '_'", .{}),
            .TooManyFnArgs => |e| writer.print("this is the 256th {s}", .{e.what}),
            .UnclosedBrace => writer.print("this opening brace", .{}),
            .UnclosedParen => writer.print("this opening parenthesis", .{}),
            .WrongValueCountVarDecl => writer.writeAll("from this variable"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .ChainingCmpOp => writer.print("split your comparison with 'and' and 'or' operators", .{}),
            .ExpectArrowBeforeFnType => writer.print("add an arrow '->' between function's arguments list and type", .{}),
            .ExpectBraceBeforeFnBody, .ExpectBraceBeforeStructBody => writer.print("add an opening brace '{{'", .{}),
            .ExpectBraceAfterStructBody => writer.print("add an closing brace '}}'", .{}),
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
            .ExpectFnName, .ExpectTypeName, .ExpectName, .ExpectStructName => writer.print("define an identifier", .{}),
            .InvalidDiscard => writer.print("add '=' token: _ = call()", .{}),
            .MissingFnParamType => writer.print("function's paramters' types are mandatory, add it after ':'", .{}),
            .TooManyFnArgs => |e| writer.print(
                "split your function into multiple small ones or pass your {s} in structures / arrays",
                .{e.what},
            ),
            .UnclosedBrace => writer.print("close the opening brace", .{}),
            .UnclosedParen => writer.print("close the opening parenthesis", .{}),
            .WrongValueCountVarDecl => writer.writeAll(
                "you must either provide no value, 1 value that will assigned to each variable or one per variable",
            ),
            else => {},
        };
    }
};
