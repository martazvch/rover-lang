pub const ParserMsg = union(enum) {
    ChainingCmpOp,
    ExpectExpr: struct { found: []const u8 },
    UnclosedParen,
    UnexpectedEof,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        // Trick for handling both !usize and !void return type
        _ = try switch (self) {
            .ChainingCmpOp => writer.write("chaining comparison operators"),
            .UnclosedParen => writer.write("unclosed parenthesis"),
            .UnexpectedEof => writer.write("unexpected end of file"),
            else => |other| blk: {
                switch (other) {
                    .ExpectExpr => |e| try writer.print("expected expression, found \"{s}\"", .{e.found}),
                    else => unreachable,
                }

                break :blk 0;
            },
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !usize {
        return switch (self) {
            .ExpectExpr, .UnclosedParen, .UnexpectedEof => writer.write("here"),
            .ChainingCmpOp => writer.write("this one is not allowed"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        _ = try switch (self) {
            .ChainingCmpOp => writer.write("split your comparison with 'and' and 'or' operators"),
            .UnclosedParen => writer.write("close the opening parenthesis"),
            else => 0,
        };
    }
};
