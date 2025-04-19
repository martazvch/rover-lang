pub const LexerMsg = union(enum) {
    leading_zeroes,
    unterminated_str,
    unexpected_char,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .leading_zeroes => writer.print("leading zeros in integer literals are not allowed", .{}),
            .unterminated_str => writer.print("unterminated string", .{}),
            .unexpected_char => writer.print("unexpected character", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            else => writer.print("here", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .leading_zeroes => writer.print("remove the leading zeros", .{}),
            .unterminated_str => writer.print("close the opening quote", .{}),
            else => {},
        };
    }
};
