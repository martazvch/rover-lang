pub const LexerMsg = union(enum) {
    leadingZeros,
    unterminatedStr,
    unexpectedChar,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .leadingZeros => writer.print("leading zeros in integer literals are not allowed", .{}),
            .unterminatedStr => writer.print("unterminated string", .{}),
            .unexpectedChar => writer.print("unexpected character", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            else => writer.print("here", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .leadingZeros => writer.print("remove the leading zeros", .{}),
            .unterminatedStr => writer.print("close the opening quote", .{}),
            else => {},
        };
    }
};
