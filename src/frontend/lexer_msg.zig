pub const LexerMsg = union(enum) {
    LeadingZeros,
    UnterminatedStr,
    UnexpectedChar,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .LeadingZeros => writer.print("leading zeros in integer literals are not allowed", .{}),
            .UnterminatedStr => writer.print("unterminated string", .{}),
            .UnexpectedChar => writer.print("unexpected character", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            else => writer.print("here", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .LeadingZeros => writer.print("remove the leading zeros", .{}),
            .UnterminatedStr => writer.print("close the opening quote", .{}),
            else => {},
        };
    }
};
