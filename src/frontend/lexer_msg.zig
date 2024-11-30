pub const LexerMsg = union(enum) {
    UnterminatedStr,
    UnexpectedChar,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .UnterminatedStr => writer.print("unterminated string", .{}),
            .UnexpectedChar => writer.print("unexpected character", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .UnterminatedStr, .UnexpectedChar => writer.print("here", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .UnterminatedStr => writer.print("close the opening quote", .{}),
            else => {},
        };
    }
};
