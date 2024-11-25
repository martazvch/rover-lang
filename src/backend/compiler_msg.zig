pub const CompilerMsg = enum {
    UnterminatedStr,
    UnexpectedChar,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !usize {
        return switch (self) {
            .UnterminatedStr => writer.write("unterminated string"),
            .UnexpectedChar => writer.write("unexpected character"),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !usize {
        return switch (self) {
            .UnterminatedStr, .UnexpectedChar => writer.write("here"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !usize {
        return switch (self) {
            .UnterminatedStr => writer.write("close the opening quote"),
            else => 0,
        };
    }
};
