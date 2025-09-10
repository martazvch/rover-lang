const Writer = @import("std").Io.Writer;

pub const LexerMsg = union(enum) {
    leading_zeroes,
    unterminated_str,
    unexpected_char,

    const Self = @This();

    pub fn getMsg(self: Self, writer: *Writer) !void {
        try switch (self) {
            .leading_zeroes => writer.writeAll("leading zeros in integer literals are not allowed"),
            .unterminated_str => writer.writeAll("unterminated string"),
            .unexpected_char => writer.writeAll("unexpected character"),
        };
    }

    pub fn getHint(self: Self, writer: *Writer) !void {
        try switch (self) {
            else => writer.writeAll("here"),
        };
    }

    pub fn getHelp(self: Self, writer: *Writer) !void {
        try switch (self) {
            .leading_zeroes => writer.writeAll("remove the leading zeros"),
            .unterminated_str => writer.writeAll("close the opening quote"),
            else => {},
        };
    }
};
