const UnsafeIterStr = @import("../unsafe_iter.zig").UnsafeIterStr;

pub const LexerMsg = enum {
    UnterminatedStr,
    UnexpectedChar,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype, _: *UnsafeIterStr) !void {
        switch (self) {
            .UnterminatedStr => _ = try writer.write("unterminated string"),
            .UnexpectedChar => _ = try writer.write("unexpected character"),
        }
    }

    pub fn get_hint(self: Self, writer: anytype, _: *UnsafeIterStr) !usize {
        return switch (self) {
            .UnterminatedStr, .UnexpectedChar => writer.write("here"),
        };
    }

    pub fn get_help(self: Self, writer: anytype, _: *UnsafeIterStr) !usize {
        return switch (self) {
            .UnterminatedStr => writer.write("close the opening quote"),
            else => 0,
        };
    }
};
