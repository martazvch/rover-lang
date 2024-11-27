pub const CompilerMsg = union(enum) {
    ImplicitCast,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        _ = try switch (self) {
            .ImplicitCast => writer.write("implicit cast"),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !usize {
        return switch (self) {
            .ImplicitCast => writer.write("expressions have different types"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        _ = try switch (self) {
            .ImplicitCast => writer.write("unterminated string"),
            else => {},
        };
    }
};
