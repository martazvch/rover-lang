pub const CompilerMsg = union(enum) {
    ImplicitCast,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .ImplicitCast => writer.print("implicit cast", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .ImplicitCast => writer.print("expressions have different types", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .ImplicitCast => writer.print("unterminated string", .{}),
            else => {},
        };
    }
};
