pub const CompilerMsg = union(enum) {
    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // try switch (self) {
        //     .ImplicitCast => writer.print("implicit cast", .{}),
        // };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // try switch (self) {
        //     .ImplicitCast => writer.print("expressions have different types", .{}),
        // };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // try switch (self) {
        //     .ImplicitCast => writer.print("unterminated string", .{}),
        //     else => {},
        // };
    }
};
