pub const AnalyzerMsg = union(enum) {
    InvalidMathBinop: struct { found: []const u8 },
    ImplicitCast: struct { side: []const u8, type_: []const u8 },

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        _ = try switch (self) {
            .InvalidMathBinop => writer.write("invalid arithmetic operation"),
            .ImplicitCast => writer.write("implicit cast"),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !usize {
        return switch (self) {
            .InvalidMathBinop => writer.write("expression is not a numeric type"),
            .ImplicitCast => writer.write("expressions have different types"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .InvalidMathBinop => |e| writer.print("expect a numeric type, found {s}", .{e.found}),
            .ImplicitCast => |e| writer.print("explicitly cast {s} to {s}", .{ e.side, e.type_ }),
        };
    }

    const Side = enum {
        Lhs,
        Rhs,

        fn str(self: Side) []const u8 {
            return switch (self) {
                .Lhs => "left hand side",
                .Rhs => "right hand side",
            };
        }
    };

    pub fn invalid_math_binop(found: []const u8) Self {
        return .{ .InvalidMathBinop = .{
            .found = found,
        } };
    }

    pub fn implicit_cast(side: Side, type_: []const u8) Self {
        return .{ .ImplicitCast = .{
            .side = side.str(),
            .type_ = type_,
        } };
    }
};
