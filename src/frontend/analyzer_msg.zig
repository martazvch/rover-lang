pub const AnalyzerMsg = union(enum) {
    FloatEqual,
    FloatEqualCast,
    InvalidArithmetic: struct { found: []const u8 },
    InvalidComparison: struct { found1: []const u8, found2: []const u8 },
    InvalidVarDeclType: struct { expect: []const u8, found: []const u8 },
    InvalidUnary: struct { found: []const u8 },
    ImplicitCast: struct { side: []const u8, type_: []const u8 },

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .FloatEqual => writer.print("floating-point values equality is unsafe", .{}),
            .FloatEqualCast => writer.print("unsafe floating-point values comparison", .{}),
            .InvalidArithmetic => writer.print("invalid arithmetic operation", .{}),
            .InvalidComparison => writer.print("invalid comparison", .{}),
            .InvalidVarDeclType => writer.print("variable delcaration type mismatch", .{}),
            .InvalidUnary => writer.print("invalid unary operation", .{}),
            .ImplicitCast => writer.print("implicit cast", .{}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .FloatEqual => writer.print("both sides are 'floats'", .{}),
            .FloatEqualCast => writer.print("this expression is implicitly casted to 'float'", .{}),
            .InvalidArithmetic => writer.print("expression is not a numeric type", .{}),
            .InvalidComparison => writer.print("expressions have different types", .{}),
            .InvalidUnary => writer.print("expression is not a boolean type", .{}),
            .InvalidVarDeclType => writer.print("expression dosen't match variable type", .{}),
            .ImplicitCast => writer.print("expressions have different types", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .FloatEqual => writer.print(
                \\floating-point values are approximations to infinitly precise real numbers. 
                \\   If you want to compare floats, you should compare against an Epsilon, like
                \\   value < 1e-6  instead of  value == 0
                \\   value - other < 1e-6  instead of  value < other
            ,
                .{},
            ),
            .FloatEqualCast => writer.print(
                \\only values of same type can be compared so expression is implicitly casted to a float.
                \\   This results in an unsafe floating-point comparison. To avoid this, explicitly
                \\   cast the expression to 'float' and compare against an Epsilon (like 1e-5)
            ,
                .{},
            ),
            .InvalidArithmetic => |e| writer.print("expect a numeric type, found '{s}'", .{e.found}),
            .InvalidComparison => |e| writer.print(
                "expressions must have the same type when compared, found '{s}' and '{s}'",
                .{ e.found1, e.found2 },
            ),
            .InvalidUnary => |e| writer.print("can only negate boolean type, found '{s}'", .{e.found}),
            .InvalidVarDeclType => |e| writer.print(
                "variable is declared of type '{s}' but expression is type '{s}'",
                .{ e.expect, e.found },
            ),
            .ImplicitCast => |e| writer.print("explicitly cast {s} to '{s}'", .{ e.side, e.type_ }),
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

    pub fn invalid_arithmetic(found: []const u8) Self {
        return .{ .InvalidArithmetic = .{
            .found = found,
        } };
    }

    pub fn implicit_cast(side: Side, type_: []const u8) Self {
        return .{ .ImplicitCast = .{
            .side = side.str(),
            .type_ = type_,
        } };
    }

    pub fn invalid_cmp(found1: []const u8, found2: []const u8) Self {
        return .{ .InvalidComparison = .{
            .found1 = found1,
            .found2 = found2,
        } };
    }
};
