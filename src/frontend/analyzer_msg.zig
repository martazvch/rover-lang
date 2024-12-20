pub const AnalyzerMsg = union(enum) {
    AlreadyDeclaredVar: struct { name: []const u8 },
    FloatEqual,
    FloatEqualCast,
    InvalidArithmetic: struct { found: []const u8 },
    InvalidAssignTarget,
    InvalidComparison: struct { found1: []const u8, found2: []const u8 },
    InvalidAssignType: struct { expect: []const u8, found: []const u8 },
    InvalidUnary: struct { found: []const u8 },
    ImplicitCast: struct { side: []const u8, type_: []const u8 },
    UndeclaredType: struct { found: []const u8 },
    UndeclaredVar: struct { name: []const u8 },
    UnusedValue,
    UseUninitVar: struct { name: []const u8 },

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .AlreadyDeclaredVar => |e| writer.print("variable '{s}' already declared in this scope", .{e.name}),
            .FloatEqual => writer.print("floating-point values equality is unsafe", .{}),
            .FloatEqualCast => writer.print("unsafe floating-point values comparison", .{}),
            .InvalidArithmetic => writer.print("invalid arithmetic operation", .{}),
            .InvalidAssignTarget => writer.print("invalid assignment target", .{}),
            .InvalidComparison => writer.print("invalid comparison", .{}),
            .InvalidAssignType => writer.print("variable declaration type mismatch", .{}),
            .InvalidUnary => writer.print("invalid unary operation", .{}),
            .ImplicitCast => writer.print("implicit cast", .{}),
            .UndeclaredType => |e| writer.print("undeclared type '{s}'", .{e.found}),
            .UndeclaredVar => |e| writer.print("undeclared variable '{s}'", .{e.name}),
            .UnusedValue => writer.print("unused value", .{}),
            .UseUninitVar => |e| writer.print("variable '{s}' is used uninitialized", .{e.name}),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .AlreadyDeclaredVar => writer.print("this name", .{}),
            .FloatEqual => writer.print("both sides are 'floats'", .{}),
            .FloatEqualCast => writer.print("this expression is implicitly casted to 'float'", .{}),
            .InvalidArithmetic => writer.print("expression is not a numeric type", .{}),
            .InvalidAssignTarget => writer.print("cannot assign to this expression", .{}),
            .InvalidComparison => writer.print("expressions have different types", .{}),
            .InvalidUnary => writer.print("expression is not a boolean type", .{}),
            .InvalidAssignType => writer.print("expression dosen't match variable type", .{}),
            .ImplicitCast => writer.print("expressions have different types", .{}),
            .UndeclaredType, .UndeclaredVar, .UseUninitVar => writer.print("here", .{}),
            .UnusedValue => writer.print("this expression produces a value", .{}),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .AlreadyDeclaredVar => writer.print("use another name or use numbers, underscore", .{}),
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
            .InvalidAssignTarget => writer.print("can only assign to variables", .{}),
            .InvalidComparison => |e| writer.print(
                "expressions must have the same type when compared, found '{s}' and '{s}'",
                .{ e.found1, e.found2 },
            ),
            .InvalidUnary => |e| writer.print("can only negate boolean type, found '{s}'", .{e.found}),
            .InvalidAssignType => |e| writer.print(
                "variable is declared of type '{s}' but expression is type '{s}'",
                .{ e.expect, e.found },
            ),
            .ImplicitCast => |e| writer.print("explicitly cast {s} to '{s}'", .{ e.side, e.type_ }),
            .UndeclaredType => writer.print("consider declaring or importing the type before use", .{}),
            .UndeclaredVar => writer.print("consider declaring or importing the variable before use", .{}),
            .UseUninitVar => writer.print("consider initializing the variable before use", .{}),
            .UnusedValue => writer.print("use '_' to ignore the value: _ = 1 + 2", .{}),
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
