const std = @import("std");

pub const AnalyzerMsg = union(enum) {
    AlreadyDeclared: struct { name: []const u8 },
    AlreadyDeclaredStruct: struct { name: []const u8 },
    DeadCode,
    DuplicateParam: struct { name: []const u8 },
    FloatEqual,
    FloatEqualCast,
    IncompatibleIfType: struct { found1: []const u8, found2: []const u8 },
    IncompatibleFnType: struct { expect: []const u8, found: []const u8 },
    InvalidArithmetic: struct { found: []const u8 },
    InvalidAssignTarget,
    InvalidAssignType: struct { expect: []const u8, found: []const u8 },
    InvalidCallTarget,
    InvalidComparison: struct { found1: []const u8, found2: []const u8 },
    InvalidLogical: struct { found: []const u8 },
    InvalidUnary: struct { found: []const u8 },
    ImplicitCast: struct { side: []const u8, type_: []const u8 },
    MissingElseClause: struct { if_type: []const u8 },
    NonBoolCond: struct { what: []const u8, found: []const u8 },
    NonSelfInitReturn,
    NonVoidWhile: struct { found: []const u8 },
    NoMain,
    ReturnOutsideFn,
    SelfInInit,
    SelfOutsideStruct,
    StructCallButNoInit,
    TooManyLocals,
    TooManyTypes,
    TypeMismatch: struct { expect: []const u8, found: []const u8 },
    UndeclaredType: struct { found: []const u8 },
    UndeclaredVar: struct { name: []const u8 },
    UnknownModule: struct { name: []const u8 },
    UnpureInGlobal,
    UnusedValue,
    UseUninitVar: struct { name: []const u8 },
    VoidAssignment,
    VoidDiscard,
    VoidParam,
    VoidPrint,
    WrongFnArgsCount: struct { expect: []const u8, found: []const u8 },

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .AlreadyDeclared => |e| writer.print("identifier '{s}' is already declared in this scope", .{e.name}),
            .AlreadyDeclaredStruct => |e| writer.print("type '{s}' is already declared", .{e.name}),
            .DeadCode => writer.print("unreachable code", .{}),
            .DuplicateParam => |e| writer.print("identifier '{s}' is already used in parameters list", .{e.name}),
            .FloatEqual => writer.print("floating-point values equality is unsafe", .{}),
            .FloatEqualCast => writer.print("unsafe floating-point values comparison", .{}),
            .IncompatibleFnType => |e| writer.print(
                "function declared as returning '{s}' type but found '{s}'",
                .{ e.expect, e.found },
            ),
            .IncompatibleIfType => |e| writer.print(
                "'if' and 'else' have incompatible types, found '{s}'  and '{s}'",
                .{ e.found1, e.found2 },
            ),
            .InvalidArithmetic => writer.print("invalid arithmetic operation", .{}),
            .InvalidAssignTarget => writer.print("invalid assignment target", .{}),
            .InvalidAssignType => writer.print("variable declaration type mismatch", .{}),
            .InvalidCallTarget => writer.writeAll("invalid call target, can only call functions and methods"),
            .InvalidComparison => writer.print("invalid comparison", .{}),
            .InvalidLogical => writer.print("logical operators must be used with booleans", .{}),
            .InvalidUnary => writer.print("invalid unary operation", .{}),
            .ImplicitCast => writer.print("implicit cast, expressions have different types", .{}),
            .NonBoolCond => |e| writer.print("non boolean condition, found type '{s}'", .{e.found}),
            .NonSelfInitReturn => writer.writeAll("initializer must return 'Self'"),
            .NonVoidWhile => writer.print("'while' statements can't return a value", .{}),
            .NoMain => writer.print("no main function found", .{}),
            .MissingElseClause => writer.print("'if' may be missing in 'else' clause", .{}),
            .ReturnOutsideFn => writer.print("return outside of a function", .{}),
            .SelfInInit => writer.writeAll("initialization function 'init' can't take 'self' as parameter"),
            .SelfOutsideStruct => writer.writeAll("only structure's methods can refer to 'self'"),
            .StructCallButNoInit => writer.writeAll("calling initializer but none have been declared in structure's definition"),
            .TooManyLocals => writer.print("too many local variables, maximum is 255", .{}),
            .TooManyTypes => writer.print("too many types declared, maximum is 268435455", .{}),
            .TypeMismatch => |e| writer.print(
                "type mismatch, expect a '{s}' but found '{s}' ",
                .{ e.expect, e.found },
            ),
            .UndeclaredType => |e| writer.print("undeclared type '{s}'", .{e.found}),
            .UndeclaredVar => |e| writer.print("undeclared variable '{s}'", .{e.name}),
            .UnknownModule => |e| writer.print("unknown module '{s}'", .{e.name}),
            .UnpureInGlobal => writer.print("non-constant expressions are not allowed in global scope", .{}),
            .UnusedValue => writer.print("unused value", .{}),
            .UseUninitVar => |e| writer.print("variable '{s}' is used uninitialized", .{e.name}),
            .VoidAssignment => writer.print("assigned value is of type 'void'", .{}),
            .VoidDiscard => writer.print("trying to discard a non value", .{}),
            .VoidParam => writer.print("function parameters can't be of 'void' type", .{}),
            .VoidPrint => writer.writeAll("try to print a 'void' value"),
            .WrongFnArgsCount => |e| writer.print(
                "expect {s} function arguments but found {s}",
                .{ e.expect, e.found },
            ),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .AlreadyDeclared, .DuplicateParam => writer.writeAll("this name"),
            .AlreadyDeclaredStruct => writer.writeAll("this type"),
            .DeadCode => writer.writeAll("code after this expression can't be reached"),
            .FloatEqual => writer.writeAll("both sides are 'floats'"),
            .FloatEqualCast => writer.writeAll("this expression is implicitly casted to 'float'"),
            .IncompatibleFnType => |e| writer.print("this expression is of type '{s}'", .{e.found}),
            .IncompatibleIfType, .UnpureInGlobal => writer.writeAll("this expression"),
            .InvalidArithmetic => writer.writeAll("expression is not a numeric type"),
            .InvalidAssignTarget => writer.writeAll("cannot assign to this expression"),
            .InvalidCallTarget => writer.writeAll("this is neither a function neither a mathod"),
            .InvalidComparison => writer.writeAll("expressions have different types"),
            .InvalidLogical => |e| writer.print("this expression resolves to a '{s}'", .{e.found}),
            .InvalidUnary,
            .NonBoolCond,
            => writer.writeAll("expression is not a boolean type"),
            .InvalidAssignType => writer.writeAll("expression doesn't match variable's type"),
            .ImplicitCast => writer.writeAll("this is implicitly casted"),
            .NonSelfInitReturn => writer.writeAll("wrong type"),
            .NonVoidWhile => |e| writer.print("'while' body produces a value of type '{s}'", .{e.found}),
            .NoMain => writer.writeAll("in this file"),
            .MissingElseClause => |e| writer.print("'if' expression is of type '{s}'", .{e.if_type}),
            .ReturnOutsideFn, .SelfInInit, .SelfOutsideStruct => writer.writeAll("here"),
            .StructCallButNoInit => writer.writeAll("this expression calls 'init' function"),
            .TooManyLocals, .TooManyTypes => writer.writeAll("this is the exceding one"),
            .TypeMismatch => |e| writer.print("this expression is a '{s}'", .{e.found}),
            .UndeclaredType, .UndeclaredVar, .UseUninitVar => writer.writeAll("here"),
            .UnknownModule => writer.writeAll("this name"),
            .UnusedValue => writer.writeAll("this expression produces a value"),
            .VoidAssignment => writer.writeAll("this expression produces no value"),
            .VoidDiscard => writer.writeAll("this expression produces no value"),
            .VoidParam => writer.writeAll("this parameter"),
            .VoidPrint => writer.writeAll("this expression is of type 'void'"),
            .WrongFnArgsCount => writer.writeAll("this call"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .AlreadyDeclared,
            .DuplicateParam,
            => writer.writeAll("use another name or introduce numbers, underscore, ..."),
            .AlreadyDeclaredStruct => writer.writeAll("use another name"),
            .DeadCode => writer.writeAll("remove unreachable code"),
            .VoidDiscard => writer.writeAll("remove the discard"),
            .FloatEqual => writer.writeAll(
                \\floating-point values are approximations to infinitly precise real numbers. 
                \\   If you want to compare floats, you should compare against an Epsilon, like
                \\   value < 1e-6  instead of  value == 0
                \\   value - other < 1e-6  instead of  value < other
                ,
            ),
            .FloatEqualCast => writer.writeAll(
                \\only values of same type can be compared so expression is implicitly casted to a float.
                \\   This results in an unsafe floating-point comparison. To avoid this, explicitly
                \\   cast the expression to 'float' and compare against an Epsilon (like 1e-5)
                ,
            ),
            .IncompatibleFnType => |e| writer.print(
                "modify function's body to match '{s}' type or change function's definition",
                .{e.expect},
            ),
            .IncompatibleIfType => writer.writeAll("make both paths return the same type"),
            .InvalidArithmetic => |e| writer.print("expect a numeric type, found '{s}'", .{e.found}),
            .InvalidAssignTarget => writer.writeAll("can only assign to variables"),
            .InvalidCallTarget => writer.writeAll("change call target to a function or a method or remove the call"),
            .InvalidComparison => |e| writer.print(
                "expressions must have the same type when compared, found '{s}' and '{s}'",
                .{ e.found1, e.found2 },
            ),
            .InvalidLogical => writer.writeAll("modify the logic to operate on booleans"),
            .InvalidUnary => |e| writer.print("can only negate boolean type, found '{s}'", .{e.found}),
            .InvalidAssignType => |e| writer.print(
                "variable is declared of type '{s}' but expression is of type '{s}'",
                .{ e.expect, e.found },
            ),
            .ImplicitCast => |e| writer.print("explicitly cast {s} to '{s}'", .{ e.side, e.type_ }),
            .MissingElseClause => writer.writeAll("add an 'else' block that evaluate to the expected type"),
            .NonBoolCond => |e| writer.print("'{s}' conditions can only be boolean type", .{e.what}),
            .NonSelfInitReturn => writer.writeAll("replace the return type with 'Self'"),
            .NonVoidWhile => writer.writeAll("use '_' to ignore the value or modify the body"),
            .NoMain => writer.writeAll("add a 'main' function that will be called automatically at execution"),
            .ReturnOutsideFn => writer.writeAll(
                "return statements are only allow to exit a function's body. " ++
                    "In loops, use 'break' otherwise remove the return",
            ),
            .SelfInInit => writer.writeAll("remove the parameter"),
            .SelfOutsideStruct => writer.writeAll("'self' is a reserved keyword. Use another paramter name"),
            .StructCallButNoInit => writer.writeAll("define an 'init' function or use structure initialization syntax: 'Foo {...}"),
            .TooManyLocals => writer.writeAll("it's a compiler's limitation for now. Try changing your code"),
            .TooManyTypes => writer.writeAll(
                "it's a compiler limitation but the code shouldn't anyway have that much types. Try rethink you code",
            ),
            .TypeMismatch => writer.writeAll("change the type to match expected one"),
            .UndeclaredType => writer.writeAll("consider declaring or importing the type before use"),
            .UndeclaredVar => writer.writeAll("consider declaring or importing the variable before use"),
            .UnknownModule => writer.writeAll(
                "create the module first and bring it in project scope (or maybe just a typo?)",
            ),
            .UnpureInGlobal => writer.writeAll(
                "use a constant expression or initialize the value later in a local scope",
            ),
            .UseUninitVar => writer.writeAll("consider initializing the variable before use"),
            .UnusedValue => writer.writeAll("use '_' to ignore the value: _ = 1 + 2"),
            .VoidAssignment => writer.writeAll("consider returning a value from expression or remove assignment"),
            .VoidParam => writer.writeAll("use a any other type than 'void' or remove parameter"),
            .VoidPrint => writer.writeAll("use a any other expression's type than 'void'"),
            .WrongFnArgsCount => writer.writeAll("refer to the function's definition to correct the call"),
        };
    }

    pub fn invalid_arithmetic(found: []const u8) Self {
        return .{ .InvalidArithmetic = .{
            .found = found,
        } };
    }

    pub fn implicit_cast(side: []const u8, type_: []const u8) Self {
        return .{ .ImplicitCast = .{
            .side = side,
            .type_ = type_,
        } };
    }

    pub fn invalid_cmp(found1: []const u8, found2: []const u8) Self {
        return .{ .InvalidComparison = .{
            .found1 = found1,
            .found2 = found2,
        } };
    }

    // TODO: No other way to take ownership of string??
    pub fn wrong_args_count(expect: usize, found: usize) !Self {
        var list = std.ArrayList(u8).init(std.heap.page_allocator);
        const writer = list.writer();
        try writer.print("{}", .{expect});

        var list1 = std.ArrayList(u8).init(std.heap.page_allocator);
        const writer1 = list1.writer();
        try writer1.print("{}", .{found});

        const tmp: AnalyzerMsg = .{ .WrongFnArgsCount = .{
            .expect = try list.toOwnedSlice(),
            .found = try list1.toOwnedSlice(),
        } };

        return tmp;
    }
};
