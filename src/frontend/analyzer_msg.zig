const std = @import("std");
const oom = @import("../utils.zig").oom;

pub const AnalyzerMsg = union(enum) {
    already_declared: struct { name: []const u8 },
    already_declared_field: struct { name: []const u8 },
    already_declared_struct: struct { name: []const u8 },
    already_imported_module: struct { name: []const u8 },
    array_elem_different_type: struct { found1: []const u8, found2: []const u8 },
    assign_to_method,
    assign_type,
    big_self_outside_struct,
    cant_infer_arary_type,
    dead_code,
    default_value_type_mismatch: struct {
        expect: []const u8,
        found: []const u8,
        kind: []const u8,

        pub fn new(expect: []const u8, found: []const u8, kind: enum { field, param }) @This() {
            return .{ .expect = expect, .found = found, .kind = @tagName(kind) };
        }
    },
    dot_type_on_non_mod: struct { found: []const u8 },
    duplicate_param: struct { name: []const u8 },
    float_equal,
    float_equal_cast,
    incompatible_if_type: struct { found1: []const u8, found2: []const u8 },
    incompatible_fn_type: struct { expect: []const u8, found: []const u8 },
    invalid_arithmetic: struct { found: []const u8 },
    invalid_assign_target,
    invalid_call_target,
    invalid_comparison: struct { found1: []const u8, found2: []const u8 },
    invalid_logical: struct { found: []const u8 },
    invalid_unary: struct { found: []const u8 },
    implicit_cast: struct { side: []const u8, type: []const u8 },
    missing_symbol_in_module: struct { symbol: []const u8, module: []const u8 },
    missing_else_clause: struct { if_type: []const u8 },
    missing_field_struct_literal: struct { name: []const u8 },
    missing_file_in_cwd: struct { file: []const u8 },
    missing_file_in_module: struct { file: []const u8, module: []const u8 },
    missing_function_param: struct { name: []const u8 },
    missing_self_method_call: struct { name: []const u8 },
    named_arg_in_bounded,
    non_array_indexing: struct { found: []const u8 },
    non_bool_cond: struct { what: []const u8, found: []const u8 },
    non_integer_index: struct { found: []const u8 },
    non_struct_field_access: struct { found: []const u8 },
    non_struct_struct_literal,
    non_void_while: struct { found: []const u8 },
    no_main,
    return_outside_fn,
    self_outside_struct,
    struct_call_but_no_init,
    too_many_locals,
    too_many_types,
    type_mismatch: struct { expect: []const u8, found: []const u8 },
    undeclared_field_access: struct { name: []const u8 },
    undeclared_type: struct { found: []const u8 },
    undeclared_var: struct { name: []const u8 },
    unknow_char_escape: struct { found: []const u8 },
    unknown_module: struct { name: []const u8 },
    unknown_param: struct { name: []const u8 },
    unknown_struct_field: struct { name: []const u8 },
    unpure_in_global,
    unpure_default: struct {
        kind: []const u8,

        pub fn new(kind: enum { field, param }) @This() {
            return .{ .kind = @tagName(kind) };
        }
    },
    unused_value,
    use_uninit_var: struct { name: []const u8 },
    void_array,
    void_discard,
    void_param,
    void_print,
    void_value,
    too_many_fn_args: struct { expect: []const u8, found: []const u8 },
    wrong_fn_args_count: struct { expect: []const u8, found: []const u8 },

    const Self = @This();

    pub fn getMsg(self: Self, writer: anytype) !void {
        try switch (self) {
            .already_declared => |e| writer.print("identifier '{s}' is already declared in this scope", .{e.name}),
            .already_declared_field => |e| writer.print("a field named '{s}' already exist in structure declaration", .{e.name}),
            .already_declared_struct => |e| writer.print("type '{s}' is already declared", .{e.name}),
            .already_imported_module => |e| writer.print("module '{s}' as already been imported", .{e.name}),
            .array_elem_different_type => |e| writer.print(
                "elements of an array must share the same type, found '{s}' and '{s}'",
                .{ e.found1, e.found2 },
            ),
            .assign_to_method => writer.writeAll("can't assign to structure's methods"),
            .assign_type => writer.writeAll("trying to assign a type"),
            .big_self_outside_struct => writer.writeAll("can't use 'Self' outside a structure"),
            .cant_infer_arary_type => writer.writeAll("can't infer array type with empty array and not declared type"),
            .dead_code => writer.print("unreachable code", .{}),
            .default_value_type_mismatch => |e| writer.print(
                "{s}'s default value doesn't match {s}'s type, expect '{s}' but found '{s}'",
                .{ e.kind, e.kind, e.expect, e.found },
            ),
            .dot_type_on_non_mod => |e| writer.print("can't use a non-module member as a type, found '{s}'", .{e.found}),
            .duplicate_param => |e| writer.print("identifier '{s}' is already used in parameters list", .{e.name}),
            .float_equal => writer.writeAll("floating-point values equality is unsafe"),
            .float_equal_cast => writer.writeAll("unsafe floating-point values comparison"),
            .incompatible_fn_type => |e| writer.print("function declared as returning '{s}' type but found '{s}'", .{ e.expect, e.found }),
            .incompatible_if_type => |e| writer.print("'if' and 'else' have incompatible types, found '{s}'  and '{s}'", .{ e.found1, e.found2 }),
            .invalid_arithmetic => writer.writeAll("invalid arithmetic operation"),
            .invalid_assign_target => writer.writeAll("invalid assignment target"),
            .invalid_call_target => writer.writeAll("invalid call target, can only call functions and methods"),
            .invalid_comparison => writer.writeAll("invalid comparison"),
            .invalid_logical => writer.writeAll("logical operators must be used with booleans"),
            .invalid_unary => writer.writeAll("invalid unary operation"),
            .implicit_cast => writer.writeAll("implicit cast, expressions have different types"),
            .missing_symbol_in_module => |e| writer.print("no symbol named '{s}' in module '{s}'", .{ e.symbol, e.module }),
            .missing_else_clause => writer.writeAll("'if' may be missing in 'else' clause"),
            .missing_field_struct_literal => |e| writer.print("missing field '{s}' in structure literal", .{e.name}),
            .missing_file_in_cwd => |e| writer.print("missing file '{s}' in module current directory", .{e.file}),
            .missing_file_in_module => |e| writer.print("missing file '{s}' in module '{s}'", .{ e.file, e.module }),
            .missing_function_param => |e| writer.print("missing argument '{s}'", .{e.name}),
            .missing_self_method_call => |e| writer.print("method '{s}' is missing 'self' parameter", .{e.name}),
            .named_arg_in_bounded => writer.writeAll("named argument are not allowed with bounded functions"),
            // TODO: when there will be traits and index trait, change description
            .non_array_indexing => |e| writer.print("can only index arrays, found '{s}'", .{e.found}),
            .non_bool_cond => |e| writer.print("non boolean condition, found type '{s}'", .{e.found}),
            .non_integer_index => |e| writer.print("non integer index, found '{s}'", .{e.found}),
            .non_struct_field_access => writer.writeAll("attempting to access a field on a non structure type"),
            .non_struct_struct_literal => writer.writeAll("structure literal syntax is for structure type "),
            .non_void_while => writer.writeAll("'while' statements can't return a value"),
            .no_main => writer.writeAll("no main function found"),
            .return_outside_fn => writer.writeAll("return outside of a function"),
            .self_outside_struct => writer.writeAll("only structure's methods can refer to 'self'"),
            .struct_call_but_no_init => writer.writeAll("calling initializer but none have been declared in structure's definition"),
            .too_many_locals => writer.writeAll("too many local variables, maximum is 255"),
            .too_many_fn_args => |e| writer.print("expect maximum {s} arguments but found {s}", .{ e.expect, e.found }),
            .too_many_types => writer.writeAll("too many types declared, maximum is 268435455"),
            .type_mismatch => |e| writer.print("type mismatch, expect type '{s}' but found '{s}' ", .{ e.expect, e.found }),
            .undeclared_field_access => |e| writer.print("field '{s}' isn't part of structure's definition", .{e.name}),
            .undeclared_type => |e| writer.print("undeclared type '{s}'", .{e.found}),
            .undeclared_var => |e| writer.print("undeclared variable '{s}'", .{e.name}),
            .unknow_char_escape => |e| writer.print("unknow character escape '{s}'", .{e.found}),
            .unknown_module => |e| writer.print("unknown module '{s}'", .{e.name}),
            .unknown_param => |e| writer.print("function doesn't have parameter '{s}'", .{e.name}),
            .unknown_struct_field => |e| writer.print("unknown structure's field '{s}'", .{e.name}),
            .unpure_in_global => writer.writeAll("non-constant expressions are not allowed in global scope"),
            .unpure_default => |e| writer.print("non-constant expressions are not allowed for {s}", .{e.kind}),
            .unused_value => writer.writeAll("unused value"),
            .use_uninit_var => |e| writer.print("variable '{s}' is used uninitialized", .{e.name}),
            .void_array => writer.writeAll("can't declare an array of 'void' values"),
            .void_discard => writer.writeAll("trying to discard a non value"),
            .void_param => writer.writeAll("function parameters can't be of 'void' type"),
            .void_print => writer.writeAll("try to print a 'void' value"),
            .void_value => writer.writeAll("value is of type 'void'"),
            .wrong_fn_args_count => |e| writer.print("wrong argument count, expect {s} but found {s}", .{ e.expect, e.found }),
        };
    }

    pub fn getHint(self: Self, writer: anytype) !void {
        try switch (self) {
            .already_declared, .already_declared_field, .duplicate_param => writer.writeAll("this name"),
            .already_declared_struct => writer.writeAll("this type"),
            .already_imported_module => writer.writeAll("this module"),
            .array_elem_different_type => writer.writeAll("this expression doesn't share previous type"),
            .assign_to_method => writer.writeAll("this field is a method"),
            .assign_type => writer.writeAll("This is a type, not a value"),
            .big_self_outside_struct => writer.writeAll("used outside a structure"),
            .cant_infer_arary_type => writer.writeAll("empty arrays don't convey any type information"),
            .dead_code => writer.writeAll("code after this expression can't be reached"),
            .dot_type_on_non_mod => writer.writeAll("this is not a module"),
            .float_equal => writer.writeAll("both sides are 'floats'"),
            .float_equal_cast => writer.writeAll("this expression is implicitly casted to 'float'"),
            .default_value_type_mismatch => |e| writer.print("this expression is of type '{s}'", .{e.found}),
            .incompatible_fn_type => |e| writer.print("this expression is of type '{s}'", .{e.found}),
            .incompatible_if_type, .unpure_in_global, .unpure_default => writer.writeAll("this expression"),
            .invalid_arithmetic => writer.writeAll("expression is not a numeric type"),
            .invalid_assign_target => writer.writeAll("cannot assign to this expression"),
            .invalid_call_target => writer.writeAll("this is neither a function neither a method"),
            .invalid_comparison => writer.writeAll("expressions have different types"),
            .invalid_logical => |e| writer.print("this expression resolves to a '{s}'", .{e.found}),
            .invalid_unary, .non_bool_cond => writer.writeAll("expression is not a boolean type"),
            .implicit_cast => writer.writeAll("this is implicitly casted"),
            .missing_symbol_in_module => writer.writeAll("this symbol is unknown"),
            .missing_else_clause => |e| writer.print("'if' expression is of type '{s}'", .{e.if_type}),
            .missing_field_struct_literal => writer.writeAll("non-exhaustive structure literal"),
            .missing_file_in_cwd, .missing_file_in_module => writer.writeAll("this file wasn't found"),
            .missing_function_param => writer.writeAll("this call"),
            .missing_self_method_call => writer.writeAll("this method"),
            .named_arg_in_bounded => writer.writeAll("this named argument"),
            .non_array_indexing => writer.writeAll("this is not an index"),
            .non_integer_index => writer.writeAll("this is not an integer"),
            .non_struct_field_access => |e| writer.print("expect a structure but found '{s}'", .{e.found}),
            .non_struct_struct_literal => writer.writeAll("this is not a structure"),
            .non_void_while => |e| writer.print("'while' body produces a value of type '{s}'", .{e.found}),
            .no_main => writer.writeAll("in this file"),
            .return_outside_fn, .self_outside_struct => writer.writeAll("here"),
            .struct_call_but_no_init => writer.writeAll("this expression calls 'init' function"),
            .too_many_locals, .too_many_types => writer.writeAll("this is the exceding one"),
            .too_many_fn_args => writer.writeAll("this call"),
            .type_mismatch => |e| writer.print("this expression is of type '{s}'", .{e.found}),
            .undeclared_field_access, .undeclared_type, .undeclared_var, .use_uninit_var => writer.writeAll("here"),
            .unknow_char_escape => writer.writeAll("unknown character to espace"),
            .unknown_module => writer.writeAll("this name"),
            .unknown_param => writer.writeAll("this parameter"),
            .unknown_struct_field => writer.writeAll("this name"),
            .unused_value => writer.writeAll("this expression produces a value"),
            .void_array => writer.writeAll("declared here"),
            .void_discard => writer.writeAll("this expression produces no value"),
            .void_param => writer.writeAll("this parameter"),
            .void_print => writer.writeAll("this expression is of type 'void'"),
            .void_value => writer.writeAll("this expression produces no value"),
            .wrong_fn_args_count => writer.writeAll("this call is invalid"),
        };
    }

    pub fn getHelp(self: Self, writer: anytype) !void {
        try switch (self) {
            .already_declared,
            .already_declared_field,
            .duplicate_param,
            => writer.writeAll("use another name or introduce numbers, underscore, ..."),
            .already_declared_struct => writer.writeAll("use another name"),
            .already_imported_module => writer.writeAll("remove the import"),
            .array_elem_different_type => writer.writeAll("modify array declaration values or use another construct"),
            .assign_to_method => writer.writeAll("for safety reasons, it is not allowed to modify structures' methods at runtime"),
            // TODO: implement type alias
            .assign_type => writer.writeAll("types aren't assignable to variables. To make type aliases, use 'type <Alias> = ..."),
            .big_self_outside_struct => writer.writeAll("'Self' can only be used in structure to refer to the current structure's type"),
            .cant_infer_arary_type => writer.writeAll(
                \\can't extract any type information from an empty array '[]'. you must either declare a type in variable's
                \\signature like: 'var arr: []int = []' or initialize the array with at least one value (not possible every time).
                \\Also, doing 'var arr: []int = []' is equivalent to 'var arr: []int'.
            ),
            .default_value_type_mismatch => |e| writer.print(
                "modify {s}'s default value to match '{s}' type or change {s}'s type",
                .{ e.kind, e.expect, e.kind },
            ),
            .dead_code => writer.writeAll("remove unreachable code"),
            .dot_type_on_non_mod => writer.writeAll("check variable declaration to see it's type"),
            .void_discard => writer.writeAll("remove the discard"),
            .float_equal => writer.writeAll(
                \\floating-point values are approximations to infinitly precise real numbers. 
                \\   If you want to compare floats, you should compare against an Epsilon, like
                \\   value < 1e-6  instead of  value == 0
                \\   value - other < 1e-6  instead of  value < other
                ,
            ),
            .float_equal_cast => writer.writeAll(
                \\only values of same type can be compared so expression is implicitly casted to a float.
                \\   This results in an unsafe floating-point comparison. To avoid this, explicitly
                \\   cast the expression to 'float' and compare against an Epsilon (like 1e-5)
                ,
            ),
            .incompatible_fn_type => |e| writer.print(
                "modify function's body to match '{s}' type or change function's definition",
                .{e.expect},
            ),
            .incompatible_if_type => writer.writeAll("make both paths return the same type"),
            .invalid_arithmetic => |e| writer.print("expect a numeric type, found '{s}'", .{e.found}),
            .invalid_assign_target => writer.writeAll("can only assign to variables"),
            .invalid_call_target => writer.writeAll("change call target to a function or a method or remove the call"),
            .invalid_comparison => |e| writer.print(
                "expressions must have the same type when compared, found '{s}' and '{s}'",
                .{ e.found1, e.found2 },
            ),
            .invalid_logical => writer.writeAll("modify the logic to operate on booleans"),
            .invalid_unary => |e| writer.print("can only negate boolean type, found '{s}'", .{e.found}),
            .implicit_cast => |e| writer.print("explicitly cast {s} to '{s}'", .{ e.side, e.type }),
            .missing_symbol_in_module => writer.writeAll("refer to module's source code or documentation to ge the list of available symbols"),
            .missing_else_clause => writer.writeAll("add an 'else' block that evaluate to the expected type"),
            .missing_field_struct_literal => writer.writeAll(
                "structure literal expressions must provide an expression for all the fields that don't have a default value",
            ),
            .missing_file_in_cwd, .missing_file_in_module => writer.writeAll("check if file is in the module or if there is a typo"),
            .missing_function_param => writer.writeAll("all non-default-value parameters must be given a value"),
            .missing_self_method_call => writer.writeAll("methods can be invoked on instances only if it defines 'self' as first parameter"),
            .named_arg_in_bounded => writer.writeAll(
                \\when bouding a function/method to a variable, you loose default values informations as the bounded function 
                \\could depend on runtime logic.
            ),
            .non_array_indexing => writer.writeAll("refer to variable's definition"),
            // TODO: when there will be Range, modify
            .non_integer_index => writer.writeAll("can only use integer values to index arrays"),
            .non_bool_cond => |e| writer.print("'{s}' conditions can only be boolean type", .{e.what}),
            .non_struct_field_access => writer.writeAll("refer to variable's definition to know its type"),
            .non_struct_struct_literal => writer.writeAll("refer to type's definition"),
            .non_void_while => writer.writeAll("use '_' to ignore the value or modify the body"),
            .no_main => writer.writeAll("add a 'main' function that will be called automatically at execution"),
            .return_outside_fn => writer.writeAll(
                "return statements are only allow to exit a function's body. " ++
                    "In loops, use 'break' otherwise remove the return",
            ),
            .self_outside_struct => writer.writeAll("'self' is a reserved keyword. Use another parameter name"),
            .struct_call_but_no_init => writer.writeAll("define an 'init' function or use structure initialization syntax: 'Foo {...}"),
            .too_many_locals => writer.writeAll("it's a compiler's limitation for now. Try changing your code"),
            .too_many_fn_args => writer.writeAll("refer to the function's definition to correct the call"),
            .too_many_types => writer.writeAll("it's a compiler limitation but the code shouldn't anyway have that much types. Try rethink you code"),
            .type_mismatch => writer.writeAll("refer to variable's definition to assign the correct type"),
            .undeclared_field_access => writer.writeAll("refer to structure's definition to see available fields or modify it"),
            .undeclared_type => writer.writeAll("consider declaring or importing the type before use"),
            .undeclared_var => writer.writeAll("consider declaring or importing the variable before use"),
            .unknow_char_escape => writer.writeAll("valid escape characters are '\\t', '\\n', '\\\\', '\\r'"),
            .unknown_module => writer.writeAll("create the module first and bring it in project scope (or maybe just a typo?)"),
            .unknown_param => writer.writeAll("refer to function's definition to see available parameters"),
            .unknown_struct_field => writer.writeAll("refer to the structure's declaration to see available fields"),
            .unpure_default => writer.writeAll("only constant expressions are allowed for default values"),
            .unpure_in_global => writer.writeAll("use a constant expression or initialize the value later in a local scope"),
            .use_uninit_var => writer.writeAll("consider initializing the variable before use"),
            .unused_value => writer.writeAll("use '_' to ignore the value: _ = 1 + 2"),
            .void_array => writer.writeAll("use any other type to declare an array"),
            .void_param => writer.writeAll("use a any other type than 'void' or remove parameter"),
            .void_print => writer.writeAll("use a any other expression's type than 'void'"),
            .void_value => writer.writeAll("consider returning a value from expression"),
            .wrong_fn_args_count => writer.writeAll("refer to function's definition to see expected arguments"),
        };
    }

    pub fn invalidArithmetic(found: []const u8) Self {
        return .{ .invalid_arithmetic = .{ .found = found } };
    }

    pub fn implicitCast(side: []const u8, typ: []const u8) Self {
        return .{ .implicit_cast = .{ .side = side, .type = typ } };
    }

    pub fn invalidCmp(found1: []const u8, found2: []const u8) Self {
        return .{ .invalid_comparison = .{ .found1 = found1, .found2 = found2 } };
    }

    // TODO: No other way to take ownership of string??
    pub fn tooManyFnArgs(expect: usize, found: usize) Self {
        var list = std.ArrayList(u8).init(std.heap.page_allocator);
        const writer = list.writer();
        writer.print("{}", .{expect}) catch oom();

        var list1 = std.ArrayList(u8).init(std.heap.page_allocator);
        const writer1 = list1.writer();
        writer1.print("{}", .{found}) catch oom();

        const tmp: AnalyzerMsg = .{ .too_many_fn_args = .{
            .expect = list.toOwnedSlice() catch oom(),
            .found = list1.toOwnedSlice() catch oom(),
        } };

        return tmp;
    }
    pub fn wrongFnArgsCount(expect: usize, found: usize) Self {
        var list = std.ArrayList(u8).init(std.heap.page_allocator);
        const writer = list.writer();
        writer.print("{}", .{expect}) catch oom();

        var list1 = std.ArrayList(u8).init(std.heap.page_allocator);
        const writer1 = list1.writer();
        writer1.print("{}", .{found}) catch oom();

        const tmp: AnalyzerMsg = .{ .wrong_fn_args_count = .{
            .expect = list.toOwnedSlice() catch oom(),
            .found = list1.toOwnedSlice() catch oom(),
        } };

        return tmp;
    }
};
