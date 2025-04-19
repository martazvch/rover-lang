pub const ParserMsg = union(enum) {
    chaining_cmp_op,
    expect_arrow_before_fn_type,
    expect_brace_before_fn_body,
    expect_brace_after_struct_body,
    expect_brace_after_struct_lit,
    expect_brace_before_struct_body,
    ExpectBraceOrDo: struct { what: []const u8 },
    expect_colon_before_type,
    ExpectExpr: struct { found: []const u8 },
    expect_field_name,
    expect_fn_name,
    expect_fn_in_struct_body,
    ExpectName: struct { kind: []const u8 },
    expect_name_after_dot,
    expect_newl_ine,
    expect_paren_after_fn_args,
    expect_paren_after_fn_name,
    expect_paren_after_fn_params,
    expect_struct_name,
    expect_type_name,
    expect_field_type_or_default,
    invalid_discard,
    missing_comma_after_field,
    missing_fn_param_type,
    self_as_non_first_param,
    struct_lit_non_ident_field,
    too_many_fn_args: struct { what: []const u8 },
    typed_self,
    unclosed_brace,
    unclosed_paren,
    WrongValueCountVarDecl: struct { expect: usize },

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype) !void {
        try switch (self) {
            .chaining_cmp_op => writer.print("chaining comparison operators", .{}),
            .expect_arrow_before_fn_type => writer.print("expect arrow '->' before function type", .{}),
            .ExpectBraceOrDo => |e| writer.print("expect opening brace or 'do' after '{s}' condition", .{e.what}),
            .expect_brace_before_fn_body => writer.print("expect opening brace before function's body", .{}),
            .expect_brace_after_struct_body => writer.print("expect opening brace after structure's body", .{}),
            .expect_brace_after_struct_lit => writer.print("expect opening brace after structure's name in structure initialization", .{}),
            .expect_brace_before_struct_body => writer.print("expect opening brace before structure's body", .{}),
            .expect_colon_before_type => writer.print("invalid variable type declaration", .{}),
            .ExpectExpr => |e| writer.print("expected expression, found \"{s}\"", .{e.found}),
            .expect_field_name => writer.writeAll("expect structure field name"),
            .expect_fn_name => writer.print("expect function name after 'fn' keyword", .{}),
            .expect_fn_in_struct_body => writer.writeAll("expect functions declaration or nothing after structure's fields"),
            .ExpectName => |e| writer.print("expect {s} name ", .{e.kind}),
            .expect_name_after_dot => writer.writeAll("expect field name after dot"),
            .expect_newl_ine => writer.print("expect new line after statement", .{}),
            .expect_paren_after_fn_args => writer.print("expect closing parenthesis ')' after function's arguments", .{}),
            .expect_paren_after_fn_name => writer.print("expect opening parenthesis '(' after function's name", .{}),
            .expect_paren_after_fn_params => writer.print("expect closing parenthesis ')' after function's parameters", .{}),
            .expect_struct_name => writer.print("expect structure name", .{}),
            .expect_type_name => writer.print("expect type name", .{}),
            .expect_field_type_or_default => writer.writeAll("structure fileds must be typed or have a default value"),
            .invalid_discard => writer.print("invalid discard expression", .{}),
            .missing_comma_after_field => writer.writeAll("comma might be missing between fields"),
            .missing_fn_param_type => writer.print("missing function's parameter's type", .{}),
            .self_as_non_first_param => writer.writeAll("'self' parameter not in first position"),
            .struct_lit_non_ident_field => writer.writeAll("trying to assign to a non-field in structure literal"),
            .too_many_fn_args => |e| writer.print("functions can't have more than 255 {s}", .{e.what}),
            .typed_self => writer.writeAll("can't specify a type for 'self', it's a keyword whose type is known by the compiler"),
            .unclosed_brace => writer.print("unclosed brace", .{}),
            .unclosed_paren => writer.print("unclosed parenthesis", .{}),
            .WrongValueCountVarDecl => |e| writer.print(
                "value count mismatch variable count, expect {} values",
                .{e.expect},
            ),
        };
    }

    pub fn get_hint(self: Self, writer: anytype) !void {
        try switch (self) {
            .chaining_cmp_op => writer.print("this one is not allowed", .{}),
            .expect_arrow_before_fn_type,
            .expect_brace_before_fn_body,
            .expect_brace_after_struct_body,
            .expect_brace_after_struct_lit,
            .expect_brace_before_struct_body,
            .ExpectBraceOrDo,
            .expect_name_after_dot,
            .expect_paren_after_fn_args,
            .expect_paren_after_fn_name,
            .expect_paren_after_fn_params,
            .expect_struct_name,
            .missing_comma_after_field,
            .missing_fn_param_type,
            => writer.print("expect to be here", .{}),
            .expect_colon_before_type => writer.print("before this identifier", .{}),
            .expect_field_name => writer.writeAll("this is not an identifier"),
            .expect_fn_in_struct_body => writer.writeAll("this is unexpected"),
            .ExpectExpr, .expect_newl_ine => writer.print("here", .{}),
            .expect_fn_name, .expect_type_name, .ExpectName => writer.print("this is not an identifier", .{}),
            .expect_field_type_or_default => writer.writeAll("this field has no type and not default value"),
            .invalid_discard => writer.print("expect an assignment to '_'", .{}),
            .self_as_non_first_param => writer.writeAll("here"),
            .struct_lit_non_ident_field => writer.writeAll("this field"),
            .typed_self => writer.writeAll("this type"),
            .too_many_fn_args => |e| writer.print("this is the 256th {s}", .{e.what}),
            .unclosed_brace => writer.print("this opening brace", .{}),
            .unclosed_paren => writer.print("this opening parenthesis", .{}),
            .WrongValueCountVarDecl => writer.writeAll("from this variable"),
        };
    }

    pub fn get_help(self: Self, writer: anytype) !void {
        try switch (self) {
            .chaining_cmp_op => writer.print("split your comparison with 'and' and 'or' operators", .{}),
            .expect_arrow_before_fn_type => writer.print("add an arrow '->' between function's arguments list and type", .{}),
            .expect_brace_before_fn_body, .expect_brace_before_struct_body => writer.print("add an opening brace '{{'", .{}),
            .expect_brace_after_struct_body => writer.writeAll("add an closing brace '}}'"),
            .expect_brace_after_struct_lit => writer.writeAll("add an openning brace '{{'"),
            .ExpectBraceOrDo => writer.print("add an opening brace '{{' or 'do' keyword before statement", .{}),
            .expect_paren_after_fn_args => writer.print("add an closing parenthesis ')' after function call", .{}),
            .expect_paren_after_fn_name => writer.print(
                "add an opening parenthesis '(' between function's name and arguments list",
                .{},
            ),
            .expect_paren_after_fn_params => writer.print(
                "add an closing parenthesis ')' between function's parameters and return type",
                .{},
            ),
            .expect_colon_before_type => writer.print("add ':' bofre type name", .{}),
            .expect_field_name => writer.writeAll("structures fields must be declared before constants and methods"),
            .expect_fn_in_struct_body => writer.writeAll("structures must declare their fields first followed by their methods, nothing more"),
            .expect_fn_name, .expect_type_name, .ExpectName, .expect_struct_name => writer.print("define an identifier", .{}),
            .expect_name_after_dot => writer.writeAll("field access syntax is: 'Structure.field'"),
            .expect_field_type_or_default => writer.writeAll("add a type to the field like: 'field: type' or add a default value like: 'field = value'"),
            .invalid_discard => writer.print("add '=' token: _ = call()", .{}),
            .missing_comma_after_field => writer.writeAll("structure fields have ot be separated by commas"),
            .missing_fn_param_type => writer.print("function's paramters' types are mandatory, add it after ':'", .{}),
            .self_as_non_first_param => writer.writeAll("'self' parameter must always be in first place. Change function's declaration"),
            .struct_lit_non_ident_field => writer.writeAll("structure literal must define a value for all fields that doesn't have a default one"),
            .too_many_fn_args => |e| writer.print(
                "split your function into multiple small ones or pass your {s} in structures / arrays",
                .{e.what},
            ),
            .typed_self => writer.writeAll("remove the type, the compiler infers the type for 'self' on its own"),
            .unclosed_brace => writer.print("close the opening brace", .{}),
            .unclosed_paren => writer.print("close the opening parenthesis", .{}),
            .WrongValueCountVarDecl => writer.writeAll(
                "you must either provide no value, 1 value that will assigned to each variable or one per variable",
            ),
            else => {},
        };
    }
};
