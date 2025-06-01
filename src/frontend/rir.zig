pub const Scope = enum(u2) { builtin, global, heap, local };
pub const Type = enum(u2) { float, int };
pub const ReturnKind = enum(u2) { explicit, implicit_value, implicit_void };

pub const Instruction = struct {
    data: Data,
    offset: usize = 0,

    pub const Data = union(enum) {
        array: usize,
        assignment: Assignment,
        binop: Binop,
        block: Block,
        bool: bool,
        call: Call,
        cast: Type,
        discard: void,
        float: f64,
        fn_decl: FnDecl,
        identifier: Variable,
        identifier_absolute: usize,
        identifier_id: usize,
        @"if": If,
        // TODO: delete later
        imported: Imported,
        int: i64,
        item_import: ItemImport,
        member: Member,
        module_import: ModuleImport,
        multiple_var_decl: usize,
        name: usize,
        null: void,
        print: void,
        @"return": Return,
        string: usize,
        struct_decl: StructDecl,
        default_value: usize,
        struct_literal: usize,
        unary: Unary,
        use: u64,
        value: Value,
        var_decl: VarDecl,
        @"while": void,
    };

    pub const Binop = struct {
        cast: Cast = .none,
        op: Op,

        pub const Cast = enum(u2) { lhs, rhs, none };
        const Op = enum(u6) {
            add_float,
            add_int,
            add_str,
            @"and",
            div_float,
            div_int,
            eq_bool,
            eq_float,
            eq_int,
            eq_str,
            ge_float,
            ge_int,
            gt_float,
            gt_int,
            le_float,
            le_int,
            lt_float,
            lt_int,
            mul_float,
            mul_int,
            mul_str,
            ne_bool,
            ne_float,
            ne_int,
            ne_str,
            @"or",
            sub_float,
            sub_int,
        };
    };

    pub const Assignment = struct { cast: bool };
    pub const Block = struct { length: usize, pop_count: u8, is_expr: bool };
    pub const Call = struct {
        arity: u8,
        tag: CallTag = .function,

        pub const CallTag = enum { bound, builtin, function, import, invoke, invoke_import, invoke_static };
    };
    pub const FnDecl = struct { body_len: u64, default_params: usize, return_kind: ReturnKind };
    pub const If = struct {
        cast: Cast,
        has_else: bool,

        pub const Cast = enum(u2) { then, @"else", none };
    };
    pub const Imported = struct { index: u64, variable: Variable };
    pub const ItemImport = struct { module_index: usize, field_index: usize, scope: Scope };
    pub const Member = struct {
        index: usize,
        kind: Kind,

        pub const Kind = enum { method, field, static_method, symbol };
    };
    pub const ModuleImport = struct { index: usize, scope: Scope };
    pub const Return = struct { value: bool, cast: bool };
    pub const StructDecl = struct { fields_count: usize, default_fields: usize, func_count: usize };
    pub const Value = struct { value_instr: usize, cast: bool = false };
    pub const Unary = struct {
        op: Op,
        typ: Type,

        pub const Op = enum { minus, bang };
    };
    pub const VarDecl = struct { variable: Variable, has_value: bool = false, cast: bool = false };
    // TODO: no need to pack here I think
    pub const Variable = packed struct { index: u62, scope: Scope };
};

comptime {
    // @compileLog(@sizeOf(Instruction));
    // @compileLog(@sizeOf(Instruction.Data));
    // @compileLog(@sizeOf(Instruction.Tag));
    // @compileLog(@sizeOf(Instruction.Binop));
    // @compileLog(@sizeOf(Instruction.Block));
    // @compileLog(@sizeOf(Instruction.call));
    // @compileLog(@sizeOf(Instruction.FnDecl));
    // @compileLog(@sizeOf(Instruction.@"if"));
    // @compileLog(@sizeOf(Instruction.Imported));
    // @compileLog(@sizeOf(Instruction.unary));
    // @compileLog(@sizeOf(Instruction.Variable));
    // @compileLog(@sizeOf(Instruction.VarDecl));
}
