pub const Scope = enum(u2) { builtin, global, heap, local };
pub const Type = enum(u2) { float, int };
pub const ReturnKind = enum(u2) { explicit, implicit_value, implicit_void };

pub const Instruction = struct {
    tag: Tag,
    data: Data = undefined,
    offset: usize = 0,

    pub const Tag = enum(u8) {
        Assignment,
        binop,
        Block,
        bool,
        call,
        Cast,
        Discard,
        field,
        float,
        FnDecl,
        Name,
        identifier,
        identifier_id,
        @"if",
        Imported,
        int,
        MultipleVarDecl,
        null,
        print,
        @"return",
        string,
        struct_decl,
        struct_literal,
        unary,
        use,
        VarDecl,
        @"while",
    };

    pub const Data = union {
        Assignment: Assignment,
        binop: Binop,
        Block: Block,
        bool: bool,
        call: Call,
        Capture: usize,
        CastTo: Type,
        field: usize,
        float: f64,
        FnDecl: FnDecl,
        id: usize,
        @"if": If,
        Imported: Imported,
        int: i64,
        @"return": Return,
        struct_decl: StructDecl,
        struct_literal: StructLiteral,
        unary: Unary,
        use: u64,
        VarDecl: VarDecl,
        variable: Variable,
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
    pub const Call = struct { arity: u8, builtin: bool };
    pub const FnDecl = struct { body_len: u64, return_kind: ReturnKind };
    pub const If = struct {
        cast: Cast,
        has_else: bool,

        pub const Cast = enum(u2) { then, @"else", none };
    };
    pub const Imported = struct { index: u64, variable: Variable };
    pub const Return = struct { value: bool, cast: bool };
    pub const StructDecl = struct { fields_count: usize, default_fields: usize, func_count: usize };
    pub const StructLiteral = struct { variable: Variable, arity: usize, end: usize };
    pub const Unary = struct {
        op: Op,
        typ: Type,

        pub const Op = enum { minus, bang };
    };
    pub const VarDecl = struct { variable: Variable, cast: bool };
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
