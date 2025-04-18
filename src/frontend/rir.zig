pub const Scope = enum(u2) { builtin, global, heap, local };
pub const Type = enum(u2) { Float, Int };
pub const ReturnKind = enum(u2) { explicit, implicit_value, implicit_void };

pub const Instruction = struct {
    tag: Tag,
    data: Data = undefined,
    offset: usize = 0,

    pub const Tag = enum(u8) {
        Assignment,
        Binop,
        Block,
        Bool,
        call,
        Cast,
        Discard,
        field,
        Float,
        FnDecl,
        Name,
        Identifier,
        IdentifierId,
        If,
        Imported,
        Int,
        MultipleVarDecl,
        Null,
        Print,
        Return,
        String,
        struct_decl,
        struct_literal,
        Unary,
        Use,
        VarDecl,
        While,
    };

    pub const Data = union {
        Assignment: Assignment,
        Binop: Binop,
        Block: Block,
        Bool: bool,
        call: Call,
        Capture: usize,
        CastTo: Type,
        field: usize,
        Float: f64,
        FnDecl: FnDecl,
        Id: usize,
        If: If,
        Imported: Imported,
        Int: i64,
        Return: Return,
        struct_decl: StructDecl,
        struct_literal: StructLiteral,
        Unary: Unary,
        Use: u64,
        VarDecl: VarDecl,
        Variable: Variable,
    };

    pub const Binop = struct {
        cast: Cast = .none,
        op: Op,

        pub const Cast = enum(u2) { lhs, rhs, none };
        const Op = enum(u6) {
            AddFloat,
            AddInt,
            AddStr,
            And,
            DivFloat,
            DivInt,
            EqBool,
            EqFloat,
            EqInt,
            EqStr,
            GeFloat,
            GeInt,
            GtFloat,
            GtInt,
            LeFloat,
            LeInt,
            LtFloat,
            LtInt,
            MulFloat,
            MulInt,
            MulStr,
            NeBool,
            NeFloat,
            NeInt,
            NeStr,
            Or,
            SubFloat,
            SubInt,
        };
    };

    pub const Assignment = struct { cast: bool };
    pub const Block = packed struct { length: usize, pop_count: u8, is_expr: bool };
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
    pub const Unary = packed struct {
        op: Op,
        typ: Type,

        pub const Op = enum(u1) { minus, bang };
    };
    pub const VarDecl = struct { variable: Variable, cast: bool };
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
    // @compileLog(@sizeOf(Instruction.If));
    // @compileLog(@sizeOf(Instruction.Imported));
    // @compileLog(@sizeOf(Instruction.Unary));
    // @compileLog(@sizeOf(Instruction.Variable));
    // @compileLog(@sizeOf(Instruction.VarDecl));
}
