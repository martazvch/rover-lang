pub const Scope = enum(u2) { Builtin, Global, Local };
pub const Type = enum(u2) { Float, Int };
pub const ReturnKind = enum(u2) { Explicit, ImplicitValue, ImplicitVoid };

pub const Instruction = struct {
    tag: Tag,
    data: Data = undefined,
    start: usize = 0,

    pub const Tag = enum(u8) {
        Assignment,
        Binop,
        Block,
        Bool,
        Cast,
        Discard,
        Float,
        FnCall,
        FnDecl,
        FnName,
        Identifier,
        If,
        Imported,
        Int,
        Null,
        Print,
        Return,
        String,
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
        CastTo: Type,
        Float: f64,
        FnCall: FnCall,
        FnDecl: FnDecl,
        Id: usize,
        If: If,
        Imported: Imported,
        Int: i64,
        Return: bool,
        Unary: Unary,
        Use: u64,
        VarDecl: VarDecl,
        Variable: Variable,
    };

    pub const Binop = struct {
        cast: Cast = .None,
        op: Op,

        pub const Cast = enum(u2) { Lhs, Rhs, None };
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

    pub const Assignment = struct { variable: Variable, cast: bool };
    pub const Block = packed struct { length: usize, pop_count: u63, is_expr: bool };
    pub const FnCall = struct { arity: u8, builtin: bool };
    pub const FnDecl = struct { body_len: u64, return_kind: ReturnKind };
    pub const If = struct {
        cast: Cast,
        has_else: bool,

        pub const Cast = enum(u2) { Then, Else, None };
    };
    pub const Imported = struct { index: u64, variable: Variable };
    pub const Unary = packed struct {
        op: Op,
        typ: Type,

        pub const Op = enum(u1) { Minus, Bang };
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
    // @compileLog(@sizeOf(Instruction.FnCall));
    // @compileLog(@sizeOf(Instruction.FnDecl));
    // @compileLog(@sizeOf(Instruction.If));
    // @compileLog(@sizeOf(Instruction.Imported));
    // @compileLog(@sizeOf(Instruction.Unary));
    // @compileLog(@sizeOf(Instruction.Variable));
    // @compileLog(@sizeOf(Instruction.VarDecl));
}
