pub const Scope = enum(u2) { Builtin, Global, Local };
pub const ReturnKind = enum { Explicit, ImplicitValue, ImplicitVoid };

// pub const Instruction = union(enum) {
//     Assignment: Variable,
//     Binop: BinopData,
//     Block: BlockData,
//     Bool: bool,
//     CastToFloat,
//     Discard,
//     Float: f64,
//     Identifier: Variable,
//     Int: i64,
//     Null,
//     Print,
//     String: usize,
//     Unary: UnaryTag,
//     VarDecl: Variable,
//     While,
//
//     pub const BinopData = struct {
//         cast: Cast = .None,
//         tag: Tag,
//
//         pub const Cast = enum { Lhs, Rhs, None };
//         pub const Tag = enum {
//             AddFloat,
//             AddInt,
//             AddStr,
//             And,
//             DivFloat,
//             DivInt,
//             EqFloat,
//             EqInt,
//             GeFloat,
//             GeInt,
//             GtFloat,
//             GtInt,
//             LeFloat,
//             LeInt,
//             LtFloat,
//             LtInt,
//             MulFloat,
//             MulInt,
//             MulStr,
//             NeFloat,
//             NeInt,
//             Or,
//             SubFloat,
//             SubInt,
//         };
//     };
//     pub const BlockData = struct { pop_count: usize, is_expr: bool };
//     pub const Side = enum { Left, Right };
//     pub const UnaryTag = enum { Minus, Bang };
//     pub const Variable = struct { index: usize, scope: Scope };
//
//     // TODO: investigate size of structs, BlockData for example
//     // uses 16 bytes, if we do a packed struct with u63 and u1
//     // we go down to 8 bytes
//     // If we do the same for all structs, we can lower Instruction sizeof
// };

pub const Instruction = struct {
    tag: Tag,
    data: Data = undefined,

    pub const Tag = enum(u8) {
        Assignment,
        Binop,
        Block,
        Bool,
        Cast,
        Discard,
        Float,
        FnDecl,
        Identifier,
        Int,
        Null,
        Print,
        Sentinel,
        String,
        Unary,
        VarDecl,
        While,
    };

    pub const Data = union {
        Binop: Binop,
        Block: Block,
        Bool: bool,
        CastTo: CastTo,
        Float: f64,
        FnDecl: ReturnKind,
        Id: usize,
        Int: i64,
        Unary: Unary,
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
            EqFloat,
            EqInt,
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
            NeFloat,
            NeInt,
            Or,
            SubFloat,
            SubInt,
        };
    };

    pub const Block = packed struct { pop_count: u61, is_expr: bool };
    pub const CastTo = enum(u1) { Float };
    pub const Unary = enum(u1) { Minus, Bang };
    pub const Variable = packed struct { index: u62, scope: Scope };
};

comptime {
    // @compileLog(@sizeOf(Instruction));
    // @compileLog(@sizeOf(Instruction.Data));
    // @compileLog(@sizeOf(Instruction.Block));
    // @compileLog(@sizeOf(Instruction.Tag));
    // @compileLog(@sizeOf(Instruction.Binop));
    // @compileLog(@sizeOf(Instruction.Unary));
}
