pub const Scope = enum { Builtin, Global, Local };
pub const ReturnKind = enum { Explicit, ImplicitValue, ImplicitVoid };

pub const Instruction = union(enum) {
    Assignment: Variable,
    Binop: BinopData,
    Block: BlockData,
    Bool: bool,
    CastToFloat,
    Discard,
    Float: f64,
    Identifier: Variable,
    Int: i64,
    Null,
    Print,
    String: usize,
    Unary: UnaryTag,
    VarDecl: Variable,
    While,

    pub const BinopData = struct {
        cast: Cast = .None,
        tag: Tag,

        pub const Cast = enum { Lhs, Rhs, None };
        pub const Tag = enum {
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
    pub const BlockData = struct { pop_count: usize, is_expr: bool };
    pub const Side = enum { Left, Right };
    pub const UnaryTag = enum { Minus, Bang };
    pub const Variable = struct { index: usize, scope: Scope };

    // TODO: investigate size of structs, BlockData for example
    // uses 16 bytes, if we do a packed struct with u63 and u1
    // we go down to 8 bytes
    // If we do the same for all structs, we can lower Instruction sizeof
};
