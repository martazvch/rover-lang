pub const Scope = enum { Builtin, Global, Local };
pub const ReturnKind = enum { Explicit, ImplicitValue, ImplicitVoid };

pub const Instruction = union(enum) {
    Binop: BinopData,
    Bool: bool,
    Discard,
    Float: f64,
    Int: i64,
    Null,
    Print,
    StrConcat,
    String: usize,
    StrMul: Side,
    Unary: UnaryTag,

    pub const BinopData = struct {
        result_type: RersultType,
        cast: Cast,

        pub const RersultType = enum { Float, Int, Str, Other };
        pub const Cast = enum { Lhs, Rhs, None };
    };
    pub const Side = enum { Left, Right };
    pub const UnaryTag = enum { Minus, Bang };
};
