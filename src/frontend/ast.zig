const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const Span = struct {
    start: usize,
    end: usize,
};

pub const VarType = union(enum) {
    Float: void,
    Int: void,
    Uint: void,
    Bool: void,
    Struct: Token,
};

pub const Stmt = union(enum) {
    VarDecl: VarDecl,
    Expr: *Expr,
};

pub const VarDecl = struct {
    name: []const u8,
    is_const: bool,
    type_: ?VarType,
    value: ?*Expr,
    span: Span,
};

// Expressions
pub const Expr = union(enum) {
    BinOp: BinOp,
    // BoolLit: BoolLit,
    // FloatLit: FloatLit,
    Grouping: Grouping,
    IntLit: IntLit,
    // UintLit: UintLit,
    Unary: Unary,
};

pub const BinOp = struct {
    lhs: *Expr,
    rhs: *Expr,
    op: Token.Kind,
};

pub const BoolLit = struct {
    value: bool,
    spa: Span,
};

pub const FloatLit = struct {
    value: f64,
    span: Span,
};

pub const Grouping = struct {
    expr: *Expr,
    span: Span,
};

pub const IntLit = struct {
    value: i64,
    span: Span,
};

pub const UintLit = struct {
    value: u64,
    span: Span,
};

pub const Unary = struct {
    op: Token.Kind,
    rhs: *Expr,
    span: Span,
};
