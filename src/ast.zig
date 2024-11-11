const std = @import("std");
const Token = @import("lexer.zig").Token;

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
    name: Token,
    is_const: bool,
    type_: ?VarType,
    value: ?*Expr,
};

// Expressions
pub const Expr = union(enum) {
    // BoolLit: BoolLit,
    // FloatLit: FloatLit,
    IntLit: IntLit,
    // UintLit: UintLit,
    BinOp: BinOp,
    Unary: Unary,

    pub fn accept(self: *const Expr, visitor: anytype) void {
        switch (self.*) {
            .BinOp => |e| visitor.binop_expr(e),
            .IntLit => |e| visitor.int_expr(e),
            .Unary => |e| visitor.unary_expr(e),
        }
    }
};

pub const BoolLit = struct {
    value: f64,
};

pub const FloatLit = struct {
    value: f64,
};

pub const IntLit = struct {
    value: i64,
};

pub const UintLit = struct {
    value: u64,
};

pub const Unary = struct {
    op: Token,
    rhs: *Expr,
};

pub const BinOp = struct {
    lhs: *Expr,
    rhs: *Expr,
    op: Token,
};
