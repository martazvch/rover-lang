const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const Span = struct {
    start: usize,
    end: usize,

    const Self = @This();

    pub fn from_source_slice(source_slice: SourceSlice) Self {
        return .{
            .start = source_slice.start,
            .end = source_slice.start + source_slice.text.len,
        };
    }
};

pub const SourceSlice = struct {
    text: []const u8,
    start: usize,

    const Self = @This();

    pub fn from_token(token: Token, source: []const u8) Self {
        return .{
            .text = token.from_source(source),
            .start = token.span.start,
        };
    }
};

pub const Stmt = union(enum) {
    Print: Print,
    VarDecl: VarDecl,
    Expr: *Expr,
};

pub const Print = struct {
    expr: *Expr,
    span: Span,
};

pub const VarDecl = struct {
    name: SourceSlice,
    is_const: bool,
    type_: ?SourceSlice,
    value: ?*Expr,
    span: Span,
};

// Expressions
pub const Expr = union(enum) {
    BinOp: BinOp,
    BoolLit: BoolLit,
    FloatLit: FloatLit,
    Grouping: Grouping,
    Identifier: Identifier,
    IntLit: IntLit,
    NullLit: NullLit,
    StringLit: StringLit,
    Unary: Unary,

    pub fn span(self: *const Expr) Span {
        return switch (self.*) {
            inline else => |e| e.span,
        };
    }
};

pub const BinOp = struct {
    lhs: *Expr,
    rhs: *Expr,
    op: Token.Kind,
    span: Span,
};

pub const BoolLit = struct {
    value: bool,
    span: Span,
};

pub const FloatLit = struct {
    value: f64,
    span: Span,
};

pub const Grouping = struct {
    expr: *Expr,
    span: Span,
};

pub const Identifier = struct {
    name: []const u8,
    span: Span,
};

pub const IntLit = struct {
    value: i64,
    span: Span,
};

pub const NullLit = struct {
    span: Span,
};

pub const StringLit = struct {
    value: []const u8,
    span: Span,
};

pub const Unary = struct {
    op: Token.Kind,
    rhs: *Expr,
    span: Span,
};
