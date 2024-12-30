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
    Assignment: Assignment,
    Discard: Discard,
    Print: Print,
    VarDecl: VarDecl,
    While: While,
    Expr: *Expr,

    pub fn span(self: *const Stmt) Span {
        return switch (self.*) {
            .Assignment => |s| .{
                .start = s.assigne.span().start,
                .end = s.value.span().end,
            },
            .Discard => |s| s.expr.span(),
            .Print => |s| s.expr.span(),
            // NOTE: wrong, but sufficient
            .VarDecl => |s| .{
                .start = s.name.start,
                .end = s.name.start + s.name.text.len,
            },
            .While => |s| .{
                .start = s.condition.span().start,
                .end = s.body.span().end,
            },
            .Expr => |e| e.span(),
        };
    }
};

pub const Assignment = struct {
    assigne: *const Expr,
    value: *const Expr,
};

pub const Discard = struct {
    expr: *const Expr,
};

pub const Print = struct {
    expr: *const Expr,
};

pub const VarDecl = struct {
    name: SourceSlice,
    is_const: bool,
    type_: ?SourceSlice,
    value: ?*Expr,
};

pub const While = struct {
    condition: *const Expr,
    body: *const Stmt,
};

// Expressions
pub const Expr = union(enum) {
    Block: Block,
    BinOp: BinOp,
    BoolLit: BoolLit,
    FloatLit: FloatLit,
    Grouping: Grouping,
    Identifier: Identifier,
    If: If,
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

pub const Block = struct {
    stmts: []Stmt,
    span: Span,
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

/// Bodies can be either blocks or single do statement
pub const If = struct {
    condition: *Expr,
    then_body: Stmt,
    else_body: ?Stmt,
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
