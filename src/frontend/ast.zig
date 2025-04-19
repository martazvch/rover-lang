const std = @import("std");

const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

source: [:0]const u8,
tokens: *const std.MultiArrayList(Token),
nodes: []Node,

pub const TokenIndex = usize;

pub const Node = union(enum) {
    assignment: struct {
        assigne: *Expr,
        value: *Expr,
    },
    print: *Expr,
    expr: *Expr,
};

pub const Expr = union(enum) {
    block: Block,
    binop: Binop,
    field: Field,
    fn_call: FnCall,
    grouping: Grouping,
    @"if": If,
    literal: Literal,
    @"return": Return,
    struct_literal: StructLiteral,
    unary: Unary,
};

pub const Block = struct {
    exprs: []Node,
    span: Span,
};

pub const Binop = struct {
    lhs: *Expr,
    rhs: *Expr,
    op: TokenIndex,
};

pub const Field = struct {
    structure: *Expr,
    field: TokenIndex,
};

pub const FnCall = struct {
    callee: *Expr,
    args: []*Expr,
};

pub const Grouping = struct {
    expr: ?*Expr,
    span: Span,
};

pub const If = struct {
    condition: *Expr,
    then: Node,
    @"else": ?Node,
};

pub const Literal = struct {
    tag: Tag,
    idx: TokenIndex,

    pub const Tag = enum { bool, float, identifier, int, null, string };
};

pub const Return = struct {
    expr: ?*Expr,
};

pub const StructLiteral = struct {
    name: TokenIndex,
    fields: []FieldAndValue,
};

pub const FieldAndValue = struct {
    name: TokenIndex,
    value: ?*Expr,
};

pub const Unary = struct {
    op: TokenIndex,
    expr: *Expr,
};

// comptime {
//     @compileLog(@sizeOf(Node));
// }
