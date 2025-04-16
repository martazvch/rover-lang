const std = @import("std");

const Token = @import("lexer.zig").Token;
const Span = @import("lexer.zig").Span;

source: [:0]const u8,
tokens: *const std.MultiArrayList(Token),
nodes: []Node,

pub const TokenIndex = usize;

pub const Node = union(enum) {
    assignment: struct {
        assigne: *Expr,
        value: *Expr,
    },
    expr: *Expr,
};

pub const Expr = union(enum) {
    fnCall: FnCall,
    grouping: Grouping,
    literal: Literal,
    unary: Unary,
};

pub const FnCall = struct {
    callee: *Expr,
    args: []*Expr,
};

pub const Grouping = struct {
    expr: ?*Expr,
    span: Span,
};

pub const Literal = struct {
    tag: Tag,
    idx: TokenIndex,

    pub const Tag = enum { bool, float, identifier, int, null, string };
};

pub const Unary = struct {
    op: TokenIndex,
    expr: *Expr,
};

// comptime {
//     @compileLog(@sizeOf(Node2));
// }
