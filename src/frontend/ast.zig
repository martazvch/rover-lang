const std = @import("std");

const Span = @import("lexer.zig").Span;
pub const TokenIndex = usize;

pub const Node = struct {
    tag: Tag,
    main: TokenIndex = 0,
    data: usize = 0,
    end: Node.Index = 0,

    pub const Index = usize;
    pub const empty: Node = .{
        .tag = .Empty,
    };

    pub const Tag = enum {
        Add,
        @"and",
        Assignment,
        Block,
        Bool,
        count,
        Discard,
        Div,
        Empty,
        Eq,
        field,
        Float,
        FnDecl,
        call,
        Ge,
        Gt,
        Grouping,
        Identifier,
        If,
        Int,
        Le,
        Lt,
        Mul,
        MultiVarDecl,
        Ne,
        Null,
        Or,
        Parameter,
        Print,
        @"return",
        self,
        String,
        StructDecl,
        struct_literal,
        Sub,
        Type,
        Unary,
        Use,
        VarDecl,
        While,
    };
};

pub const Stmt = union(enum) {
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
