const std = @import("std");

const Token = @import("lexer.zig").Token;

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
        And,
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
        member,
        Mul,
        MultiVarDecl,
        Ne,
        Null,
        Or,
        Parameter,
        Print,
        Return,
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
