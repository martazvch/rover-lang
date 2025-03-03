const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const TokenIndex = usize;
pub const NullNode = 0;

pub const Node = struct {
    tag: Tag,
    main: TokenIndex = 0,
    data: usize = 0,

    pub const Index = usize;
    pub const Empty: Node = .{
        .tag = .Empty,
    };

    pub const Tag = enum {
        Add,
        And,
        Assignment,
        Block,
        Bool,
        Discard,
        Div,
        Empty,
        Eq,
        Float,
        FnDecl,
        FnDeclEnd,
        FnCall,
        FnCallEnd,
        Ge,
        Gt,
        Grouping,
        Identifier,
        If,
        Int,
        Le,
        Lt,
        Mul,
        MultiValueDecl,
        MultiVarDecl,
        Ne,
        Null,
        Or,
        Parameter,
        Print,
        Return,
        String,
        Sub,
        Type,
        Unary,
        Use,
        VarDecl,
        While,
    };
};
