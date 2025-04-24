const std = @import("std");

const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

source: [:0]const u8,
tokens: *const std.MultiArrayList(Token),
nodes: []Node,

const Ast = @This();
pub const TokenIndex = usize;

pub const Node = union(enum) {
    assignment: Assignment,
    discard: *Expr,
    fn_decl: FnDecl,
    multi_var_decl: MultiVarDecl,
    print: *Expr,
    struct_decl: StructDecl,
    use: Use,
    var_decl: VarDecl,
    @"while": While,
    expr: *Expr,

    pub fn getSpan(self: *const Node, ast: *const Ast) Span {
        return switch (self.*) {
            inline else => |e| e.getSpan(ast),
        };
    }
};

pub const Assignment = struct {
    assigne: *Expr,
    value: *Expr,

    pub fn getSpan(self: *const Assignment, ast: *const Ast) Span {
        return .{
            .start = self.assigne.getSpan(ast).start,
            .end = self.value.getSpan(ast).end,
        };
    }
};

pub const FnDecl = struct {
    name: TokenIndex,
    params: []Param,
    body: Block,
    return_type: ?*Type,

    pub fn getSpan(self: *const FnDecl, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.name];
    }
};

pub const MultiVarDecl = struct {
    decls: []VarDecl,

    pub fn getSpan(self: *const MultiVarDecl, ast: *const Ast) Span {
        return .{
            .start = self.decls[0].getSpan(ast).start,
            .end = self.decls[self.decls.len - 1].getSpan(ast).end,
        };
    }
};

pub const Param = struct {
    name: TokenIndex,
    typ: *Type,

    pub fn getSpan(self: *const Param, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.name];
    }
};

pub const Type = union(enum) {
    function: struct {
        params: []*Type,
        return_type: ?*Type,
        span: Span,
    },
    scalar: TokenIndex,
    self: void,

    pub fn getSpan(self: *const Type, ast: *const Ast) Span {
        return switch (self.*) {
            .function => |t| t.span,
            .scalar, .void => |t| ast.tokens.items(.span)[t],
        };
    }
};

pub const StructDecl = struct {
    name: TokenIndex,
    fields: []VarDecl,
    functions: []FnDecl,

    pub fn getSpan(self: *const StructDecl, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.name];
    }
};

pub const Use = struct {
    names: []const TokenIndex,

    pub fn getSpan(self: *const Use, ast: *const Ast) Span {
        return .{
            .start = ast.tokens.items(.span)[self.names[0]].start,
            .end = ast.tokens.items(.span)[self.names[self.names.len - 1]].end,
        };
    }
};

pub const VarDecl = struct {
    name: TokenIndex,
    typ: ?*Type,
    value: ?*Expr,

    pub fn getSpan(self: *const VarDecl, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.name];
    }
};

pub const While = struct {
    cond: *Expr,
    body: Block,

    pub fn getSpan(self: *const While, ast: *const Ast) Span {
        return self.cond.getSpan(ast);
    }
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

    pub fn getSpan(self: *const Expr, ast: *const Ast) Span {
        return switch (self.*) {
            inline else => |e| e.getSpan(ast),
        };
    }
};

pub const Block = struct {
    exprs: []Node,
    span: Span,

    pub fn getSpan(self: *const Block, _: *const Ast) Span {
        return self.span;
    }
};

pub const Binop = struct {
    lhs: *Expr,
    rhs: *Expr,
    op: TokenIndex,

    pub fn getSpan(self: *const Binop, ast: *const Ast) Span {
        return .{
            .start = self.lhs.getSpan(ast).start,
            .end = self.rhs.getSpan(ast).end,
        };
    }
};

pub const Field = struct {
    structure: *Expr,
    field: TokenIndex,
    pub fn getSpan(self: *const Field, ast: *const Ast) Span {
        return .{
            .start = self.structure.getSpan(ast).start,
            .end = ast.tokens.items(.span)[self.field].end,
        };
    }
};

pub const FnCall = struct {
    callee: *Expr,
    args: []*Expr,

    pub fn getSpan(self: *const FnCall, ast: *const Ast) Span {
        return self.callee.getSpan(ast);
    }
};

pub const Grouping = struct {
    expr: ?*Expr,
    span: Span,

    pub fn getSpan(self: *const Grouping, _: *const Ast) Span {
        return self.span;
    }
};

pub const If = struct {
    condition: *Expr,
    then: Node,
    @"else": ?Node,

    pub fn getSpan(self: *const If, ast: *const Ast) Span {
        return self.condition.getSpan(ast);
    }
};

pub const Literal = struct {
    tag: Tag,
    idx: TokenIndex,

    pub const Tag = enum { bool, float, identifier, int, null, string };

    pub fn getSpan(self: *const Literal, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.idx];
    }
};

pub const Return = struct {
    expr: ?*Expr,
    kw: TokenIndex,

    pub fn getSpan(self: *const Return, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.kw];
    }
};

pub const StructLiteral = struct {
    name: TokenIndex,
    fields: []FieldAndValue,

    pub fn getSpan(self: *const StructLiteral, ast: *const Ast) Span {
        return ast.tokens.items(.span)[self.name];
    }
};

pub const FieldAndValue = struct {
    name: TokenIndex,
    value: ?*Expr,
};

pub const Unary = struct {
    op: TokenIndex,
    expr: *Expr,

    pub fn getSpan(self: *const Unary, ast: *const Ast) Span {
        return .{
            .start = ast.tokens.items(.span)[self.op].start,
            .end = self.expr.getSpan(ast).end,
        };
    }
};

pub fn toSource(self: *const Ast, node: anytype) []const u8 {
    const span = node.getSpan(self);
    return self.source[span.start..span.end];
}

// comptime {
//     @compileLog(@sizeOf(Node));
//     @compileLog(@sizeOf(Expr));
// }
