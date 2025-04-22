const std = @import("std");

const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

source: [:0]const u8,
tokens: *const std.MultiArrayList(Token),
nodes: []Node,

const Self = @This();
pub const TokenIndex = usize;

pub const Node = union(enum) {
    assignment: struct {
        assigne: *Expr,
        value: *Expr,
    },
    discard: *Expr,
    fn_decl: FnDecl,
    multi_var_decl: []VarDecl,
    print: *Expr,
    struct_decl: StructDecl,
    use: []const TokenIndex,
    var_decl: VarDecl,
    @"while": struct {
        cond: *Expr,
        body: Block,
    },
    expr: *Expr,
};

pub const FnDecl = struct {
    name: TokenIndex,
    params: []Param,
    body: Block,
    return_type: ?*Type,
};

pub const Param = struct {
    name: TokenIndex,
    typ: *Type,
};

pub const Type = union(enum) {
    scalar: TokenIndex,
    function: struct {
        params: []*Type,
        return_type: ?*Type,
    },
    self: void,
};

pub const StructDecl = struct {
    name: TokenIndex,
    fields: []VarDecl,
    functions: []FnDecl,
};

pub const VarDecl = struct {
    name: TokenIndex,
    typ: ?*Type,
    value: ?*Expr,
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
    kw: TokenIndex,
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

pub fn toSource(self: *const Self, node: *const Node) []const u8 {
    const span = self.toSpan(node);
    return self.source[span.start..span.end];
}

pub fn toSpan(self: *const Self, node: *const Node) Span {
    return switch (node.*) {
        .expr => |e| self.exprToSpan(e),
        else => unreachable,
    };
}

fn exprToSpan(self: *const Self, expr: *const Expr) Span {
    return switch (expr.*) {
        .block => |e| e.span,
        .binop => |e| .{
            .start = self.exprToSpan(e.lhs).start,
            .end = self.exprToSpan(e.rhs).end,
        },
        .field => |e| .{
            .start = self.exprToSpan(e.structure).start,
            .end = self.tokens.items(.span)[e.field].end,
        },
        .fn_call => |e| self.exprToSpan(e.callee),
        .grouping => |e| e.span,
        .@"if" => |e| self.exprToSpan(e.condition),
        .literal => |e| self.tokens.items(.span)[e.idx],
        .@"return" => |e| self.tokens.items(.span)[e.kw],
        .struct_literal => |e| self.tokens.items(.span)[e.name],
        .unary => |e| .{
            .start = self.tokens.items(.span)[e.op].start,
            .end = self.exprToSpan(e.expr).end,
        },
    };
}

// comptime {
//     @compileLog(@sizeOf(Node));
// }
