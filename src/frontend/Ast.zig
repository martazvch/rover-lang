const std = @import("std");

const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

source: [:0]const u8,
token_tags: []const Token.Tag,
token_spans: []const Span,
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
};

pub const Assignment = struct {
    assigne: *Expr,
    value: *Expr,
};

pub const FnDecl = struct {
    name: TokenIndex,
    params: []Param,
    body: Block,
    return_type: ?*Type,
};

pub const MultiVarDecl = struct {
    decls: []VarDecl,
};

pub const Param = struct {
    name: TokenIndex,
    typ: ?*Type = null,
    value: ?*Expr = null,
};

pub const Type = union(enum) {
    array: struct {
        openning: TokenIndex,
        child: *Type,
    },
    function: Fn,
    fields: []TokenIndex,
    scalar: TokenIndex,
    self: TokenIndex,

    pub const Fn = struct {
        params: []*Type,
        return_type: ?*Type,
        span: Span,
    };
};

pub const StructDecl = struct {
    name: TokenIndex,
    fields: []VarDecl,
    functions: []FnDecl,
};

pub const Use = struct {
    names: []const TokenIndex,
    items: ?[]const ItemAndAlias,
    alias: ?TokenIndex,

    pub const ItemAndAlias = struct {
        item: TokenIndex,
        alias: ?TokenIndex,
    };
};

pub const VarDecl = struct {
    name: TokenIndex,
    typ: ?*Type,
    value: ?*Expr,
};

pub const While = struct {
    condition: *Expr,
    body: Block,
};

pub const Expr = union(enum) {
    array: Array,
    array_access: ArrayAccess,
    block: Block,
    binop: Binop,
    field: Field,
    fn_call: FnCall,
    grouping: Grouping,
    @"if": If,
    literal: Literal,
    named_arg: NamedArg,
    @"return": Return,
    struct_literal: StructLiteral,
    unary: Unary,
};

pub const Array = struct {
    values: []*Expr,
    span: Span,
};

pub const ArrayAccess = struct {
    array: *Expr,
    index: *Expr,
    end: TokenIndex,
};

pub const Block = struct {
    nodes: []Node,
    span: Span,
};

pub const Binop = struct {
    lhs: *Expr,
    rhs: *Expr,
    op: Token.Tag,
};

pub const Field = struct {
    structure: *Expr,
    field: TokenIndex,
};

pub const FnCall = struct {
    callee: *Expr,
    args: []*Expr,
};

pub const NamedArg = struct {
    name: TokenIndex,
    value: *Expr,
};

pub const Grouping = struct {
    expr: *Expr,
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

    pub const Tag = enum { bool, float, identifier, int, null, self, string };
};

pub const Return = struct {
    expr: ?*Expr,
    kw: TokenIndex,
};

pub const StructLiteral = struct {
    structure: *Expr,
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

/// Can be used with any `*Node`, `*Expr` or a `token index`
pub fn toSource(self: *const Ast, node: anytype) []const u8 {
    const span = if (@TypeOf(node) == usize)
        self.token_spans[node]
    else
        self.getSpan(node.*);

    return self.source[span.start..span.end];
}

/// Should be either a token index or a Node
pub fn getSpan(self: *const Ast, anynode: anytype) Span {
    const NodeType, const node = switch (@typeInfo(@TypeOf(anynode))) {
        .pointer => |ptr| .{ ptr.child, anynode.* },
        else => .{ @TypeOf(anynode), anynode },
    };

    return switch (NodeType) {
        usize => self.token_spans[anynode],
        Node => switch (node) {
            inline else => |*n| self.getSpan(n.*),
        },
        Assignment => .{
            .start = self.getSpan(node.assigne.*).start,
            .end = self.getSpan(node.value.*).end,
        },
        FnDecl, Param, StructDecl, VarDecl => self.token_spans[node.name],
        MultiVarDecl => .{
            .start = self.getSpan(node.decls[0]).start,
            .end = self.getSpan(node.decls[node.decls.len - 1]).end,
        },
        Type => switch (node) {
            .array => |t| .{
                .start = self.token_spans[t.openning].start,
                .end = self.getSpan(t.child).end,
            },
            .fields => |t| .{
                .start = self.token_spans[t[0]].start,
                .end = self.token_spans[t[t.len - 1]].end,
            },
            .function => |t| t.span,
            .scalar, .self => |t| self.token_spans[t],
        },
        Use => .{
            .start = self.token_spans[node.names[0]].start,
            .end = self.token_spans[node.names[node.names.len - 1]].end,
        },
        While => self.getSpan(node.condition.*),
        Expr => switch (node) {
            inline else => |*e| self.getSpan(e.*),
        },
        Array => .{
            .start = self.token_spans[node.span.start].start,
            .end = self.token_spans[node.span.end].end,
        },
        ArrayAccess => .{
            .start = self.getSpan(node.array).start,
            .end = self.token_spans[node.end].end,
        },
        Block => node.span,
        Binop => .{
            .start = self.getSpan(node.lhs.*).start,
            .end = self.getSpan(node.rhs.*).end,
        },
        Field => .{
            .start = self.getSpan(node.structure.*).start,
            .end = self.token_spans[node.field].end,
        },
        FnCall => self.getSpan(node.callee),
        Grouping => node.span,
        If => self.getSpan(node.condition),
        Literal => self.token_spans[node.idx],
        NamedArg => .{
            .start = self.token_spans[node.name].start,
            .end = self.getSpan(node.value).end,
        },
        Return => self.token_spans[node.kw],
        StructLiteral => self.getSpan(node.structure),
        Unary => .{
            .start = self.token_spans[node.op].start,
            .end = self.getSpan(node.expr).end,
        },
        else => @compileError("Trying to get span on a non Node object and not usize"),
    };
}

// comptime {
//     @compileLog(@sizeOf(Node));
//     @compileLog(@sizeOf(Expr));
// }
