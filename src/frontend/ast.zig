const std = @import("std");
const Token = @import("lexer.zig").Token;

// pub const SourceSlice = struct {
//     text: []const u8,
//     start: usize,
//
//     const Self = @This();
//
//     pub fn from_token(token: Token, source: []const u8) Self {
//         return .{
//             .text = token.from_source(source),
//             .start = token.span.start,
//         };
//     }
// };

// pub const Type = union(enum) {
//     Entity: SourceSlice,
//     Function: Fn,
//
//     pub const Fn = struct {
//         params: []const SourceSlice,
//         return_type: ?SourceSlice,
//         start: usize,
//     };
// };

// pub const Stmt = union(enum) {
//     Assignment: Assignment,
//     Discard: Discard,
//     FnDecl: FnDecl,
//     Print: Print,
//     Use: Use,
//     VarDecl: VarDecl,
//     While: While,
//     Expr: *Expr,
//
//     pub fn span(self: *const Stmt) Span {
//         return switch (self.*) {
//             .Assignment => |s| .{
//                 .start = s.assigne.span().start,
//                 .end = s.value.span().end,
//             },
//             .Discard => |s| s.expr.span(),
//             .FnDecl => |s| .{
//                 .start = s.name.start,
//                 .end = s.name.start + s.name.text.len,
//             },
//             .Print => |s| s.expr.span(),
//             .Use => |s| s.span,
//             // NOTE: wrong, but sufficient
//             .VarDecl => |s| .{
//                 .start = s.name.start,
//                 .end = s.name.start + s.name.text.len,
//             },
//             .While => |s| .{
//                 .start = s.condition.span().start,
//                 .end = s.body.span().end,
//             },
//             .Expr => |e| e.span(),
//         };
//     }
// };

// pub const Assignment = struct {
//     assigne: *const Expr,
//     value: *const Expr,
// };
//
// pub const Discard = struct {
//     expr: *const Expr,
// };
//
// pub const FnDecl = struct {
//     name: SourceSlice,
//     params: [256]Parameter,
//     arity: u16,
//     body: Block,
//     return_type: ?Type,
// };
//
// pub const Parameter = struct {
//     name: SourceSlice,
//     type_: Type,
// };
//
// pub const Print = struct {
//     expr: *const Expr,
// };
//
// pub const Use = struct {
//     module: []const SourceSlice,
//     span: Span,
// };
//
// pub const VarDecl = struct {
//     name: SourceSlice,
//     is_const: bool,
//     type_: ?Type,
//     value: ?*Expr,
// };
//
// pub const While = struct {
//     condition: *const Expr,
//     body: *const Stmt,
// };

pub const TokenIndex = usize;

pub const NullNode = 0;

// Behaves like a binary tree, tupple of (root, lhs, rhs)
pub const Node = struct {
    tag: Tag,
    root: TokenIndex,
    data: Data,

    pub const Index = usize;
    pub const Data = struct { lhs: Index, rhs: Index };

    pub const Tag = enum {
        // Block,
        Add,
        Assignment,
        Block,
        Bool,
        Discard,
        Div,
        Float,
        FnDecl,
        // FnCall,
        Grouping,
        Identifier,
        // If,
        Int,
        Mul,
        Null,
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

// Expressions
// pub const Expr = union(enum) {
//     Block: Block,
//     BinOp: BinOp,
//     BoolLit: BoolLit,
//     FloatLit: FloatLit,
//     FnCall: FnCall,
//     Grouping: Grouping,
//     Identifier: Identifier,
//     If: If,
//     IntLit: IntLit,
//     NullLit: NullLit,
//     Return: Return,
//     StringLit: StringLit,
//     Unary: Unary,
//
//     pub fn span(self: *const Expr) Span {
//         return switch (self.*) {
//             inline else => |e| e.span,
//         };
//     }
// };
//
// pub const Block = struct {
//     stmts: []Stmt,
//     span: Span,
// };
//
// pub const BinOp = struct {
//     lhs: *Expr,
//     rhs: *Expr,
//     op: Token.Kind,
//     span: Span,
// };
//
// pub const BoolLit = struct {
//     value: bool,
//     span: Span,
// };
//
// pub const FloatLit = struct {
//     value: f64,
//     span: Span,
// };
//
// pub const FnCall = struct {
//     callee: *const Expr,
//     args: [256]*const Expr,
//     arity: usize,
//     span: Span,
// };
//
// pub const Grouping = struct {
//     expr: *Expr,
//     span: Span,
// };
//
// pub const Identifier = struct {
//     name: []const u8,
//     span: Span,
// };
//
// /// Bodies can be either blocks or single do statement
// pub const If = struct {
//     condition: *Expr,
//     then_body: Stmt,
//     else_body: ?Stmt,
//     span: Span,
// };
//
// pub const IntLit = struct {
//     value: i64,
//     span: Span,
// };
//
// pub const NullLit = struct {
//     span: Span,
// };
//
// pub const Return = struct {
//     expr: ?*Expr,
//     span: Span,
// };
//
// pub const StringLit = struct {
//     value: []const u8,
//     span: Span,
// };
//
// pub const Unary = struct {
//     op: Token.Kind,
//     rhs: *Expr,
//     span: Span,
// };
