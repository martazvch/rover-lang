const std = @import("std");
const Token = @import("lexer.zig").Token;
const Type = @import("analyzer.zig").Type;
const Ast = @import("ast.zig");
const Span = @import("ast.zig").Span;

pub const BinOp = struct {
    type_: Type = .Null,
    cast: Cast = .None,

    // We assume we only need to know the side because the cast will always
    // be from int to float
    const Cast = enum { Lhs, Rhs, None };
};

pub const Unary = struct {
    type_: Type = .Null,
};

pub const Variable = struct {
    scope: Scope,
    index: u64,

    const Scope = enum { Global, Local };
};
