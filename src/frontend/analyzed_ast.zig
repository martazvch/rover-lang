const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = @import("lexer.zig").Token;
const Ast = @import("ast.zig");
const Span = @import("ast.zig").Span;
const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;

pub const Type = u32;
pub const Null: Type = 0;
pub const Int: Type = 1;
pub const Float: Type = 2;
pub const Bool: Type = 3;
pub const Str: Type = 4;

pub const AnalyzedStmt = union(enum) {
    Binop: BinOp,
    Unary: Unary,
    Variable: Variable,
};

pub const BinOp = struct {
    type_: Type,
    cast: Cast = .None,

    // We assume we only need to know the side because the cast will always
    // be from int to float
    const Cast = enum { Lhs, Rhs, None };
};

pub const Unary = struct {
    type_: Type,
};

pub const Variable = struct {
    scope: Scope,
    index: usize,

    const Scope = enum { Global, Local };
};
