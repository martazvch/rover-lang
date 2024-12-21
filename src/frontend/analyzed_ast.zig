const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = @import("lexer.zig").Token;
const Ast = @import("ast.zig");
const Span = @import("ast.zig").Span;
const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;

pub const Type = u32;
pub const Void: Type = 0;
pub const Null: Type = 1;
pub const Int: Type = 2;
pub const Float: Type = 3;
pub const Bool: Type = 4;
pub const Str: Type = 5;

pub const AnalyzedStmt = union(enum) {
    Assignment: Assignment,
    Block: Block,
    Binop: BinOp,
    Unary: Unary,
    Variable: Variable,
};

pub const Block = struct {
    // Number of locals to pop at the end of scope
    pop_count: usize,
    // Flag for implicit return
    returns_value: bool,
};

pub const Assignment = struct {
    cast: Cast = .No,

    // Cast assumes that it is a cast to float
    const Cast = enum { Yes, No };
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
