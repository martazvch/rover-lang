const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = @import("lexer.zig").Token;
const Ast = @import("ast.zig");
const Span = @import("ast.zig").Span;
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;

pub const Scope = enum { Global, Local };
pub const ReturnKind = enum { Explicit, ImplicitValue, ImplicitVoid };

pub const AnalyzedStmt = union(enum) {
    Assignment: Assignment,
    Block: Block,
    Binop: BinOp,
    FnDecl: FnDecl,
    If: If,
    Unary: Unary,
    Variable: Variable,
};

pub const Block = struct {
    /// Number of locals to pop at the end of scope
    pop_count: usize,
    /// Tells if the block returns a value or not
    is_expr: bool,
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

pub const FnDecl = struct {
    /// The variable holding the function
    variable: Variable,
    /// How this function returns a value
    return_kind: ReturnKind,
};

pub const If = struct {
    cast: Cast = .None,

    // We assume we only need to know the branch because the cast will always
    // be from int to float
    const Cast = enum { Then, Else, None };
};

pub const Unary = struct {
    type_: Type,
};

pub const Variable = struct {
    scope: Scope,
    index: usize,
};
