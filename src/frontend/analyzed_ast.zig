const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = @import("lexer.zig").Token;
const Type = @import("analyzer.zig").Type;
const Ast = @import("ast.zig");
const Span = @import("ast.zig").Span;
const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;

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

pub const AstExtra = struct {
    binops: ArrayList(BinOp),
    unaries: ArrayList(Unary),
    variables: ArrayList(Variable),

    pub const Iter = struct {
        binops: UnsafeIter(BinOp),
        unaries: UnsafeIter(Unary),
        variables: UnsafeIter(Variable),
    };

    pub fn init(allocator: Allocator) AstExtra {
        return .{
            .binops = ArrayList(BinOp).init(allocator),
            .unaries = ArrayList(Unary).init(allocator),
            .variables = ArrayList(Variable).init(allocator),
        };
    }

    pub fn deinit(self: *AstExtra) void {
        self.binops.deinit();
        self.unaries.deinit();
        self.variables.deinit();
    }

    pub fn reinit(self: *AstExtra) void {
        self.binops.clearRetainingCapacity();
        self.unaries.clearRetainingCapacity();
        self.variables.clearRetainingCapacity();
    }

    pub fn as_iter(self: *AstExtra) Iter {
        return .{
            .binops = UnsafeIter(BinOp).init(self.binops.items),
            .unaries = UnsafeIter(Unary).init(self.unaries.items),
            .variables = UnsafeIter(Variable).init(self.variables.items),
        };
    }
};
