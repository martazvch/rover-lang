const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Ast = @import("Ast.zig");
const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const oom = @import("../utils.zig").oom;

arena: std.heap.ArenaAllocator,
allocator: Allocator,
interner: *Interner,
ast: *Ast,

const Self = @This();

const CaptureCtx = struct {
    stack: ArrayListUnmanaged(Scope) = .{},
    current: *Scope,
    depth: usize = 0,

    const Scope = struct {
        locals: AutoArrayHashMapUnmanaged(InternerIdx, VarDecl) = .{},
        captures: Ast.FnDecl.Meta.Captures = .{},
    };
    const VarDecl = struct {
        name: InternerIdx,
        depth: usize = 0,
        node: *Ast.VarDecl,
    };

    pub const empty: CaptureCtx = .{ .current = undefined };

    pub fn open(self: *CaptureCtx, allocator: Allocator) void {
        self.depth += 1;
        self.stack.append(allocator, .{}) catch oom();
        self.updateCurrent();
    }

    pub fn close(self: *CaptureCtx) Scope {
        self.depth -= 1;
        const popped = self.stack.pop() orelse unreachable;
        if (self.stack.items.len > 0) self.updateCurrent();
        return popped;
    }

    fn updateCurrent(self: *CaptureCtx) void {
        self.current = &self.stack.items[self.stack.items.len - 1];
    }

    pub fn declareLocal(self: *CaptureCtx, allocator: Allocator, name: InternerIdx, node: *Ast.VarDecl) void {
        self.current.locals.put(allocator, name, .{
            .name = name,
            .depth = self.depth,
            .node = node,
        }) catch oom();
    }

    /// Tries to retreive a variable from scopes and the local offset of its scope
    pub fn getVariable(self: *const CaptureCtx, name: InternerIdx) ?*const VarDecl {
        var i = self.stack.items.len;

        while (i > 0) {
            i -= 1;
            const stack = &self.stack.items[i];

            if (stack.locals.getPtr(name)) |local| {
                return local;
            }
        }

        return null;
    }
};

pub fn init(self: *Self, allocator: Allocator, interner: *Interner, ast: *Ast) void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();
    self.interner = interner;
    self.ast = ast;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub fn walk(self: *Self) void {
    var ctx: CaptureCtx = .empty;
    ctx.open(self.allocator);

    for (self.ast.nodes) |*node| {
        switch (node.*) {
            .fn_decl => |*n| if (n.has_callable) self.functionCaptures(n, &ctx),
            else => {},
        }
    }
}

fn functionCaptures(self: *Self, node: *Ast.FnDecl, ctx: *CaptureCtx) void {
    ctx.open(self.allocator);

    for (node.params) |*p| {
        const interned = self.interner.intern(self.ast.toSource(p.name));
        ctx.declareLocal(self.allocator, interned, p);
    }
    for (node.body.nodes) |*n| {
        self.captureFromNode(n, ctx);
    }

    const scope = ctx.close();
    node.meta.captures = scope.captures;
}

fn captureFromNode(self: *Self, node: *Ast.Node, ctx: *CaptureCtx) void {
    switch (node.*) {
        .assignment => |*n| {
            self.captureFromExpr(n.assigne, ctx);
            self.captureFromExpr(n.value, ctx);
        },
        .discard => |e| self.captureFromExpr(e, ctx),
        .fn_decl => |*n| self.functionCaptures(n, ctx),
        .multi_var_decl => |*n| {
            for (n.decls) |decl| if (decl.value) |val| {
                self.captureFromExpr(val, ctx);
            };
        },
        .print => |expr| self.captureFromExpr(expr, ctx),
        .struct_decl => |*n| {
            for (n.fields) |f| if (f.value) |val| {
                self.captureFromExpr(val, ctx);
            };
            for (n.functions) |*func| {
                self.functionCaptures(func, ctx);
            }
        },
        .use => {},
        .var_decl => |*n| {
            if (n.value) |val| {
                self.captureFromExpr(val, ctx);
            }

            const interned = self.interner.intern(self.ast.toSource(n.name));
            ctx.declareLocal(self.allocator, interned, n);
        },
        .@"while" => |*n| {
            self.captureFromExpr(n.condition, ctx);
            for (n.body.nodes) |*subn| {
                self.captureFromNode(subn, ctx);
            }
        },
        .expr => |e| self.captureFromExpr(e, ctx),
    }
}

fn captureFromExpr(self: *Self, expr: *Ast.Expr, ctx: *CaptureCtx) void {
    switch (expr.*) {
        .array => |*e| for (e.values) |val| {
            self.captureFromExpr(val, ctx);
        },
        .array_access => |*e| {
            self.captureFromExpr(e.array, ctx);
            self.captureFromExpr(e.index, ctx);
        },
        .binop => |*e| {
            self.captureFromExpr(e.lhs, ctx);
            self.captureFromExpr(e.rhs, ctx);
        },
        .block => |*e| {
            for (e.nodes) |*n| {
                self.captureFromNode(n, ctx);
            }
        },
        .fn_call => |*e| {
            self.captureFromExpr(e.callee, ctx);
            for (e.args) |arg| {
                self.captureFromExpr(arg, ctx);
            }
        },
        .closure => |*e| for (e.body.nodes) |*n| {
            self.captureFromNode(n, ctx);
        },
        .field => |*e| self.captureFromExpr(e.structure, ctx),
        .grouping => |e| self.captureFromExpr(e.expr, ctx),
        .@"if" => |*e| {
            self.captureFromExpr(e.condition, ctx);
            self.captureFromNode(&e.then, ctx);
            if (e.@"else") |*n| self.captureFromNode(n, ctx);
        },
        .literal => |e| {
            if (e.tag != .identifier) return;

            const interned = self.interner.intern(self.ast.toSource(e.idx));
            // If there is no variable associated with this name, it must be a symbol
            // so we don't care
            const variable = ctx.getVariable(interned) orelse return;

            if (variable.depth < ctx.depth) {
                variable.node.meta.captured = true;
                ctx.current.captures.put(self.allocator, interned, {}) catch oom();
            }
        },
        .named_arg => |e| self.captureFromExpr(e.value, ctx),
        .@"return" => |e| if (e.expr) |val| self.captureFromExpr(val, ctx),
        .struct_literal => |e| {
            self.captureFromExpr(e.structure, ctx);
            for (e.fields) |f| {
                if (f.value) |val| self.captureFromExpr(val, ctx);
            }
        },
        .unary => |e| self.captureFromExpr(e.expr, ctx),
    }
}
