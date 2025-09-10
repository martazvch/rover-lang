const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const oom = @import("../utils.zig").oom;
const Ast = @import("Ast.zig");

allocator: Allocator,
interner: *Interner,
ast: *Ast,

const Self = @This();

const CaptureCtx = struct {
    stack: ArrayList(Scope),
    current: *Scope,
    depth: usize,

    const Scope = struct {
        locals: AutoArrayHashMapUnmanaged(InternerIdx, VarDecl),
        captures: Ast.FnDecl.Meta.Captures,

        pub const empty: Scope = .{ .locals = .empty, .captures = .empty };

        pub fn addCapture(self: *Scope, allocator: Allocator, name: InternerIdx, index: usize, is_local: bool) usize {
            self.captures.put(allocator, name, .{ .index = index, .is_local = is_local }) catch oom();
            return self.captures.count() - 1;
        }
    };
    const VarDecl = struct {
        name: InternerIdx,
        depth: usize,
        index: usize,
        node: ?*Ast.VarDecl,
    };

    pub const empty: CaptureCtx = .{ .stack = .empty, .current = undefined, .depth = 0 };

    pub fn open(self: *CaptureCtx, allocator: Allocator) void {
        self.depth += 1;
        self.stack.append(allocator, .empty) catch oom();
        self.updateCurrent();
    }

    pub fn close(self: *CaptureCtx) Scope {
        self.depth -= 1;
        const popped = self.stack.pop().?;
        if (self.stack.items.len > 0) self.updateCurrent();
        return popped;
    }

    fn updateCurrent(self: *CaptureCtx) void {
        self.current = &self.stack.items[self.stack.items.len - 1];
    }

    pub fn declareLocal(self: *CaptureCtx, allocator: Allocator, name: InternerIdx, node: ?*Ast.VarDecl) void {
        self.current.locals.put(allocator, name, .{
            .name = name,
            .depth = self.depth,
            .index = self.current.locals.count(),
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

    pub fn resolveCapture(self: *CaptureCtx, allocator: Allocator, name: InternerIdx) void {
        _ = self.resolveCaptureInScope(allocator, name, self.stack.items.len - 1);
    }

    pub fn resolveCaptureInScope(self: *CaptureCtx, allocator: Allocator, name: InternerIdx, scope_index: usize) ?usize {
        if (scope_index == 1) return null;

        const current = &self.stack.items[scope_index];
        if (current.locals.get(name) != null) return null;

        const enclosing = &self.stack.items[scope_index - 1];

        if (enclosing.locals.getPtr(name)) |local| {
            // TODO: Can't close over another closure?
            local.node.?.meta.captured = true;
            return current.addCapture(allocator, name, local.index, true);
        } else {
            if (self.resolveCaptureInScope(allocator, name, scope_index - 1)) |capt_index| {
                return current.addCapture(allocator, name, capt_index, false);
            }
        }

        return null;
    }

    fn resolveLocal(self: *CaptureCtx, name: InternerIdx) ?usize {
        const local = self.current.locals.getPtr(name) orelse return null;
        return local.index;
    }
};

pub fn init(allocator: Allocator, interner: *Interner, ast: *Ast) Self {
    return .{ .allocator = allocator, .ast = ast, .interner = interner };
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

    if (scope.captures.count() > 0) {
        ctx.declareLocal(self.allocator, self.interner.intern(self.ast.toSource(node.name)), null);
    }
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
        .closure => |*e| self.functionCaptures(e, ctx),
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
            ctx.resolveCapture(self.allocator, interned);
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
