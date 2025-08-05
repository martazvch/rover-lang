const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const GenReport = @import("../reporter.zig").GenReport;
const oom = @import("../utils.zig").oom;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const IrBuilder = @import("IrBuilder.zig");
const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const Span = @import("Lexer.zig").Span;
const TM2 = @import("TM2.zig");
const TokenTag = @import("Lexer.zig").Token.Tag;

pub const Type = union(enum) {
    void,
    int,
    float,
    bool,
    str,
    null,
    function: Function,
    structure: Structure,

    pub const Function = struct {
        params: AutoArrayHashMapUnmanaged(InternerIdx, Parameter) = .{},
        return_type: *const Type,
        is_method: bool,

        pub fn proto(self: *const Function, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
            var res: AutoArrayHashMapUnmanaged(usize, bool) = .{};
            res.ensureTotalCapacity(allocator, self.params.count()) catch oom();

            var kv = self.params.iterator();
            while (kv.next()) |entry| {
                res.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.default);
            }

            return res;
        }
    };

    pub const Parameter = struct {
        type: *const Type,
        default: bool = false,
    };

    pub const Structure = struct {
        name: InternerIdx,
        functions: AutoArrayHashMapUnmanaged(usize, Field) = .{},
        fields: AutoArrayHashMapUnmanaged(usize, Field) = .{},
        default_value_fields: usize = 0,

        pub const Field = struct {
            /// Field's type
            type: *const Type,
            /// Has a default value
            default: bool = false,
        };

        pub fn proto(self: *const Structure, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
            var res: AutoArrayHashMapUnmanaged(usize, bool) = .{};
            res.ensureTotalCapacity(allocator, self.fields.count() - self.default_value_fields) catch oom();

            var kv = self.fields.iterator();
            while (kv.next()) |entry| {
                if (!entry.value_ptr.default) {
                    res.putAssumeCapacity(entry.key_ptr.*, false);
                }
            }

            return res;
        }
    };

    pub fn is(self: *const Type, tag: std.meta.Tag(Type)) bool {
        return std.meta.activeTag(self.*) == tag;
    }

    pub fn isNumeric(self: *const Type) bool {
        return self.is(.int) or self.is(.float);
    }

    pub fn isSymbol(self: *const Type) bool {
        return self.is(.function);
    }

    pub fn canCastTo(self: *const Type, other: *const Type) bool {
        return if (self.is(.int) and other.is(.float))
            true
        else
            false;
    }

    // TODO: maybe name + kind + scope index is enough?
    pub fn hash(self: Type, hasher: anytype) void {
        const asBytes = std.mem.asBytes;

        comptime {
            if (@typeInfo(@TypeOf(hasher)) != .pointer) {
                @compileError("You must pass a pointer to a haser");
            }

            if (!@hasDecl(@TypeOf(hasher.*), "update")) {
                @compileError("Hasher must have an 'update' method");
            }
        }

        hasher.update(asBytes(&@intFromEnum(self)));

        switch (self) {
            .void, .int, .float, .bool, .str, .null => {},
            .function => |ty| {
                for (ty.params.values()) |param| {
                    param.type.hash(hasher);
                }
                ty.return_type.hash(hasher);
                hasher.update(asBytes(&@intFromBool(ty.is_method)));
            },
            .structure => |ty| {
                hasher.update(asBytes(&ty.name));
                // for (ty.fields.values()) |f| {
                //     f.type.hash(hasher);
                // }
                // for (ty.functions.values()) |f| {
                //     f.type.hash(hasher);
                // }
            },
            // .pointer => unreachable,
        }
    }

    pub fn toString(self: *const Type, allocator: Allocator, interner: *const Interner) []const u8 {
        var res: std.ArrayListUnmanaged(u8) = .{};
        var writer = res.writer(allocator);

        switch (self.*) {
            .int, .float, .bool, .str, .null, .void => return @tagName(self.*),
            .function => |ty| {
                writer.writeAll("fn (") catch oom();
                for (ty.params.values(), 0..) |p, i| {
                    writer.writeAll(p.type.toString(allocator, interner)) catch oom();
                    if (i != ty.params.count() - 1) writer.writeAll(", ") catch oom();
                }
                writer.writeAll(") -> ") catch oom();
                writer.writeAll(ty.return_type.toString(allocator, interner)) catch oom();
            },
            .structure => |ty| return interner.getKey(ty.name).?,
            // else => unreachable,
        }

        return res.toOwnedSlice(allocator) catch oom();
    }
};

const TypeInterner = struct {
    arena: Allocator,
    interned: AutoHashMapUnmanaged(u64, *Type) = .{},
    cache: Cache,

    const Cache = CreateCache(&.{ .int, .float, .bool, .str, .null, .void });

    pub fn init(arena: Allocator) TypeInterner {
        return .{ .arena = arena, .cache = undefined };
    }

    pub fn CreateCache(comptime types: []const Type) type {
        var fields: []const std.builtin.Type.StructField = &.{};

        inline for (types) |ty| {
            fields = fields ++ .{std.builtin.Type.StructField{
                .name = @tagName(ty),
                .type = *const Type,
                .default_value_ptr = null,
                .is_comptime = false,
                .alignment = 1,
            }};
        }

        return @Type(.{ .@"struct" = .{
            .layout = .auto,
            .fields = fields,
            .decls = &.{},
            .is_tuple = false,
        } });
    }

    pub fn cacheFrequentTypes(self: *TypeInterner) void {
        const tags = std.meta.fields(Type);
        inline for (@typeInfo(Cache).@"struct".fields) |f| {
            inline for (tags) |tag| {
                if (comptime std.mem.eql(u8, f.name, tag.name)) {
                    @field(self.cache, f.name) = self.intern(@unionInit(Type, tag.name, {}));
                }
            }
        }
    }

    pub fn intern(self: *TypeInterner, ty: Type) *Type {
        var hasher = std.hash.Wyhash.init(0);
        ty.hash(&hasher);
        const hash = hasher.final();

        if (self.interned.get(hash)) |interned| {
            return interned;
        }

        const new_type = self.arena.create(Type) catch oom();
        new_type.* = ty;
        self.interned.put(self.arena, hash, new_type) catch oom();

        return new_type;
    }
};

const ScopeStack = struct {
    scopes: ArrayListUnmanaged(Scope) = .{},
    current: *Scope,
    builtins: AutoHashMapUnmanaged(InternerIdx, *const Type) = .{},
    symbol_count: usize = 0,

    pub const empty: ScopeStack = .{ .current = undefined };

    const Scope = struct {
        variables: AutoHashMapUnmanaged(InternerIdx, Variable) = .{},
        symbols: AutoArrayHashMapUnmanaged(InternerIdx, Symbol) = .{},
        offset: usize,

        pub const Symbol = struct { type: *const Type, index: usize };
    };

    pub fn open(self: *ScopeStack, allocator: Allocator, offset_from_child: bool) void {
        const offset = if (offset_from_child) self.current.variables.count() + self.current.offset else 0;
        self.scopes.append(allocator, .{ .offset = offset }) catch oom();
        self.updateCurrent();
    }

    pub fn close(self: *ScopeStack) usize {
        const popped = self.scopes.pop().?;
        self.updateCurrent();
        return popped.variables.count();
    }

    pub const ScopeCaptures = struct {
        count: usize,
        captured: []const usize,
    };

    pub fn closeGetCaptures(self: *ScopeStack, allocator: Allocator) ScopeCaptures {
        const popped = self.scopes.pop().?;
        self.updateCurrent();

        var captured: ArrayListUnmanaged(usize) = .{};

        var iter = popped.variables.valueIterator();
        while (iter.next()) |v| {
            if (v.kind == .captured) {
                captured.append(allocator, v.index) catch oom();
            }
        }

        return .{ .count = popped.variables.count(), .captured = captured.toOwnedSlice(allocator) catch oom() };
    }

    pub fn initGlobalScope(self: *ScopeStack, allocator: Allocator, interner: *Interner, type_interner: *const TypeInterner) void {
        self.open(allocator, false);
        const builtins = std.meta.fields(TypeInterner.Cache);
        self.builtins.ensureUnusedCapacity(allocator, builtins.len) catch oom();

        inline for (builtins) |builtin| {
            self.builtins.putAssumeCapacity(interner.intern(builtin.name), @field(type_interner.cache, builtin.name));
        }
    }

    fn updateCurrent(self: *ScopeStack) void {
        self.current = &self.scopes.items[self.scopes.items.len - 1];
    }

    pub fn isGlobal(self: *ScopeStack) bool {
        return self.scopes.items.len == 1;
    }

    pub fn declareVar(self: *ScopeStack, allocator: Allocator, name: InternerIdx, ty: *const Type, initialized: bool) error{TooManyLocals}!usize {
        if (self.current.variables.count() > 255 and !self.isGlobal()) {
            return error.TooManyLocals;
        }

        const index = self.current.variables.count();
        self.current.variables.put(allocator, name, .{
            .type = ty,
            .kind = if (self.isGlobal()) .global else .local,
            .initialized = initialized,
            .index = index,
        }) catch oom();

        return index;
    }

    /// Tries to retreive a variable from scopes and the local offset its scope
    pub fn getVariable(self: *const ScopeStack, name: InternerIdx) ?struct { *Variable, usize } {
        var i = self.scopes.items.len;

        while (i > 0) {
            i -= 1;
            const scope = &self.scopes.items[i];

            if (scope.variables.getPtr(name)) |variable| {
                return .{ variable, scope.offset };
            }
        }

        return null;
    }

    pub fn declareSymbol(self: *ScopeStack, allocator: Allocator, name: InternerIdx, ty: *const Type) void {
        self.current.symbols.put(allocator, name, .{ .type = ty, .index = self.symbol_count }) catch oom();
        self.symbol_count += 1;
    }

    /// Forward declares a symbol without incrementing global symbol count
    pub fn forwardDeclareSymbol(self: *ScopeStack, allocator: Allocator, name: InternerIdx) *Scope.Symbol {
        self.current.symbols.put(allocator, name, .{ .type = undefined, .index = self.symbol_count }) catch oom();
        self.symbol_count += 1;

        return self.current.symbols.getPtr(name).?;
    }

    /// Removes symbol name from **current** scope
    pub fn removeSymbol(self: *ScopeStack, name: InternerIdx) void {
        _ = self.current.symbols.fetchOrderedRemove(name);
    }

    pub fn getSymbol(self: *const ScopeStack, name: InternerIdx) ?*Scope.Symbol {
        var i = self.scopes.items.len;

        while (i > 0) {
            i -= 1;
            const scope = &self.scopes.items[i];

            if (scope.symbols.getPtr(name)) |sym| {
                return sym;
            }
        }

        return null;
    }

    pub fn getType(self: *ScopeStack, name: InternerIdx) ?*const Type {
        if (self.builtins.get(name)) |builtin| {
            return builtin;
        } else if (self.getSymbol(name)) |sym| {
            return sym.type;
        }

        return null;
    }

    pub fn isInScope(self: *const ScopeStack, name: InternerIdx) bool {
        return self.current.variables.get(name) != null;
    }
};

fn declareVariable(self: *Self, name: InternerIdx, ty: *const Type, initialized: bool, span: Span) Error!usize {
    return self.scope.declareVar(self.allocator, name, ty, initialized) catch self.err(.too_many_locals, span);
}

fn declareSymbol(self: *Self, name: InternerIdx, ty: *const Type) void {
    self.scope.declareSymbol(self.allocator, name, ty);

    if (self.scope.isGlobal()) {
        self.globals.append(self.allocator, self.scope.symbol_count) catch oom();
    }
}

const Context = struct {
    side: enum { lhs, rhs } = .lhs,
    fn_type: ?*Type = null,
    struct_type: ?*const Type = null,
    ref_count: bool = false,
    cow: bool = false,
    allow_partial: bool = true,
    returns: bool = false,

    const ContextSnapshot = struct {
        saved: Context,
        ctx: *Context,

        pub fn restore(self: ContextSnapshot) void {
            self.ctx.* = self.saved;
        }
    };

    pub fn snapshot(self: *Context) ContextSnapshot {
        return .{ .saved = self.*, .ctx = self };
    }

    pub fn reset(self: *Context) void {
        self.* = .{};
    }
};

const Variable = struct {
    /// Variable's type
    type: *const Type,
    /// Kind: global, local or captured
    kind: enum { local, global, captured },
    /// Is initialized
    initialized: bool,
    /// Index of declaration
    index: usize = 0,
};

const Self = @This();
const Error = error{Err};
const Result = Error!*const Type;
pub const AnalyzerReport = GenReport(AnalyzerMsg);

arena: std.heap.ArenaAllocator,
allocator: Allocator,
interner: *Interner,
errs: ArrayListUnmanaged(AnalyzerReport),
warns: ArrayListUnmanaged(AnalyzerReport),

ast: *const Ast,
scope: ScopeStack,
// tm: TM2,
type_interner: TypeInterner,
ir_builder: IrBuilder,
main: ?usize,
globals: ArrayListUnmanaged(usize),

cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },

pub fn init(self: *Self, allocator: Allocator, interner: *Interner) void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();
    self.interner = interner;
    self.scope = .empty;
    self.errs = .{};
    self.warns = .{};
    self.ir_builder = .init(allocator);
    // self.tm = .init(allocator);
    // self.tm.declareBuiltinTypes(interner);
    self.type_interner = .init(self.allocator);
    self.type_interner.cacheFrequentTypes();
    self.scope.initGlobalScope(allocator, interner, &self.type_interner);
    self.main = null;
    self.globals = .{};

    self.cached_names = .{
        .empty = self.interner.intern(""),
        .main = self.interner.intern("main"),
        .std = self.interner.intern("std"),
        .self = self.interner.intern("self"),
        .Self = self.interner.intern("Self"),
        .init = self.interner.intern("init"),
    };
}

pub fn deinit(self: *Self) void {
    self.tm.deinit();
}

fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    self.errs.append(self.allocator, AnalyzerReport.err(kind, span)) catch oom();
    return error.Err;
}

fn warn(self: *Self, kind: AnalyzerMsg, span: Span) void {
    self.warns.append(self.allocator, AnalyzerReport.warn(kind, span)) catch oom();
}

fn makeInstruction(self: *Self, data: Instruction.Data, mode: IrBuilder.Mode) void {
    const instr: Instruction = .{ .data = data, .offset = 0 };
    self.ir_builder.emit(instr, mode);
}

pub fn analyze(self: *Self, ast: *const Ast) Error!void {
    self.ast = ast;
    var ctx: Context = .{};

    for (ast.nodes) |*node| {
        const node_type = self.analyzeNode(node, &ctx) catch |e| {
            switch (e) {
                // error.too_many_types => self.err(.too_many_types, ctx.ast.getSpan(node)) catch continue,
                error.Err => continue,
            }
        };

        ctx.reset();

        if (!self.isVoid(node_type)) {
            self.err(.unused_value, self.ast.getSpan(node)) catch {};
        }
    }

    return;
    // In REPL mode, no need for main function
    // if (self.repl)
    //     return
    // else if (self.main == null) self.err(.no_main, .{ .start = 0, .end = 0 }) catch {};
}

fn analyzeNode(self: *Self, node: *const Node, ctx: *Context) Result {
    // if (self.scope_depth == 0 and !self.repl and !self.isPure(node.*)) {
    //     // TODO: add block, not allowed to have local scopes in global scope
    //     return if (std.meta.activeTag(node.*) == .expr and std.meta.activeTag(node.expr.*) == .@"return")
    //         self.err(.return_outside_fn, self.ast.getSpan(node))
    //     else
    //         self.err(.unpure_in_global, self.ast.getSpan(node));
    // }
    //
    // self.state.chain.reset();

    switch (node.*) {
        .assignment => |*n| try self.assignment(n, ctx),
        .discard => |n| try self.discard(n, ctx),
        .fn_decl => |*n| _ = try self.fnDeclaration(n, ctx),
        .multi_var_decl => |*n| try self.multiVarDecl(n, ctx),
        .print => |n| try self.print(n, ctx),
        .struct_decl => |*n| try self.structDecl(n, ctx),
        // .use => |*n| try self.use(n),
        .var_decl => |*n| try self.varDeclaration(n, ctx),
        .@"while" => |*n| try self.whileStmt(n, ctx),
        .expr => |n| return self.analyzeExpr(n, ctx),
        else => unreachable,
    }

    return self.type_interner.intern(.void);
}

fn assignment(self: *Self, node: *const Ast.Assignment, ctx: *Context) !void {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    ctx.allow_partial = false;
    const index = self.ir_builder.reserveInstr();

    ctx.side = .rhs;
    const value_type = try self.analyzeExpr(node.value, ctx);
    ctx.side = .lhs;

    const assigne_type, const cow = switch (node.assigne.*) {
        .literal => |*e| b: {
            if (e.tag != .identifier) {
                return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne));
            }

            var assigne = try self.expectVariableIdentifier(e.idx);

            if (!assigne.initialized) assigne.initialized = true;

            break :b .{ assigne.type, false };
        },
        else => b: {
            const ty = try self.analyzeExpr(node.assigne, ctx);
            try self.isAssignmentAllowed(ty, self.ast.getSpan(node.assigne));

            break :b .{ ty, false };
        },
        // .field => |*e| b: {
        //     var field_infos = try self.field(e, true);
        //     const field_type = &field_infos.field;
        //
        //     if (field_infos.kind == .method) {
        //         return self.err(.assign_to_method, self.ast.getSpan(e.field));
        //     }
        //
        //     // TODO: why false?
        //     break :b .{ field_type.*, false };
        // },
        // .array_access => |*e| .{ try self.arrayAccess(e), true },
        // else => return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne)),
    };
    const coherence = try self.performTypeCoercion(assigne_type, value_type, false, self.ast.getSpan(node.value));

    self.makeInstruction(.{ .assignment = .{ .cast = coherence.cast, .cow = cow } }, .{ .setAt = index });
}

fn isAssignmentAllowed(self: *Self, assigne_type: *const Type, err_span: Span) Error!void {
    switch (assigne_type.*) {
        .function, .structure => return self.err(.invalid_assign_target, err_span),
        else => {},
    }
}

fn discard(self: *Self, expr: *const Expr, ctx: *Context) !void {
    self.makeInstruction(.{ .discard = undefined }, .add);
    const discarded = try self.analyzeExpr(expr, ctx);

    if (self.isVoid(discarded)) return self.err(.void_discard, self.ast.getSpan(expr));
}

fn checkFunctionCaptures(self: *Self, node: *const Ast.FnDecl) void {
    var locals: AutoHashMapUnmanaged(InternerIdx, usize) = .{};

    for (node.body.nodes) |n| {
        switch (n) {
            .var_decl => |v| {
                const interned = self.interner.intern(self.ast.toSource(v.name));

                // If already declared, we consider we are in another scope
                const index = if (locals.contains(interned)) locals.count() + 1 else locals.count();
                locals.put(self.allocator, interned, index) catch oom();
            },
            .expr => |expr| {
                const c = if (expr.* == .closure) expr.closure else continue;

                for (c.body.nodes) |cn| {
                    const ce = if (cn == .expr) cn.expr else continue;
                    const lit = if (ce.* == .literal) ce.literal else continue;
                    if (lit.tag != .identifier) continue;

                    const interned = self.interner.intern(self.ast.toSource(lit.idx));
                    if (locals.contains(interned)) {
                        std.debug.print("Contained\n", .{});
                    }
                }
            },
            else => {},
        }
    }
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl, ctx: *Context) Error!*const Type {
    self.checkFunctionCaptures(node);

    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const name = try self.internIfNotInScope(node.name);

    const fn_idx = self.ir_builder.reserveInstr();
    // TODO: delete and merge this into other Instructions
    self.ir_builder.emit(.{ .data = .{ .name = name } }, .add);

    // Forward declaration in outer scope for recursion
    var sym = self.scope.forwardDeclareSymbol(self.allocator, name);
    var fn_type: Type.Function = .{
        .return_type = undefined,
        .is_method = ctx.struct_type != null,
    };

    self.scope.open(self.allocator, false);
    errdefer _ = self.scope.close();

    const params, const default_count = try self.fnParams(node.params, ctx);
    fn_type.params = params;

    const return_type = try self.checkAndGetType(node.return_type, ctx);
    fn_type.return_type = return_type;
    const interned_type = self.type_interner.intern(.{ .function = fn_type });

    // Update type for resolution in function's body
    ctx.fn_type = interned_type;
    sym.type = interned_type;
    const len = try self.fnBody(node.body.nodes, &fn_type, ctx);

    _ = self.scope.close();

    // If in a structure declaration, we remove the symbol as it's gonna live inside the structure
    if (ctx.struct_type != null) {
        self.scope.removeSymbol(name);
    }

    if (name == self.cached_names.main and self.scope.isGlobal()) {
        self.main = sym.index;
    }

    self.makeInstruction(
        .{ .fn_decl = .{
            .index = sym.index,
            .body_len = len,
            .default_params = default_count,
            .return_kind = if (ctx.returns) .explicit else if (self.isVoid(interned_type)) .implicit_void else .implicit_value,
        } },
        .{ .setAt = fn_idx },
    );

    return interned_type;
}

fn fnParams(
    self: *Self,
    params: []Ast.Param,
    ctx: *Context,
) Error!struct { AutoArrayHashMapUnmanaged(InternerIdx, Type.Parameter), usize } {
    var params_type: AutoArrayHashMapUnmanaged(InternerIdx, Type.Parameter) = .{};
    params_type.ensureTotalCapacity(self.allocator, params.len) catch oom();
    var default_count: usize = 0;

    for (params, 0..) |*p, i| {
        const param_name = self.interner.intern(self.ast.toSource(p.name));

        if (i == 0 and param_name == self.cached_names.self) {
            const struct_type = ctx.struct_type orelse {
                @panic("Self outside of structure");
            };

            _ = try self.declareVariable(param_name, struct_type, true, .zero);
            params_type.putAssumeCapacity(param_name, .{ .type = struct_type, .default = false });
            continue;
        }

        if (self.scope.isInScope(param_name)) {
            return self.err(
                .{ .duplicate_param = .{ .name = self.ast.toSource(p.name) } },
                self.ast.getSpan(p.name),
            );
        }

        var param_type = try self.checkAndGetType(p.typ, ctx);
        if (p.value) |val| {
            default_count += 1;
            param_type = try self.defaultValue(param_type, val, ctx);
        }

        if (self.isVoid(param_type)) {
            return self.err(.void_param, self.ast.getSpan(p.name));
        }

        _ = try self.declareVariable(param_name, param_type, true, self.ast.getSpan(p.name));
        params_type.putAssumeCapacity(param_name, .{ .type = param_type, .default = p.value != null });
    }

    return .{ params_type, default_count };
}

fn fnBody(self: *Self, body: []Node, fn_type: *const Type.Function, ctx: *Context) Error!usize {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    var had_err = false;
    var final_type: *const Type = self.type_interner.cache.void;
    var deadcode_start: usize = 0;
    var deadcode_count: usize = 0;
    const len = body.len;

    for (body, 0..) |*n, i| {
        // If previous statement returned, it's only dead code now
        if (deadcode_start == 0 and ctx.returns) {
            self.warn(.dead_code, self.ast.getSpan(n));
            deadcode_start = self.ir_builder.instructions.len;
            deadcode_count = len - i;
        }

        // If last statement, we don't allow partial anymore (for return)
        // Usefull for 'if' for example, in this case we want all the branches to return something
        if (i == len - 1) ctx.allow_partial = false;

        // We try to analyze the whole body
        const ty = self.analyzeNode(n, ctx) catch {
            had_err = true;
            continue;
        };

        // If we analyze dead code, we don't update the type
        if (deadcode_start == 0) final_type = ty;

        // If last expression produced a value and that it wasn't the last one and it wasn't a return, error
        if (!self.isVoid(final_type) and i != len - 1 and !ctx.returns) {
            self.err(.unused_value, self.ast.getSpan(n)) catch {};
        }
    }

    // We strip unused instructions for them not to be compiled
    if (deadcode_start > 0) {
        self.ir_builder.instructions.shrinkRetainingCapacity(deadcode_start);
    }

    if (!had_err and final_type != fn_type.return_type) {
        return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(fn_type.return_type),
                .found = self.getTypeName(final_type),
            } },
            self.ast.getSpan(body[body.len - 1]),
        );
    }

    return len - deadcode_count;
}

fn defaultValue(self: *Self, decl_type: *const Type, default_value: *const Expr, ctx: *Context) Result {
    const value_type = try self.analyzeExpr(default_value, ctx);
    const coerce = try self.performTypeCoercion(decl_type, value_type, true, self.ast.getSpan(default_value));
    return coerce.type;
}

fn print(self: *Self, expr: *const Expr, ctx: *Context) Error!void {
    self.makeInstruction(.print, .add);
    const ty = try self.analyzeExpr(expr, ctx);

    if (self.isVoid(ty)) {
        return self.err(.void_print, self.ast.getSpan(expr));
    }
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) Error!void {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const name = try self.internIfNotInScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);
    const index = self.ir_builder.reserveInstr();

    var initialized = false;
    var cast = false;

    if (node.value) |value| {
        initialized = true;
        ctx.allow_partial = false;
        ctx.side = .rhs;

        const value_type = try self.analyzeExpr(value, ctx);
        const coherence = try self.performTypeCoercion(checked_type, value_type, true, self.ast.getSpan(value));
        checked_type = coherence.type;
        cast = coherence.cast;
    }

    const decl_index = try self.declareVariable(name, checked_type, initialized, self.ast.getSpan(node.name));
    self.makeInstruction(
        .{ .var_decl = .{
            .has_value = initialized,
            .cast = cast,
            .variable = .{ .index = decl_index, .scope = if (self.scope.isGlobal()) .global else .local },
        } },
        .{ .setAt = index },
    );
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl, ctx: *Context) Error!void {
    self.makeInstruction(.{ .multiple_var_decl = node.decls.len }, .add);

    for (node.decls) |*n| {
        try self.varDeclaration(n, ctx);
    }
}

fn structDecl(self: *Self, node: *const Ast.StructDecl, ctx: *Context) !void {
    const name = self.interner.intern(self.ast.toSource(node.name));

    if (self.scope.isInScope(name)) {
        return self.err(.{ .already_declared_struct = .{ .name = self.ast.toSource(node.name) } }, self.ast.getSpan(node));
    }

    // We forward declare for self referencing
    const sym = self.scope.forwardDeclareSymbol(self.allocator, name);

    const index = self.ir_builder.reserveInstr();
    // TODO: merge as function's name
    self.makeInstruction(.{ .name = name }, .add);

    self.scope.open(self.allocator, false);
    defer _ = self.scope.close();

    var ty: Type.Structure = .{ .name = name };
    ty.fields.ensureTotalCapacity(self.allocator, node.fields.len) catch oom();
    ty.functions.ensureTotalCapacity(self.allocator, @intCast(node.functions.len)) catch oom();

    // Create type before functions to allow 'self' to refer to the structure
    const interned_type = self.type_interner.intern(.{ .structure = ty });
    ctx.struct_type = interned_type;
    const interned_struct = &interned_type.structure;

    try self.structureFields(node.fields, interned_struct, ctx);
    sym.type = interned_type;

    for (node.functions) |*f| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        const fn_type = try self.fnDeclaration(f, ctx);
        interned_struct.functions.putAssumeCapacity(fn_name, .{ .type = fn_type });
        sym.type = interned_type;
    }

    self.makeInstruction(
        .{ .struct_decl = .{
            .index = sym.index,
            .fields_count = node.fields.len,
            .default_fields = interned_struct.default_value_fields,
            .func_count = node.functions.len,
        } },
        .{ .setAt = index },
    );
}

fn structureFields(self: *Self, fields: []const Ast.VarDecl, ty: *Type.Structure, ctx: *Context) Error!void {
    for (fields) |*f| {
        var struct_field: Type.Structure.Field = .{ .type = undefined };
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (ty.fields.get(field_name) != null) {
            return self.err(
                .{ .already_declared_field = .{ .name = self.ast.toSource(f.name) } },
                self.ast.getSpan(f.name),
            );
        }

        const field_type = try self.checkAndGetType(f.typ, ctx);
        const field_value_type = if (f.value) |value| blk: {
            // if (!self.isPure(value.*)) {
            //     return self.err(.{ .unpure_default = .new(.field) }, self.ast.getSpan(value));
            // }

            struct_field.default = true;
            ty.default_value_fields += 1;

            break :blk try self.analyzeExpr(value, ctx);
        } else self.type_interner.cache.void;

        if (!self.isVoid(field_value_type) and !self.isVoid(field_type) and field_value_type != field_type) {
            if (field_value_type.canCastTo(field_type)) {
                self.makeInstruction(.{ .cast = .float }, .add);
            } else return self.err(
                .{ .default_value_type_mismatch = .new(self.getTypeName(field_type), self.getTypeName(field_value_type), .field) },
                self.ast.getSpan(f.name),
            );
        }

        // From parsing, we know that there is either a type or default value. If no declared type, we take
        // the one from the default value
        struct_field.type = if (self.isVoid(field_type)) field_value_type else field_type;
        ty.fields.putAssumeCapacity(field_name, struct_field);
    }
}

fn whileStmt(self: *Self, node: *const Ast.While, ctx: *Context) Error!void {
    self.makeInstruction(.{ .@"while" = undefined }, .add);
    const cond_type = try self.analyzeExpr(node.condition, ctx);

    if (!cond_type.is(.bool)) return self.err(
        .{ .non_bool_cond = .{
            .what = "while",
            .found = self.getTypeName(cond_type),
        } },
        self.ast.getSpan(node.condition),
    );

    const body_type = try self.block(&node.body, ctx);

    if (!self.isVoid(body_type)) return self.err(
        .{ .non_void_while = .{ .found = self.getTypeName(body_type) } },
        self.ast.getSpan(node.body),
    );
}

fn analyzeExpr(self: *Self, expr: *const Expr, ctx: *Context) Result {
    return switch (expr.*) {
        // .array => |*e| self.array(e),
        // .array_access => |*e| self.arrayAccess(e),
        .block => |*e| self.block(e, ctx),
        .binop => |*e| self.binop(e, ctx),
        .closure => |*e| self.closure(e, ctx),
        .field => |*e| (try self.field(e, ctx)).type,
        .fn_call => |*e| self.call(e, ctx),
        .grouping => |*e| self.analyzeExpr(e.expr, ctx),
        .@"if" => |*e| self.ifExpr(e, ctx),
        .literal => |*e| self.literal(e, ctx),
        // .named_arg => unreachable,
        .@"return" => |*e| self.returnExpr(e, ctx),
        .struct_literal => |*e| self.structLiteral(e, ctx),
        .unary => |*e| self.unary(e, ctx),
        else => unreachable,
    };
}

// TODO: handle dead code eleminitaion so that it can be used in function's body?
fn block(self: *Self, expr: *const Ast.Block, ctx: *Context) Result {
    self.scope.open(self.allocator, true);
    errdefer _ = self.scope.close();

    const index = self.ir_builder.reserveInstr();
    var final_type = self.type_interner.cache.void;

    for (expr.nodes, 0..) |*node, i| {
        final_type = try self.analyzeNode(node, ctx);

        if (!self.isVoid(final_type) and i != expr.nodes.len - 1) {
            return self.err(.unused_value, expr.span);
        }
    }

    const count = self.scope.close();
    // TODO: protect cast
    self.makeInstruction(
        .{ .block = .{
            .length = expr.nodes.len,
            .pop_count = @intCast(count),
            .is_expr = !self.isVoid(final_type),
        } },
        .{ .setAt = index },
    );

    return final_type;
}

fn binop(self: *Self, expr: *const Ast.Binop, ctx: *Context) Result {
    const op = expr.op;
    const index = self.ir_builder.reserveInstr();

    const lhs = try self.analyzeExpr(expr.lhs, ctx);
    const rhs = try self.analyzeExpr(expr.rhs, ctx);

    if (isStringConcat(op, lhs, rhs)) {
        self.makeInstruction(.{ .binop = .{ .op = .add_str } }, .{ .setAt = index });
        return self.type_interner.cache.str;
    } else if (isStringRepeat(op, lhs, rhs)) {
        self.makeInstruction(
            .{ .binop = .{ .op = .mul_str, .cast = if (rhs.is(.int)) .rhs else .lhs } },
            .{ .setAt = index },
        );
        return self.type_interner.cache.str;
    }

    var data = Instruction.Binop{ .op = undefined };
    var result_type = lhs;

    switch (op) {
        .plus, .slash, .star, .minus => {
            try self.expectNumeric(lhs, self.ast.getSpan(expr.lhs));
            try self.expectNumeric(rhs, self.ast.getSpan(expr.rhs));
            const info = self.getArithmeticOp(op, lhs, rhs, expr);
            data = info.instr;
            result_type = info.result_type;
        },
        .equal_equal, .bang_equal => {
            const info = try self.getEqualityOp(op, lhs, rhs, expr);
            data = info.instr;
            result_type = info.result_type;
        },
        .greater_equal, .greater, .less_equal, .less => {
            try self.expectNumeric(lhs, self.ast.getSpan(expr.lhs));
            try self.expectNumeric(rhs, self.ast.getSpan(expr.rhs));
            const info = try self.getComparisonOp(op, lhs, rhs, expr);
            data = info.instr;
            result_type = info.result_type;
        },
        .@"and", .@"or" => {
            try self.checkBooleanLogic(lhs, rhs, expr);
            data.op = if (op == .@"and") .@"and" else .@"or";
            result_type = self.type_interner.cache.bool;
        },
        else => unreachable,
    }

    self.makeInstruction(.{ .binop = data }, .{ .setAt = index });
    return result_type;
}

fn closure(self: *Self, expr: *const Ast.Closure, ctx: *Context) Result {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const closure_idx = self.ir_builder.reserveInstr();

    self.scope.open(self.allocator, false);
    errdefer _ = self.scope.close();

    const params, const default_count = try self.fnParams(expr.params, ctx);

    // Update type for resolution in function's body
    const closure_type: Type.Function = .{
        .params = params,
        .return_type = try self.checkAndGetType(expr.return_type, ctx),
        .is_method = ctx.struct_type != null,
    };
    const interned_type = self.type_interner.intern(.{ .function = closure_type });

    ctx.fn_type = interned_type;
    const len = try self.fnBody(expr.body.nodes, &closure_type, ctx);

    const scope_stats = self.scope.closeGetCaptures(self.allocator);

    self.makeInstruction(
        .{ .closure = .{
            .body_len = len,
            .default_params = default_count,
            .captures = scope_stats.captured,
            .return_kind = if (ctx.returns) .explicit else if (self.isVoid(interned_type)) .implicit_void else .implicit_value,
        } },
        .{ .setAt = closure_idx },
    );
    return interned_type;
}

fn isStringConcat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .plus and lhs.is(.str) and rhs.is(.str);
}

fn isStringRepeat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .star and ((lhs.is(.str) and rhs.is(.int)) or (lhs.is(.int) and rhs.is(.str)));
}

fn expectNumeric(self: *Self, ty: *const Type, err_span: Span) Error!void {
    if (!ty.isNumeric()) {
        return self.err(AnalyzerMsg.invalidArithmetic(self.getTypeName(ty)), err_span);
    }
}

const ArithmeticResult = struct {
    instr: Instruction.Binop,
    result_type: *const Type,
};

fn getArithmeticOp(self: *Self, op: TokenTag, lhs: *const Type, rhs: *const Type, expr: *const Ast.Binop) ArithmeticResult {
    var data = Instruction.Binop{ .op = switch (op) {
        .plus => .add_float,
        .slash => .div_float,
        .star => .mul_float,
        .minus => .sub_float,
        else => unreachable,
    } };
    var result_type = lhs;

    switch (lhs.*) {
        .float => {
            if (rhs.is(.int)) {
                data.cast = .rhs;
                self.warn(AnalyzerMsg.implicitCast("right hand side", self.getTypeName(lhs)), self.ast.getSpan(expr.rhs));
            }
        },
        .int => {
            if (rhs.is(.float)) {
                data.cast = .lhs;
                self.warn(AnalyzerMsg.implicitCast("left hand side", self.getTypeName(rhs)), self.ast.getSpan(expr.lhs));
                result_type = self.type_interner.cache.float;
            } else {
                data.op = switch (op) {
                    .plus => .add_int,
                    .slash => .div_int,
                    .star => .mul_int,
                    .minus => .sub_int,
                    else => unreachable,
                };
            }
        },
        else => unreachable,
    }

    return .{ .instr = data, .result_type = result_type };
}

fn getEqualityOp(self: *Self, op: TokenTag, lhs: *const Type, rhs: *const Type, expr: *const Ast.Binop) Error!ArithmeticResult {
    var data = Instruction.Binop{ .op = undefined };

    data.op = switch (op) {
        .equal_equal => switch (lhs.*) {
            .bool => .eq_bool,
            .int => .eq_int,
            .float => .eq_float,
            .str => .eq_str,
            else => .eq_str,
        },
        .bang_equal => switch (lhs.*) {
            .bool => .ne_bool,
            .int => .ne_int,
            .float => .ne_float,
            .str => .ne_str,
            else => .ne_str,
        },
        else => unreachable,
    };

    if (lhs != rhs) {
        if ((lhs.is(.int) and rhs.is(.float)) or (lhs.is(.float) and rhs.is(.int))) {
            if (lhs.is(.int)) {
                data.cast = .lhs;
                self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
            } else {
                data.cast = .rhs;
                self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
            }

            data.op = if (op == .equal_equal) .eq_float else .ne_float;
        } else {
            return self.err(
                AnalyzerMsg.invalidCmp(self.getTypeName(lhs), self.getTypeName(rhs)),
                self.ast.getSpan(expr),
            );
        }
    } else {
        if (lhs.is(.float)) {
            self.warn(.float_equal, self.ast.getSpan(expr));
        }
    }

    return .{ .instr = data, .result_type = self.type_interner.cache.bool };
}

fn getComparisonOp(self: *Self, op: TokenTag, lhs: *const Type, rhs: *const Type, expr: *const Ast.Binop) Error!ArithmeticResult {
    const result_type = self.type_interner.cache.bool;

    return switch (lhs.*) {
        .float => switch (rhs.*) {
            .float => {
                self.warn(.float_equal, self.ast.getSpan(expr));
                return .{ .instr = .{ .op = floatCompOp(op), .cast = .none }, .result_type = result_type };
            },
            .int => {
                self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
                return .{ .instr = .{ .op = floatCompOp(op), .cast = .rhs }, .result_type = result_type };
            },
            else => unreachable,
        },
        .int => switch (rhs.*) {
            .float => {
                self.warn(.float_equal_cast, self.ast.getSpan(expr.lhs));
                return .{ .instr = .{ .op = floatCompOp(op), .cast = .lhs }, .result_type = result_type };
            },
            .int => return .{ .instr = .{ .op = intCompOp(op), .cast = .none }, .result_type = result_type },
            else => unreachable,
        },
        else => unreachable,
    };
}

fn floatCompOp(op: TokenTag) Instruction.Binop.Op {
    return switch (op) {
        .less => .lt_float,
        .less_equal => .le_float,
        .greater => .gt_float,
        .greater_equal => .ge_float,
        else => unreachable,
    };
}

fn intCompOp(op: TokenTag) Instruction.Binop.Op {
    return switch (op) {
        .less => .lt_int,
        .less_equal => .le_int,
        .greater => .gt_int,
        .greater_equal => .ge_int,
        else => unreachable,
    };
}

fn checkBooleanLogic(self: *Self, lhs: *const Type, rhs: *const Type, expr: *const Ast.Binop) Error!void {
    if (!lhs.is(.bool)) {
        return self.err(.{ .invalid_logical = .{ .found = self.getTypeName(lhs) } }, self.ast.getSpan(expr.lhs));
    }
    if (!rhs.is(.bool)) {
        return self.err(.{ .invalid_logical = .{ .found = self.getTypeName(rhs) } }, self.ast.getSpan(expr.rhs));
    }
}

const FieldResult = struct {
    type: *const Type,
    is_value: bool,
};

/// Returns the type of the callee and if it's a value, not a type
fn field(self: *Self, expr: *const Ast.Field, ctx: *Context) Error!FieldResult {
    const index = self.ir_builder.reserveInstr();
    const field_result: FieldResult = switch (expr.structure.*) {
        .field => |*e| try self.field(e, ctx),
        .literal => |e| b: {
            const ty, const kind = try self.identifier(e.idx, true, ctx);
            break :b .{ .type = ty, .is_value = kind == .variable };
        },
        else => .{ .type = try self.analyzeExpr(expr.structure, ctx), .is_value = true },
    };

    const field_type, const field_index = switch (field_result.type.*) {
        // .module => |ty| {},
        .structure => |*ty| try self.structureAccess(expr.field, ty),
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.getTypeName(field_result.type) } },
            self.ast.getSpan(expr.structure),
        ),
    };

    const kind: Instruction.Field.Kind = switch (field_type.*) {
        // TODO: create just a 'function'. For now we need this because we get static method from
        // symbols that are loaded on stack, not in register so we need a separate logic
        .function => if (field_result.is_value) .method else .static_method,
        else => .field,
    };

    self.makeInstruction(
        .{ .field = .{ .index = field_index, .kind = kind, .rc_action = .none } },
        .{ .setAt = index },
    );

    return .{ .type = field_type, .is_value = field_result.is_value };
}

fn structureAccess(self: *Self, field_tk: Ast.TokenIndex, struct_type: *const Type.Structure) Error!struct { *const Type, usize } {
    const text = self.ast.toSource(field_tk);
    const field_name = self.interner.intern(text);

    return if (struct_type.fields.getPtr(field_name)) |f|
        .{ f.type, struct_type.fields.getIndex(field_name).? }
    else if (struct_type.functions.getPtr(field_name)) |f|
        .{ f.type, struct_type.functions.getIndex(field_name).? }
    else
        self.err(.{ .undeclared_field_access = .{ .name = text } }, self.ast.getSpan(field_tk));
}

// fn analyzeChain(self: *Self, expr: *const Ast.Expr, ctx: *Context) Error!struct { ?*const Type, *const Type } {
//     var prev: ?*const Type = null;
//     _ = &prev;
//     var current: *const Type = undefined;
//
//     while (true) {
//         current = switch (expr.*) {
//             .field => unreachable,
//             else => .{ null, try self.analyzeExpr(expr, ctx) },
//         };
//     }
// }

// fn resolveMethodCall(self: *Self, callee: *const Ast.Field, ctx: *Context) Result {}

fn call(self: *Self, expr: *const Ast.FnCall, ctx: *Context) Result {
    const index = self.ir_builder.reserveInstr();

    var args: ArrayListUnmanaged(*const Expr) = .{};
    defer args.deinit(self.allocator);

    // if (expr.callee.* == .field) {
    //     args.append(self.allocator, expr.callee.field.structure) catch oom();
    // }

    const callee = switch (expr.callee.*) {
        .field => |*f| b: {
            const ty = try self.field(f, ctx);

            if (ty.is_value) {
                args.append(self.allocator, expr.callee.field.structure) catch oom();
            }

            break :b ty.type;
        },
        else => try self.analyzeExpr(expr.callee, ctx),
    };

    // const callee = try self.analyzeExpr(expr.callee, ctx);

    if (!callee.is(.function)) {
        return self.err(.invalid_call_target, self.ast.getSpan(expr));
    }

    args.appendSlice(self.allocator, expr.args) catch oom();

    const arity, const default_count = try self.fnArgsList(args.items, &callee.function, self.ast.getSpan(expr), ctx);

    // const arity, const default_count = args_list: {
    //     // Desugar foo.bar() into bar(foo) by putting foo back onto stack
    //     if (expr.callee.* == .field) {
    //         _ = try self.analyzeExpr(expr.callee.field.structure, ctx);
    //         const arity, const default_count = try self.fnArgsList(expr, &callee.function, true, ctx);
    //         break :args_list .{ arity + 1, default_count };
    //     } else {
    //         break :args_list try self.fnArgsList(expr, &callee.function, false, ctx);
    //     }
    // };

    // TODO: protect casts
    self.makeInstruction(
        .{ .call = .{ .arity = @intCast(arity), .default_count = @intCast(default_count), .invoke = expr.callee.* == .field } },
        .{ .setAt = index },
    );

    return callee.function.return_type;

    // const index = self.ir_builder.reserveInstr();
    // // if (expr.callee.* == .field) {
    // //     try self.resolveMethodCall(expr.callee.field);
    // // }
    // const callee = try self.analyzeExpr(expr.callee, ctx);
    //
    // if (!callee.is(.function)) {
    //     return self.err(.invalid_call_target, self.ast.getSpan(expr));
    // }
    //
    // const arity, const default_count = try self.fnArgsList(expr, &callee.function, ctx);
    // // TODO: protect casts
    // self.makeInstruction(
    //     .{ .call = .{ .arity = @intCast(arity), .default_count = @intCast(default_count), .invoke = expr.callee.* == .field } },
    //     .{ .setAt = index },
    // );
    //
    // return callee.function.return_type;
}

// fn fnArgsList(self: *Self, callee: *const Ast.FnCall, ty: *const Type.Function, ctx: *Context) Error!struct { usize, usize } {
fn fnArgsList(self: *Self, args: []*const Expr, ty: *const Type.Function, err_span: Span, ctx: *Context) Error!struct { usize, usize } {
    // If it's a bound method, 'self' is implictly inside the function so we skip it
    // const offset = @intFromBool(ty.kind == .method);
    // const offset = @intFromBool(implicit_first);
    const offset = 0;
    // const param_count = ty.params.count() - offset;
    const param_count = ty.params.count();

    // if (callee.args.len > param_count) return self.err(
    if (args.len > param_count) return self.err(
        // AnalyzerMsg.tooManyFnArgs(param_count, callee.args.len),
        AnalyzerMsg.tooManyFnArgs(param_count, args.len),
        err_span,
    );

    var proto = ty.proto(self.allocator);
    var proto_values = proto.values()[offset..];
    const start = self.ir_builder.instructions.len;
    self.ir_builder.ensureUnusedSize(param_count);

    // We initialize all the values used for the initialization. By default, we put empty data under
    // the form of 'default_value' but we check for all real param default to mark their index (order
    // of declaration) so that the compiler can emit the right index
    var default_count: usize = 0;
    // Self is always the first parameter
    for (ty.params.values()[offset..]) |*f| {
        self.makeInstruction(.{ .default_value = default_count }, .add_no_alloc);
        if (f.default) default_count += 1;
    }

    for (args, 0..) |arg, i| {
        var cast = false;
        var value_instr: usize = 0;
        var param_info: *const Type.Parameter = undefined;
        var param_index: usize = undefined;

        switch (arg.*) {
            .named_arg => |na| {
                const name = self.interner.intern(self.ast.toSource(na.name));
                proto.putAssumeCapacity(name, true);
                param_index = proto.getIndex(name).?;

                param_info = ty.params.getPtr(name) orelse return self.err(
                    .{ .unknown_param = .{ .name = self.ast.toSource(na.name) } },
                    self.ast.getSpan(na.name),
                );

                value_instr = self.ir_builder.instructions.len;
                const value_type = try self.analyzeExpr(na.value, ctx);
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(na.value))).cast;
            },
            else => {
                value_instr = self.ir_builder.instructions.len;
                const value_type = try self.analyzeExpr(arg, ctx);
                param_info = &ty.params.values()[i + offset];
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(arg))).cast;
                proto_values[i] = true;
                param_index = i;
            },
        }

        // We take into account here too
        self.makeInstruction(.{ .value = .{ .value_instr = value_instr, .cast = cast } }, .{ .setAt = start + param_index - offset });
    }

    // Check if any missing non-default parameter
    const err_count = self.errs.items.len;

    for (proto_values, 0..) |has_value, i| if (!has_value) {
        self.err(
            .{ .missing_function_param = .{ .name = self.interner.getKey(proto.keys()[i + offset]).? } },
            err_span,
        ) catch {};
    };

    return if (err_count < self.errs.items.len) error.Err else .{ param_count + offset, default_count };
}

/// Tries to find a match from variables and symbols and returns its type while emitting an instruction
fn identifier(
    self: *Self,
    token_name: Ast.TokenIndex,
    initialized: bool,
    ctx: *const Context,
) Error!struct { *const Type, enum { variable, symbol } } {
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    if (self.variableIdentifier(name)) |variable| {
        if (initialized and !variable.initialized) {
            return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(token_name));
        }

        return .{ variable.type, .variable };
    }

    if (self.symbolIdentifier(name)) |sym| {
        return .{ sym.type, .symbol };
    }

    if (name == self.cached_names.Self) {
        if (ctx.struct_type) |ty| {
            return .{ ty, .variable };
        } else {
            return self.err(.big_self_outside_struct, self.ast.getSpan(token_name));
        }

        return .{ &self.big_self, .variable };
    }

    return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(token_name));
}

/// Tries to find a variable in scopes and returns it while emitting an instruction
fn variableIdentifier(self: *Self, name: InternerIdx) ?*Variable {
    const variable, const scope_offset = self.scope.getVariable(name) orelse return null;
    // TODO: handle captured
    self.makeInstruction(.{ .identifier = .{
        .index = variable.index + scope_offset,
        .scope = if (variable.kind == .local) .local else .global,
    } }, .add);

    return variable;
}

fn expectVariableIdentifier(self: *Self, token_name: Ast.TokenIndex) Error!*Variable {
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    return self.variableIdentifier(name) orelse return self.err(
        .{ .undeclared_var = .{ .name = self.interner.getKey(name).? } },
        self.ast.getSpan(token_name),
    );
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn symbolIdentifier(self: *Self, name: InternerIdx) ?*ScopeStack.Scope.Symbol {
    const sym = self.scope.getSymbol(name) orelse return null;
    // TODO: protect cast
    self.makeInstruction(.{ .symbol_id = @intCast(sym.index) }, .add);

    return sym;
}

fn ifExpr(self: *Self, expr: *const Ast.If, ctx: *Context) Result {
    const index = self.ir_builder.reserveInstr();
    var data: Instruction.If = .{ .cast = .none, .has_else = false };

    const cond_type = try self.analyzeExpr(expr.condition, ctx);

    // We can continue to analyze if the condition isn't a bool
    if (!cond_type.is(.bool)) {
        self.err(
            .{ .non_bool_cond = .{ .what = "if", .found = self.getTypeName(cond_type) } },
            self.ast.getSpan(expr.condition),
        ) catch {};
    }

    // Analyze then branch
    const then_type = try self.analyzeNode(&expr.then, ctx);
    const then_returned = ctx.returns;
    var final_type = if (then_returned) self.type_interner.cache.void else then_type;
    ctx.returns = false;

    var else_returned = false;

    if (expr.@"else") |*n| {
        const else_type = try self.analyzeNode(n, ctx);
        data.has_else = true;
        else_returned = ctx.returns;

        if (!else_returned) {
            if (then_returned) {
                final_type = else_type;
            } else {
                try self.ifTypeCoherenceAndCast(then_type, else_type, expr, &data);
            }
        }
    } else if (!then_type.is(.void) and !ctx.allow_partial) {
        return self.err(
            .{ .missing_else_clause = .{ .if_type = self.getTypeName(then_type) } },
            self.ast.getSpan(expr),
        );
    }

    // The whole instructions returns out of scope
    ctx.returns = then_returned and else_returned;
    self.makeInstruction(.{ .@"if" = data }, .{ .setAt = index });

    return if (ctx.returns) self.type_interner.cache.void else final_type;
}

fn ifTypeCoherenceAndCast(self: *Self, then_type: *const Type, else_type: *const Type, expr: *const Ast.If, data: *Instruction.If) Error!void {
    if (then_type == else_type) return;

    if (else_type.canCastTo(then_type)) {
        data.cast = .@"else";
        self.warn(AnalyzerMsg.implicitCast("then branch", "float"), self.ast.getSpan(expr.then));
    } else if (then_type.canCastTo(else_type)) {
        data.cast = .then;
        self.warn(AnalyzerMsg.implicitCast("else branch", "float"), self.ast.getSpan(expr.@"else".?));
    } else return self.err(
        .{ .incompatible_if_type = .{
            .found1 = self.getTypeName(then_type),
            .found2 = self.getTypeName(else_type),
        } },
        self.ast.getSpan(expr),
    );
}

fn literal(self: *Self, expr: *const Ast.Literal, ctx: *Context) Result {
    const text = self.ast.toSource(expr);

    switch (expr.tag) {
        .bool => {
            self.makeInstruction(.{ .bool = self.ast.token_tags[expr.idx] == .true }, .add);
            return self.type_interner.cache.bool;
        },
        .identifier, .self => return (try self.identifier(expr.idx, true, ctx)).@"0",
        .int => {
            const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                // TODO: error handling, only one possible it's invalid char
                std.debug.print("Error parsing integer\n", .{});
                break :blk 0;
            };
            self.makeInstruction(.{ .int = value }, .add);
            return self.type_interner.cache.int;
        },
        .float => {
            const value = std.fmt.parseFloat(f64, text) catch blk: {
                // TODO: error handling, only one possible it's invalid char or too big
                std.debug.print("Error parsing float\n", .{});
                break :blk 0.0;
            };
            self.makeInstruction(.{ .float = value }, .add);
            return self.type_interner.cache.float;
        },
        .null => {
            self.makeInstruction(.null, .add);
            return self.type_interner.cache.null;
        },
        .string => {
            const no_quotes = text[1 .. text.len - 1];
            var final: ArrayListUnmanaged(u8) = .{};
            var i: usize = 0;

            while (i < no_quotes.len) : (i += 1) {
                const c = no_quotes[i];

                if (c == '\\') {
                    i += 1;

                    // Safe access here because lexer checked if the string and termianted
                    switch (no_quotes[i]) {
                        'n' => final.append(self.allocator, '\n') catch oom(),
                        't' => final.append(self.allocator, '\t') catch oom(),
                        '"' => final.append(self.allocator, '"') catch oom(),
                        'r' => final.append(self.allocator, '\r') catch oom(),
                        '\\' => final.append(self.allocator, '\\') catch oom(),
                        else => return self.err(
                            .{ .unknow_char_escape = .{ .found = no_quotes[i .. i + 1] } },
                            self.ast.getSpan(expr),
                        ),
                    }
                } else final.append(self.allocator, c) catch oom();
            }

            const value = self.interner.intern(final.toOwnedSlice(self.allocator) catch oom());
            self.makeInstruction(.{ .string = value }, .add);
            return self.type_interner.cache.str;
        },
    }
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral, ctx: *Context) Error!*const Type {
    const index = self.ir_builder.reserveInstr();
    const ty = try self.analyzeExpr(expr.structure, ctx);

    const struct_type = if (ty.is(.structure)) ty.structure else {
        return self.err(.non_struct_struct_literal, self.ast.getSpan(expr.structure));
    };

    var proto = struct_type.proto(self.allocator);
    defer proto.deinit(self.allocator);

    const start = self.ir_builder.count();
    self.ir_builder.ensureUnusedSize(struct_type.fields.count());

    // We initialize all the values used for the initialization. By default, we put empty data under
    // the form of 'struct_default' but we check for all real struct default to mark their index (order
    // of declaration) so that the compiler can emit the right index
    var default_count: usize = 0;
    for (struct_type.fields.values()) |f| {
        self.makeInstruction(.{ .default_value = default_count }, .add);
        if (f.default) default_count += 1;
    }

    // BUG: Doesn't check for field duplication
    for (expr.fields) |*fv| {
        const field_name = self.interner.intern(self.ast.toSource(fv.name));
        const f = struct_type.fields.get(field_name) orelse return self.err(
            .{ .unknown_struct_field = .{ .name = self.ast.toSource(fv.name) } },
            self.ast.getSpan(fv.name),
        );
        const field_index = struct_type.fields.getIndex(field_name).?;

        const value_instr = self.ir_builder.count();
        proto.putAssumeCapacity(field_name, true);

        const typ = if (fv.value) |val|
            try self.analyzeExpr(val, ctx)
        else // Syntax: { x } instead of { x = x }
            (try self.expectVariableIdentifier(fv.name)).type;

        const span = if (fv.value) |val| self.ast.getSpan(val) else self.ast.getSpan(fv.name);
        const coercion = try self.performTypeCoercion(f.type, typ, false, span);

        self.makeInstruction(
            .{ .value = .{ .value_instr = value_instr, .cast = coercion.cast } },
            .{ .setAt = start + field_index },
        );
    }

    if (expr.fields.len != proto.count()) {
        var kv = proto.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.*) self.err(
                .{ .missing_field_struct_literal = .{ .name = self.interner.getKey(entry.key_ptr.*).? } },
                self.ast.getSpan(expr.structure),
            ) catch {};
        }

        return error.Err;
    }

    // TODO: implement an invoke strategy
    // TODO: protect cast
    self.makeInstruction(
        .{ .struct_literal = .{
            .fields_count = @intCast(struct_type.fields.count()),
            .default_count = @intCast(default_count),
        } },
        .{ .setAt = index },
    );

    return ty;
}

fn returnExpr(self: *Self, expr: *const Ast.Return, ctx: *Context) Result {
    const index = self.ir_builder.reserveInstr();
    var data: Instruction.Return = .{ .value = false, .cast = false };

    var ty = if (expr.expr) |e| blk: {
        data.value = true;
        break :blk try self.analyzeExpr(e, ctx);
    } else self.type_interner.cache.void;

    // We check after to advance node idx
    const fn_type = ctx.fn_type orelse return self.err(.return_outside_fn, self.ast.getSpan(expr));
    const return_type = fn_type.function.return_type;

    // We do that here because we can insert a cast
    if (return_type != ty) {
        if (ty.canCastTo(return_type)) {
            data.cast = true;
            self.makeInstruction(.{ .cast = .float }, .add);
            ty = self.type_interner.cache.float;
        } else return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(return_type),
                .found = self.getTypeName(ty),
            } },
            if (expr.expr) |e| self.ast.getSpan(e) else self.ast.getSpan(expr),
        );
    }

    ctx.returns = true;
    self.makeInstruction(.{ .@"return" = data }, .{ .setAt = index });

    return ty;
}

fn unary(self: *Self, expr: *const Ast.Unary, ctx: *Context) Result {
    const op = self.ast.token_tags[expr.op];
    const index = self.ir_builder.reserveInstr();
    var data: Instruction.Unary = .{ .op = if (op == .not) .bang else .minus, .typ = .float };

    const rhs = try self.analyzeExpr(expr.expr, ctx);

    if (op == .not and !rhs.is(.bool)) {
        return self.err(
            .{ .invalid_unary = .{ .found = self.getTypeName(rhs) } },
            self.ast.getSpan(expr),
        );
    } else if (op == .minus and !rhs.isNumeric()) {
        return self.err(
            AnalyzerMsg.invalidArithmetic(self.getTypeName(rhs)),
            self.ast.getSpan(expr),
        );
    }

    if (rhs.is(.int)) data.typ = .int;

    self.makeInstruction(.{ .unary = data }, .{ .setAt = index });

    return rhs;
}

/// Checks if identifier name is already declared, otherwise interns it and returns the key
fn internIfNotInScope(self: *Self, token: usize) Error!usize {
    const name = self.interner.intern(self.ast.toSource(token));

    if (self.scope.isInScope(name)) return self.err(
        .{ .already_declared = .{ .name = self.interner.getKey(name).? } },
        self.ast.getSpan(token),
    );

    return name;
}

/// Checks that the node is a declared type and return it's value. If node is `.empty`, returns `void`
fn checkAndGetType(self: *Self, ty: ?*const Ast.Type, ctx: *const Context) Result {
    return if (ty) |t| return switch (t.*) {
        .array => |arr_type| {
            _ = arr_type;
            unreachable;
            // const child = try self.checkAndGetType(arr_type.child);
            //
            // if (child == .void) {
            //     return self.err(.void_array, self.ast.getSpan(arr_type.child));
            // }
            //
            // return self.type_manager.getOrCreateArray(child);
        },
        .fields => |fields| {
            _ = fields;
            unreachable;
            // var module: Pipeline.Module = undefined;
            //
            // for (fields[0 .. fields.len - 1]) |f| {
            //     const module_variable = try self.resolveIdentifier(f, true);
            //
            //     if (module_variable.ty.getKind() != .module) return self.err(
            //         .{ .dot_type_on_non_mod = .{ .found = self.getTypeName(module_variable.ty) } },
            //         self.ast.getSpan(f),
            //     );
            //
            //     const module_index = module_variable.ty.getValue();
            //     module = self.modules.values()[module_index];
            // }
            //
            // const name_token = fields[fields.len - 1];
            // const name = self.interner.intern(self.ast.toSource(name_token));
            // const final = module.symbols.get(name) orelse return self.err(
            //     .{ .missing_symbol_in_module = .{ .module = module.name, .symbol = self.ast.toSource(name_token) } },
            //     self.ast.getSpan(name_token),
            // );
            // return final.type;
        },
        .function => |func| {
            var params: AutoArrayHashMapUnmanaged(InternerIdx, Type.Parameter) = .{};
            for (func.params, 0..) |p, i| {
                const p_type = try self.checkAndGetType(p, ctx);
                params.put(self.allocator, i, .{ .type = p_type, .default = false }) catch oom();
            }

            return self.type_interner.intern(.{ .function = .{
                .params = params,
                .return_type = try self.checkAndGetType(func.return_type, ctx),
                .is_method = false,
            } });
        },
        .scalar => {
            const interned = self.interner.intern(self.ast.toSource(t));

            const found_type = self.scope.getType(interned) orelse {
                if (interned == self.cached_names.Self) {
                    if (ctx.struct_type) |struct_type| {
                        return struct_type;
                    } else {
                        return self.err(.big_self_outside_struct, self.ast.getSpan(t));
                    }
                } else {
                    return self.err(.{ .undeclared_type = .{ .found = self.ast.toSource(t) } }, self.ast.getSpan(t));
                }
            };

            return found_type;
        },
        .self => if (ctx.struct_type) |struct_type| struct_type else self.err(.self_outside_struct, self.ast.getSpan(t)),
    } else self.type_interner.cache.void;
}

const TypeCoherence = struct { type: *const Type, cast: bool = false };

/// Checks for `void` values, array inference, cast and function type generation
/// The goal is to see if the two types are equivalent and if so, make the transformations needed
fn performTypeCoercion(self: *Self, decl: *const Type, value: *const Type, emit_cast: bool, span: Span) Error!TypeCoherence {
    var cast = false;
    var local_decl = decl;
    const local_value = value;

    if (self.isVoid(value)) return self.err(.void_value, span);

    // if (value.is(.array)) {
    //     local_value = try self.inferArrayType(decl, value, span);
    // }

    // if (local_decl == .void) {
    //     // For functions, we get an anonymus type from it to not point to the same type infos
    //     // This is made to avoid getting like default values that can't be used on
    //     local_decl = try self.type_manager.createBoundedFnType(self.allocator, local_value);
    // } else
    // if (!self.isTypeEqual(local_decl, local_value)) {

    if (self.isVoid(local_decl)) {
        local_decl = local_value;
    } else {
        if (local_decl != local_value) {
            // One case in wich we can coerce, int -> float
            // if (self.checkCast(local_decl, local_value, emit_cast)) {
            if (local_value.canCastTo(local_decl)) {
                cast = true;
                // if (emit_cast) self.addInstr(.{ .cast = .float });
                // if (emit_cast) self.ir_builder.castTo(.add);
                if (emit_cast) self.makeInstruction(.{ .cast = .float }, .add);
            } else return self.err(
                .{
                    .type_mismatch = .{
                        .expect = self.getTypeName(local_decl),
                        .found = self.getTypeName(local_value),
                    },
                },
                span,
            );
        }
    }

    return .{ .type = local_decl, .cast = cast };
}

fn isVoid(self: *const Self, ty: *const Type) bool {
    return ty == self.type_interner.cache.void;
}

fn getTypeName(self: *const Self, ty: *const Type) []const u8 {
    return ty.toString(self.allocator, self.interner);
}

// /// Helpers used for errors
// fn getTypeName(self: *const Self, ty: *const Type) []const u8 {
//     // if (ty.is(.function)) {
//     //     return self.getFnTypeName(ty) catch oom();
//     // } else if (ty.is(.array)) {
//     //     return self.getArrayTypeName(ty) catch oom();
//     // } else {
//     self.type_interner.
//     const index = self.tm.getInternerIndex(ty);
//     return self.interner.getKey(index).?;
//     // }
// }
