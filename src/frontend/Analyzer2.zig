const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const Span = @import("Lexer.zig").Span;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const oom = @import("../utils.zig").oom;
const TM2 = @import("TM2.zig");
const IrBuilder = @import("IrBuilder.zig");

pub const Type = union(enum) {
    void,
    int,
    float,
    bool,
    str,
    null,
    self,
    function: struct {
        // TODO: Do I need a map? Not just a slice?
        params: AutoArrayHashMapUnmanaged(InternerIdx, Parameter),
        return_type: *const Type,
    },

    pub const Parameter = struct {
        type: *const Type,
        default: bool = false,
    };

    pub fn eql(self: *const Type, other: *const Type) bool {
        return std.meta.eql(self.*, other.*);
    }

    pub fn cast(self: *const Type, other: *const Type) bool {
        return if (self.* == .int and other.* == .float)
            true
        else
            false;
    }

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
            .void, .int, .float, .bool, .str, .null, .self => {},
            .function => |typ| {
                for (typ.params.values()) |param| {
                    param.type.hash(hasher);
                }
                typ.return_type.hash(hasher);
            },
        }
    }
};

const TypeInterner = struct {
    arena: Allocator,
    interned: std.AutoHashMapUnmanaged(u64, *Type),
    cache: Cache,

    const Cache = struct {
        void: *const Type,
        bool: *const Type,
        int: *const Type,
        float: *const Type,
        str: *const Type,
        null: *const Type,

        pub const empty: Cache = .{
            .void = undefined,
            .bool = undefined,
            .int = undefined,
            .float = undefined,
            .str = undefined,
            .null = undefined,
        };
    };

    pub fn init(arena: Allocator) TypeInterner {
        return .{ .arena = arena, .interned = .{}, .cache = .empty };
    }

    pub fn cacheFrequentTypes(self: *TypeInterner) void {
        self.cache.void = self.intern(.void);
        self.cache.bool = self.intern(.bool);
        self.cache.int = self.intern(.int);
        self.cache.float = self.intern(.float);
        self.cache.str = self.intern(.str);
        self.cache.null = self.intern(.null);
    }

    pub fn intern(self: *TypeInterner, typ: Type) *Type {
        var hasher = std.hash.Wyhash.init(0);
        typ.hash(&hasher);
        const hash = hasher.final();

        if (self.interned.get(hash)) |interned| {
            return interned;
        }

        const new_type = self.arena.create(Type) catch oom();
        new_type.* = typ;
        self.interned.put(self.arena, hash, new_type) catch oom();

        return new_type;
    }
};

const Scope = struct {
    variables: AutoArrayHashMapUnmanaged(InternerIdx, Variable),
    parent: ?*Scope,

    symbols: AutoArrayHashMapUnmanaged(InternerIdx, Symbol),
    symbols_offset: usize,

    pub const Symbol = struct { type: *const Type, global: bool };

    pub const empty: Scope = .{ .variables = .{}, .symbols = .{}, .symbols_offset = 0, .parent = null };

    pub fn open(self: *Scope) void {
        const scope: Scope = .{
            .variables = .{},
            .symbols = .{},
            .symbols_offset = self.symbols.count(),
            .parent = self,
        };
        self.* = scope;
    }

    pub fn close(self: *Scope) usize {
        std.debug.assert(self.parent != null);

        const var_count = self.variables.count();
        const symbol_offset = self.symbols_offset + self.symbols.count();
        self.* = self.parent.?.*;
        self.symbols_offset = symbol_offset;
        return var_count;
    }

    pub fn isGlobal(self: *const Scope) bool {
        return self.parent == null;
    }

    pub fn declareVar(self: *Scope, allocator: Allocator, name: InternerIdx, typ: *const Type, initialized: bool) error{TooManyLocals}!usize {
        if (self.variables.count() > 255 and !self.isGlobal()) {
            return error.TooManyLocals;
        }

        self.variables.put(allocator, name, .{
            .typ = typ,
            .kind = if (self.isGlobal()) .global else .local,
            .initialized = initialized,
        }) catch oom();

        return self.variables.count();
    }

    pub fn declareSymbol(self: *Scope, allocator: Allocator, name: InternerIdx, typ: *const Type) void {
        self.symbols.put(allocator, name, .{ .type = typ, .global = self.isGlobal() }) catch oom();
    }

    pub fn getSymbolIdx(self: *const Scope, name: InternerIdx) usize {
        var scope: ?*Scope = self;

        while (scope) |s| {
            if (s.symbols.getIndex(name)) |i| {
                return i + s.symbols_offset;
            }

            if (s.parent) |p| {
                scope = p;
            } else break;
        }

        @panic("Symbol not found");
    }

    pub fn isInScope(self: *const Scope, name: InternerIdx) bool {
        return self.variables.get(name) != null;
    }
};

const SymbolTable = struct {};

fn declareVariable(self: *Self, name: InternerIdx, typ: *const Type, initialized: bool, span: Span) Error!usize {
    return self.scope.declareVar(self.allocator, name, typ, initialized) catch self.err(.too_many_locals, span);
}

const Context = struct {
    side: enum { lhs, rhs } = .lhs,
    fn_type: ?*Type = null,
    struct_type: ?*Type = null,
    ref_count: bool = false,
    cow: bool = false,
    allow_partial: bool = false,
    returns: bool = false,

    pub fn restore(self: *Context, saved: Context) void {
        self.* = saved;
    }

    pub fn reset(self: *Context) void {
        self.* = .{};
    }
};

const Variable = struct {
    /// Variable's type
    typ: *const Type,
    /// Kind: global, local or captured
    kind: enum { local, global, captured },
    /// Is initialized
    initialized: bool,
};

const Self = @This();
const Error = error{Err};
pub const AnalyzerReport = GenReport(AnalyzerMsg);

arena: std.heap.ArenaAllocator,
allocator: Allocator,
interner: *Interner,
errs: ArrayListUnmanaged(AnalyzerReport),
warns: ArrayListUnmanaged(AnalyzerReport),

ast: *const Ast,
scope: Scope,
tm: TM2,
type_interner: TypeInterner,
ir_builder: IrBuilder,
main: ?usize,

cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },

pub fn init(self: *Self, allocator: Allocator, interner: *Interner) void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();
    self.interner = interner;
    self.scope = .empty;
    self.errs = .{};
    self.warns = .{};
    self.ir_builder = .init(allocator);
    self.tm = .init(allocator);
    self.tm.declareBuiltinTypes(interner);
    self.type_interner = .init(self.allocator);
    self.type_interner.cacheFrequentTypes();
    self.main = null;

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

fn analyzeNode(self: *Self, node: *const Node, ctx: *Context) Error!*const Type {
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
        // .assignment => |*n| try self.assignment(n),
        // .discard => |n| try self.discard(n),
        .fn_decl => |*n| _ = try self.fnDeclaration(n, ctx),
        // .multi_var_decl => |*n| try self.multiVarDecl(n),
        .print => |n| try self.print(n, ctx),
        // .struct_decl => |*n| try self.structDecl(n),
        // .use => |*n| try self.use(n),
        .var_decl => |*n| try self.varDeclaration(n, ctx),
        // .@"while" => |*n| try self.whileStmt(n),
        .expr => |n| return self.analyzeExpr(n, ctx),
        else => unreachable,
    }

    return self.type_interner.intern(.void);
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl, ctx: *Context) Error!void {
    const name = try self.internIfNotInScope(node.name);

    if (name == self.cached_names.main) {
        // TODO: Error
        if (!self.scope.isGlobal())
            @panic("Main in local scope")
        else if (self.tm.isDeclared(name))
            @panic("Multiple mains");

        self.main = self.ir_builder.instructions.len;
    }

    const fn_idx = self.ir_builder.reserveInstr();
    // TODO: delete and merge this into other Instructions
    self.ir_builder.name(name, .add);

    self.scope.open();

    // We reserve slot 0 for potential 'self'
    _ = try self.declareVariable(self.cached_names.self, ctx.struct_type orelse self.type_interner.cache.void, true, .zero);

    var params_type: AutoArrayHashMapUnmanaged(InternerIdx, Type.Parameter) = .{};
    params_type.ensureTotalCapacity(self.allocator, node.params.len) catch oom();
    var default_count: usize = 0;

    for (node.params) |*p| {
        const param_name = self.interner.intern(self.ast.toSource(p.name));

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

    const return_type = try self.checkAndGetType(node.return_type, ctx);
    const fn_type = self.type_interner.intern(.{ .function = .{ .params = params_type, .return_type = return_type } });
    self.scope.declareSymbol(self.allocator, name, fn_type);

    const body_type, const had_err, const len = self.fnBody(node.body.nodes, ctx);

    if (!had_err and body_type != return_type) {
        return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(return_type),
                .found = self.getTypeName(body_type),
            } },
            self.ast.getSpan(node.body.nodes[node.body.nodes.len - 1]),
        );
    }

    _ = self.scope.close();

    self.ir_builder.declareFunction(
        len,
        default_count,
        if (ctx.returns) .explicit else if (self.isVoid(body_type)) .implicit_void else .implicit_value,
        .{ .setAt = fn_idx },
    );
}

fn fnBody(self: *Self, body: []Node, ctx: *Context) struct { *const Type, bool, usize } {
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
        const typ = self.analyzeNode(n, ctx) catch {
            had_err = true;
            continue;
        };

        // If we analyze dead code, we don't update the type
        if (deadcode_start == 0) final_type = typ;

        // If last expression produced a value and that it wasn't the last one and it wasn't a return, error
        if (!self.isVoid(final_type) and i != len - 1 and !ctx.returns) {
            self.err(.unused_value, self.ast.getSpan(n)) catch {};
        }
    }

    return .{ final_type, had_err, len - deadcode_count };
}

fn defaultValue(self: *Self, decl_type: *const Type, default_value: *const Expr, ctx: *Context) Error!*const Type {
    const value_type = try self.analyzeExpr(default_value, ctx);
    const coerce = try self.performTypeCoercion(decl_type, value_type, true, self.ast.getSpan(default_value));
    return coerce.type;
}

fn print(self: *Self, expr: *const Expr, ctx: *Context) Error!void {
    self.ir_builder.print(.add);
    const typ = try self.analyzeExpr(expr, ctx);

    if (self.isVoid(typ)) {
        return self.err(.void_print, self.ast.getSpan(expr));
    }
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) Error!void {
    const name = try self.internIfNotInScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);
    const index = self.ir_builder.reserveInstr();

    var initialized = false;
    var cast = false;
    // var data = Instruction.VarDecl{ .variable = undefined };

    if (node.value) |value| {
        // data.has_value = true;
        initialized = true;

        // const last = self.state.allow_partial;
        // self.state.side = .rhs;
        // self.state.allow_partial = false;
        // defer {
        //     self.state.side = .none;
        //     self.state.allow_partial = last;
        // }

        ctx.allow_partial = false;
        ctx.side = .rhs;

        // const sft = try self.getStructAndFieldOrLiteralInfos(value, false);
        //
        // if (sft.is_type and sft.field.is(.@"struct")) {
        //     return self.err(.assign_type, self.ast.getSpan(value));
        // }

        const value_type = try self.analyzeExpr(value, ctx);
        const coherence = try self.performTypeCoercion(checked_type, value_type, true, self.ast.getSpan(value));
        checked_type = coherence.type;
        // data.cast = coherence.cast;
        cast = coherence.cast;
    }

    // data.variable = try self.declareVariable(name, checked_type, initialized, index, .variable, node.name);
    const decl_index = try self.declareVariable(name, checked_type, initialized, self.ast.getSpan(node.name));
    self.ir_builder.declareVariable(initialized, cast, decl_index, self.scope.isGlobal(), .{ .setAt = index });
    // self.setInstr(index, .{ .var_decl = data });

    // if (data.variable.scope == .global) {
    //     self.addSymbol(name, checked_type);
    // }
}

fn analyzeExpr(self: *Self, expr: *const Expr, ctx: *Context) Error!*const Type {
    return switch (expr.*) {
        // .array => |*e| self.array(e),
        // .array_access => |*e| self.arrayAccess(e),
        .block => |*e| self.block(e, ctx),
        // .binop => |*e| self.binop(e),
        // .field => |*e| (try self.field(e, true)).field,
        // .fn_call => |*e| self.call(e),
        // .grouping => |*e| self.analyzeExpr(e.expr),
        // .@"if" => |*e| self.ifExpr(e),
        .literal => |*e| self.literal(e, ctx),
        // .named_arg => unreachable,
        // .@"return" => |*e| self.returnExpr(e),
        // .struct_literal => |*e| self.structLiteral(e),
        // .unary => |*e| self.unary(e),
        else => unreachable,
    };
}

fn block(self: *Self, expr: *const Ast.Block, ctx: *Context) Error!*const Type {
    self.scope.open();
    errdefer _ = self.scope.close();

    const index = self.ir_builder.reserveInstr();
    var final_type = self.type_interner.cache.void;

    for (expr.nodes, 0..) |*node, i| {
        final_type = try self.analyzeNode(node, ctx);

        if (self.isVoid(final_type) and i != expr.nodes.len - 1) {
            return self.err(.unused_value, expr.span);
        }
    }

    const var_count = self.scope.close();
    // TODO: protect cast
    self.ir_builder.block(expr.nodes.len, @intCast(var_count), !self.isVoid(final_type), .{ .setAt = index });

    return final_type;
}

fn literal(self: *Self, expr: *const Ast.Literal, ctx: *Context) Error!*const Type {
    const text = self.ast.toSource(expr);

    switch (expr.tag) {
        .bool => {
            self.ir_builder.boolLiteral(self.ast.token_tags[expr.idx] == .true, .add);
            return self.type_interner.cache.bool;
        },
        .identifier, .self => {
            const resolved = try self.resolveIdentifier(expr.idx, true, ctx);
            self.ir_builder.identifier(resolved.index, .add);

            return resolved.variable.typ;
        },
        .int => {
            const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                // TODO: error handling, only one possible it's invalid char
                std.debug.print("Error parsing integer\n", .{});
                break :blk 0;
            };
            self.ir_builder.intLiteral(value, .add);
            return self.type_interner.cache.int;
        },
        .float => {
            const value = std.fmt.parseFloat(f64, text) catch blk: {
                // TODO: error handling, only one possible it's invalid char or too big
                std.debug.print("Error parsing float\n", .{});
                break :blk 0.0;
            };
            self.ir_builder.floatLiteral(value, .add);
            return self.type_interner.cache.float;
        },
        .null => {
            self.ir_builder.nullLiteral(.add);
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
            self.ir_builder.strLiteral(value, .add);
            return self.type_interner.cache.str;
        },
    }
}

fn resolveIdentifier(
    self: *Self,
    token_name: Ast.TokenIndex,
    initialized: bool,
    ctx: *const Context,
) Error!struct { index: usize, variable: *Variable } {
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    if (self.scope.variables.getPtr(name)) |variable| {
        if (initialized and !variable.initialized) {
            return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(token_name));
        }

        return .{ .index = self.scope.variables.getIndex(name).?, .variable = variable };
    } else if (name == self.cached_names.Self) {
        _ = ctx;
        unreachable;
        // if (ctx.struct_type == null) {
        //     return self.err(.big_self_outside_struct, self.ast.getSpan(token_name));
        // }
        //
        // return &self.big_self;
    } else {
        return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(name));
    }
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

/// Checks that the node is a declared type and return it's value. If node is
/// `.empty`, returns `void`
fn checkAndGetType(self: *Self, typ: ?*const Ast.Type, ctx: *const Context) Error!*const Type {
    return if (typ) |t| return switch (t.*) {
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
            //     if (module_variable.typ.getKind() != .module) return self.err(
            //         .{ .dot_type_on_non_mod = .{ .found = self.getTypeName(module_variable.typ) } },
            //         self.ast.getSpan(f),
            //     );
            //
            //     const module_index = module_variable.typ.getValue();
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
        // .function => |*fn_type| self.createAnonymousFnType(fn_type),
        .function => unreachable,
        .scalar => {
            const interned = self.interner.intern(self.ast.toSource(t));

            const declared = self.tm.symbols.get(interned) orelse {
                if (interned == self.cached_names.Self) {
                    if (ctx.struct_type) |ty| {
                        return ty;
                    } else {
                        return self.err(.big_self_outside_struct, self.ast.getSpan(t));
                    }
                } else {
                    return self.err(.{ .undeclared_type = .{ .found = self.ast.toSource(t) } }, self.ast.getSpan(t));
                }
            };

            return self.type_interner.intern(declared);
        },
        .self => if (ctx.struct_type) |ty| ty else self.err(.self_outside_struct, self.ast.getSpan(t)),
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
            if (local_decl.cast(local_value)) {
                cast = true;
                // if (emit_cast) self.addInstr(.{ .cast = .float });
                if (emit_cast) self.ir_builder.castTo(.add);
            } else return self.err(
                .{ .type_mismatch = .{
                    .expect = self.getTypeName(local_decl),
                    .found = self.getTypeName(local_value),
                } },
                span,
            );
        }
    }

    return .{ .type = local_decl, .cast = cast };
}

pub fn isVoid(self: *const Self, typ: *const Type) bool {
    return typ == self.type_interner.cache.void;
}

/// Helpers used for errors
fn getTypeName(self: *const Self, typ: *const Type) []const u8 {
    // if (typ.is(.function)) {
    //     return self.getFnTypeName(typ) catch oom();
    // } else if (typ.is(.array)) {
    //     return self.getArrayTypeName(typ) catch oom();
    // } else {
    const index = self.tm.getInternerIndex(typ);
    return self.interner.getKey(index).?;
    // }
}
