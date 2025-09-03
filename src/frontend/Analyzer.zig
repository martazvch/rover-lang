const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const Pipeline = @import("../Pipeline.zig");
const GenReport = @import("../reporter.zig").GenReport;
const oom = @import("../utils.zig").oom;
const EnumFromStruct = @import("../utils.zig").EnumFromStruct;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const IrBuilder = @import("IrBuilder.zig");
const LexicalScope = @import("LexicalScope.zig");
const Importer = @import("Importer.zig");
const Sb = @import("../StringBuilder.zig");
const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const Span = @import("Lexer.zig").Span;
const TokenTag = @import("Lexer.zig").Token.Tag;
const Type = @import("types.zig").Type;
const TypeInterner = @import("types.zig").TypeInterner;

pub const AnalyzedModule = struct {
    name: []const u8,
    globals: LexicalScope.VariableMap,
    symbols: LexicalScope.SymbolArrMap,
};

const Context = struct {
    fn_type: ?*const Type = null,
    struct_type: ?*const Type = null,
    allow_partial: bool = true,
    returns: bool = false,
    in_call: bool = false,
    in_array: bool = false,

    /// Auto-generate an enum with all field names
    const Field = EnumFromStruct(Context);

    const ContextSnapshot = struct {
        saved: Context,
        ctx: *Context,

        pub fn restore(self: ContextSnapshot) void {
            self.ctx.* = self.saved;
        }
    };

    pub fn setAndGetPrevious(self: *Context, comptime ctx_field: Field, value: @TypeOf(@field(self, @tagName(ctx_field)))) @TypeOf(value) {
        const prev = @field(self, @tagName(ctx_field));
        @field(self, @tagName(ctx_field)) = value;

        return prev;
    }

    pub fn snapshot(self: *Context) ContextSnapshot {
        return .{ .saved = self.*, .ctx = self };
    }

    pub fn reset(self: *Context) void {
        self.* = .{};
    }
};

const Self = @This();
const Error = error{Err};
const Result = Error!*const Type;
pub const AnalyzerReport = GenReport(AnalyzerMsg);

allocator: Allocator,
pipeline: *Pipeline,
interner: *Interner,
path: *Sb,
containers: Sb,

errs: ArrayListUnmanaged(AnalyzerReport),
warns: ArrayListUnmanaged(AnalyzerReport),
ast: *const Ast,
scope: LexicalScope,
type_interner: *TypeInterner,
ir_builder: IrBuilder,
main: ?usize,
// TODO: useless, we can have the last scope in scope?
globals: ArrayListUnmanaged(usize),

cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },

pub fn init(allocator: Allocator, pipeline: *Pipeline) Self {
    return .{
        .allocator = allocator,
        .pipeline = pipeline,
        .interner = &pipeline.ctx.interner,
        .path = &pipeline.ctx.path_builder,
        .containers = .empty,
        .type_interner = &pipeline.ctx.type_interner,
        .ast = undefined,
        .errs = .empty,
        .warns = .empty,
        .scope = .empty,
        .ir_builder = .init(allocator),
        .globals = .empty,
        .main = null,

        .cached_names = .{
            .empty = pipeline.ctx.interner.intern(""),
            .main = pipeline.ctx.interner.intern("main"),
            .std = pipeline.ctx.interner.intern("std"),
            .self = pipeline.ctx.interner.intern("self"),
            .Self = pipeline.ctx.interner.intern("Self"),
            .init = pipeline.ctx.interner.intern("init"),
        },
    };
}

fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    self.errs.append(self.allocator, AnalyzerReport.err(kind, span)) catch oom();
    return error.Err;
}

fn warn(self: *Self, kind: AnalyzerMsg, span: Span) void {
    self.warns.append(self.allocator, AnalyzerReport.warn(kind, span)) catch oom();
}

fn makeInstruction(self: *Self, data: Instruction.Data, offset: usize, mode: IrBuilder.Mode) void {
    self.ir_builder.emit(.{ .data = data, .offset = offset }, mode);
}

pub fn analyze(self: *Self, ast: *const Ast, module_name: []const u8, expect_main: bool) AnalyzedModule {
    self.ast = ast;
    self.scope.initGlobalScope(self.allocator, self.interner, self.type_interner);
    var ctx: Context = .{};

    for (ast.nodes) |*node| {
        ctx.reset();
        const node_type = self.analyzeNode(node, &ctx) catch continue;

        if (!self.isVoid(node_type)) {
            self.err(.unused_value, self.ast.getSpan(node)) catch {};
        }
    }

    if (expect_main and self.main == null) {
        self.err(.no_main, .{ .start = 0, .end = 0 }) catch {};
    }

    return .{ .name = module_name, .globals = self.scope.current.variables, .symbols = self.scope.current.symbols };
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
        .use => |*n| try self.use(n, ctx),
        .var_decl => |*n| try self.varDeclaration(n, ctx),
        .@"while" => |*n| try self.whileStmt(n, ctx),
        .expr => |n| return self.analyzeExpr(n, ctx),
    }

    return self.type_interner.cache.void;
}

fn assignment(self: *Self, node: *const Ast.Assignment, ctx: *Context) !void {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();
    const span = self.ast.getSpan(node.assigne);

    ctx.allow_partial = false;
    const index = self.ir_builder.reserveInstr();
    const value_type = try self.analyzeExpr(node.value, ctx);

    const maybe_assigne_type = switch (node.assigne.*) {
        .literal => |*e| b: {
            if (e.tag != .identifier) break :b null;
            var assigne = try self.expectVariableIdentifier(e.idx);
            assigne.initialized = true;
            if (assigne.constant) return self.err(
                .{ .assign_to_constant = .{ .name = self.ast.toSource(e.idx) } },
                span,
            );
            break :b assigne.type;
        },
        .field => |*e| b: {
            const field_result = try self.field(e, ctx);
            if (!field_result.mutable) return self.err(.assign_to_struct_fn, span);
            break :b if (field_result.lhs_is_value) field_result.type else null;
        },
        .fn_call => return self.err(.invalid_assign_target, span),
        else => try self.analyzeExpr(node.assigne, ctx),
    };
    const assigne_type = maybe_assigne_type orelse return self.err(.invalid_assign_target, span);
    const coherence = try self.performTypeCoercion(assigne_type, value_type, false, self.ast.getSpan(node.value));

    self.makeInstruction(
        .{ .assignment = .{
            .cast = coherence.cast,
            .cow = assigne_type.isHeap(),
            .incr_rc = value_type.isHeap() and !isHeapLiteral(node.value),
        } },
        span.start,
        .{ .set_at = index },
    );
}

fn isAssignmentAllowed(self: *Self, assigne_type: *const Type, err_span: Span) Error!void {
    switch (assigne_type.*) {
        .function, .structure => return self.err(.invalid_assign_target, err_span),
        else => {},
    }
}

fn discard(self: *Self, expr: *const Expr, ctx: *Context) !void {
    const span = self.ast.getSpan(expr);
    self.makeInstruction(.{ .discard = undefined }, span.start, .add);
    const discarded = try self.analyzeExpr(expr, ctx);

    if (self.isVoid(discarded)) return self.err(.void_discard, span);
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl, ctx: *Context) Error!*const Type {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);
    const fn_idx = self.ir_builder.reserveInstr();

    // Forward declaration in outer scope for recursion
    var sym = self.scope.forwardDeclareSymbol(self.allocator, name);
    var fn_type: Type.Function = .{ .return_type = undefined, .is_method = undefined };

    self.scope.open(self.allocator, false);
    errdefer _ = self.scope.close();

    const captures_instrs_data = try self.loadFunctionCaptures(node.meta.captures.keys());
    const param_res = try self.fnParams(node.params, ctx);
    fn_type.params = param_res.params;
    fn_type.is_method = param_res.is_method;

    const return_type = try self.checkAndGetType(node.return_type, ctx);
    fn_type.return_type = return_type;
    const interned_type = self.type_interner.intern(.{ .function = fn_type });

    // Update type for resolution in function's body
    ctx.fn_type = interned_type;
    sym.type = interned_type;
    const len = try self.fnBody(node.body.nodes, &fn_type, ctx);
    _ = self.scope.close();

    // If in a structure declaration, we remove the symbol as it's gonna live inside the structure
    const captures_count = self.makeFunctionCapturesInstr(captures_instrs_data, span.start);
    const is_closure = captures_count > 0;

    if (ctx.struct_type != null) {
        self.scope.removeSymbol(name);
    } else if (is_closure) {
        self.scope.removeSymbol(name);
        _ = try self.declareVariable(name, interned_type, false, true, true, span);
    }

    if (name == self.cached_names.main and self.scope.isGlobal()) {
        self.main = sym.index;
    }

    // TODO: protect the cast
    self.makeInstruction(
        .{ .fn_decl = .{
            .kind = if (is_closure) .closure else .{ .symbol = sym.index },
            .name = name,
            .body_len = len,
            .default_params = @intCast(param_res.default_count),
            .captures_count = captures_count,
            .return_kind = if (ctx.returns) .explicit else if (self.isVoid(return_type)) .implicit_void else .implicit_value,
        } },
        span.start,
        .{ .set_at = fn_idx },
    );

    return interned_type;
}

fn loadFunctionCaptures(self: *Self, captures: []InternerIdx) Error![]const Instruction.Data {
    var instructions: ArrayListUnmanaged(Instruction.Data) = .{};
    instructions.ensureTotalCapacity(self.allocator, captures.len) catch oom();

    for (captures) |capt| {
        const variable_infos = self.scope.getVariable(capt) orelse unreachable;
        const variable = variable_infos.@"0";
        _ = try self.declareVariable(capt, variable.type, true, true, false, .zero);

        instructions.appendAssumeCapacity(.{ .identifier = .{ .index = variable.index, .scope = .local, .unbox = false } });
    }

    return instructions.toOwnedSlice(self.allocator) catch oom();
}

fn makeFunctionCapturesInstr(self: *Self, instrs: []const Instruction.Data, offset: usize) usize {
    for (instrs) |instr| {
        self.makeInstruction(instr, offset, .add);
    }
    return instrs.len;
}

const Params = struct {
    params: AutoArrayHashMapUnmanaged(InternerIdx, Type.Parameter),
    default_count: usize,
    is_method: bool,
};
fn fnParams(
    self: *Self,
    params: []Ast.VarDecl,
    ctx: *Context,
) Error!Params {
    var params_type: AutoArrayHashMapUnmanaged(InternerIdx, Type.Parameter) = .{};
    params_type.ensureTotalCapacity(self.allocator, params.len) catch oom();
    var default_count: usize = 0;
    var is_method = false;

    for (params, 0..) |*p, i| {
        const span = self.ast.getSpan(p.name);
        const param_name = self.interner.intern(self.ast.toSource(p.name));

        if (i == 0 and param_name == self.cached_names.self) {
            // TODO:
            const struct_type = ctx.struct_type orelse @panic("Self outside of structure");

            is_method = true;
            _ = try self.declareVariable(param_name, struct_type, p.meta.captured, true, true, .zero);
            params_type.putAssumeCapacity(param_name, .{ .type = struct_type, .default = false });
            continue;
        }

        if (self.scope.isVarOrSymInCurrentScope(param_name)) {
            return self.err(.{ .duplicate_param = .{ .name = self.ast.toSource(p.name) } }, span);
        }

        var param_type = try self.checkAndGetType(p.typ, ctx);
        if (p.value) |val| {
            // if (!self.isPure(value.*)) {
            //     return self.err(.{ .unpure_default = .new(.param) }, self.ast.getSpan(value));
            // }
            const value_type = try self.defaultValue(param_type, val, ctx);
            const coerce = try self.performTypeCoercion(param_type, value_type, true, span);
            default_count += 1;
            param_type = coerce.type;
        }

        if (self.isVoid(param_type)) {
            return self.err(.void_param, span);
        }

        _ = try self.declareVariable(param_name, param_type, p.meta.captured, true, true, span);
        params_type.putAssumeCapacity(param_name, .{
            .type = param_type,
            .default = p.value != null,
            .captured = p.meta.captured,
        });
    }

    return .{ .params = params_type, .default_count = default_count, .is_method = is_method };
}

fn fnBody(self: *Self, body: []Node, fn_type: *const Type.Function, ctx: *Context) Error!usize {
    var had_err = false;
    var final_type: *const Type = self.type_interner.cache.void;
    var deadcode_start: usize = 0;
    var deadcode_count: usize = 0;
    const len = body.len;

    for (body, 0..) |*n, i| {
        // If previous statement returned, it's only dead code now
        if (deadcode_start == 0 and ctx.returns) {
            self.warn(.dead_code, self.ast.getSpan(n));
            deadcode_start = self.ir_builder.count();
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
        if (i != len - 1 and !ctx.returns and !self.isVoid(final_type)) {
            self.err(.unused_value, self.ast.getSpan(n)) catch {};
        }
    }

    // We strip unused instructions to not compile them
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
    const span = self.ast.getSpan(expr);
    self.makeInstruction(.print, span.start, .add);
    const ty = try self.analyzeExpr(expr, ctx);

    if (self.isVoid(ty)) {
        return self.err(.void_print, span);
    }
}

fn use(self: *Self, node: *const Ast.Use, _: *Context) Error!void {
    const name_token = if (node.alias) |alias| alias else node.names[node.names.len - 1];
    const module_name = self.interner.intern(self.ast.toSource(name_token));

    if (self.scope.isModuleImported(module_name)) {
        return self.err(
            .{ .already_declared = .{ .name = self.ast.toSource(name_token) } },
            self.ast.getSpan(name_token),
        );
    }

    const old_path_length = self.path.len();
    defer self.path.shrink(self.allocator, old_path_length);

    const result = Importer.fetchImportedFile(self.allocator, self.ast, node.names, self.path);
    const file_infos = switch (result) {
        .ok => |f| f,
        .err => |e| {
            self.errs.append(self.allocator, e) catch oom();
            return error.Err;
        },
    };

    const interned = self.interner.intern(file_infos.path);

    if (!self.pipeline.ctx.module_interner.analyzed.contains(interned)) {
        var pipeline = self.pipeline.createSubPipeline();
        // TODO: proper error handling, for now just print errors and exit
        // const module = pipeline.run(file_infos.name, interned, file_infos.content) catch {
        _ = pipeline.run(file_infos.name, file_infos.path, file_infos.content) catch {
            std.process.exit(0);
        };
    }

    if (node.items) |items| {
        const mod = self.pipeline.ctx.module_interner.analyzed.get(interned).?;
        const mod_index = self.pipeline.ctx.module_interner.analyzed.getIndex(interned).?;

        for (items) |item| {
            const item_name = self.interner.intern(self.ast.toSource(item.item));
            var sym = mod.symbols.get(item_name) orelse return self.err(
                .{ .missing_symbol_in_module = .{
                    .module = self.ast.toSource(node.names[node.names.len - 1]),
                    .symbol = self.ast.toSource(item.item),
                } },
                self.ast.getSpan(item.item),
            );

            // TODO: error
            if (!sym.type.is(.structure) and !sym.type.is(.function)) {
                @panic("Import not supported yet");
            }

            const item_token = if (item.alias) |alias| alias else item.item;
            const item_interned = self.interner.intern(self.ast.toSource(item_token));
            self.scope.declareExternSymbol(self.allocator, item_interned, mod_index, sym);
        }
    } else {
        self.scope.declareModule(self.allocator, module_name, self.type_interner.intern(.{ .module = interned }));
    }
}

fn expectValue(self: *Self, expr: *const Ast.Expr, ctx: *Context) Error!*const Type {
    const span = self.ast.getSpan(expr);

    const value_type = switch (expr.*) {
        .literal => |*e| b: {
            if (e.tag == .identifier) {
                const value = self.identifier(e.idx, true, ctx) catch break :b null;
                if (value.kind == .symbol and value.type.* != .function) break :b null;
                break :b value.type;
            } else break :b try self.literal(e, ctx);
        },
        .field => |*e| b: {
            const field_result = try self.field(e, ctx);
            break :b if (field_result.assignable) field_result.type else null;
        },
        else => try self.analyzeExpr(expr, ctx),
    };

    return value_type orelse self.err(.assign_type, span);
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) Error!void {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const span = self.ast.getSpan(node.name);
    const name = try self.internIfNotInCurrentScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);
    const index = self.ir_builder.reserveInstr();

    var has_value = false;
    var cast = false;
    var incr_rc = false;

    if (node.value) |value| {
        has_value = true;
        ctx.allow_partial = false;

        const value_type = try self.expectValue(value, ctx);
        incr_rc = value_type.isHeap() and !isHeapLiteral(value);

        const coherence = try self.performTypeCoercion(checked_type, value_type, true, self.ast.getSpan(value));
        checked_type = coherence.type;
        cast = coherence.cast;
    }

    const decl_index = try self.declareVariable(name, checked_type, node.meta.captured, has_value, false, span);

    self.makeInstruction(
        .{ .var_decl = .{
            .box = node.meta.captured,
            .cast = cast,
            .has_value = has_value,
            .variable = .{ .index = decl_index, .scope = if (self.scope.isGlobal()) .global else .local, .unbox = false },
            .incr_rc = incr_rc,
        } },
        span.start,
        .{ .set_at = index },
    );
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl, ctx: *Context) Error!void {
    self.makeInstruction(.{ .multiple_var_decl = node.decls.len }, self.ast.getSpan(node).start, .add);

    for (node.decls) |*n| {
        try self.varDeclaration(n, ctx);
    }
}

fn structDecl(self: *Self, node: *const Ast.StructDecl, ctx: *Context) !void {
    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);
    // We forward declare for self referencing
    const sym = self.scope.forwardDeclareSymbol(self.allocator, name);

    const index = self.ir_builder.reserveInstr();
    // TODO: merge as function's name
    self.makeInstruction(.{ .name = name }, span.start, .add);

    self.scope.open(self.allocator, false);
    defer _ = self.scope.close();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    var buf: [1024]u8 = undefined;
    const container_name = self.containers.renderWithSep(&buf, ".");

    var ty: Type.Structure = .empty(name, self.interner.internKeepRef(self.allocator, container_name));
    ty.fields.ensureTotalCapacity(self.allocator, node.fields.len) catch oom();
    ty.functions.ensureTotalCapacity(self.allocator, @intCast(node.functions.len)) catch oom();

    // Create type before functions to allow 'self' to refer to the structure
    const interned_type = self.type_interner.intern(.{ .structure = ty });
    ctx.struct_type = interned_type;
    defer ctx.struct_type = null;

    const interned_struct = &interned_type.structure;

    // BUG: interned type doesn't take into account fields
    try self.structureFields(node.fields, interned_struct, ctx);
    sym.type = interned_type;

    for (node.functions) |*f| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        const fn_type = try self.fnDeclaration(f, ctx);
        interned_struct.functions.putAssumeCapacity(fn_name, .{ .type = fn_type });
        // BUG: doesn't update anything
        sym.type = interned_type;
    }

    self.makeInstruction(
        .{ .struct_decl = .{
            .index = sym.index,
            .fields_count = node.fields.len,
            .default_fields = interned_struct.default_value_fields,
            .func_count = node.functions.len,
        } },
        span.start,
        .{ .set_at = index },
    );
}

fn structureFields(self: *Self, fields: []const Ast.VarDecl, ty: *Type.Structure, ctx: *Context) Error!void {
    for (fields) |*f| {
        const span = self.ast.getSpan(f.name);
        var struct_field: Type.Structure.Field = .{ .type = undefined };
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (ty.fields.get(field_name) != null) {
            return self.err(
                .{ .already_declared_field = .{ .name = self.ast.toSource(f.name) } },
                span,
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
                self.makeInstruction(.{ .cast = .float }, span.start, .add);
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
    const span = self.ast.getSpan(node.condition);
    self.makeInstruction(.{ .@"while" = undefined }, span.start, .add);
    const cond_type = try self.analyzeExpr(node.condition, ctx);

    if (!cond_type.is(.bool)) return self.err(
        .{ .non_bool_cond = .{ .what = "while", .found = self.getTypeName(cond_type) } },
        span,
    );

    const body_type = try self.block(&node.body, ctx);

    if (!self.isVoid(body_type)) return self.err(
        .{ .non_void_while = .{ .found = self.getTypeName(body_type) } },
        self.ast.getSpan(node.body),
    );
}

fn analyzeExpr(self: *Self, expr: *const Expr, ctx: *Context) Result {
    return switch (expr.*) {
        .array => |*e| self.array(e, ctx),
        .array_access => |*e| self.arrayAccess(e, ctx),
        .block => |*e| self.block(e, ctx),
        .binop => |*e| self.binop(e, ctx),
        .closure => |*e| self.closure(e, ctx),
        .field => |*e| (try self.field(e, ctx)).type,
        .fn_call => |*e| self.call(e, ctx),
        .grouping => |*e| self.analyzeExpr(e.expr, ctx),
        .@"if" => |*e| self.ifExpr(e, ctx),
        .literal => |*e| self.literal(e, ctx),
        .named_arg => unreachable,
        .@"return" => |*e| self.returnExpr(e, ctx),
        .struct_literal => |*e| self.structLiteral(e, ctx),
        .unary => |*e| self.unary(e, ctx),
    };
}

fn array(self: *Self, expr: *const Ast.Array, ctx: *Context) Result {
    const index = self.ir_builder.reserveInstr();
    var value_type = self.type_interner.cache.void;
    var patch_casts = false;

    var elems: ArrayListUnmanaged(Instruction.Array.Elem) = .{};
    elems.ensureUnusedCapacity(self.allocator, expr.values.len) catch oom();

    for (expr.values) |val| {
        const span = self.ast.getSpan(val);
        var cast = false;
        var typ = try self.analyzeExpr(val, ctx);

        // TODO: error
        if (self.isVoid(typ)) @panic("Void value in array");

        if (!self.isVoid(value_type) and value_type != typ) {
            if (!value_type.canCastTo(typ)) return self.err(
                .{ .array_elem_different_type = .{ .found1 = self.getTypeName(value_type), .found2 = self.getTypeName(typ) } },
                span,
            );

            // Backtrack casts
            if (!patch_casts) {
                for (elems.items) |*e| {
                    e.cast = true;
                }
                patch_casts = true;
            } else {
                cast = true;
            }
            typ = value_type;
        }

        value_type = typ;
        elems.appendAssumeCapacity(.{ .cast = cast, .incr_rc = typ.isHeap() and !isHeapLiteral(val) });
    }

    self.makeInstruction(
        .{ .array = .{ .elems = elems.toOwnedSlice(self.allocator) catch oom() } },
        self.ast.getSpan(expr).start,
        .{ .set_at = index },
    );

    return self.type_interner.intern(.{ .array = value_type });
}

fn arrayAccess(self: *Self, expr: *const Ast.ArrayAccess, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.array);
    const index = self.ir_builder.reserveInstr();
    const arr = try self.analyzeExpr(expr.array, ctx);

    const type_value = switch (arr.*) {
        .array => |child| child,
        else => return self.err(
            .{ .non_array_indexing = .{ .found = self.getTypeName(arr) } },
            span,
        ),
    };

    try self.expectArrayIndex(expr.index, ctx);
    self.makeInstruction(
        .{ .array_access = .{ .cow = false, .incr_ref = false } },
        span.start,
        .{ .set_at = index },
    );

    return type_value;
}

/// Analyze the expression and return an error if the type isn't an integer
fn expectArrayIndex(self: *Self, expr: *const Expr, ctx: *Context) Error!void {
    const index = try self.analyzeExpr(expr, ctx);

    if (index != self.type_interner.cache.int) return self.err(
        .{ .non_integer_index = .{ .found = self.getTypeName(index) } },
        self.ast.getSpan(expr),
    );
}

// TODO: handle dead code eleminitaion so that it can be used in function's body?
fn block(self: *Self, expr: *const Ast.Block, ctx: *Context) Result {
    self.scope.open(self.allocator, true);
    errdefer _ = self.scope.close();

    const index = self.ir_builder.reserveInstr();
    var final_type = self.type_interner.cache.void;
    var len: usize = expr.nodes.len;

    for (expr.nodes, 0..) |*node, i| {
        final_type = try self.analyzeNode(node, ctx);

        if (!self.isVoid(final_type) and i != expr.nodes.len - 1) {
            return self.err(.unused_value, expr.span);
        }

        // Nothing to do at compile time for import statements
        if (node.* == .use) {
            len -= 1;
        }
    }

    const count = self.scope.close();
    // TODO: protect cast
    self.makeInstruction(
        .{ .block = .{
            .length = len,
            .pop_count = @intCast(count),
            .is_expr = !self.isVoid(final_type),
        } },
        self.ast.getSpan(expr).start,
        .{ .set_at = index },
    );

    return final_type;
}

fn binop(self: *Self, expr: *const Ast.Binop, ctx: *Context) Result {
    const op = expr.op;
    const index = self.ir_builder.reserveInstr();

    const lhs = try self.analyzeExpr(expr.lhs, ctx);
    const rhs = try self.analyzeExpr(expr.rhs, ctx);

    const lhs_span = self.ast.getSpan(expr.lhs);
    const rhs_span = self.ast.getSpan(expr.rhs);

    if (isStringConcat(op, lhs, rhs)) {
        self.makeInstruction(.{ .binop = .{ .op = .add_str } }, lhs_span.start, .{ .set_at = index });
        return self.type_interner.cache.str;
    } else if (isStringRepeat(op, lhs, rhs)) {
        self.makeInstruction(
            .{ .binop = .{ .op = .mul_str, .cast = if (rhs.is(.int)) .rhs else .lhs } },
            lhs_span.start,
            .{ .set_at = index },
        );
        return self.type_interner.cache.str;
    }

    var instr = Instruction.Binop{ .op = undefined };
    var result_type = lhs;

    switch (op) {
        .plus, .slash, .star, .minus => {
            try self.expectNumeric(lhs, lhs_span);
            try self.expectNumeric(rhs, rhs_span);
            const info = self.getArithmeticOp(op, lhs, rhs, expr);
            instr = info.instr;
            result_type = info.result_type;
        },
        .equal_equal, .bang_equal => {
            const info = try self.getEqualityOp(op, lhs, rhs, expr);
            instr = info.instr;
            result_type = info.result_type;
        },
        .greater_equal, .greater, .less_equal, .less => {
            try self.expectNumeric(lhs, lhs_span);
            try self.expectNumeric(rhs, rhs_span);
            const info = try self.getComparisonOp(op, lhs, rhs, expr);
            instr = info.instr;
            result_type = info.result_type;
        },
        .@"and", .@"or" => {
            try self.checkBooleanLogic(lhs, rhs, expr);
            instr.op = if (op == .@"and") .@"and" else .@"or";
            result_type = self.type_interner.cache.bool;
        },
        else => unreachable,
    }

    self.makeInstruction(.{ .binop = instr }, lhs_span.start, .{ .set_at = index });
    return result_type;
}

fn closure(self: *Self, expr: *const Ast.FnDecl, ctx: *Context) Result {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const closure_idx = self.ir_builder.reserveInstr();

    self.scope.open(self.allocator, false);
    defer _ = self.scope.close();

    const captures_instrs_data = try self.loadFunctionCaptures(expr.meta.captures.keys());
    const param_res = try self.fnParams(expr.params, ctx);

    // Update type for resolution in function's body
    const closure_type: Type.Function = .{
        .params = param_res.params,
        .return_type = try self.checkAndGetType(expr.return_type, ctx),
        .is_method = false,
    };
    const interned_type = self.type_interner.intern(.{ .function = closure_type });

    ctx.fn_type = interned_type;
    const len = try self.fnBody(expr.body.nodes, &closure_type, ctx);

    const offset = self.ast.getSpan(expr).start;
    const captures_count = self.makeFunctionCapturesInstr(captures_instrs_data, offset);

    // TODO: protect the cast
    self.makeInstruction(
        .{ .fn_decl = .{
            .kind = .closure,
            .name = null,
            .body_len = len,
            .default_params = @intCast(param_res.default_count),
            .captures_count = captures_count,
            .return_kind = if (ctx.returns) .explicit else if (self.isVoid(interned_type)) .implicit_void else .implicit_value,
        } },
        offset,
        .{ .set_at = closure_idx },
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
    var instr = Instruction.Binop{ .op = switch (op) {
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
                instr.cast = .rhs;
                self.warn(AnalyzerMsg.implicitCast("right hand side", self.getTypeName(lhs)), self.ast.getSpan(expr.rhs));
            }
        },
        .int => {
            if (rhs.is(.float)) {
                instr.cast = .lhs;
                self.warn(AnalyzerMsg.implicitCast("left hand side", self.getTypeName(rhs)), self.ast.getSpan(expr.lhs));
                result_type = self.type_interner.cache.float;
            } else {
                instr.op = switch (op) {
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

    return .{ .instr = instr, .result_type = result_type };
}

fn getEqualityOp(self: *Self, op: TokenTag, lhs: *const Type, rhs: *const Type, expr: *const Ast.Binop) Error!ArithmeticResult {
    var instr = Instruction.Binop{ .op = undefined };

    instr.op = switch (op) {
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
                instr.cast = .lhs;
                self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
            } else {
                instr.cast = .rhs;
                self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
            }

            instr.op = if (op == .equal_equal) .eq_float else .ne_float;
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

    return .{ .instr = instr, .result_type = self.type_interner.cache.bool };
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
    // TODO: remove when bug fix in implicit first arg
    is_method: bool = false,
    // TODO: remove when bug fix in implicit first arg
    lhs_is_value: bool = false,

    assignable: bool = false,
    mutable: bool = false,
};

/// Returns the type of the callee and if it's a value, not a type
fn field(self: *Self, expr: *const Ast.Field, ctx: *Context) Error!FieldResult {
    const span = self.ast.getSpan(expr.structure);
    const index = self.ir_builder.reserveInstr();
    const struct_res: FieldResult = switch (expr.structure.*) {
        .field => |*e| try self.field(e, ctx),
        .literal => |e| b: {
            const ident = try self.identifier(e.idx, true, ctx);
            break :b .{ .type = ident.type, .lhs_is_value = ident.kind == .variable };
        },
        else => .{ .type = try self.analyzeExpr(expr.structure, ctx), .lhs_is_value = true },
    };

    const field_res = switch (struct_res.type.*) {
        .module => |ty| return self.moduleAccess(expr.field, ty, index),
        .structure => |*ty| try self.structureAccess(expr.field, ty, struct_res.lhs_is_value, ctx),
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.getTypeName(struct_res.type) } },
            span,
        ),
    };

    const kind: Instruction.Field.Kind = switch (field_res.kind) {
        // TODO: create just a 'function'. For now we need this because we get static method from
        // symbols that are loaded on stack, not in register so we need a separate logic
        .function => if (struct_res.lhs_is_value) .method else .static_method,
        else => .field,
    };

    if (kind == .method and !ctx.in_call) {
        return self.boundMethod(field_res.type, field_res.index, span, index);
    }

    self.makeInstruction(
        .{ .field = .{ .index = field_res.index, .kind = kind } },
        span.start,
        .{ .set_at = index },
    );

    return .{
        .type = field_res.type,
        .is_method = kind == .method,
        .lhs_is_value = struct_res.lhs_is_value,
        // TODO: later we'll have nested declaration and this flag will have to be computed
        .assignable = true,
        .mutable = kind == .field,
    };
}

const AccessResult = struct {
    type: *const Type,
    kind: enum { field, function },
    index: usize,
};

fn structureAccess(
    self: *Self,
    field_tk: Ast.TokenIndex,
    struct_type: *const Type.Structure,
    is_value: bool,
    ctx: *const Context,
) Error!AccessResult {
    const text = self.ast.toSource(field_tk);
    const field_name = self.interner.intern(text);

    return if (struct_type.fields.getPtr(field_name)) |f|
        .{ .type = f.type, .kind = .field, .index = struct_type.fields.getIndex(field_name).? }
    else if (struct_type.functions.getPtr(field_name)) |f| b: {
        const function = f.type.function;

        check: {
            if (!ctx.in_call) break :check;
            if (is_value and !function.is_method) {
                return self.err(.{ .call_static_on_instance = .{ .name = text } }, self.ast.getSpan(field_tk));
            } else if (!is_value and function.is_method) {
                return self.err(.{ .call_method_on_type = .{ .name = text } }, self.ast.getSpan(field_tk));
            }
        }

        break :b .{ .type = f.type, .kind = .function, .index = struct_type.functions.getIndex(field_name).? };
    } else self.err(.{ .undeclared_field_access = .{ .name = text } }, self.ast.getSpan(field_tk));
}

fn moduleAccess(
    self: *Self,
    field_tk: Ast.TokenIndex,
    module_idx: InternerIdx,
    instr_index: usize,
) Error!FieldResult {
    const span = self.ast.getSpan(field_tk);
    const text = self.ast.toSource(field_tk);
    const field_name = self.interner.intern(text);
    const module = self.pipeline.ctx.module_interner.getAnalyzed(module_idx).?;
    // TODO: error
    const sym = module.symbols.getPtr(field_name) orelse return self.err(
        .{ .missing_symbol_in_module = .{ .module = module.name, .symbol = text } },
        span,
    );

    const index = self.pipeline.ctx.module_interner.analyzed.getIndex(module_idx).?;

    // TODO: protect the cast
    self.makeInstruction(
        .{ .load_symbol = .{ .module_index = index, .symbol_index = @intCast(sym.index) } },
        span.start,
        .{ .set_at = instr_index },
    );

    const is_struct = sym.type.* == .structure;
    return .{ .type = sym.type, .assignable = !is_struct, .mutable = !is_struct };
}

fn boundMethod(self: *Self, func_type: *const Type, field_index: usize, span: Span, instr_idx: usize) FieldResult {
    const bounded_type = func_type.function.toBoundMethod(self.cached_names.self, self.allocator);
    const ty = self.type_interner.intern(.{ .function = bounded_type });
    self.makeInstruction(.{ .bound_method = field_index }, span.start, .{ .set_at = instr_idx });

    return .{ .type = ty, .is_method = false, .lhs_is_value = true, .assignable = true };
}

fn call(self: *Self, expr: *const Ast.FnCall, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const index = self.ir_builder.reserveInstr();

    var args: ArrayListUnmanaged(*const Expr) = .{};
    defer args.deinit(self.allocator);

    const ctx_call = ctx.setAndGetPrevious(.in_call, true);

    // Implicit first argument
    const callee = switch (expr.callee.*) {
        .field => |*f| b: {
            const ty = try self.field(f, ctx);

            if (ty.lhs_is_value and ty.is_method) {
                args.append(self.allocator, expr.callee.field.structure) catch oom();
            }

            break :b ty.type;
        },
        else => try self.analyzeExpr(expr.callee, ctx),
    };

    if (!callee.is(.function)) {
        return self.err(.invalid_call_target, span);
    }

    args.appendSlice(self.allocator, expr.args) catch oom();

    // Restore state before arguments analyzis
    ctx.in_call = ctx_call;
    const arity, const default_count = try self.fnArgsList(args.items, &callee.function, span, ctx);

    // TODO: protect casts
    self.makeInstruction(
        .{ .call = .{ .arity = @intCast(arity), .default_count = @intCast(default_count) } },
        span.start,
        .{ .set_at = index },
    );

    return callee.function.return_type;
}

// TODO: rewrite
fn fnArgsList(self: *Self, args: []*const Expr, ty: *const Type.Function, err_span: Span, ctx: *Context) Error!struct { usize, usize } {
    const param_count = ty.params.count();

    if (args.len > param_count) return self.err(
        AnalyzerMsg.tooManyFnArgs(param_count, args.len),
        err_span,
    );

    var proto = ty.proto(self.allocator);
    var proto_values = proto.values();
    const start = self.ir_builder.count();
    self.ir_builder.ensureUnusedSize(param_count);

    // We initialize all the values used for the initialization. By default, we put empty data under
    // the form of 'default_value' but we check for all real param default to mark their index (order
    // of declaration) so that the compiler can emit the right index
    var default_count: usize = 0;
    for (ty.params.values()) |*f| {
        self.makeInstruction(.{ .default_value = default_count }, err_span.start, .add_no_alloc);
        if (f.default) default_count += 1;
    }

    for (args, 0..) |arg, i| {
        var cast = false;
        var value_instr: usize = 0;
        var param_info: *const Type.Parameter = undefined;
        var param_index: usize = undefined;
        const span_start = self.ast.getSpan(arg).start;

        switch (arg.*) {
            .named_arg => |na| {
                const name = self.interner.intern(self.ast.toSource(na.name));
                proto.putAssumeCapacity(name, true);
                param_index = proto.getIndex(name).?;

                param_info = ty.params.getPtr(name) orelse return self.err(
                    .{ .unknown_param = .{ .name = self.ast.toSource(na.name) } },
                    self.ast.getSpan(na.name),
                );

                value_instr = self.ir_builder.count();
                const value_type = try self.analyzeExpr(na.value, ctx);
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(na.value))).cast;
            },
            else => {
                value_instr = self.ir_builder.count();
                const value_type = try self.analyzeExpr(arg, ctx);
                param_info = &ty.params.values()[i];
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(arg))).cast;
                proto_values[i] = true;
                param_index = i;
            },
        }

        self.makeInstruction(
            .{ .value = .{ .value_instr = value_instr, .cast = cast, .box = param_info.captured } },
            span_start,
            .{ .set_at = start + param_index },
        );
    }

    // Check if any missing non-default parameter
    const err_count = self.errs.items.len;

    for (proto_values, 0..) |has_value, i| if (!has_value) {
        self.err(
            .{ .missing_function_param = .{ .name = self.interner.getKey(proto.keys()[i]).? } },
            err_span,
        ) catch {};
    };

    return if (err_count < self.errs.items.len) error.Err else .{ param_count, default_count };
}

/// Tries to find a match from variables and symbols and returns its type while emitting an instruction
fn identifier(
    self: *Self,
    token_name: Ast.TokenIndex,
    initialized: bool,
    ctx: *const Context,
) Error!struct { type: *const Type, kind: enum { variable, symbol, module } } {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    if (self.variableIdentifier(name, span)) |variable| {
        if (initialized and !variable.initialized) {
            return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(token_name));
        }

        return .{ .type = variable.type, .kind = .variable };
    }

    const sym_name = if (name == self.cached_names.Self) b: {
        const struct_type = ctx.struct_type orelse return self.err(.big_self_outside_struct, span);
        break :b struct_type.structure.symbol.name;
    } else name;

    if (self.symbolIdentifier(sym_name, span)) |sym| {
        return .{ .type = sym.type, .kind = .symbol };
    }

    if (self.externSymbolIdentifier(sym_name, span)) |sym| {
        return .{ .type = sym.type, .kind = .symbol };
    }

    if (self.scope.getModule(name)) |mod| {
        return .{ .type = mod, .kind = .module };
    }

    return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(token_name));
}

fn expectVariableIdentifier(self: *Self, token_name: Ast.TokenIndex) Error!*LexicalScope.Variable {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    return self.variableIdentifier(name, span) orelse return self.err(
        .{ .undeclared_var = .{ .name = self.interner.getKey(name).? } },
        span,
    );
}

/// Tries to find a variable in scopes and returns it while emitting an instruction
fn variableIdentifier(self: *Self, name: InternerIdx, span: Span) ?*LexicalScope.Variable {
    const variable, const scope_offset = self.scope.getVariable(name) orelse return null;

    // TODO: useless
    const scope: rir.Scope = switch (variable.kind) {
        .local => .local,
        .global => .global,
    };

    self.makeInstruction(
        .{ .identifier = .{
            .index = variable.index + scope_offset,
            .scope = scope,
            .unbox = variable.captured,
        } },
        span.start,
        .add,
    );

    return variable;
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn symbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?*LexicalScope.Symbol {
    const sym = self.scope.getSymbol(name) orelse return null;
    // TODO: protect cast
    self.makeInstruction(
        .{ .load_symbol = .{ .module_index = null, .symbol_index = @intCast(sym.index) } },
        span.start,
        .add,
    );

    return sym;
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn externSymbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?*LexicalScope.Symbol {
    const ext = self.scope.getExternSymbol(name) orelse return null;
    // TODO: protect cast
    self.makeInstruction(
        .{ .load_symbol = .{ .module_index = ext.module_index, .symbol_index = @intCast(ext.symbol.index) } },
        span.start,
        .add,
    );

    return &ext.symbol;
}

fn ifExpr(self: *Self, expr: *const Ast.If, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.condition);
    const index = self.ir_builder.reserveInstr();
    var instr: Instruction.If = .{ .cast = .none, .has_else = false };

    const cond_type = try self.analyzeExpr(expr.condition, ctx);

    // We can continue to analyze if the condition isn't a bool
    if (!cond_type.is(.bool)) {
        self.err(
            .{ .non_bool_cond = .{ .what = "if", .found = self.getTypeName(cond_type) } },
            span,
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
        instr.has_else = true;
        else_returned = ctx.returns;

        if (!else_returned) {
            if (then_returned) {
                final_type = else_type;
            } else {
                try self.ifTypeCoherenceAndCast(then_type, else_type, expr, &instr);
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
    self.makeInstruction(.{ .@"if" = instr }, span.start, .{ .set_at = index });

    return if (ctx.returns) self.type_interner.cache.void else final_type;
}

fn ifTypeCoherenceAndCast(
    self: *Self,
    then_type: *const Type,
    else_type: *const Type,
    expr: *const Ast.If,
    instr: *Instruction.If,
) Error!void {
    if (then_type == else_type) return;

    if (else_type.canCastTo(then_type)) {
        instr.cast = .@"else";
        self.warn(AnalyzerMsg.implicitCast("else branch", "float"), self.ast.getSpan(expr.@"else".?));
    } else if (then_type.canCastTo(else_type)) {
        instr.cast = .then;
        self.warn(AnalyzerMsg.implicitCast("then branch", "float"), self.ast.getSpan(expr.then));
    } else return self.err(
        .{ .incompatible_if_type = .{
            .found1 = self.getTypeName(then_type),
            .found2 = self.getTypeName(else_type),
        } },
        self.ast.getSpan(expr),
    );
}

fn literal(self: *Self, expr: *const Ast.Literal, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const text = self.ast.toSource(expr);

    switch (expr.tag) {
        .bool => {
            self.makeInstruction(.{ .bool = self.ast.token_tags[expr.idx] == .true }, span.start, .add);
            return self.type_interner.cache.bool;
        },
        .identifier, .self => return (try self.identifier(expr.idx, true, ctx)).type,
        .int => {
            const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                // TODO: error handling, only one possible it's invalid char
                std.debug.print("Error parsing integer\n", .{});
                break :blk 0;
            };
            self.makeInstruction(.{ .int = value }, span.start, .add);
            return self.type_interner.cache.int;
        },
        .float => {
            const value = std.fmt.parseFloat(f64, text) catch blk: {
                // TODO: error handling, only one possible it's invalid char or too big
                std.debug.print("Error parsing float\n", .{});
                break :blk 0.0;
            };
            self.makeInstruction(.{ .float = value }, span.start, .add);
            return self.type_interner.cache.float;
        },
        .null => {
            self.makeInstruction(.null, span.start, .add);
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
                            span,
                        ),
                    }
                } else final.append(self.allocator, c) catch oom();
            }

            const value = self.interner.intern(final.toOwnedSlice(self.allocator) catch oom());
            self.makeInstruction(.{ .string = value }, span.start, .add);
            return self.type_interner.cache.str;
        },
    }
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral, ctx: *Context) Error!*const Type {
    const span = self.ast.getSpan(expr.structure);
    const index = self.ir_builder.reserveInstr();
    const ty = try self.analyzeExpr(expr.structure, ctx);

    const struct_type = if (ty.is(.structure)) ty.structure else {
        return self.err(.non_struct_struct_literal, span);
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
        self.makeInstruction(.{ .default_value = default_count }, span.start, .add);
        if (f.default) default_count += 1;
    }

    // BUG: Doesn't check for field duplication
    for (expr.fields) |*fv| {
        const field_span = self.ast.getSpan(fv.name);
        const field_name = self.interner.intern(self.ast.toSource(fv.name));

        const f = struct_type.fields.get(field_name) orelse return self.err(
            .{ .unknown_struct_field = .{ .name = self.ast.toSource(fv.name) } },
            field_span,
        );
        const field_index = struct_type.fields.getIndex(field_name).?;

        const value_instr = self.ir_builder.count();
        proto.putAssumeCapacity(field_name, true);

        const typ = if (fv.value) |val|
            try self.analyzeExpr(val, ctx)
        else // Syntax: { x } instead of { x = x }
            (try self.expectVariableIdentifier(fv.name)).type;

        const value_span = if (fv.value) |val| self.ast.getSpan(val) else field_span;
        const coercion = try self.performTypeCoercion(f.type, typ, false, value_span);

        self.makeInstruction(
            .{ .value = .{ .value_instr = value_instr, .cast = coercion.cast, .box = false } },
            field_span.start,
            .{ .set_at = start + field_index },
        );
    }

    if (expr.fields.len != proto.count()) {
        var kv = proto.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.*) self.err(
                .{ .missing_field_struct_literal = .{ .name = self.interner.getKey(entry.key_ptr.*).? } },
                span,
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
        span.start,
        .{ .set_at = index },
    );

    return ty;
}

fn returnExpr(self: *Self, expr: *const Ast.Return, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const index = self.ir_builder.reserveInstr();
    var instr: Instruction.Return = .{ .value = false, .cast = false };

    var ty = if (expr.expr) |e| blk: {
        instr.value = true;
        break :blk try self.analyzeExpr(e, ctx);
    } else self.type_interner.cache.void;

    // We check after to advance node idx
    const fn_type = ctx.fn_type orelse return self.err(.return_outside_fn, span);
    const return_type = fn_type.function.return_type;

    // We do that here because we can insert a cast
    if (return_type != ty) {
        if (ty.canCastTo(return_type)) {
            instr.cast = true;
            self.makeInstruction(.{ .cast = .float }, span.start, .add);
            ty = self.type_interner.cache.float;
        } else return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(return_type),
                .found = self.getTypeName(ty),
            } },
            if (expr.expr) |e| self.ast.getSpan(e) else span,
        );
    }

    ctx.returns = true;
    self.makeInstruction(.{ .@"return" = instr }, span.start, .{ .set_at = index });

    return ty;
}

fn unary(self: *Self, expr: *const Ast.Unary, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const op = self.ast.token_tags[expr.op];
    const index = self.ir_builder.reserveInstr();
    var instr: Instruction.Unary = .{ .op = if (op == .not) .bang else .minus, .typ = .float };

    const rhs = try self.analyzeExpr(expr.expr, ctx);

    if (op == .not and !rhs.is(.bool)) {
        return self.err(.{ .invalid_unary = .{ .found = self.getTypeName(rhs) } }, span);
    } else if (op == .minus and !rhs.isNumeric()) {
        return self.err(AnalyzerMsg.invalidArithmetic(self.getTypeName(rhs)), span);
    }

    if (rhs.is(.int)) instr.typ = .int;

    self.makeInstruction(.{ .unary = instr }, span.start, .{ .set_at = index });

    return rhs;
}

/// Checks if identifier name is already declared, otherwise interns it and returns the key
fn internIfNotInCurrentScope(self: *Self, token: usize) Error!usize {
    const name = self.interner.intern(self.ast.toSource(token));

    if (self.scope.isVarOrSymInCurrentScope(name)) return self.err(
        .{ .already_declared = .{ .name = self.interner.getKey(name).? } },
        self.ast.getSpan(token),
    );

    return name;
}

/// Checks that the node is a declared type and return it's value. If node is `.empty`, returns `void`
fn checkAndGetType(self: *Self, ty: ?*const Ast.Type, ctx: *const Context) Result {
    return if (ty) |t| return switch (t.*) {
        .array => |arr_type| {
            const child = try self.checkAndGetType(arr_type.child, ctx);

            if (self.isVoid(child)) {
                return self.err(.void_array, self.ast.getSpan(arr_type.child));
            }

            return self.type_interner.intern(.{ .array = child });
        },
        .fields => |fields| {
            if (fields.len > 2) @panic("Nested types are not supported yet");

            const module_token = fields[0];
            const module_infos = try self.identifier(module_token, true, ctx);
            const module_type = module_infos.type;

            if (!module_type.is(.module)) return self.err(
                .{ .dot_type_on_non_mod = .{ .found = self.getTypeName(module_type) } },
                self.ast.getSpan(module_token),
            );

            const module = self.pipeline.ctx.module_interner.getAnalyzed(module_type.module) orelse {
                @panic("Non existing module");
            };

            const symbol_token = fields[1];
            const symbol_name = self.interner.intern(self.ast.toSource(symbol_token));
            const final = module.symbols.get(symbol_name) orelse return self.err(
                .{ .missing_symbol_in_module = .{
                    .module = self.ast.toSource(module_token),
                    .symbol = self.ast.toSource(symbol_token),
                } },
                self.ast.getSpan(symbol_token),
            );

            return final.type;
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
    if (self.isVoid(decl) and value.* == .array and self.isVoid(value.array)) {
        return self.err(.cant_infer_arary_type, span);
    }

    // TODO: proper error handling
    if (value.is(.module)) @panic("Can't use modules in expressions");

    if (self.isVoid(local_decl)) {
        local_decl = local_value;
    } else {
        if (local_decl != local_value) {
            // One case in wich we can coerce, int -> float
            if (local_value.canCastTo(local_decl)) {
                cast = true;
                if (emit_cast) self.makeInstruction(.{ .cast = .float }, span.start, .add);
            } else return self.err(
                .{ .type_mismatch = .{ .expect = self.getTypeName(local_decl), .found = self.getTypeName(local_value) } },
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

/// Checks if the expression is a literal generating a heap object
fn isHeapLiteral(expr: *const Expr) bool {
    return switch (expr.*) {
        .array, .struct_literal => true,
        else => false,
    };
}

fn declareVariable(
    self: *Self,
    name: InternerIdx,
    ty: *const Type,
    captured: bool,
    initialized: bool,
    constant: bool,
    span: Span,
) Error!usize {
    return self.scope.declareVar(
        self.allocator,
        name,
        ty,
        captured,
        initialized,
        constant,
    ) catch self.err(.too_many_locals, span);
}

fn declareSymbol(self: *Self, name: InternerIdx, ty: *const Type) void {
    self.scope.declareSymbol(self.allocator, name, ty);

    if (self.scope.isGlobal()) {
        self.globals.append(self.allocator, self.scope.symbol_count) catch oom();
    }
}
