const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const FieldEnum = std.meta.FieldEnum;

const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const Pipeline = @import("../Pipeline.zig");
const GenReport = @import("../reporter.zig").GenReport;
const Sb = @import("../StringBuilder.zig");
const oom = @import("../utils.zig").oom;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const Importer = @import("Importer.zig");
const IrBuilder = @import("IrBuilder.zig");
const LexScope = @import("LexicalScope.zig");
const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const Span = @import("Lexer.zig").Span;
const TokenTag = @import("Lexer.zig").Token.Tag;
const Type = @import("types.zig").Type;
const TypeInterner = @import("types.zig").TypeInterner;

pub const AnalyzedModule = struct {
    name: []const u8,
    globals: LexScope.VariableMap,
    symbols: LexScope.SymbolArrMap,
};

const Context = struct {
    fn_type: ?*const Type,
    struct_type: ?*const Type,
    allow_partial: bool,
    returns: bool,
    in_call: bool,

    pub const empty: Context = .{
        .fn_type = null,
        .struct_type = null,
        .allow_partial = true,
        .returns = false,
        .in_call = false,
    };

    const ContextSnapshot = struct {
        saved: Context,
        ctx: *Context,

        pub fn restore(self: ContextSnapshot) void {
            self.ctx.* = self.saved;
        }
    };

    pub fn setAndGetPrevious(self: *Context, comptime f: FieldEnum(Context), value: @FieldType(Context, @tagName(f))) @TypeOf(value) {
        const prev = @field(self, @tagName(f));
        @field(self, @tagName(f)) = value;

        return prev;
    }

    pub fn snapshot(self: *Context) ContextSnapshot {
        return .{ .saved = self.*, .ctx = self };
    }

    pub fn reset(self: *Context) void {
        self.* = .empty;
    }
};

const Self = @This();
const Error = error{Err};
const TypeResult = Error!*const Type;
const TypeInfos = struct {
    type: *const Type,
    heap_ref: bool = false,
    is_sym: bool = false,
    comp_time: bool = false,

    pub fn newType(ty: *const Type) TypeInfos {
        return .{ .type = ty };
    }
};
const Result = Error!TypeInfos;
pub const AnalyzerReport = GenReport(AnalyzerMsg);

allocator: Allocator,
pipeline: *Pipeline,
interner: *Interner,
path: *Sb,
containers: Sb,

errs: ArrayList(AnalyzerReport),
warns: ArrayList(AnalyzerReport),
ast: *const Ast,
scope: LexScope,
type_interner: *TypeInterner,
ir_builder: IrBuilder,
main: ?usize,

module_name: InternerIdx,
cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },

pub fn init(allocator: Allocator, pipeline: *Pipeline, module_name: InternerIdx) Self {
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
        .main = null,

        .module_name = module_name,
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
    var ctx: Context = .empty;

    // Excluding file extension
    self.containers.append(self.allocator, module_name[0 .. module_name.len - 3]);

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

    return .{
        .name = module_name,
        .globals = self.scope.current.variables,
        .symbols = self.scope.current.symbols,
    };
}

fn analyzeNode(self: *Self, node: *const Node, ctx: *Context) TypeResult {
    const res = try self.analyzeNodeInfos(node, ctx);
    return res.type;
}

fn analyzeNodeInfos(self: *Self, node: *const Node, ctx: *Context) Result {
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
        .expr => |n| return try self.analyzeExprInfos(n, ctx),
    }

    return .newType(self.type_interner.cache.void);
}

fn assignment(self: *Self, node: *const Ast.Assignment, ctx: *Context) Error!void {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();
    const span = self.ast.getSpan(node.assigne);

    ctx.allow_partial = false;
    const index = self.ir_builder.reserveInstr();
    const value_res = try self.analyzeExprInfos(node.value, ctx);

    const maybe_assigne: ?TypeInfos = switch (node.assigne.*) {
        .literal => |*e| b: {
            if (e.tag != .identifier) break :b null;
            var assigne = try self.expectVariableIdentifier(e.idx);
            if (assigne.constant) return self.err(
                .{ .assign_to_constant = .{ .name = self.ast.toSource(e.idx) } },
                span,
            );
            assigne.initialized = true;
            break :b .newType(assigne.type);
        },
        .field => |*e| b: {
            const field_result = try self.field(e, ctx);
            // TODO: wrong error name?
            if (field_result.is_sym) return self.err(.assign_to_struct_fn, span);
            // Resolving methods without call result in a bound method
            if (field_result.type.* == .function and field_result.type.function.kind == .bound) return self.err(.assign_to_struct_fn, span);
            break :b .newType(field_result.type);
        },
        // TODO: later, it will be authorized with returned references
        .fn_call => return self.err(.invalid_assign_target, span),
        .array_access => try self.analyzeExprInfos(node.assigne, ctx),
        else => return self.err(.invalid_assign_target, span),
    };
    const assigne = maybe_assigne orelse return self.err(.invalid_assign_target, span);
    const coherence = try self.performTypeCoercion(assigne.type, value_res.type, false, self.ast.getSpan(node.value));

    self.makeInstruction(
        .{ .assignment = .{
            .cast = coherence.cast,
            .cow = assigne.type.isHeap(),
            .incr_rc = value_res.heap_ref,
        } },
        span.start,
        .{ .set_at = index },
    );
}

fn discard(self: *Self, expr: *const Expr, ctx: *Context) Error!void {
    const span = self.ast.getSpan(expr);
    self.makeInstruction(.{ .discard = undefined }, span.start, .add);
    const discarded = try self.analyzeExpr(expr, ctx);

    if (self.isVoid(discarded)) return self.err(.void_discard, span);
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl, ctx: *Context) Error!void {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);
    const fn_idx = self.ir_builder.reserveInstr();

    // Forward declaration in outer scope for recursion
    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    // Forward declaration in outer scope for recursion
    var sym = self.scope.forwardDeclareSymbol(self.allocator, name);

    self.scope.open(self.allocator, false);
    errdefer _ = self.scope.close();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    const captures_instrs_data = try self.loadFunctionCaptures(&node.meta.captures);
    const param_res = try self.fnParams(node.params, ctx);

    var fn_type: Type.Function = .{
        .loc = .{ .name = name, .container = container_name },
        .params = param_res.decls,
        .return_type = try self.checkAndGetType(node.return_type, ctx),
        .kind = if (param_res.is_method) .method else .normal,
    };
    const interned_type = self.type_interner.intern(.{ .function = fn_type });

    sym.type = interned_type;

    // Save the index because function's body could invalidated `sym` pointer
    ctx.fn_type = interned_type;
    const len = try self.fnBody(node.body.nodes, &fn_type, span, ctx);
    _ = self.scope.close();

    // If in a structure declaration, we remove the symbol as it's gonna live inside the structure
    const captures_count = self.makeFunctionCapturesInstr(captures_instrs_data, span.start);
    const is_closure = captures_count > 0 and !self.scope.isGlobal();

    if (is_closure) {
        _ = self.scope.removeSymbolFromScope(name);
        _ = try self.declareVariable(name, interned_type, false, true, true, false, span);
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
            .return_kind = if (ctx.returns) .explicit else if (self.isVoid(fn_type.return_type)) .implicit_void else .implicit_value,
        } },
        span.start,
        .{ .set_at = fn_idx },
    );
}

fn loadFunctionCaptures(self: *Self, captures: *const Ast.FnDecl.Meta.Captures) Error![]const Instruction.Data {
    var instructions: ArrayList(Instruction.Data) = .{};
    instructions.ensureTotalCapacity(self.allocator, captures.count()) catch oom();

    var it = captures.iterator();
    while (it.next()) |capt| {
        const name = capt.key_ptr.*;
        const capt_infos = capt.value_ptr.*;
        const variable, _ = self.scope.getVariable(name) orelse unreachable;
        _ = try self.declareVariable(name, variable.type, true, true, false, false, .zero);
        instructions.appendAssumeCapacity(.{ .capture = .{ .index = capt_infos.index, .is_local = capt_infos.is_local } });
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
    decls: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter),
    default_count: usize,
    is_method: bool,
};
fn fnParams(self: *Self, params: []Ast.VarDecl, ctx: *Context) Error!Params {
    var decls: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter) = .empty;
    decls.ensureTotalCapacity(self.allocator, params.len) catch oom();

    var default_count: usize = 0;
    var is_method = false;

    for (params, 0..) |*p, i| {
        const span = self.ast.getSpan(p.name);
        const param_name = self.interner.intern(self.ast.toSource(p.name));

        if (i == 0 and param_name == self.cached_names.self) {
            const self_type = ctx.struct_type orelse return self.err(.self_outside_struct, span);

            is_method = true;
            _ = try self.declareVariable(param_name, self_type, p.meta.captured, true, true, false, .zero);
            decls.putAssumeCapacity(param_name, .{ .type = self_type, .default = false, .captured = false });
            continue;
        }

        if (self.scope.isVarOrSymInCurrentScope(param_name)) {
            return self.err(.{ .duplicate_param = .{ .name = self.ast.toSource(p.name) } }, span);
        }

        var param_type = try self.checkAndGetType(p.typ, ctx);
        if (p.value) |val| {
            const value_res = try self.defaultValue(param_type, val, ctx);
            const coerce = try self.performTypeCoercion(param_type, value_res.type, true, span);

            if (!value_res.comp_time) {
                return self.err(.{ .non_comptime_default = .new(.parameter) }, self.ast.getSpan(val));
            }

            default_count += 1;
            param_type = coerce.type;
        }

        if (self.isVoid(param_type)) {
            return self.err(.void_param, span);
        }

        _ = try self.declareVariable(param_name, param_type, p.meta.captured, true, true, false, span);
        decls.putAssumeCapacity(param_name, .{
            .type = param_type,
            .default = p.value != null,
            .captured = p.meta.captured,
        });
    }

    return .{ .decls = decls, .default_count = default_count, .is_method = is_method };
}

fn fnBody(self: *Self, body: []Node, fn_type: *const Type.Function, name_span: Span, ctx: *Context) Error!usize {
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
        ctx.allow_partial = i < len - 1;

        // We try to analyze the whole body
        // const ty = self.analyzeNode(n, ctx) catch {
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
        const err_span = if (body.len > 0) self.ast.getSpan(body[body.len - 1]) else name_span;

        // TODO: make 'typeCoercion' accept a param: 'allow_void_decl' to check for first if cond
        // there is the same in return or fnBody
        if (self.isVoid(fn_type.return_type) and !self.isVoid(final_type)) {
            return self.err(.{ .type_mismatch = .{ .expect = "void", .found = self.getTypeName(final_type) } }, err_span);
        } else {
            const coerce = try self.performTypeCoercion(fn_type.return_type, final_type, false, err_span);

            if (coerce.cast) @panic("Casting return value from function not implemented");
        }
    }

    return len - deadcode_count;
}

fn defaultValue(self: *Self, decl_type: *const Type, default_value: *const Expr, ctx: *Context) Result {
    var value_res = try self.analyzeExprInfos(default_value, ctx);
    const coerce = try self.performTypeCoercion(decl_type, value_res.type, true, self.ast.getSpan(default_value));
    value_res.type = coerce.type;
    return value_res;
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

    // TODO: don't check only path but path + name?
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
            const sym = mod.symbols.get(item_name) orelse return self.err(
                .{ .missing_symbol_in_module = .{
                    .module = self.ast.toSource(node.names[node.names.len - 1]),
                    .symbol = self.ast.toSource(item.item),
                } },
                self.ast.getSpan(item.item),
            );

            // TODO: error
            if (!sym.type.is(.function) and !sym.type.is(.structure)) {
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

// TODO: document better this, it means having an assignable (non symbol) i think
fn expectValue(self: *Self, expr: *const Ast.Expr, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    const value_res: ?TypeInfos = switch (expr.*) {
        .literal => |*e| b: {
            if (e.tag != .identifier) break :b try self.literal(e, ctx);

            const value = self.identifier(e.idx, true, ctx) catch break :b null;
            if (value.kind == .symbol and value.type.* != .function) break :b null;
            break :b .{ .type = value.type, .heap_ref = value.type.isHeap() };
        },
        .field => |*e| b: {
            const field_res = try self.field(e, ctx);
            if (field_res.is_sym and field_res.type.* != .function) break :b null;
            break :b field_res;
        },
        else => try self.analyzeExprInfos(expr, ctx),
    };
    const final = value_res orelse return self.err(.assign_type, span);

    return if (self.isVoid(final.type)) self.err(.void_value, span) else final;
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) Error!void {
    const span = self.ast.getSpan(node.name);
    const name = try self.internIfNotInCurrentScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);
    const index = self.ir_builder.reserveInstr();

    var has_value = false;
    var cast = false;
    var incr_rc = false;
    var comp_time = false;

    if (node.value) |value| {
        has_value = true;
        ctx.allow_partial = false;

        const value_res = try self.expectValue(value, ctx);
        incr_rc = value_res.heap_ref;
        comp_time = value_res.comp_time;

        const coherence = try self.performTypeCoercion(checked_type, value_res.type, true, self.ast.getSpan(value));
        checked_type = coherence.type;
        cast = coherence.cast;
    }

    const decl_index = try self.declareVariable(name, checked_type, node.meta.captured, has_value, false, comp_time, span);

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

fn structDecl(self: *Self, node: *const Ast.StructDecl, ctx: *Context) Error!void {
    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);
    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    const sym = self.scope.forwardDeclareSymbol(self.allocator, name);
    var ty: Type.Structure = .{
        .loc = .{ .name = name, .container = container_name },
        .fields = .empty,
        .functions = .empty,
        .defaults = 0,
    };
    ty.fields.ensureTotalCapacity(self.allocator, node.fields.len) catch oom();
    ty.functions.ensureTotalCapacity(self.allocator, @intCast(node.functions.len)) catch oom();

    const index = self.ir_builder.reserveInstr();
    // TODO: merge as function's name
    self.makeInstruction(.{ .name = name }, span.start, .add);

    self.scope.open(self.allocator, false);
    defer _ = self.scope.close();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    // Create type before functions to allow 'self' to refer to the structure
    const interned_type = self.type_interner.intern(.{ .structure = ty });
    const interned_struct = &interned_type.structure;

    sym.type = interned_type;
    ctx.struct_type = interned_type;
    defer ctx.struct_type = null;

    // BUG: interned type doesn't take into account fields
    try self.structureFields(node.fields, interned_struct, ctx);

    for (node.functions) |*f| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        try self.fnDeclaration(f, ctx);
        // At this point, the symbol exists
        const fn_type = self.scope.removeSymbolFromScope(fn_name).?.type;
        interned_struct.functions.putAssumeCapacity(fn_name, fn_type);
    }

    self.makeInstruction(
        .{ .struct_decl = .{
            .index = sym.index,
            .fields_count = node.fields.len,
            .default_fields = interned_struct.defaults,
            .func_count = node.functions.len,
        } },
        span.start,
        .{ .set_at = index },
    );
}

fn structureFields(self: *Self, fields: []const Ast.VarDecl, ty: *Type.Structure, ctx: *Context) Error!void {
    for (fields) |*f| {
        const span = self.ast.getSpan(f.name);
        var struct_field: Type.Structure.Field = undefined;
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (ty.fields.get(field_name) != null) {
            return self.err(
                .{ .already_declared_field = .{ .name = self.ast.toSource(f.name) } },
                span,
            );
        }

        const field_type = try self.checkAndGetType(f.typ, ctx);
        const field_value_type = if (f.value) |value| blk: {
            ty.defaults += 1;
            struct_field.default = true;
            const res = try self.analyzeExprInfos(value, ctx);

            if (!res.comp_time) {
                return self.err(.{ .non_comptime_default = .new(.field) }, self.ast.getSpan(value));
            }

            break :blk res.type;
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

    const body = try self.block(&node.body, ctx);

    if (!self.isVoid(body.type)) return self.err(
        .{ .non_void_while = .{ .found = self.getTypeName(body.type) } },
        self.ast.getSpan(node.body),
    );
}

fn analyzeExpr(self: *Self, expr: *const Expr, ctx: *Context) TypeResult {
    const res = try self.analyzeExprInfos(expr, ctx);
    return res.type;
}

fn analyzeExprInfos(self: *Self, expr: *const Expr, ctx: *Context) Result {
    const ty = try switch (expr.*) {
        .array => |*e| self.array(e, ctx),
        .array_access => |*e| self.arrayAccess(e, ctx),
        .block => |*e| self.block(e, ctx),
        .binop => |*e| self.binop(e, ctx),
        .closure => |*e| self.closure(e, ctx),
        .field => |*e| self.field(e, ctx),
        .fn_call => |*e| self.call(e, ctx),
        .grouping => |*e| self.analyzeExprInfos(e.expr, ctx),
        .@"if" => |*e| self.ifExpr(e, ctx),
        .literal => |*e| self.literal(e, ctx),
        .named_arg => unreachable,
        .@"return" => |*e| self.returnExpr(e, ctx),
        .struct_literal => |*e| self.structLiteral(e, ctx),
        .unary => |*e| self.unary(e, ctx),
    };

    if (self.scope.isGlobal() and !ty.comp_time) {
        return self.err(.non_comptime_in_global, self.ast.getSpan(expr));
    }

    return ty;
}

fn array(self: *Self, expr: *const Ast.Array, ctx: *Context) Result {
    const index = self.ir_builder.reserveInstr();
    var final_type = self.type_interner.cache.void;
    var backpatched = false;
    var pure = true;

    var elems: ArrayList(Instruction.Array.Elem) = .{};
    elems.ensureUnusedCapacity(self.allocator, expr.values.len) catch oom();

    for (expr.values) |val| {
        var cast = false;
        const span = self.ast.getSpan(val);
        var val_res = try self.analyzeExprInfos(val, ctx);

        if (self.isVoid(val_res.type)) return self.err(.void_value, span);

        if (!self.isVoid(final_type) and final_type != val_res.type) {
            // If new value can't be cast to current array type
            if (!val_res.type.canCastTo(final_type)) {
                // If we didn't already changed array type based on new value (ex from []int to []float)
                // and that the array type is castable to value type, change array type and backpatch casts
                if (!backpatched and final_type.canCastTo(val_res.type)) {
                    for (elems.items) |*e| {
                        e.cast = true;
                    }
                    backpatched = true;
                } else return self.err(
                    .{ .array_elem_different_type = .{ .found1 = self.getTypeName(final_type), .found2 = self.getTypeName(val_res.type) } },
                    span,
                );
            } else {
                cast = true;
                val_res.type = final_type;
            }
        }

        final_type = val_res.type;
        pure = pure and val_res.comp_time;
        elems.appendAssumeCapacity(.{ .cast = cast, .incr_rc = val_res.heap_ref });
    }

    self.makeInstruction(
        .{ .array = .{ .elems = elems.toOwnedSlice(self.allocator) catch oom() } },
        self.ast.getSpan(expr).start,
        .{ .set_at = index },
    );

    return .{ .type = self.type_interner.intern(.{ .array = .{ .child = final_type } }), .comp_time = pure };
}

fn arrayAccess(self: *Self, expr: *const Ast.ArrayAccess, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.array);
    self.makeInstruction(.array_access, span.start, .add);
    const arr = try self.analyzeExpr(expr.array, ctx);

    const type_value = switch (arr.*) {
        .array => |ty| ty.child,
        else => return self.err(
            .{ .non_array_indexing = .{ .found = self.getTypeName(arr) } },
            span,
        ),
    };
    try self.expectArrayIndex(expr.index, ctx);

    return .{ .type = type_value, .heap_ref = type_value.isHeap() };
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
    var final: TypeInfos = .newType(self.type_interner.cache.void);
    var len: usize = expr.nodes.len;
    var pure = false;

    for (expr.nodes, 0..) |*node, i| {
        final = try self.analyzeNodeInfos(node, ctx);
        pure = pure and final.comp_time;

        if (!self.isVoid(final.type) and i != expr.nodes.len - 1) {
            return self.err(.unused_value, expr.span);
        }

        // Nothing to do at compile time for import statements
        if (node.* == .use) len -= 1;
    }

    const count = self.scope.close();
    // TODO: protect cast
    self.makeInstruction(
        .{ .block = .{ .length = len, .pop_count = @intCast(count), .is_expr = !self.isVoid(final.type) } },
        self.ast.getSpan(expr).start,
        .{ .set_at = index },
    );

    return final;
}

fn binop(self: *Self, expr: *const Ast.Binop, ctx: *Context) Result {
    const op = expr.op;
    const index = self.ir_builder.reserveInstr();

    const lhs = try self.analyzeExprInfos(expr.lhs, ctx);
    const rhs = try self.analyzeExprInfos(expr.rhs, ctx);

    const lhs_span = self.ast.getSpan(expr.lhs);
    const rhs_span = self.ast.getSpan(expr.rhs);

    if (isStringConcat(op, lhs.type, rhs.type)) {
        self.makeInstruction(.{ .binop = .{ .op = .add_str } }, lhs_span.start, .{ .set_at = index });
        return .newType(self.type_interner.cache.str);
    } else if (isStringRepeat(op, lhs.type, rhs.type)) {
        self.makeInstruction(
            .{ .binop = .{ .op = .mul_str, .cast = if (rhs.type.is(.int)) .rhs else .lhs } },
            lhs_span.start,
            .{ .set_at = index },
        );
        return .newType(self.type_interner.cache.str);
    }

    var instr = Instruction.Binop{ .op = undefined };
    var result_type = lhs.type;

    switch (op) {
        .plus, .slash, .star, .minus => {
            try self.expectNumeric(lhs.type, lhs_span);
            try self.expectNumeric(rhs.type, rhs_span);
            const info = self.getArithmeticOp(op, lhs.type, rhs.type, expr);
            instr = info.instr;
            result_type = info.result_type;
        },
        .equal_equal, .bang_equal => {
            const info = try self.getEqualityOp(op, lhs.type, rhs.type, expr);
            instr = info.instr;
            result_type = info.result_type;
        },
        .greater_equal, .greater, .less_equal, .less => {
            try self.expectNumeric(lhs.type, lhs_span);
            try self.expectNumeric(rhs.type, rhs_span);
            const info = try self.getComparisonOp(op, lhs.type, rhs.type, expr);
            instr = info.instr;
            result_type = info.result_type;
        },
        .@"and", .@"or" => {
            try self.checkBooleanLogic(lhs.type, rhs.type, expr);
            instr.op = if (op == .@"and") .@"and" else .@"or";
            result_type = self.type_interner.cache.bool;
        },
        else => unreachable,
    }

    self.makeInstruction(.{ .binop = instr }, lhs_span.start, .{ .set_at = index });

    return .{ .type = result_type, .comp_time = lhs.comp_time and rhs.comp_time };
}

fn closure(self: *Self, expr: *const Ast.FnDecl, ctx: *Context) Result {
    const closure_idx = self.ir_builder.reserveInstr();

    self.scope.open(self.allocator, false);
    defer _ = self.scope.close();

    const captures_instrs_data = try self.loadFunctionCaptures(&expr.meta.captures);
    const param_res = try self.fnParams(expr.params, ctx);

    // Update type for resolution in function's body
    const closure_type: Type.Function = .{
        .loc = null,
        .params = param_res.decls,
        .return_type = try self.checkAndGetType(expr.return_type, ctx),
        .kind = .normal,
    };
    const interned_type = self.type_interner.intern(.{ .function = closure_type });

    const span = self.ast.getSpan(expr);
    const offset = span.start;

    ctx.fn_type = interned_type;
    const len = try self.fnBody(expr.body.nodes, &closure_type, span, ctx);

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

    return .newType(interned_type);
}

fn isStringConcat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .plus and lhs.is(.str) and rhs.is(.str);
}

fn isStringRepeat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .star and ((lhs.is(.str) and rhs.is(.int)) or (lhs.is(.int) and rhs.is(.str)));
}

fn expectNumeric(self: *Self, ty: *const Type, err_span: Span) Error!void {
    if (!ty.isNumeric()) {
        return self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(ty) } }, err_span);
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
    var instr = Instruction.Binop{ .op = switch (op) {
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
    } };

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
                .{ .invalid_comparison = .{ .found1 = self.getTypeName(lhs), .found2 = self.getTypeName(rhs) } },
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

fn field(self: *Self, expr: *const Ast.Field, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.structure);
    const index = self.ir_builder.reserveInstr();

    const struct_res: TypeInfos = switch (expr.structure.*) {
        .field => |*f| try self.field(f, ctx),
        .literal => |e| b: {
            const ident = try self.identifier(e.idx, true, ctx);
            break :b .{ .type = ident.type, .heap_ref = true, .is_sym = ident.kind != .variable };
        },
        else => try self.analyzeExprInfos(expr.structure, ctx),
    };

    const field_res = switch (struct_res.type.*) {
        .module => |ty| return self.moduleAccess(expr.field, ty, index),
        .structure => |*ty| try self.structureAccess(expr.field, ty, struct_res.is_sym, ctx),
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.getTypeName(struct_res.type) } },
            span,
        ),
    };

    const kind: Instruction.Field.Kind = switch (field_res.kind) {
        // TODO: create just a 'function'. For now we need this because we get static method from
        // symbols that are loaded on stack, not in register so we need a separate logic
        .function => if (struct_res.is_sym) .static_method else .method,
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

    // Lhs must be heap_ref too. It's used to allow calls to break chains like: getVec().point1
    // If the object returned by getVec() is a literal for example, the rest of the chain will be a stack allocated object
    return .{ .type = field_res.type, .heap_ref = struct_res.heap_ref and field_res.type.isHeap(), .is_sym = struct_res.is_sym };
}

const AccessResult = struct {
    type: *const Type,
    kind: enum { field, function },
    index: usize,
};

fn structureAccess(self: *Self, field_tk: Ast.TokenIndex, ty: *const Type.Structure, is_symbol: bool, ctx: *const Context) Error!AccessResult {
    const text = self.ast.toSource(field_tk);
    const field_name = self.interner.intern(text);

    return if (ty.fields.getPtr(field_name)) |f|
        .{ .type = f.type, .kind = .field, .index = ty.fields.getIndex(field_name).? }
    else if (ty.functions.get(field_name)) |f| b: {
        const function = &f.function;

        if (ctx.in_call) {
            if (is_symbol and function.kind == .method) {
                return self.err(.{ .call_method_on_type = .{ .name = text } }, self.ast.getSpan(field_tk));
            } else if (!is_symbol and function.kind != .method) {
                return self.err(.{ .call_static_on_instance = .{ .name = text } }, self.ast.getSpan(field_tk));
            }
        }

        break :b .{ .type = f, .kind = .function, .index = ty.functions.getIndex(field_name).? };
    } else self.err(.{ .undeclared_field_access = .{ .name = text } }, self.ast.getSpan(field_tk));
}

fn moduleAccess(self: *Self, field_tk: Ast.TokenIndex, module_idx: InternerIdx, instr_index: usize) Result {
    const span = self.ast.getSpan(field_tk);
    const text = self.ast.toSource(field_tk);

    const field_name = self.interner.intern(text);
    const module = self.pipeline.ctx.module_interner.getAnalyzed(module_idx).?;
    const sym = module.symbols.get(field_name) orelse return self.err(
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

    return .{ .type = sym.type, .is_sym = true };
}

fn boundMethod(self: *Self, func_type: *const Type, field_index: usize, span: Span, instr_idx: usize) Result {
    const bounded_type = func_type.function.toBoundMethod(self.allocator);
    const ty = self.type_interner.intern(.{ .function = bounded_type });
    self.makeInstruction(.{ .bound_method = field_index }, span.start, .{ .set_at = instr_idx });

    return .{ .type = ty };
}

fn call(self: *Self, expr: *const Ast.FnCall, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const index = self.ir_builder.reserveInstr();

    const ctx_call = ctx.setAndGetPrevious(.in_call, true);
    const callee = try self.analyzeExpr(expr.callee, ctx);

    if (!callee.is(.function)) {
        return self.err(.invalid_call_target, span);
    }

    // Restore state before arguments analyzis
    ctx.in_call = ctx_call;
    const args_res = try self.fnArgsList(expr.args, &callee.function, span, ctx);

    // TODO: protect casts
    self.makeInstruction(
        .{ .call = .{ .arity = @intCast(args_res.arity), .implicit_first = callee.function.kind == .method } },
        span.start,
        .{ .set_at = index },
    );

    // TODO: later when functions will be able to return references, rework this
    return .newType(callee.function.return_type);
}

// TODO: rewrite
const ArgsListRes = struct { arity: usize, default_count: usize };

fn fnArgsList(self: *Self, args: []*Expr, ty: *const Type.Function, err_span: Span, ctx: *Context) Error!ArgsListRes {
    var proto = ty.proto(self.allocator);
    const param_count = proto.count();
    const params = ty.params.values()[@intFromBool(ty.kind == .method)..];

    if (args.len > param_count) return self.err(.{ .too_many_fn_args = .{ .expect = param_count, .found = args.len } }, err_span);

    var proto_values = proto.values();
    const start = self.ir_builder.count();
    self.ir_builder.ensureUnusedSize(param_count);

    // We initialize all the values used for the initialization. By default, we put empty data under
    // the form of 'default_value' but we check for all real param default to mark their index (order
    // of declaration) so that the compiler can emit the right index
    var default_count: usize = 0;
    for (proto_values) |default| {
        self.makeInstruction(.{ .default_value = default_count }, err_span.start, .add_no_alloc);
        if (default) default_count += 1;
    }

    for (args, 0..) |arg, i| {
        var cast = false;
        var value_instr: usize = 0;
        var param_info: *const Type.Function.Parameter = undefined;
        var param_index: usize = undefined;
        const span_start = self.ast.getSpan(arg).start;

        switch (arg.*) {
            .named_arg => |na| {
                if (ty.kind == .bound) {
                    return self.err(.named_arg_in_bounded, self.ast.getSpan(na.name));
                }

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
                param_info = &params[i];
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(arg))).cast;
                proto_values[i] = true;
                param_index = i;
            },
        }

        // TODO: implement incr_rc
        self.makeInstruction(
            .{ .value = .{
                .value_instr = value_instr,
                .cast = cast,
                .box = param_info.captured,
                .incr_rc = false,
            } },
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

    return if (err_count < self.errs.items.len) error.Err else .{ .arity = param_count, .default_count = default_count };
}

const IdentRes = struct { type: *const Type, kind: enum { variable, symbol, module }, comp_time: bool = true };

/// Tries to find a match from variables and symbols and returns its type while emitting an instruction
fn identifier(self: *Self, token_name: Ast.TokenIndex, initialized: bool, ctx: *const Context) Error!IdentRes {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    if (self.variableIdentifier(name, span)) |variable| {
        if (initialized and !variable.initialized) {
            return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(token_name));
        }

        return .{ .type = variable.type, .kind = .variable, .comp_time = variable.comp_time };
    }

    const sym_name = if (name == self.cached_names.Self) b: {
        const struct_type = ctx.struct_type orelse return self.err(.big_self_outside_struct, span);
        break :b struct_type.structure.loc.?.name;
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

fn expectVariableIdentifier(self: *Self, token_name: Ast.TokenIndex) Error!*LexScope.Variable {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    return self.variableIdentifier(name, span) orelse return self.err(
        .{ .undeclared_var = .{ .name = self.interner.getKey(name).? } },
        span,
    );
}

/// Tries to find a variable in scopes and returns it while emitting an instruction
fn variableIdentifier(self: *Self, name: InternerIdx, span: Span) ?*LexScope.Variable {
    const variable, const scope_offset = self.scope.getVariable(name) orelse return null;

    self.makeInstruction(
        .{ .identifier = .{
            .index = variable.index + scope_offset,
            .scope = switch (variable.kind) {
                .local => .local,
                .global => .global,
            },
            .unbox = variable.captured,
        } },
        span.start,
        .add,
    );

    return variable;
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn symbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?*LexScope.Symbol {
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
fn externSymbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?*LexScope.Symbol {
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
    var instr: Instruction.If = .{ .cast = .none, .has_else = false, .incr_rc_else = false, .incr_rc_then = false };

    const cond_type = try self.analyzeExprInfos(expr.condition, ctx);
    var pure = cond_type.comp_time;

    // We can continue to analyze if the condition isn't a bool
    if (!cond_type.type.is(.bool)) self.err(
        .{ .non_bool_cond = .{ .what = "if", .found = self.getTypeName(cond_type.type) } },
        span,
    ) catch {};

    // Analyze then branch
    const then_res = try self.analyzeNodeInfos(&expr.then, ctx);
    instr.incr_rc_then = then_res.heap_ref;
    pure = pure and then_res.comp_time;

    const then_returned = ctx.returns;
    var final_type = if (then_returned) self.type_interner.cache.void else then_res.type;
    ctx.returns = false;

    var else_returned = false;

    if (expr.@"else") |*n| {
        const else_res = try self.analyzeNodeInfos(n, ctx);
        instr.has_else = true;
        instr.incr_rc_else = else_res.heap_ref;
        pure = pure and else_res.comp_time;
        else_returned = ctx.returns;

        if (!else_returned) {
            if (then_returned) {
                final_type = else_res.type;
            } else {
                try self.ifTypeCoherenceAndCast(then_res.type, else_res.type, expr, &instr);
            }
        }
    } else if (!then_res.type.is(.void) and !ctx.allow_partial) {
        return self.err(
            .{ .missing_else_clause = .{ .if_type = self.getTypeName(then_res.type) } },
            self.ast.getSpan(expr),
        );
    }

    // The whole instructions returns out of scope
    ctx.returns = then_returned and else_returned;
    self.makeInstruction(.{ .@"if" = instr }, span.start, .{ .set_at = index });

    return if (ctx.returns)
        .newType(self.type_interner.cache.void)
    else
        .{ .type = final_type, .comp_time = pure };
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
        .{ .incompatible_if_type = .{ .found1 = self.getTypeName(then_type), .found2 = self.getTypeName(else_type) } },
        self.ast.getSpan(expr),
    );
}

fn literal(self: *Self, expr: *const Ast.Literal, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const text = self.ast.toSource(expr);

    const ty = b: {
        switch (expr.tag) {
            .bool => {
                self.makeInstruction(.{ .bool = self.ast.token_tags[expr.idx] == .true }, span.start, .add);
                break :b self.type_interner.cache.bool;
            },
            .identifier, .self => {
                const res = try self.identifier(expr.idx, true, ctx);
                return .{ .type = res.type, .heap_ref = res.type.isHeap(), .comp_time = res.comp_time };
            },
            .int => {
                const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                    // TODO: error handling, only one possible it's invalid char
                    std.debug.print("Error parsing integer\n", .{});
                    break :blk 0;
                };
                self.makeInstruction(.{ .int = value }, span.start, .add);
                break :b self.type_interner.cache.int;
            },
            .float => {
                const value = std.fmt.parseFloat(f64, text) catch blk: {
                    // TODO: error handling, only one possible it's invalid char or too big
                    std.debug.print("Error parsing float\n", .{});
                    break :blk 0.0;
                };
                self.makeInstruction(.{ .float = value }, span.start, .add);
                break :b self.type_interner.cache.float;
            },
            .null => {
                self.makeInstruction(.null, span.start, .add);
                break :b self.type_interner.cache.null;
            },
            .string => {
                const no_quotes = text[1 .. text.len - 1];
                var final: ArrayList(u8) = .{};
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

                break :b self.type_interner.cache.str;
            },
        }
    };

    return .{ .type = ty, .comp_time = true };
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.structure);
    const index = self.ir_builder.reserveInstr();
    const struct_res = try self.analyzeExprInfos(expr.structure, ctx);
    var comp_time = struct_res.comp_time;

    const struct_type = if (struct_res.type.is(.structure)) struct_res.type.structure else {
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

        const res: TypeInfos = if (fv.value) |value|
            try self.analyzeExprInfos(value, ctx)
        else b: {
            // Syntax: { x } instead of { x = x }
            const value_res = try self.expectVariableIdentifier(fv.name);
            break :b .{ .type = value_res.type, .heap_ref = value_res.type.isHeap(), .comp_time = value_res.comp_time };
        };

        comp_time = comp_time and res.comp_time;
        const value_span = if (fv.value) |val| self.ast.getSpan(val) else field_span;
        const coercion = try self.performTypeCoercion(f.type, res.type, false, value_span);

        self.makeInstruction(
            .{ .value = .{ .value_instr = value_instr, .cast = coercion.cast, .box = false, .incr_rc = res.heap_ref } },
            field_span.start,
            .{ .set_at = start + field_index },
        );
    }

    var has_err = false;
    var kv = proto.iterator();
    while (kv.next()) |entry| {
        if (!entry.value_ptr.*) {
            has_err = true;
            self.err(
                .{ .missing_field_struct_literal = .{ .name = self.interner.getKey(entry.key_ptr.*).? } },
                span,
            ) catch {};
        }
    }

    if (has_err) return error.Err;

    // TODO: implement an invoke strategy
    // TODO: protect cast
    self.makeInstruction(
        .{ .struct_literal = .{ .fields_count = @intCast(struct_type.fields.count()) } },
        span.start,
        .{ .set_at = index },
    );

    return .{ .type = struct_res.type, .comp_time = comp_time };
}

fn returnExpr(self: *Self, expr: *const Ast.Return, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const index = self.ir_builder.reserveInstr();
    var instr: Instruction.Return = .{ .value = false, .cast = false };

    // We check after to advance node idx
    const fn_type = ctx.fn_type orelse return self.err(.return_outside_fn, span);
    const ty = fn_type.function.return_type;

    var value_res = if (expr.expr) |e| blk: {
        instr.value = true;
        break :blk try self.analyzeExprInfos(e, ctx);
    } else TypeInfos.newType(self.type_interner.cache.void);

    // We do that here because we can insert a cast
    if (ty != value_res.type) {
        if (self.isVoid(ty) and !self.isVoid(value_res.type)) {
            return self.err(
                .{ .type_mismatch = .{ .expect = "void", .found = self.getTypeName(value_res.type) } },
                if (expr.expr) |e| self.ast.getSpan(e) else span,
            );
        } else {
            const coerce = try self.performTypeCoercion(ty, value_res.type, true, if (expr.expr) |e| self.ast.getSpan(e) else span);
            value_res.type = coerce.type;
            instr.cast = coerce.cast;
        }
    }

    ctx.returns = true;
    self.makeInstruction(.{ .@"return" = instr }, span.start, .{ .set_at = index });

    return value_res;
}

fn unary(self: *Self, expr: *const Ast.Unary, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const op = self.ast.token_tags[expr.op];
    const index = self.ir_builder.reserveInstr();
    var instr: Instruction.Unary = .{ .op = if (op == .not) .bang else .minus, .typ = .float };

    const rhs = try self.analyzeExprInfos(expr.expr, ctx);
    const ty = rhs.type;

    if (op == .not and !ty.is(.bool)) {
        return self.err(.{ .invalid_unary = .{ .found = self.getTypeName(ty) } }, span);
    } else if (op == .minus and !ty.isNumeric()) {
        return self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(ty) } }, span);
    }

    if (ty.is(.int)) instr.typ = .int;

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
fn checkAndGetType(self: *Self, ty: ?*const Ast.Type, ctx: *const Context) TypeResult {
    return if (ty) |t| return switch (t.*) {
        .array => |arr_type| {
            const child = try self.checkAndGetType(arr_type.child, ctx);

            if (self.isVoid(child)) {
                return self.err(.void_array, self.ast.getSpan(arr_type.child));
            }

            return self.type_interner.intern(.{ .array = .{ .child = child } });
        },
        .fields => |fields| {
            // TODO: Error
            if (fields.len > 2) @panic("Nested types are not supported yet");

            const module_token = fields[0];
            const module_infos = try self.identifier(module_token, true, ctx);
            const module_type = module_infos.type;

            if (!module_type.is(.module)) return self.err(
                .{ .dot_type_on_non_mod = .{ .found = self.getTypeName(module_type) } },
                self.ast.getSpan(module_token),
            );

            // If `identifier` returned no error and it's a module, safe unwrap
            const module = self.pipeline.ctx.module_interner.getAnalyzed(module_type.module).?;

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
            var params: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter) = .{};
            for (func.params, 0..) |p, i| {
                const p_type = try self.checkAndGetType(p, ctx);
                params.put(self.allocator, i, .{ .type = p_type, .default = false, .captured = false }) catch oom();
            }

            return self.type_interner.intern(.{ .function = .{
                .loc = null,
                .params = params,
                .return_type = try self.checkAndGetType(func.return_type, ctx),
                .kind = .normal,
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
    if (decl == value) return .{ .type = decl, .cast = false };

    var cast = false;
    var local_decl = decl;
    var local_value = value;

    if (self.isVoid(value)) return self.err(.void_value, span);

    if (value.is(.array)) {
        local_value = try self.inferArrayType(decl, value, span);
    } else if (value.is(.function)) {
        // Functions function's return types like: 'fn add() -> fn(int) -> int' don't have a declaration
        // There is also the case when assigning to a variable and infering type like: var bound = foo.method
        // Here, we want `bound` to be an anonymus function, it loses all declaration infos because it's a runtime value
        if (self.isVoid(decl)) return .{
            .type = if (value.function.loc != null)
                self.type_interner.intern(.{ .function = value.function.toAnon(self.allocator) })
            else
                value,
            .cast = false,
        };
        return self.checkFunctionEq(decl, value, span);
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

/// Checks if two different pointers to function type are equal, due to anonymus ones
fn checkFunctionEq(self: *Self, decl: *const Type, value: *const Type, span: Span) Error!TypeCoherence {
    const f1 = decl.function;
    const f2 = value.function;

    check: {
        if (f1.loc != null and f2.loc != null or f1.params.count() != f2.params.count()) break :check;
        if (f1.return_type != f2.return_type) break :check;

        for (f1.params.values(), f2.params.values()) |p1, p2| {
            if (p1.type != p2.type) break :check;
        }

        return .{ .type = decl, .cast = false };
    }

    return self.err(
        .{ .type_mismatch = .{ .expect = self.getTypeName(decl), .found = self.getTypeName(value) } },
        span,
    );
}

/// Try to infer array value type from variable's declared type
fn inferArrayType(self: *Self, decl: *const Type, value: *const Type, span: Span) Error!*const Type {
    // Get nested item's type from: [][][]int -> int
    const child = value.array.getChild();

    // Empty array like: []
    if (self.isVoid(child)) {
        // No type declared and empty array like: var a = [], else infer from declaration
        return if (self.isVoid(decl)) self.err(.cant_infer_arary_type, span) else decl;
    }

    return value;
}

fn isVoid(self: *const Self, ty: *const Type) bool {
    return ty == self.type_interner.cache.void;
}

fn getTypeName(self: *const Self, ty: *const Type) []const u8 {
    return ty.toString(self.allocator, &self.scope, self.module_name, self.interner, &self.pipeline.ctx.module_interner);
}

fn declareVariable(
    self: *Self,
    name: InternerIdx,
    ty: *const Type,
    captured: bool,
    initialized: bool,
    constant: bool,
    comp_time: bool,
    span: Span,
) Error!usize {
    return self.scope.declareVar(
        self.allocator,
        name,
        ty,
        captured,
        initialized,
        constant,
        comp_time,
    ) catch self.err(.too_many_locals, span);
}
