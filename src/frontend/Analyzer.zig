const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
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
const InstrIndex = rir.Index;
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
    decl_type: ?*const Type,
    // allow_partial: bool,
    // returns: bool,
    in_call: bool,

    pub const empty: Context = .{
        .fn_type = null,
        .struct_type = null,
        .decl_type = null,
        // .allow_partial = true,
        // .returns = false,
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

    /// Sets the declaration type only if the type is not `void`
    pub fn setDecl(self: *Context, decl: *const Type) void {
        if (decl.is(.void)) return;
        self.decl_type = decl;
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
const TypeResult = Error!struct { type: *const Type, instr: InstrIndex };
const StmtResult = Error!InstrIndex;
const TypeInfos = struct {
    type: *const Type,
    heap: bool = false,
    is_sym: bool = false,
    comp_time: bool = false,

    pub fn newType(ty: *const Type) TypeInfos {
        return .{ .type = ty };
    }
};
const InstrInfos = struct {
    ti: TypeInfos,
    cf: ControlFlow = .none,
    instr: InstrIndex,

    pub const ControlFlow = enum {
        @"break",
        @"return",
        none,

        pub fn exitScope(self: ControlFlow) bool {
            return self == .@"return" or self == .@"break";
        }
    };

    pub fn new(ti: TypeInfos, index: InstrIndex) InstrInfos {
        return .{ .ti = ti, .instr = index };
    }

    /// Constructor with control flow infos
    pub fn newCf(ti: TypeInfos, cf: ControlFlow, index: InstrIndex) InstrInfos {
        return .{ .ti = ti, .cf = cf, .instr = index };
    }

    pub fn fromType(ty: *const Type, index: InstrIndex) InstrInfos {
        return .{ .ti = .{ .type = ty }, .instr = index };
    }

    pub fn fromTypeCf(ty: *const Type, cf: ControlFlow, index: InstrIndex) InstrInfos {
        return .{ .ti = .{ .type = ty }, .cf = cf, .instr = index };
    }
};
const Result = Error!InstrInfos;
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
ti: *TypeInterner,
irb: IrBuilder,
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
        .ti = &pipeline.ctx.type_interner,
        .ast = undefined,
        .errs = .empty,
        .warns = .empty,
        .scope = .empty,
        .irb = .init(allocator),
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

pub fn analyze(self: *Self, ast: *const Ast, module_name: []const u8, expect_main: bool) AnalyzedModule {
    self.ast = ast;
    self.scope.initGlobalScope(self.allocator, self.interner, self.ti);
    var ctx: Context = .empty;

    // Excluding file extension
    self.containers.append(self.allocator, module_name[0 .. module_name.len - 3]);

    for (ast.nodes) |*node| {
        const res = self.analyzeNode(node, &ctx) catch continue;
        self.irb.addRootInstr(res.instr);
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
    return .{ .type = res.ti.type, .instr = res.instr };
}

fn analyzeNodeInfos(self: *Self, node: *const Node, ctx: *Context) Result {
    const instr = switch (node.*) {
        .assignment => |*n| try self.assignment(n, ctx),
        .discard => |n| try self.discard(n, ctx),
        .fn_decl => |*n| try self.fnDeclaration(n, ctx),
        .multi_var_decl => |*n| try self.multiVarDecl(n, ctx),
        .print => |n| try self.print(n, ctx),
        .struct_decl => |*n| try self.structDecl(n, ctx),
        .use => |*n| b: {
            try self.use(n, ctx);
            break :b self.irb.addInstr(.noop, 0);
        },
        .var_decl => |*n| try self.varDeclaration(n, ctx),
        .@"while" => |*n| try self.whileStmt(n, ctx),
        .expr => |n| return try self.analyzeExprInfos(n, false, ctx),
    };

    return .fromType(self.ti.getCached(.void), instr);
}

fn assignment(self: *Self, node: *const Ast.Assignment, ctx: *Context) StmtResult {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();
    const span = self.ast.getSpan(node.assigne);

    // ctx.allow_partial = false;
    var value_res = try self.analyzeExprInfos(node.value, true, ctx);

    const maybe_assigne: ?InstrInfos = switch (node.assigne.*) {
        .literal => |*e| b: {
            if (e.tag != .identifier) break :b null;

            var assigne = try self.expectVariableIdentifier(e.idx);
            if (assigne.variable.constant) return self.err(.{ .assign_to_constant = .{ .name = self.ast.toSource(e.idx) } }, span);

            assigne.variable.initialized = true;
            break :b .fromType(assigne.variable.type, assigne.instr);
        },
        .field => |*e| b: {
            const field_result = try self.field(e, ctx);
            if (field_result.ti.is_sym) return self.err(.assign_to_struct_fn, span);
            // Resolving methods without call result in a bound method
            if (field_result.ti.type.* == .function and field_result.ti.type.function.kind == .bound) {
                return self.err(.assign_to_struct_fn, span);
            }
            break :b .fromType(field_result.ti.type, field_result.instr);
        },
        .fn_call => return self.err(.invalid_assign_target, span),
        .array_access => try self.analyzeExprInfos(node.assigne, true, ctx),
        else => return self.err(.invalid_assign_target, span),
    };
    const assigne = maybe_assigne orelse return self.err(.invalid_assign_target, span);
    const coerce = try self.performTypeCoercion(assigne.ti.type, value_res.ti.type, false, self.ast.getSpan(node.value));

    self.checkWrap(&value_res.instr, coerce.cast, value_res.ti.heap);

    return self.irb.addInstr(
        .{ .assignment = .{ .assigne = assigne.instr, .value = value_res.instr, .cow = assigne.ti.type.isHeap() } },
        span.start,
    );
}

fn discard(self: *Self, expr: *const Expr, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(expr);
    _ = span; // autofix
    const res = try self.analyzeExprInfos(expr, true, ctx);

    // if (res.ti.type.is(.void)) return self.err(.void_discard, span);

    return self.irb.wrapInstr(.discard, res.instr);
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl, ctx: *Context) StmtResult {
    // TODO: check if not useless
    const snapshot = ctx.snapshot();
    defer snapshot.restore();

    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);

    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    // Forward declaration in outer scope for recursion
    var sym = self.scope.forwardDeclareSymbol(self.allocator, name);

    self.scope.open(self.allocator, true, null);
    errdefer _ = self.scope.close();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    const captures = try self.loadFunctionCaptures(&node.meta.captures);
    const is_closure = captures.len > 0;

    const param_res = try self.fnParams(node.params, ctx);

    var fn_type: Type.Function = .{
        .loc = .{ .name = name, .container = container_name },
        .params = param_res.decls,
        .return_type = try self.checkAndGetType(node.return_type, ctx),
        .kind = if (param_res.is_method) .method else .normal,
    };
    const interned_type = self.ti.intern(.{ .function = fn_type });
    sym.type = interned_type;
    ctx.fn_type = interned_type;

    const body_instrs, const returns = try self.fnBody(node.body.nodes, &fn_type, span, ctx);
    _ = self.scope.close();

    // If it's a closure, it lives on the stack at runtime
    if (is_closure) {
        _ = self.scope.removeSymbolFromScope(name);
        _ = try self.declareVariable(name, interned_type, false, true, true, false, span);
    }

    if (name == self.cached_names.main and self.scope.isGlobal()) {
        self.main = sym.index;
    }

    return self.irb.addInstr(
        .{
            .fn_decl = .{
                .kind = if (is_closure) .closure else .{ .symbol = sym.index },
                .name = name,
                .body = body_instrs,
                .defaults = param_res.defaults,
                .captures = captures,
                // .return_kind = if (ctx.returns) .explicit else if (fn_type.return_type.is(.void)) .implicit_void else .implicit_value,
                // TODO: maybe remove?
                .return_kind = if (returns) .explicit else if (fn_type.return_type.is(.void)) .implicit_void else .implicit_value,
            },
        },
        span.start,
    );
}

fn loadFunctionCaptures(self: *Self, captures_meta: *const Ast.FnDecl.Meta.Captures) Error![]const Instruction.FnDecl.Capture {
    var captures: ArrayList(Instruction.FnDecl.Capture) = .empty;
    captures.ensureTotalCapacity(self.allocator, captures_meta.count()) catch oom();

    var it = captures_meta.iterator();
    while (it.next()) |capt| {
        const name = capt.key_ptr.*;
        const capt_infos = capt.value_ptr.*;
        const variable, _ = self.scope.getVariable(name) orelse unreachable;
        _ = try self.declareVariable(name, variable.type, true, true, false, false, .zero);
        captures.appendAssumeCapacity(.{ .index = capt_infos.index, .local = capt_infos.is_local });
    }

    return captures.toOwnedSlice(self.allocator) catch oom();
}

const Params = struct {
    decls: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter),
    defaults: []const InstrIndex,
    is_method: bool,
};

fn fnParams(self: *Self, params: []Ast.VarDecl, ctx: *Context) Error!Params {
    var decls: AutoArrayHashMapUnmanaged(InternerIdx, Type.Function.Parameter) = .empty;
    decls.ensureTotalCapacity(self.allocator, params.len) catch oom();

    var is_method = false;
    var defaults: ArrayList(InstrIndex) = .empty;

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
            return self.err(.{ .already_declared_param = .{ .name = self.ast.toSource(p.name) } }, span);
        }

        var param_type = try self.checkAndGetType(p.typ, ctx);

        if (p.value) |val| {
            const value_res = try self.defaultValue(param_type, val, ctx);

            if (!value_res.ti.comp_time) {
                return self.err(.{ .non_comptime_default = .new(.parameter) }, self.ast.getSpan(val));
            }

            defaults.append(self.allocator, value_res.instr) catch oom();
            param_type = value_res.ti.type;
        }

        if (param_type.is(.void)) return self.err(.void_param, span);

        _ = try self.declareVariable(param_name, param_type, p.meta.captured, true, true, false, span);
        decls.putAssumeCapacity(param_name, .{
            .type = param_type,
            .default = p.value != null,
            .captured = p.meta.captured,
        });
    }

    return .{ .decls = decls, .defaults = defaults.toOwnedSlice(self.allocator) catch oom(), .is_method = is_method };
}

/// Analyses function's body returning all of the instructions and a flag indicating if the function returns
fn fnBody(self: *Self, body: []Node, fn_type: *const Type.Function, name_span: Span, ctx: *Context) Error!struct { []const InstrIndex, bool } {
    const err_count = self.errs.items.len;
    var final_type: *const Type = self.ti.getCached(.void);
    var deadcode = false;
    var returns = false;
    const len = body.len;

    var instrs: ArrayList(InstrIndex) = .empty;
    instrs.ensureTotalCapacity(self.allocator, body.len) catch oom();

    for (body, 0..) |*n, i| {
        // If previous statement returned, it's only dead code now
        if (!deadcode and returns) {
            self.warn(.dead_code, self.ast.getSpan(n));
            deadcode = true;
        }

        // We try to analyze the whole body
        const res = self.analyzeNodeInfos(n, ctx) catch continue;

        // If we analyze dead code, we don't update the type and don't add instructions
        if (deadcode) continue;

        final_type = res.ti.type;
        returns = res.cf == .@"return";

        // If last expression produced a value and that it wasn't the last one we pop it
        if (i != len - 1 and !final_type.is(.void)) {
            instrs.appendAssumeCapacity(self.irb.wrapPreviousInstr(.pop));
        } else {
            instrs.appendAssumeCapacity(res.instr);
        }
    }

    if (!returns and !fn_type.return_type.is(.void)) {
        const span = if (body.len == 0) name_span else self.ast.getSpan(body[body.len - 1]);
        return self.err(.{ .fn_expect_value = .{ .expect = self.getTypeName(fn_type.return_type) } }, span);
    }

    // If no errors and different types
    if (err_count == self.errs.items.len and final_type != fn_type.return_type) {
        const err_span = if (body.len > 0) self.ast.getSpan(body[body.len - 1]) else name_span;
        const coerce = try self.performTypeCoercion(fn_type.return_type, final_type, true, err_span);
        if (coerce.cast) {
            const i = &instrs.items[instrs.items.len - 1];
            i.* = self.irb.wrapInstr(.cast_to_float, i.*);
        }
    }

    return .{ instrs.toOwnedSlice(self.allocator) catch oom(), returns };
}

fn defaultValue(self: *Self, decl_type: *const Type, default_value: *const Expr, ctx: *Context) Result {
    var value_res = try self.analyzeExprInfos(default_value, true, ctx);
    const coerce = try self.performTypeCoercion(decl_type, value_res.ti.type, false, self.ast.getSpan(default_value));
    value_res.ti.type = coerce.type;
    if (coerce.cast) value_res.instr = self.irb.wrapInstr(.cast_to_float, value_res.instr);

    return value_res;
}

fn print(self: *Self, expr: *const Expr, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(expr);
    _ = span; // autofix
    const res = try self.analyzeExpr(expr, true, ctx);

    // TODO: just 'produces no value' error is enough
    // if (res.type.is(.void)) return self.err(.void_value, span);

    return self.irb.wrapInstr(.print, res.instr);
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
        self.scope.declareModule(self.allocator, module_name, self.ti.intern(.{ .module = interned }));
    }
}

fn expectAssignableValue(self: *Self, expr: *const Ast.Expr, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    const value_res: ?InstrInfos = switch (expr.*) {
        .literal => |*e| b: {
            if (e.tag != .identifier) break :b try self.literal(e, ctx);

            const value = self.identifier(e.idx, true, ctx) catch break :b null;
            if (value.kind == .symbol and value.type.* != .function) break :b null;
            break :b .{ .ti = .{ .type = value.type, .heap = value.type.isHeap() }, .instr = value.instr };
        },
        .field => |*e| b: {
            const field_res = try self.field(e, ctx);
            if (field_res.ti.is_sym and field_res.ti.type.* != .function) break :b null;
            break :b field_res;
        },
        else => try self.analyzeExprInfos(expr, true, ctx),
    };

    return value_res orelse self.err(.assign_type, span);
}

fn checkWrap(self: *Self, instr: *InstrIndex, cast: bool, heap: bool) void {
    if (heap) {
        instr.* = self.irb.wrapInstr(.incr_rc, instr.*);
    } else if (cast) {
        instr.* = self.irb.wrapInstr(.cast_to_float, instr.*);
    }
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node.name);
    const name = try self.internIfNotInCurrentScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);

    ctx.setDecl(checked_type);
    defer ctx.decl_type = null;

    var comp_time = false;

    const value_instr = if (node.value) |value| instr: {
        const snapshot = ctx.snapshot();
        defer snapshot.restore();

        var value_res = try self.expectAssignableValue(value, ctx);
        comp_time = value_res.ti.comp_time;

        const coercion = try self.performTypeCoercion(checked_type, value_res.ti.type, false, self.ast.getSpan(value));
        checked_type = coercion.type;
        self.checkWrap(&value_res.instr, coercion.cast, value_res.ti.heap);

        break :instr value_res.instr;
    } else null;

    const decl_index = try self.declareVariable(name, checked_type, node.meta.captured, value_instr != null, false, comp_time, span);

    return self.irb.addInstr(
        .{ .var_decl = .{
            .box = node.meta.captured,
            .value = value_instr,
            .variable = .{ .index = decl_index, .scope = if (self.scope.isGlobal()) .global else .local },
        } },
        span.start,
    );
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl, ctx: *Context) StmtResult {
    var decls: ArrayList(InstrIndex) = .empty;
    decls.ensureTotalCapacity(self.allocator, node.decls.len) catch oom();

    for (node.decls) |*n| {
        decls.appendAssumeCapacity(try self.varDeclaration(n, ctx));
    }

    return self.irb.addInstr(
        .{ .multiple_var_decl = .{ .decls = decls.toOwnedSlice(self.allocator) catch oom() } },
        self.ast.getSpan(node).start,
    );
}

fn structDecl(self: *Self, node: *const Ast.StructDecl, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node);
    const name = try self.internIfNotInCurrentScope(node.name);

    var buf: [1024]u8 = undefined;
    const container_name = self.interner.internKeepRef(self.allocator, self.containers.renderWithSep(&buf, "."));

    const sym = self.scope.forwardDeclareSymbol(self.allocator, name);
    var ty: Type.Structure = .{
        .loc = .{ .name = name, .container = container_name },
        .fields = .empty,
        .functions = .empty,
    };
    ty.fields.ensureTotalCapacity(self.allocator, node.fields.len) catch oom();
    ty.functions.ensureTotalCapacity(self.allocator, @intCast(node.functions.len)) catch oom();

    self.scope.open(self.allocator, true, null);
    defer _ = self.scope.close();

    self.containers.append(self.allocator, self.ast.toSource(node.name));
    defer _ = self.containers.pop();

    // Create type before functions to allow 'self' to refer to the structure
    const interned_type = self.ti.intern(.{ .structure = ty });
    const interned_struct = &interned_type.structure;

    sym.type = interned_type;
    ctx.struct_type = interned_type;
    defer ctx.struct_type = null;

    const default_fields = try self.structureFields(node.fields, interned_struct, ctx);

    var funcs: ArrayList(InstrIndex) = .empty;
    funcs.ensureTotalCapacity(self.allocator, node.functions.len) catch oom();

    for (node.functions) |*f| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        funcs.appendAssumeCapacity(try self.fnDeclaration(f, ctx));
        // At this point, the symbol exists
        const fn_type = self.scope.removeSymbolFromScope(fn_name).?.type;
        interned_struct.functions.putAssumeCapacity(fn_name, fn_type);
    }

    return self.irb.addInstr(
        .{ .struct_decl = .{
            .name = name,
            .index = sym.index,
            .fields_count = node.fields.len,
            .default_fields = default_fields,
            .functions = funcs.toOwnedSlice(self.allocator) catch oom(),
        } },
        span.start,
    );
}

fn structureFields(self: *Self, fields: []const Ast.VarDecl, ty: *Type.Structure, ctx: *Context) Error![]const InstrIndex {
    var default_fields: ArrayList(InstrIndex) = .empty;

    for (fields) |*f| {
        const span = self.ast.getSpan(f.name);
        var struct_field: Type.Structure.Field = undefined;
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (ty.fields.get(field_name) != null) return self.err(
            .{ .already_declared_field = .{ .name = self.ast.toSource(f.name) } },
            span,
        );

        struct_field.type = try self.checkAndGetType(f.typ, ctx);

        if (f.value) |value| {
            struct_field.default = true;
            var res = try self.analyzeExprInfos(value, true, ctx);

            if (!res.ti.comp_time) return self.err(.{ .non_comptime_default = .new(.field) }, self.ast.getSpan(value));
            // if (res.ti.type.is(.void)) return self.err(.void_value, self.ast.getSpan(value));

            if (struct_field.type.is(.void)) struct_field.type = res.ti.type;

            if (res.ti.type != struct_field.type) {
                if (res.ti.type.canCastTo(struct_field.type)) {
                    res.instr = self.irb.wrapInstr(.cast_to_float, res.instr);
                } else return self.err(
                    .{ .default_value_type_mismatch = .new(self.getTypeName(struct_field.type), self.getTypeName(res.ti.type), .field) },
                    self.ast.getSpan(f.name),
                );
            }

            default_fields.append(self.allocator, res.instr) catch oom();
        }

        ty.fields.putAssumeCapacity(field_name, struct_field);
    }

    return default_fields.toOwnedSlice(self.allocator) catch oom();
}

fn whileStmt(self: *Self, node: *const Ast.While, ctx: *Context) StmtResult {
    const span = self.ast.getSpan(node.condition);
    const cond_res = try self.analyzeExpr(node.condition, true, ctx);

    if (!cond_res.type.is(.bool)) return self.err(
        .{ .non_bool_cond = .{ .what = "while", .found = self.getTypeName(cond_res.type) } },
        span,
    );

    const body_res = try self.block(&node.body, false, ctx);

    // TODO: when changing rules on last expression in block, useless error
    if (!body_res.ti.type.is(.void)) return self.err(
        .{ .non_void_while = .{ .found = self.getTypeName(body_res.ti.type) } },
        self.ast.getSpan(node.body),
    );

    return self.irb.addInstr(.{ .@"while" = .{ .cond = cond_res.instr, .body = body_res.instr } }, span.start);
}

fn analyzeExpr(self: *Self, expr: *const Expr, exp_val: bool, ctx: *Context) TypeResult {
    const res = try self.analyzeExprInfos(expr, exp_val, ctx);
    return .{ .type = res.ti.type, .instr = res.instr };
}

fn analyzeExprInfos(self: *Self, expr: *const Expr, exp_val: bool, ctx: *Context) Result {
    const res = try switch (expr.*) {
        .array => |*e| self.array(e, ctx),
        .array_access => |*e| self.arrayAccess(e, ctx),
        .block => |*e| self.block(e, exp_val, ctx),
        .binop => |*e| self.binop(e, ctx),
        .@"break" => |*e| self.breakExpr(e, exp_val, ctx),
        .closure => |*e| self.closure(e, ctx),
        .extractor => |*e| self.extractor(e, ctx),
        .field => |*e| self.field(e, ctx),
        .fn_call => |*e| self.call(e, ctx),
        .grouping => |*e| self.analyzeExprInfos(e.expr, exp_val, ctx),
        .@"if" => |*e| self.ifExpr(e, exp_val, ctx),
        .literal => |*e| self.literal(e, ctx),
        .named_arg => unreachable,
        .@"return" => |*e| self.returnExpr(e, ctx),
        .struct_literal => |*e| self.structLiteral(e, ctx),
        .unary => |*e| self.unary(e, ctx),
    };

    if (exp_val and (res.ti.type.is(.void) or res.ti.type.is(.never))) {
        return self.err(.void_value, self.ast.getSpan(expr));
    }

    if (self.scope.isGlobal() and !res.ti.comp_time) {
        return self.err(.non_comptime_in_global, self.ast.getSpan(expr));
    }

    return res;
}

fn array(self: *Self, expr: *const Ast.Array, ctx: *Context) Result {
    var final_type = self.ti.getCached(.void);
    var backpatched = false;
    var pure = true;

    var values: ArrayList(InstrIndex) = .empty;
    values.ensureUnusedCapacity(self.allocator, expr.values.len) catch oom();

    for (expr.values) |val| {
        const span = self.ast.getSpan(val);

        const val_res = try self.analyzeExprInfos(val, true, ctx);
        var val_type = val_res.ti.type;
        var val_instr = val_res.instr;

        if (!final_type.is(.void) and final_type != val_type) {
            // If new value can't be cast to current array type
            if (val_type.canCastTo(final_type)) {
                val_type = final_type;
                val_instr = self.irb.wrapInstr(.cast_to_float, val_instr);
            } else {
                // If we didn't already changed array type based on new value (ex from []int to []float)
                // and that the array type is castable to value type, change array type and backpatch casts
                if (!backpatched and final_type.canCastTo(val_type)) {
                    for (values.items) |*e| {
                        e.* = self.irb.wrapInstr(.cast_to_float, e.*);
                    }
                    backpatched = true;
                } else return self.err(
                    .{ .array_elem_different_type = .{ .found1 = self.getTypeName(final_type), .found2 = self.getTypeName(val_type) } },
                    span,
                );
            }
        }

        self.checkWrap(&val_instr, false, val_res.ti.heap);
        final_type = val_type;
        pure = pure and val_res.ti.comp_time;
        values.appendAssumeCapacity(val_instr);
    }

    return .new(
        .{ .type = self.ti.intern(.{ .array = .{ .child = final_type } }), .comp_time = pure },
        self.irb.addInstr(
            .{ .array = .{ .values = values.toOwnedSlice(self.allocator) catch oom() } },
            self.ast.getSpan(expr).start,
        ),
    );
}

fn arrayAccess(self: *Self, expr: *const Ast.ArrayAccess, ctx: *Context) Result {
    var indicies: ArrayList(InstrIndex) = .empty;

    const arr_res = arr: {
        if (expr.array.* == .array_access) {
            break :arr try self.arrayAccessChain(expr, &indicies, ctx);
        } else {
            indicies.append(self.allocator, try self.expectArrayIndex(expr.index, ctx)) catch oom();
            break :arr try self.analyzeExpr(expr.array, true, ctx);
        }
    };

    if (!arr_res.type.is(.array)) return self.err(
        .{ .non_array_indexing = .{ .found = self.getTypeName(arr_res.type) } },
        self.ast.getSpan(expr.array),
    );

    const child_type = arr_res.type.array.getChildAt(indicies.items.len - 1) orelse return self.err(
        .{ .array_mismatch_dim = .{ .declared = arr_res.type.array.depth(), .accessed = indicies.items.len } },
        self.ast.getSpan(expr),
    );

    return .new(
        .{ .type = child_type, .heap = child_type.isHeap() },
        self.irb.addInstr(
            .{ .array_access = .{ .array = arr_res.instr, .indicies = indicies.toOwnedSlice(self.allocator) catch oom() } },
            self.ast.getSpan(expr).start,
        ),
    );
}

fn arrayAccessChain(self: *Self, expr: *const Ast.ArrayAccess, indicies: *ArrayList(InstrIndex), ctx: *Context) TypeResult {
    var current = expr;

    while (current.array.* == .array_access) {
        indicies.append(self.allocator, try self.expectArrayIndex(current.index, ctx)) catch oom();
        current = &current.array.array_access;
    }
    indicies.append(self.allocator, try self.expectArrayIndex(current.index, ctx)) catch oom();

    return self.analyzeExpr(current.array, true, ctx);
}

/// Analyze the expression and return an error if the type isn't an integer
fn expectArrayIndex(self: *Self, expr: *const Expr, ctx: *Context) Error!InstrIndex {
    const res = try self.analyzeExpr(expr, true, ctx);

    if (res.type != self.ti.cache.int) return self.err(
        .{ .non_integer_index = .{ .found = self.getTypeName(res.type) } },
        self.ast.getSpan(expr),
    );

    return res.instr;
}

// TODO: handle dead code eleminitaion so that it can be used in function's body?
fn block(self: *Self, expr: *const Ast.Block, exp_val: bool, ctx: *Context) Result {
    self.scope.open(self.allocator, false, self.internLabel(expr.label));

    var instrs: ArrayList(InstrIndex) = .empty;
    instrs.ensureTotalCapacity(self.allocator, expr.nodes.len) catch oom();

    var pure = false;
    var cf: InstrInfos.ControlFlow = .none;

    for (expr.nodes) |*node| {
        errdefer _ = self.scope.close();

        var res = try self.analyzeNodeInfos(node, ctx);
        pure = pure and res.ti.comp_time;

        if (!res.ti.type.is(.void)) {
            instrs.appendAssumeCapacity(self.irb.wrapPreviousInstr(.pop));
        } else {
            instrs.appendAssumeCapacity(res.instr);
        }

        if (res.cf.exitScope()) {
            cf = res.cf;
            break;
        }
    }

    if (exp_val and cf == .none) return self.err(.block_all_path_dont_return, self.ast.getSpan(expr));

    const pop_count, const breaks = self.scope.close();
    var final_type = ctx.decl_type orelse self.ti.getCached(.void);

    for (breaks) |b| {
        const coerce = try self.performTypeCoercion(final_type, b.type, false, b.span);
        if (coerce.cast) self.irb.wrapInstrInplace(.cast_to_float, b.instr);
        final_type = coerce.type;
    }

    // TODO: protect cast
    return .newCf(
        .newType(final_type),
        if (cf == .@"return") .@"return" else .none,
        self.irb.addInstr(
            .{ .block = .{
                .instrs = instrs.toOwnedSlice(self.allocator) catch oom(),
                .pop_count = @intCast(pop_count),
                .is_expr = !final_type.is(.void),
            } },
            self.ast.getSpan(expr).start,
        ),
    );
}

fn internLabel(self: *Self, label: ?Ast.TokenIndex) ?InternerIdx {
    const lbl = label orelse return null;
    return self.interner.intern(self.ast.toSource(lbl));
}

fn binop(self: *Self, expr: *const Ast.Binop, ctx: *Context) Result {
    const lhs = try self.analyzeExprInfos(expr.lhs, true, ctx);
    const rhs = try self.analyzeExprInfos(expr.rhs, true, ctx);

    const lhs_type = lhs.ti.type;
    const rhs_type = rhs.ti.type;

    const lhs_span = self.ast.getSpan(expr.lhs);
    const rhs_span = self.ast.getSpan(expr.rhs);

    if (isStringConcat(expr.op, lhs_type, rhs_type)) {
        return .fromType(
            self.ti.cache.str,
            self.irb.addInstr(.{ .binop = .{ .op = .add_str, .lhs = lhs.instr, .rhs = rhs.instr } }, lhs_span.start),
        );
    } else if (isStringRepeat(expr.op, lhs_type, rhs_type)) {
        return .fromType(
            self.ti.cache.str,
            self.irb.addInstr(
                .{ .binop = .{
                    .op = .mul_str,
                    .lhs = if (lhs_type.is(.int)) lhs.instr else rhs.instr,
                    .rhs = if (lhs_type.is(.int)) rhs.instr else lhs.instr,
                } },
                lhs_span.start,
            ),
        );
    }

    const op: Instruction.Binop.Op, const lhs_instr, const rhs_instr, const ty = instr: {
        switch (expr.op) {
            .plus, .slash, .star, .minus => {
                const lhs_instr, const rhs_instr, const ty = self.binopArithmeticCoercion(lhs, rhs) catch |e| return switch (e) {
                    error.NonNumLsh => self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(lhs.ti.type) } }, lhs_span),
                    error.NonNumRhs => self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(rhs.ti.type) } }, rhs_span),
                };
                break :instr .{ getArithmeticOp(expr.op, ty), lhs_instr, rhs_instr, ty };
            },
            .greater_equal, .greater, .less_equal, .less => {
                const lhs_instr, const rhs_instr, const ty = self.binopArithmeticCoercion(lhs, rhs) catch |e| return switch (e) {
                    error.NonNumLsh => self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(lhs.ti.type) } }, lhs_span),
                    error.NonNumRhs => self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(rhs.ti.type) } }, rhs_span),
                };

                if (lhs_type.is(.float) or rhs_type.is(.float)) self.warn(.float_equal, self.ast.getSpan(expr));

                break :instr .{ getArithmeticOp(expr.op, ty), lhs_instr, rhs_instr, self.ti.getCached(.bool) };
            },
            .equal_equal, .bang_equal => {
                const lhs_instr, const rhs_instr, const ty = self.binopComparisonCoercion(lhs, rhs) catch |e| return switch (e) {
                    error.NonNullLhs => self.err(.{ .non_null_comp_optional = .{ .found = self.getTypeName(lhs_type) } }, lhs_span),
                    error.NonNullRhs => self.err(.{ .non_null_comp_optional = .{ .found = self.getTypeName(rhs_type) } }, rhs_span),
                    error.Invalid => self.err(
                        .{ .invalid_comparison = .{ .found1 = self.getTypeName(lhs_type), .found2 = self.getTypeName(rhs_type) } },
                        self.ast.getSpan(expr),
                    ),
                };

                if (lhs_type.is(.float) or rhs_type.is(.float)) self.warn(.float_equal, self.ast.getSpan(expr));

                break :instr .{ getComparisonOp(expr.op, ty), lhs_instr, rhs_instr, self.ti.getCached(.bool) };
            },
            .@"and", .@"or" => {
                if (!lhs_type.is(.bool)) return self.err(.{ .invalid_logical = .{ .found = self.getTypeName(lhs_type) } }, lhs_span);
                if (!rhs_type.is(.bool)) return self.err(.{ .invalid_logical = .{ .found = self.getTypeName(rhs_type) } }, rhs_span);

                break :instr .{ if (expr.op == .@"and") .@"and" else .@"or", lhs.instr, rhs.instr, self.ti.getCached(.bool) };
            },
            else => unreachable,
        }
    };

    return .new(
        .{ .type = ty, .comp_time = lhs.ti.comp_time and rhs.ti.comp_time },
        self.irb.addInstr(.{ .binop = .{ .op = op, .lhs = lhs_instr, .rhs = rhs_instr } }, lhs_span.start),
    );
}

fn isStringConcat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .plus and lhs.is(.str) and rhs.is(.str);
}

fn isStringRepeat(op: TokenTag, lhs: *const Type, rhs: *const Type) bool {
    return op == .star and ((lhs.is(.str) and rhs.is(.int)) or (lhs.is(.int) and rhs.is(.str)));
}

fn binopArithmeticCoercion(
    self: *Self,
    lhs: InstrInfos,
    rhs: InstrInfos,
) error{ NonNumLsh, NonNumRhs }!struct { InstrIndex, InstrIndex, *const Type } {
    const lhs_type = lhs.ti.type;
    const rhs_type = rhs.ti.type;

    if (lhs_type == rhs_type) return .{ lhs.instr, rhs.instr, lhs_type };
    if (!lhs_type.isNumeric()) return error.NonNumLsh;
    if (!rhs_type.isNumeric()) return error.NonNumRhs;

    if (lhs_type.is(.int) and rhs_type.is(.float)) {
        return .{ self.irb.wrapInstr(.cast_to_float, lhs.instr), rhs.instr, rhs_type };
    }

    if (rhs_type.is(.int) and lhs_type.is(.float)) {
        return .{ lhs.instr, self.irb.wrapInstr(.cast_to_float, rhs.instr), lhs_type };
    }

    return .{ lhs.instr, rhs.instr, lhs_type };
}

fn getArithmeticOp(op: TokenTag, ty: *const Type) Instruction.Binop.Op {
    return switch (op) {
        .plus => if (ty.is(.int)) .add_int else .add_float,
        .slash => if (ty.is(.int)) .div_int else .div_float,
        .star => if (ty.is(.int)) .mul_int else .mul_float,
        .minus => if (ty.is(.int)) .sub_int else .sub_float,

        .less => if (ty.is(.int)) .lt_int else .lt_float,
        .less_equal => if (ty.is(.int)) .le_int else .le_float,
        .greater => if (ty.is(.int)) .gt_int else .gt_float,
        .greater_equal => if (ty.is(.int)) .ge_int else .ge_float,
        else => unreachable,
    };
}

fn binopComparisonCoercion(
    self: *Self,
    lhs: InstrInfos,
    rhs: InstrInfos,
) error{ NonNullLhs, NonNullRhs, Invalid }!struct { InstrIndex, InstrIndex, *const Type } {
    const lhs_type = lhs.ti.type;
    const rhs_type = rhs.ti.type;

    if (lhs_type == rhs_type) return .{ lhs.instr, rhs.instr, lhs_type };

    arithmetic: {
        return self.binopArithmeticCoercion(lhs, rhs) catch break :arithmetic;
    }

    if (lhs_type.is(.optional) or rhs_type.is(.optional)) {
        if (lhs_type.is(.null) or rhs_type.is(.null)) {
            if (lhs_type.is(.optional)) {
                return .{ lhs.instr, rhs.instr, lhs_type };
            } else {
                return .{ rhs.instr, lhs.instr, rhs_type };
            }
        }

        if (lhs_type.is(.optional)) return error.NonNullRhs;
        if (rhs_type.is(.optional)) return error.NonNullLhs;
    }

    return error.Invalid;
}

fn getComparisonOp(op: TokenTag, ty: *const Type) Instruction.Binop.Op {
    return switch (ty.*) {
        .bool => if (op == .equal_equal) .eq_bool else .ne_bool,
        .int => if (op == .equal_equal) .eq_int else .ne_int,
        .float => if (op == .equal_equal) .eq_float else .ne_float,
        .str => if (op == .equal_equal) .eq_str else .ne_str,
        .null, .optional => if (op == .equal_equal) .eq_null else .ne_null,
        else => unreachable,
    };
}

fn breakExpr(self: *Self, expr: *const Ast.Break, exp_val: bool, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    const ty, const expr_instr = brk: {
        const e = expr.expr orelse break :brk .{ self.ti.getCached(.void), null };

        const res = try self.analyzeExpr(e, exp_val, ctx);
        break :brk .{ res.type, res.instr };
    };

    const scope, const depth = self.scope.getScopeByName(self.internLabel(expr.label)) catch return self.err(
        .{ .undeclared_block_label = .{ .name = self.ast.toSource(expr.label.?) } },
        self.ast.getSpan(expr.label.?),
    );

    const instr = self.irb.addInstr(.{ .@"break" = .{ .instr = expr_instr, .depth = depth } }, span.start);
    scope.breaks.append(self.allocator, .{ .instr = instr, .type = ty, .span = span }) catch oom();

    return .fromTypeCf(ty, .@"break", instr);
}

fn closure(self: *Self, expr: *const Ast.FnDecl, ctx: *Context) Result {
    self.scope.open(self.allocator, true, null);
    defer _ = self.scope.close();

    const captures = try self.loadFunctionCaptures(&expr.meta.captures);
    const param_res = try self.fnParams(expr.params, ctx);

    // Update type for resolution in function's body
    const closure_type: Type.Function = .{
        .loc = null,
        .params = param_res.decls,
        .return_type = try self.checkAndGetType(expr.return_type, ctx),
        .kind = .normal,
    };
    const interned_type = self.ti.intern(.{ .function = closure_type });

    const span = self.ast.getSpan(expr);
    const offset = span.start;

    ctx.fn_type = interned_type;
    const body_instrs, const returns = try self.fnBody(expr.body.nodes, &closure_type, span, ctx);

    // TODO: protect the cast
    return .fromType(
        interned_type,
        self.irb.addInstr(
            .{ .fn_decl = .{
                .kind = .closure,
                .name = null,
                .body = body_instrs,
                .defaults = param_res.defaults,
                .captures = captures,
                .return_kind = if (returns) .explicit else if (interned_type.is(.void)) .implicit_void else .implicit_value,
            } },
            offset,
        ),
    );
}

fn extractor(self: *Self, expr: *const Ast.Extractor, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const expr_ty = try self.analyzeExpr(expr.expr, true, ctx);

    // TODO: error
    const ty = expr_ty.type.getChildIfOptional() orelse @panic("Extract non optional");

    // TODO: be sure that it's in the correct scope
    const binding = try self.internIfNotInCurrentScope(expr.alias);
    _ = try self.forwardDeclareVariable(binding, ty, false, self.ast.getSpan(expr.alias));

    return .fromType(self.ti.cache.bool, self.irb.addInstr(.{ .extractor = expr_ty.instr }, span.start));
}

fn field(self: *Self, expr: *const Ast.Field, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.structure);

    const struct_res: InstrInfos = switch (expr.structure.*) {
        .field => |*f| try self.field(f, ctx),
        .literal => |e| b: {
            const ident = try self.identifier(e.idx, true, ctx);
            break :b .new(.{ .type = ident.type, .heap = true, .is_sym = ident.kind != .variable }, ident.instr);
        },
        else => try self.analyzeExprInfos(expr.structure, true, ctx),
    };

    const field_res = switch (struct_res.ti.type.*) {
        .module => |ty| return self.moduleAccess(expr.field, ty),
        .structure => |*ty| try self.structureAccess(expr.field, ty, struct_res.ti.is_sym, ctx),
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.getTypeName(struct_res.ti.type) } },
            span,
        ),
    };

    const kind: Instruction.Field.Kind = switch (field_res.kind) {
        // TODO: create just a 'function'. For now we need this because we get static method from
        // symbols that are loaded on stack, not in register so we need a separate logic
        .function => if (struct_res.ti.is_sym) .static_method else .method,
        else => .field,
    };

    if (kind == .method and !ctx.in_call) {
        return self.boundMethod(field_res.type, field_res.index, struct_res.instr, span);
    }

    // Lhs must be heap_ref too. It's used to allow calls to break chains like: getVec().point1
    // If the object returned by getVec() is a literal for example, the rest of the chain will be a stack allocated object
    return .new(
        .{
            .type = field_res.type,
            .heap = struct_res.ti.heap and field_res.type.isHeap(),
            .is_sym = struct_res.ti.is_sym,
        },
        self.irb.addInstr(
            .{ .field = .{ .structure = struct_res.instr, .index = field_res.index, .kind = kind } },
            span.start,
        ),
    );
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

fn moduleAccess(self: *Self, field_tk: Ast.TokenIndex, module_idx: InternerIdx) Result {
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
    return .new(
        .{ .type = sym.type, .is_sym = true },
        self.irb.addInstr(
            .{ .load_symbol = .{ .module_index = index, .symbol_index = @intCast(sym.index) } },
            span.start,
        ),
    );
}

fn boundMethod(self: *Self, func_type: *const Type, field_index: usize, structure: InstrIndex, span: Span) Result {
    const bounded_type = func_type.function.toBoundMethod(self.allocator);
    const ty = self.ti.intern(.{ .function = bounded_type });

    return .fromType(ty, self.irb.addInstr(.{ .bound_method = .{ .structure = structure, .index = field_index } }, span.start));
}

fn call(self: *Self, expr: *const Ast.FnCall, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);

    const ctx_call = ctx.setAndGetPrevious(.in_call, true);
    const callee = try self.analyzeExpr(expr.callee, true, ctx);

    if (!callee.type.is(.function)) return self.err(.invalid_call_target, span);

    // Restore state before arguments analyzis
    ctx.in_call = ctx_call;
    const args_res = try self.fnArgsList(expr.args, &callee.type.function, span, ctx);

    return .fromType(
        callee.type.function.return_type,
        self.irb.addInstr(
            .{ .call = .{ .callee = callee.instr, .args = args_res, .implicit_first = callee.type.function.kind == .method } },
            span.start,
        ),
    );
}

fn fnArgsList(self: *Self, args: []*Expr, ty: *const Type.Function, err_span: Span, ctx: *Context) Error![]const Instruction.Arg {
    var proto = ty.proto(self.allocator);
    const param_count = proto.count();
    const params = ty.params.values()[@intFromBool(ty.kind == .method)..];

    if (args.len > param_count) return self.err(.{ .too_many_fn_args = .{ .expect = param_count, .found = args.len } }, err_span);

    var instrs = self.allocator.alloc(Instruction.Arg, params.len) catch oom();
    var proto_values = proto.values();

    var default_count: usize = 0;
    for (proto_values, 0..) |val, i| {
        if (val.default) {
            instrs[i] = .{ .default = default_count };
            default_count += 1;
        } else {
            instrs[i] = undefined;
        }
    }

    for (args, 0..) |arg, i| {
        var param_info: *const Type.Function.Parameter = undefined;

        var value, const index, const value_span = value: {
            switch (arg.*) {
                .named_arg => |na| {
                    if (ty.kind == .bound) return self.err(.named_arg_in_bounded, self.ast.getSpan(na.name));

                    const name = self.interner.intern(self.ast.toSource(na.name));
                    const gop = proto.getOrPutAssumeCapacity(name);

                    if (gop.value_ptr.done) return self.err(
                        .{ .duplicate_param = .{ .name = self.ast.toSource(na.name) } },
                        self.ast.getSpan(na.name),
                    );
                    gop.value_ptr.done = true;

                    param_info = ty.params.getPtr(name) orelse return self.err(
                        .{ .unknown_param = .{ .name = self.ast.toSource(na.name) } },
                        self.ast.getSpan(na.name),
                    );

                    const value_type = try self.analyzeExpr(na.value, true, ctx);

                    break :value .{ value_type, gop.index, self.ast.getSpan(na.value) };
                },
                else => {
                    const value_type = try self.analyzeExpr(arg, true, ctx);
                    param_info = &params[i];
                    proto_values[i].done = true;

                    break :value .{ value_type, i, self.ast.getSpan(arg) };
                },
            }
        };

        const coerce = try self.performTypeCoercion(param_info.type, value.type, false, value_span);

        self.checkWrap(&value.instr, coerce.cast, false);
        if (param_info.captured) value.instr = self.irb.wrapPreviousInstr(.box);

        instrs[index] = .{ .instr = value.instr };
    }

    // Check if any missing non-default parameter
    const err_count = self.errs.items.len;

    for (proto.keys(), proto_values) |k, v| {
        if (v.done or v.default) continue;
        self.err(.{ .missing_function_param = .{ .name = self.interner.getKey(k).? } }, err_span) catch {};
    }

    return if (err_count < self.errs.items.len) error.Err else instrs;
}

const IdentRes = struct {
    type: *const Type,
    kind: enum { variable, symbol, module },
    comp_time: bool = true,
    instr: InstrIndex,
};

/// Tries to find a match from variables and symbols and returns its type while emitting an instruction
fn identifier(self: *Self, token_name: Ast.TokenIndex, initialized: bool, ctx: *const Context) Error!IdentRes {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    if (self.variableIdentifier(name, span)) |res| {
        if (initialized and !res.variable.initialized) {
            return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(token_name));
        }

        return .{
            .type = res.variable.type,
            .kind = .variable,
            .comp_time = res.variable.comp_time,
            .instr = res.instr,
        };
    }

    const sym_name = if (name == self.cached_names.Self) b: {
        const struct_type = ctx.struct_type orelse return self.err(.big_self_outside_struct, span);
        break :b struct_type.structure.loc.?.name;
    } else name;

    if (self.symbolIdentifier(sym_name, span)) |res| {
        return .{ .type = res.sym.type, .kind = .symbol, .instr = res.instr };
    }

    if (self.externSymbolIdentifier(sym_name, span)) |res| {
        return .{ .type = res.sym.type, .kind = .symbol, .instr = res.instr };
    }

    if (self.scope.getModule(name)) |mod| {
        return .{ .type = mod, .kind = .module, .instr = 0 };
    }

    return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(token_name));
}

const VariableInstr = struct { variable: *LexScope.Variable, instr: InstrIndex };

fn expectVariableIdentifier(self: *Self, token_name: Ast.TokenIndex) Error!VariableInstr {
    const span = self.ast.getSpan(token_name);
    const text = self.ast.toSource(token_name);
    const name = self.interner.intern(text);

    return self.variableIdentifier(name, span) orelse return self.err(
        .{ .undeclared_var = .{ .name = self.interner.getKey(name).? } },
        span,
    );
}

/// Tries to find a variable in scopes and returns it while emitting an instruction
fn variableIdentifier(self: *Self, name: InternerIdx, span: Span) ?VariableInstr {
    const variable, const scope_offset = self.scope.getVariable(name) orelse return null;

    // TODO: use scope directly instead of this shenanigan
    var instr = self.irb.addInstr(
        .{ .identifier = .{ .index = variable.index + scope_offset, .scope = switch (variable.kind) {
            .local => .local,
            .global => .global,
        } } },
        span.start,
    );

    self.checkWrap(&instr, false, false);
    if (variable.captured) instr = self.irb.wrapPreviousInstr(.unbox);

    return .{ .variable = variable, .instr = instr };
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn symbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?struct { sym: *LexScope.Symbol, instr: InstrIndex } {
    const sym = self.scope.getSymbol(name) orelse return null;

    // TODO: protect cast
    return .{
        .sym = sym,
        .instr = self.irb.addInstr(
            .{ .load_symbol = .{ .module_index = null, .symbol_index = @intCast(sym.index) } },
            span.start,
        ),
    };
}

/// Tries to find a symbol in scopes and returns it while emitting an instruction
fn externSymbolIdentifier(self: *Self, name: InternerIdx, span: Span) ?struct { sym: *LexScope.Symbol, instr: InstrIndex } {
    const ext = self.scope.getExternSymbol(name) orelse return null;

    // TODO: protect cast
    return .{
        .sym = &ext.symbol,
        .instr = self.irb.addInstr(
            .{ .load_symbol = .{ .module_index = ext.module_index, .symbol_index = @intCast(ext.symbol.index) } },
            span.start,
        ),
    };
}

fn ifExpr(self: *Self, expr: *const Ast.If, exp_val: bool, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.condition);

    const cond_res = try self.analyzeExprInfos(expr.condition, true, ctx);
    var pure = cond_res.ti.comp_time;

    // We can continue to analyze if the condition isn't a bool
    if (!cond_res.ti.type.is(.bool)) self.err(
        .{ .non_bool_cond = .{ .what = "if", .found = self.getTypeName(cond_res.ti.type) } },
        span,
    ) catch {};

    // Analyze then branch
    var then_res = try self.analyzeNodeInfos(&expr.then, ctx);
    self.checkWrap(&then_res.instr, false, then_res.ti.heap);

    pure = pure and then_res.ti.comp_time;

    var else_cf: ?InstrInfos.ControlFlow = null;
    var else_ty: ?*const Type = null;
    var else_instr: ?InstrIndex = null;

    if (expr.@"else") |*n| {
        const else_res = try self.analyzeNodeInfos(n, ctx);
        else_instr = if (else_res.ti.heap) self.irb.wrapInstr(.incr_rc, else_res.instr) else else_res.instr;

        pure = pure and else_res.ti.comp_time;
        else_cf = else_res.cf;
        else_ty = else_res.ti.type;
    } else if (exp_val) {
        return self.err(
            .{ .missing_else_clause = .{ .if_type = self.getTypeName(then_res.ti.type) } },
            self.ast.getSpan(expr),
        );
    }

    const branch_res = try self.mergeIfBranch(then_res.ti.type, then_res.cf, else_ty, else_cf, expr, ctx);

    if (branch_res.cast_then) then_res.instr = self.irb.wrapInstr(.cast_to_float, then_res.instr);
    if (branch_res.cast_else) else_instr = self.irb.wrapInstr(.cast_to_float, else_instr.?);

    return .newCf(
        .{ .type = branch_res.type, .comp_time = pure },
        if (branch_res.type.is(.never)) .@"return" else .none,
        self.irb.addInstr(
            .{ .@"if" = .{ .cond = cond_res.instr, .then = then_res.instr, .@"else" = else_instr } },
            span.start,
        ),
    );
}

const IfBranchRes = struct { type: *const Type, cast_then: bool = false, cast_else: bool = false };

fn mergeIfBranch(
    self: *Self,
    then_ty: *const Type,
    then_cf: InstrInfos.ControlFlow,
    else_ty: ?*const Type,
    else_cf: ?InstrInfos.ControlFlow,
    expr: *const Ast.If,
    ctx: *const Context,
) Error!IfBranchRes {
    // Always take declaration as truth
    var final = self.getDeclOrVoid(ctx);
    // TODO: Res is not really used...
    var res: IfBranchRes = .{ .type = final };

    const then_span = self.ast.getSpan(expr.then);

    const then_res = try self.checkBranchReturn(then_ty, then_cf);
    if (then_res.cast) res.cast_then = true;

    const else_type = else_ty orelse return res;
    const else_span = self.ast.getSpan(expr.@"else".?);

    const else_res = try self.checkBranchReturn(else_type, else_cf.?);
    if (else_res.cast) res.cast_else = true;

    // Type checking. We don't use perform type coercion because the function check against a
    // declaration. Here, we don't have any reference/declaration, the two branches must be coherent
    {
        // Here, as the declaration can be 'void' like: var a = if true do 5 else 8.5
        // even after type coercion against declaration we have to check the branches together
        if (then_res.type == else_res.type) {
            final = then_res.type;
        }

        // If then returned but we are here, only else is producing a type
        // BUG: what if else returns too?
        else if (then_cf == .@"return") {
            final = else_res.type;
        }
        // If else returned but not then, then is producing a value
        else if (else_cf.? == .@"return") {
            final = then_res.type;
        }

        // If both branch don't return a value, error
        else if (then_res.type.is(.void) and !else_res.type.is(.void)) {
            return self.err(.void_value, then_span);
        }
        // Check same condition
        else if (else_res.type.is(.void) and !then_res.type.is(.void)) {
            return self.err(.void_value, else_span);
        }

        // Check if they can cast to each other
        else if (else_res.type.canCastTo(then_res.type)) {
            final = then_res.type;
            res.cast_else = true;
        } else if (then_res.type.canCastTo(else_res.type)) {
            final = else_res.type;
            res.cast_then = true;
        }

        // Check if there is a nullable coercion possible
        else if (then_res.type.is(.null) and !else_res.type.is(.null)) {
            final = self.ti.intern(.{ .optional = else_res.type });
        }
        // Check if there is a nullable coercion possible
        else if (else_res.type.is(.null) and !then_res.type.is(.null)) {
            final = self.ti.intern(.{ .optional = then_res.type });
        }

        // Otherwise, error
        else return self.err(
            .{ .type_mismatch = .{ .expect = self.getTypeName(then_res.type), .found = self.getTypeName(else_res.type) } },
            else_span,
        );
    }

    return .{ .type = if (then_cf.exitScope() and else_cf.?.exitScope()) self.ti.getCached(.never) else final };
}

fn checkBranchReturn(self: *Self, ty: *const Type, cf: InstrInfos.ControlFlow) Error!TypeCoherence {
    return .{ .type = if (cf.exitScope()) self.ti.getCached(.void) else ty, .cast = false };
}

fn literal(self: *Self, expr: *const Ast.Literal, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const text = self.ast.toSource(expr);

    const ty, const instr = b: {
        switch (expr.tag) {
            .bool => break :b .{ self.ti.cache.bool, self.irb.addInstr(.{ .bool = self.ast.token_tags[expr.idx] == .true }, span.start) },
            .identifier, .self => {
                const res = try self.identifier(expr.idx, true, ctx);
                return .new(
                    .{ .type = res.type, .heap = res.type.isHeap(), .comp_time = res.comp_time },
                    res.instr,
                );
            },
            .int => {
                const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                    // TODO: error handling, only one possible it's invalid char
                    std.debug.print("Error parsing integer\n", .{});
                    break :blk 0;
                };

                break :b .{ self.ti.cache.int, self.irb.addInstr(.{ .int = value }, span.start) };
            },
            .float => {
                const value = std.fmt.parseFloat(f64, text) catch blk: {
                    // TODO: error handling, only one possible it's invalid char or too big
                    std.debug.print("Error parsing float\n", .{});
                    break :blk 0.0;
                };

                break :b .{ self.ti.cache.float, self.irb.addInstr(.{ .float = value }, span.start) };
            },
            .null => break :b .{ self.ti.cache.null, self.irb.addInstr(.null, span.start) },
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

                break :b .{ self.ti.cache.str, self.irb.addInstr(.{ .string = value }, span.start) };
            },
        }
    };

    return .{ .ti = .{ .type = ty, .comp_time = true }, .instr = instr };
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral, ctx: *Context) Result {
    const span = self.ast.getSpan(expr.structure);
    const struct_res = try self.analyzeExprInfos(expr.structure, true, ctx);
    var comp_time = struct_res.ti.comp_time;

    const struct_type = struct_res.ti.type.as(.structure) orelse {
        return self.err(.non_struct_struct_literal, span);
    };

    var proto = struct_type.proto(self.allocator);
    defer proto.deinit(self.allocator);

    var values = self.allocator.alloc(Instruction.Arg, struct_type.fields.count()) catch oom();

    var default_count: usize = 0;
    for (struct_type.fields.values(), 0..) |f, i| {
        if (f.default) {
            values[i] = .{ .default = default_count };
            default_count += 1;
        } else {
            values[i] = undefined;
        }
    }

    for (expr.fields) |*fv| {
        const field_span = self.ast.getSpan(fv.name);
        const field_name = self.interner.intern(self.ast.toSource(fv.name));

        const f = struct_type.fields.get(field_name) orelse return self.err(
            .{ .unknown_struct_field = .{ .name = self.ast.toSource(fv.name) } },
            field_span,
        );
        const field_index = struct_type.fields.getIndex(field_name).?;

        const gop = proto.getOrPutAssumeCapacity(field_name);
        if (gop.value_ptr.done) {
            return self.err(.{ .duplicate_field = .{ .name = self.interner.getKey(field_name).? } }, field_span);
        }
        gop.value_ptr.done = true;

        var res: InstrInfos = if (fv.value) |value|
            try self.analyzeExprInfos(value, true, ctx)
        else b: {
            // Syntax: { x } instead of { x = x }
            const res = try self.expectVariableIdentifier(fv.name);
            break :b .new(
                .{ .type = res.variable.type, .heap = res.variable.type.isHeap(), .comp_time = res.variable.comp_time },
                res.instr,
            );
        };

        comp_time = comp_time and res.ti.comp_time;
        const value_span = if (fv.value) |val| self.ast.getSpan(val) else field_span;
        const coercion = try self.performTypeCoercion(f.type, res.ti.type, false, value_span);

        self.checkWrap(&res.instr, coercion.cast, res.ti.heap);
        values[field_index] = .{ .instr = res.instr };
    }

    // Check for non-completed prototype
    const err_count = self.errs.items.len;

    for (proto.keys(), proto.values()) |k, v| {
        if (v.done or v.default) continue;
        self.err(.{ .missing_field_struct_literal = .{ .name = self.interner.getKey(k).? } }, span) catch {};
    }

    if (self.errs.items.len > err_count) return error.Err;

    return .new(
        .{ .type = struct_res.ti.type, .comp_time = comp_time },
        self.irb.addInstr(
            .{ .struct_literal = .{ .structure = struct_res.instr, .values = values } },
            span.start,
        ),
    );
}

fn returnExpr(self: *Self, expr: *const Ast.Return, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const fn_type = ctx.fn_type orelse return self.err(.return_outside_fn, span);
    const ty = fn_type.function.return_type;

    // errdefer ctx.returns = false;
    // defer ctx.returns = true;

    const exp = expr.expr orelse return .{
        .ti = .newType(self.ti.getCached(.void)),
        .cf = .@"return",
        .instr = self.irb.addInstr(.{ .@"return" = .{ .value = null } }, span.start),
    };

    var value_res = try self.analyzeExprInfos(exp, false, ctx);

    // We do that here because we can insert a cast
    if (ty != value_res.ti.type) {
        const err_span = if (expr.expr) |e| self.ast.getSpan(e) else span;
        const coerce = try self.performTypeCoercion(ty, value_res.ti.type, true, err_span);
        value_res.ti.type = coerce.type;
        if (coerce.cast) value_res.instr = self.irb.wrapInstr(.cast_to_float, value_res.instr);
    }

    return .newCf(
        value_res.ti,
        .@"return",
        self.irb.addInstr(.{ .@"return" = .{ .value = value_res.instr } }, span.start),
    );
}

fn unary(self: *Self, expr: *const Ast.Unary, ctx: *Context) Result {
    const span = self.ast.getSpan(expr);
    const op = self.ast.token_tags[expr.op];

    const rhs = try self.analyzeExprInfos(expr.expr, true, ctx);
    const ty = rhs.ti.type;

    if (op == .not and !ty.is(.bool)) {
        return self.err(.{ .invalid_unary = .{ .found = self.getTypeName(ty) } }, span);
    }
    if (op == .minus and !ty.isNumeric()) {
        return self.err(.{ .invalid_arithmetic = .{ .found = self.getTypeName(ty) } }, span);
    }

    return .new(
        rhs.ti,
        self.irb.addInstr(
            .{ .unary = .{
                .op = if (op == .not) .bang else .minus,
                .typ = if (ty.is(.int)) .int else .float,
                .instr = rhs.instr,
            } },
            span.start,
        ),
    );
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
fn checkAndGetType(self: *Self, ty: ?*const Ast.Type, ctx: *const Context) Error!*const Type {
    const t = ty orelse return self.ti.getCached(.void);

    return switch (t.*) {
        .array => |arr_type| {
            const child = try self.checkAndGetType(arr_type.child, ctx);

            if (child.is(.void)) {
                return self.err(.void_array, self.ast.getSpan(arr_type.child));
            }

            return self.ti.intern(.{ .array = .{ .child = child } });
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

            return self.ti.intern(.{ .function = .{
                .loc = null,
                .params = params,
                .return_type = try self.checkAndGetType(func.return_type, ctx),
                .kind = .normal,
            } });
        },
        .optional => |opt| {
            const child = try self.checkAndGetType(opt.child, ctx);
            return self.ti.intern(.{ .optional = child });
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
    };
}

const TypeCoherence = struct { type: *const Type, cast: bool = false };

/// Checks for `void` values, array inference, cast and function type generation
/// The goal is to see if the two types are equivalent and if so, make the transformations needed
fn performTypeCoercion(self: *Self, decl: *const Type, value: *const Type, decl_explicit_void: bool, span: Span) Error!TypeCoherence {
    if (decl == value) return .{ .type = decl, .cast = false };

    if (value.is(.null)) {
        return if (decl.is(.optional))
            .{ .type = decl, .cast = false }
        else
            self.err(.{ .null_assign_to_non_optional = .{ .expect = self.getTypeName(decl) } }, span);
    }

    // If this is 'never', it means we ended with a control flow in which all branches returned
    // In that case, the type has already been tested against function's type
    if (value.is(.never)) return .{ .type = decl, .cast = false };

    // If we don't assign an optional, extract the chgild type from declaration
    var current_decl = if (decl.is(.optional)) decl.optional else decl;
    // Then if it's the same, return it
    if (current_decl == value) return .{ .type = decl, .cast = false };

    check: {
        if (value.is(.array)) {
            return self.checkArrayType(current_decl, value, span) catch |e| switch (e) {
                error.mismatch => break :check,
                else => |narrowed| return narrowed,
            };
        } else if (value.is(.function)) {
            return self.checkFunctionEq(current_decl, value) catch break :check;
        }

        // TODO: proper error handling
        if (value.is(.module)) @panic("Can't use modules in expressions");

        var cast = false;

        if (current_decl.is(.void)) {
            // If a void in declaration is an explicit expected type, we can't allow any value
            // Used for example when a function doesn't declare any return type
            if (decl_explicit_void and !value.is(.void)) break :check;

            current_decl = value;
        } else if (current_decl != value) {
            // One case in wich we can coerce, int -> float
            if (value.canCastTo(current_decl)) {
                cast = true;
            } else break :check;
        }

        // We wrap again the non-optional
        if (decl.is(.optional)) {
            current_decl = self.ti.intern(.{ .optional = current_decl });
        }

        return .{ .type = current_decl, .cast = cast };
    }

    return self.err(
        .{ .type_mismatch = .{ .expect = self.getTypeName(decl), .found = self.getTypeName(value) } },
        span,
    );
}

/// Checks if two different pointers to function type are equal, due to anonymus ones
fn checkFunctionEq(self: *Self, decl: *const Type, value: *const Type) Error!TypeCoherence {
    // Functions function's return types like: 'fn add() -> fn(int) -> int' don't have a declaration
    // There is also the case when assigning to a variable and infering type like: var bound = foo.method
    // Here, we want `bound` to be an anonymus function, it loses all declaration infos because it's a runtime value
    if (decl.is(.void)) return .{
        .type = if (value.function.loc != null)
            self.ti.intern(.{ .function = value.function.toAnon(self.allocator) })
        else
            value,
        .cast = false,
    };

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

    return error.Err;
}

/// Try to infer array value type from variable's declared type
fn checkArrayType(self: *Self, decl: *const Type, value: *const Type, span: Span) (Error || error{mismatch})!TypeCoherence {
    const depth_value, const child_value = value.array.getDepthAndChild();

    check: {
        if (decl.is(.void)) {
            // Empty array like: []
            if (child_value.is(.void)) {
                // No type declared and empty array like: var a = [], else infer from declaration
                return self.err(.cant_infer_arary_type, span);
            }
        } else {
            if (!decl.is(.array)) break :check;
            const depth_decl, const child_decl = decl.array.getDepthAndChild();
            if (depth_value != depth_decl) break :check;
            if (!child_value.is(.void)) {
                if (depth_value != depth_decl or child_value != child_decl) break :check;
            }
        }

        return .{ .type = value, .cast = false };
    }

    return error.mismatch;
}

fn getTypeName(self: *const Self, ty: *const Type) []const u8 {
    return ty.toString(self.allocator, &self.scope, self.module_name, self.interner, &self.pipeline.ctx.module_interner);
}

pub fn getDeclOrVoid(self: *const Self, ctx: *const Context) *const Type {
    return ctx.decl_type orelse self.ti.getCached(.void);
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
    ) catch return self.err(.too_many_locals, span);
}

fn forwardDeclareVariable(self: *Self, name: InternerIdx, ty: *const Type, captured: bool, span: Span) Error!void {
    return self.scope.declareVarInFutureScope(self.allocator, name, ty, captured) catch self.err(.too_many_locals, span);
}
