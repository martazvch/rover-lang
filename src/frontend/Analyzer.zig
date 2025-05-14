const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const Interner = @import("../Interner.zig");
const Pipeline = @import("../Pipeline.zig");
const GenReport = @import("../reporter.zig").GenReport;
const oom = @import("../utils.zig").oom;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const rir = @import("rir.zig");
const ReturnKind = rir.ReturnKind;
const Instruction = rir.Instruction;
const Span = @import("Lexer.zig").Span;
const TypeManager = @import("TypeManager.zig");
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;
const TypeInfo = TypeSys.TypeInfo;

ast: *const Ast,
pipeline: *Pipeline,
interner: *Interner,
type_manager: *TypeManager,

instructions: MultiArrayList(Instruction),
warns: ArrayListUnmanaged(AnalyzerReport),
errs: ArrayListUnmanaged(AnalyzerReport),
declarations: AutoHashMapUnmanaged(usize, Type),

globals: ArrayListUnmanaged(Variable),
locals: ArrayListUnmanaged(Variable),
imports: ArrayListUnmanaged(Variable),
scope_depth: usize,
local_offset: usize,
heap_count: usize,
main: ?usize,
state: State,
symbols: TypeSys.Symbols,
modules: ArrayListUnmanaged(Pipeline.Module),

// TODO: mettre en struct
empty_interned: usize,
main_interned: usize,
std_interned: usize,
self_interned: usize,
init_interned: usize,

arena: std.heap.ArenaAllocator,
allocator: Allocator,
repl: bool,

const Self = @This();
const Error = error{Err} || TypeManager.Error;

const Variable = struct {
    index: usize = 0,
    typ: Type = .void,
    depth: usize,
    name: usize,
    decl: usize = 0,
    initialized: bool = false,
    captured: bool = false,
    kind: Tag = .variable,

    pub const Tag = enum { variable, func, param, module, @"struct" };

    pub fn scope(self: *const Variable) rir.Scope {
        return if (self.captured) .heap else if (self.depth == 0) .global else .local;
    }

    /// Converts analyzer `Variable` representation to IR `Variable`
    pub fn toVar(self: *const Variable) Instruction.Variable {
        return .{ .index = @intCast(self.index), .scope = self.scope() };
    }
};

const State = struct {
    /// In a context that allow partially returning a value
    allow_partial: bool = true,
    /// In a function
    in_fn: bool = false,
    /// Current function's type
    fn_type: Type = .void,
    /// Current structure's type
    struct_type: Type = .void,
    /// Flag to tell if last statement returned from scope
    returns: bool = false,
};

pub const AnalyzerReport = GenReport(AnalyzerMsg);

pub fn init(self: *Self, allocator: Allocator, pipeline: *Pipeline, interner: *Interner, repl: bool) void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();
    self.pipeline = pipeline;
    self.type_manager = &pipeline.type_manager;

    self.errs = .{};
    self.warns = .{};
    self.instructions = .{};
    self.declarations = .{};
    self.globals = .{};
    self.locals = .{};
    self.imports = .{};
    self.scope_depth = 0;
    self.local_offset = 0;
    self.heap_count = 0;
    self.main = null;
    self.state = .{};
    self.symbols = .{};
    self.modules = .{};
    self.interner = interner;

    // TODO: cache this?
    self.empty_interned = self.interner.intern("");
    self.main_interned = self.interner.intern("main");
    self.std_interned = self.interner.intern("std");
    self.self_interned = self.interner.intern("self");
    self.init_interned = self.interner.intern("init");

    self.repl = repl;
}

/// For REPL
pub fn reinit(self: *Self) void {
    self.scope_depth = 0;
    self.local_offset = 0;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

/// Adds a new instruction and add it's `start` field and returns its index.
fn addInstr(self: *Self, instr: Instruction) usize {
    self.instructions.append(self.allocator, .{
        .tag = instr.tag,
        .data = instr.data,
    }) catch oom();

    return self.instructions.len - 1;
}

fn isNumeric(self: *Self, expr: *const Expr, t: Type) Error!void {
    if (t != .int and t != .float) return self.err(
        AnalyzerMsg.invalidArithmetic(self.getTypeName(t)),
        self.ast.getSpan(expr),
    );
}

fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    self.errs.append(self.allocator, AnalyzerReport.err(kind, span)) catch oom();
    return error.Err;
}

fn warn(self: *Self, kind: AnalyzerMsg, span: Span) void {
    self.warns.append(self.allocator, AnalyzerReport.warn(kind, span)) catch oom();
}

fn addSymbol(self: *Self, name: usize, typ: Type) void {
    self.symbols.put(self.allocator, name, .{ .index = self.symbols.count(), .type = typ }) catch oom();
}

pub fn analyze(self: *Self, ast: *const Ast) void {
    self.ast = ast;

    for (ast.nodes) |*node| {
        const node_type = self.analyzeNode(node) catch |e| {
            switch (e) {
                error.too_many_types => self.err(.too_many_types, self.ast.getSpan(node)) catch continue,
                error.Err => continue,
            }
        };

        if (node_type != .void) {
            self.err(.unused_value, self.ast.getSpan(node)) catch {};
        }
    }

    // In REPL mode, no need for main function
    if (self.repl)
        return
    else if (self.main == null) self.err(.no_main, .{ .start = 0, .end = 0 }) catch {};
}

fn analyzeNode(self: *Self, node: *const Node) Error!Type {
    if (self.scope_depth == 0 and !self.repl and !self.isPure(node.*)) {
        // TODO: add block, not allowed to have local scopes in global scope
        return if (std.meta.activeTag(node.*) == .expr and std.meta.activeTag(node.expr.*) == .@"return")
            self.err(.return_outside_fn, self.ast.getSpan(node))
        else
            self.err(.unpure_in_global, self.ast.getSpan(node));
    }

    switch (node.*) {
        .assignment => |*n| try self.assignment(n),
        .discard => |n| try self.discard(n),
        .fn_decl => |*n| _ = try self.fnDeclaration(n),
        .multi_var_decl => |*n| try self.multiVarDecl(n),
        .print => |n| try self.print(n),
        .struct_decl => |*n| try self.structDecl(n),
        .use => |*n| try self.use(n),
        .var_decl => |*n| try self.varDeclaration(n),
        .@"while" => |*n| try self.whileStmt(n),
        .expr => |n| return self.analyzeExpr(n),
    }

    return .void;
}

fn assignment(self: *Self, node: *const Ast.Assignment) !void {
    const last = self.state.allow_partial;
    self.state.allow_partial = false;
    errdefer self.state.allow_partial = last;

    var cast = false;
    const idx = self.addInstr(.{ .tag = .assignment });

    const value_type = try self.analyzeExpr(node.value);

    if (value_type == .void) {
        return self.err(.void_assignment, self.ast.getSpan(node.value));
    }

    var assigne_type = switch (node.assigne.*) {
        .literal => |*e| blk: {
            if (e.tag != .identifier) return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne));

            var assigne = try self.identifier(e.idx, false);

            if (!assigne.initialized) assigne.initialized = true;

            // If we assign a bound method that match the type (checked after), update type's infos
            const extra = value_type.getExtra();

            if (extra == .bound_method) {
                assigne.typ.setExtra(.bound_method);
            } else if (extra == .imported) {
                assigne.typ.setExtra(.imported);
            }

            // TODO: later, when function's parameters will maybe be a reference, allow it
            if (assigne.kind != .variable) return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne));

            break :blk assigne.typ;
        },
        .field => |*e| (try self.field(e)).field,
        else => return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne)),
    };

    // If type is unknown, we update it
    if (assigne_type == .void) {
        assigne_type = value_type;
    } else if (assigne_type != value_type and !self.checkEqualFnType(assigne_type, value_type)) {
        // One case in wich we can coerce; int -> float
        if (assigne_type == .float and value_type == .int) {
            cast = true;
        } else return self.err(
            .{ .invalid_assign_type = .{
                .expect = self.getTypeName(assigne_type),
                .found = self.getTypeName(value_type),
            } },
            self.ast.getSpan(node.assigne),
        );
    }

    self.instructions.items(.data)[idx] = .{ .assignment = .{ .cast = cast } };
    self.state.allow_partial = last;
}

fn discard(self: *Self, expr: *const Expr) !void {
    _ = self.addInstr(.{ .tag = .discard });
    const discarded = try self.analyzeExpr(expr);

    if (discarded == .void) return self.err(.void_discard, self.ast.getSpan(expr));
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl) Error!Type {
    const save_local_offset = self.local_offset;
    const save_scope_depth = self.scope_depth;
    const save_state = self.state;

    errdefer {
        self.local_offset = save_local_offset;

        if (self.scope_depth > save_scope_depth) {
            for (0..self.scope_depth - save_scope_depth) |_| {
                _ = self.endScope();
            }
        }

        self.scope_depth = save_scope_depth;
        self.state = save_state;
    }

    const name_idx = try self.checkName(node.name);

    if (self.main == null and self.scope_depth == 0 and name_idx == self.main_interned) {
        self.main = self.instructions.len;
    }

    // Check in current scope
    const fn_idx = self.addInstr(.{ .tag = .fn_decl, .data = undefined });
    // We add function's name for runtime access
    _ = self.addInstr(.{ .tag = .name, .data = .{ .id = name_idx } });

    // We declare before body for recursion
    const type_idx = try self.type_manager.reserveInfo();
    const fn_type: Type = .create(.func, .none, type_idx);
    const fn_var = try self.declareVariable(name_idx, fn_type, true, self.instructions.len, .func, node.name);
    _ = self.addInstr(.{ .tag = .var_decl, .data = .{ .var_decl = .{ .variable = fn_var, .cast = false } } });

    if (self.scope_depth == 0) {
        self.addSymbol(name_idx, fn_type);
    }

    self.scope_depth += 1;
    self.local_offset = self.locals.items.len;

    // We put back allow partial at the beginning of the function in case we are the last statement of an enclosing
    // function, which has the effect of putting allow_partial to false
    self.state.in_fn = true;
    self.state.allow_partial = true;

    // We reserve slot 0 for potential 'self'
    _ = try self.declareVariable(self.empty_interned, self.state.struct_type, true, self.instructions.len, .param, 0);

    var params_type: ArrayListUnmanaged(Type) = .{};
    params_type.ensureTotalCapacity(self.allocator, node.params.len) catch oom();

    for (node.params) |p| {
        const decl = self.instructions.len;
        const param_idx = self.checkName(p.name) catch |e| {
            self.errs.items[self.errs.items.len - 1] = AnalyzerReport.err(.{ .duplicate_param = .{ .name = self.ast.toSource(p.name) } }, self.ast.getSpan(p.name));
            return e;
        };

        const param_type = if (param_idx == self.self_interned) blk: {
            if (self.state.struct_type == .void) return self.err(.self_outside_struct, self.ast.getSpan(p.name));
            if (name_idx == self.init_interned) return self.err(.self_in_init, self.ast.getSpan(p.name));

            self.locals.items[self.locals.items.len - 1].name = self.self_interned;
            break :blk self.state.struct_type;
        } else blk: {
            const param_type = try self.checkAndGetType(p.typ);
            if (param_type == .void) return self.err(.void_param, self.ast.getSpan(p.name));

            _ = try self.declareVariable(param_idx, param_type, true, decl, .param, p.name);
            break :blk param_type;
        };
        params_type.appendAssumeCapacity(param_type);
    }

    const return_type = try self.checkAndGetType(node.return_type);

    if (name_idx == self.init_interned and return_type != .self) {
        return self.err(.non_self_init_return, if (node.return_type) |t|
            self.ast.getSpan(t)
        else
            self.ast.getSpan(node.name));
    }

    self.state.fn_type = return_type;

    self.type_manager.setInfo(type_idx, .{
        .func = .{
            .params = params_type.toOwnedSlice(self.allocator) catch oom(),
            .return_type = return_type,
        },
    });

    // ------
    //  Body
    // ------
    // We don't use block because we don't want to emit extra data from the block
    self.scope_depth += 1;
    var body_err = false;
    var body_type: Type = .void;
    var deadcode_start: usize = 0;
    var deadcode_count: usize = 0;
    const len = node.body.nodes.len;

    for (node.body.nodes, 0..) |*n, i| {
        // If previous statement returned, it's only dead code now
        if (deadcode_start == 0 and self.state.returns) {
            self.warn(.dead_code, self.ast.getSpan(n));
            deadcode_start = self.instructions.len;
            deadcode_count = len - i;
        }

        // If last statement, we don't allow partial anymore (for return)
        // Usefull for 'if' for example, in this case we want all the branches
        // to return something
        if (i == len - 1) self.state.allow_partial = false;

        // We try to analyze the whole body
        const typ = self.analyzeNode(n) catch |e| switch (e) {
            error.Err => {
                body_err = true;
                continue;
            },
            else => return e,
        };

        // If we analyze dead code, we don't update the type
        if (deadcode_start == 0) body_type = typ;

        // If last expression produced a value and that it wasn't the last one and it
        // wasn't a return, error
        if (body_type != .void and i != len - 1 and !self.state.returns) {
            self.err(.unused_value, self.ast.getSpan(n)) catch {};
        }
    }

    if (!body_err and body_type != return_type and !self.checkEqualFnType(body_type, return_type)) {
        return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(return_type),
                .found = self.getTypeName(body_type),
            } },
            if (node.body.nodes.len > 0)
                self.ast.getSpan(node.body.nodes[node.body.nodes.len - 1])
            else
                self.ast.getSpan(node.name),
        );
    }

    // We strip unused instructions for them not to be compiled
    if (deadcode_start > 0)
        self.instructions.shrinkRetainingCapacity(deadcode_start);

    // Two levels: 1 for function's name + params and another one for body
    _ = self.endScope();
    _ = self.endScope();

    // Switch back to locals before function call
    self.local_offset = save_local_offset;

    const return_kind: ReturnKind = if (self.state.returns)
        .explicit
    else if (body_type == .void)
        .implicit_void
    else
        .implicit_value;

    self.state = save_state;

    self.instructions.items(.data)[fn_idx] = .{ .fn_decl = .{
        .body_len = len - deadcode_count,
        .return_kind = return_kind,
    } };

    return fn_type;
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl) !void {
    _ = self.addInstr(.{ .tag = .multiple_var_decl, .data = .{ .id = node.decls.len } });

    for (node.decls) |*n| {
        try self.varDeclaration(n);
    }
}

fn print(self: *Self, expr: *const Expr) !void {
    _ = self.addInstr(.{ .tag = .print });
    const typ = try self.analyzeExpr(expr);

    if (typ == .void)
        return self.err(.void_print, self.ast.getSpan(expr));
}

fn structDecl(self: *Self, node: *const Ast.StructDecl) !void {
    const name = self.interner.intern(self.ast.toSource(node.name));

    if (self.type_manager.isDeclared(name))
        return self.err(.{ .already_declared_struct = .{ .name = self.ast.toSource(node.name) } }, self.ast.getSpan(node));

    // We forward declare for self referencing
    const type_idx = try self.type_manager.reserveInfo();
    const struct_type: Type = .create(.@"struct", .none, type_idx);
    self.state.struct_type = struct_type;
    defer self.state.struct_type = .void;
    errdefer self.state.struct_type = .void;
    const struct_var = try self.declareVariable(name, struct_type, true, self.instructions.len, .@"struct", node.name);

    const struct_idx = self.addInstr(.{ .tag = .struct_decl });
    // We add function's name for runtime access
    _ = self.addInstr(.{ .tag = .name, .data = .{ .id = name } });
    _ = self.addInstr(.{ .tag = .var_decl, .data = .{ .var_decl = .{ .variable = struct_var, .cast = false } } });

    self.scope_depth += 1;
    errdefer _ = self.endScope();

    var infos: TypeSys.StructInfo = .{ .functions = .{}, .fields = .{}, .default_value_fields = 0 };

    infos.fields.ensureTotalCapacity(self.allocator, @intCast(node.fields.len)) catch oom();

    for (node.fields, 0..) |*f, i| {
        var field_infos: TypeSys.MemberInfo = .{ .type = undefined, .index = i };
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (infos.fields.get(field_name) != null) {
            return self.err(
                .{ .already_declared_field = .{ .name = self.ast.toSource(f.name) } },
                self.ast.getSpan(f.name),
            );
        }

        const field_type = try self.checkAndGetType(f.typ);

        const field_value_type = if (f.value) |value| blk: {
            if (!self.isPure(value.*)) {
                return self.err(.unpure_field_default, self.ast.getSpan(value));
            }

            field_infos.default = true;
            infos.default_value_fields += 1;

            _ = self.addInstr(.{ .tag = .member, .data = .{ .member = .{ .index = i, .kind = .field } } });
            break :blk try self.analyzeExpr(value);
        } else .void;

        if (field_value_type != .void and field_type != .void and field_value_type != field_type) {
            return self.err(
                .{ .default_value_type_mismatch = .{
                    .expect = self.getTypeName(field_type),
                    .found = self.getTypeName(field_value_type),
                } },
                self.ast.getSpan(f.name),
            );
        }

        // From parsing, we know that there is either a type or default value. If no declared type, we take
        // the one from the default value
        field_infos.type = if (field_type == .void) field_value_type else field_type;
        infos.fields.putAssumeCapacity(field_name, field_infos);
    }

    // Create type before functions to allow 'self' to refer to the structure
    var type_info: TypeInfo = .{ .@"struct" = infos };
    self.type_manager.setInfo(type_idx, type_info);
    self.type_manager.addType(name, struct_type);

    infos.functions.ensureTotalCapacity(self.allocator, @intCast(node.functions.len)) catch oom();

    for (node.functions, 0..) |*f, i| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        const fn_type = try self.fnDeclaration(f);
        infos.functions.putAssumeCapacity(fn_name, .{ .index = i, .type = fn_type });

        // Update type with each new method
        type_info = .{ .@"struct" = infos };
        self.type_manager.setInfo(type_idx, type_info);
        self.type_manager.addType(name, struct_type);
    }

    _ = self.endScope();
    self.addSymbol(name, struct_type);

    self.instructions.items(.data)[struct_idx] = .{ .struct_decl = .{
        .fields_count = node.fields.len,
        .default_fields = infos.default_value_fields,
        .func_count = node.functions.len,
    } };
}

fn use(self: *Self, node: *const Ast.Use) !void {
    const first_name = self.interner.intern(self.ast.toSource(node.names[0]));

    // For now, "std" is interned at initialization in slot 1
    if (first_name == self.std_interned) {
        const idx = self.addInstr(.{ .tag = .use });
        // TODO: For now, il keeps synchronized the different arrays of
        // nodes/instructions
        _ = self.addInstr(.{ .tag = .null, .data = undefined });

        // TODO: support real imports
        if (node.names.len > 2) @panic("Use statements can't import more than std + one module");

        // 1 less because we parsed "std"
        for (node.names[1..]) |n| {
            if (self.type_manager.importNative(self.ast.toSource(n))) |*module| {
                const all_fn_names = module.keys();

                for (all_fn_names) |fn_name| {
                    const name_idx = self.interner.intern(fn_name);

                    // TODO: Error handling
                    const func = module.get(fn_name).?;

                    const info: TypeInfo = .{
                        .func = .{
                            // TODO: One side is fixed size, not the other one. Delete allocation?
                            .params = self.allocator.dupe(Type, func.params[0..func.arity]) catch oom(),
                            .return_type = func.return_type,
                            .tag = .builtin,
                        },
                    };

                    // Declare the type and additional informations
                    const typ = try self.type_manager.declare(name_idx, .func, .builtin, info);
                    // Declare the variable
                    const variable = try self.declareVariable(name_idx, typ, true, self.instructions.len, .func, n);

                    _ = self.addInstr(.{ .tag = .imported, .data = .{ .imported = .{
                        .index = func.index,
                        .variable = variable,
                    } } });
                }

                self.instructions.items(.data)[idx] = .{ .use = all_fn_names.len };

                return;
            } else return self.err(
                .{ .unknown_module = .{ .name = self.ast.toSource(n) } },
                self.ast.getSpan(n),
            );
        }
    } else {
        // Here, we try to resolve imports like: use math or use math.matrix
        // We import the file as a structure
        //
        // Later, support special items imported like: use math.math{ vec2, vec3 } or use math.*
        // TODO: what to do for local imports? We leave them all without discarding any of the imports
        // even the nested ones so we keep the index good
        const module = try self.importModule(node);
        const last = node.names[node.names.len - 1];
        const module_name = self.interner.intern(self.ast.toSource(last));
        const module_type: Type = .create(.module, .none, @intCast(self.modules.items.len));

        const variable = try self.declareVariable(module_name, module_type, true, self.instructions.len, .module, last);
        _ = self.addInstr(.{ .tag = .module_import, .data = .{ .module_import = .{ .index = self.modules.items.len, .scope = variable.scope } } });
        self.modules.append(self.allocator, module) catch oom();
        self.addImport(module_name, module_type);
    }
}

fn addImport(self: *Self, name: usize, typ: Type) void {
    const variable: Variable = .{
        .name = name,
        .typ = typ,
        .depth = self.scope_depth,
        .kind = .module,
        .decl = self.instructions.len,
    };

    self.imports.append(self.allocator, variable) catch oom();
}

fn importModule(self: *Self, node: *const Ast.Use) Error!Pipeline.Module {
    var cwd = std.fs.cwd();

    for (node.names, 0..) |n, i| {
        const name = self.ast.toSource(n);

        if (i == node.names.len - 1) {
            const file_name = self.allocator.alloc(u8, name.len + 3) catch oom();
            defer self.allocator.free(file_name);

            @memcpy(file_name[0..name.len], name);
            @memcpy(file_name[name.len..], ".rv");

            const file = cwd.openFile(file_name, .{}) catch {
                const owned_name = self.allocator.dupe(u8, file_name) catch oom();
                const e: AnalyzerMsg = if (node.names.len > 1)
                    .{ .missing_file_in_module = .{
                        .file = owned_name,
                        .module = self.ast.toSource(node.names[node.names.len - 2]),
                    } }
                else
                    .{ .missing_file_in_cwd = .{ .file = owned_name } };

                return self.err(e, self.ast.getSpan(n));
            };
            defer file.close();

            // The file has a new line inserted by default
            const size = file.getEndPos() catch @panic("Rover internal error: wrong import file end position");
            const buf = self.allocator.alloc(u8, size + 1) catch oom();

            _ = file.readAll(buf) catch @panic("Rover internal error: error while reading imported file");
            buf[size] = 0;

            var pipeline = self.pipeline.createSubPipeline();
            // Exit for now, just showing the error of the sub-pipeline
            const module = pipeline.run(file_name, buf[0..size :0]) catch {
                std.process.exit(0);
            };

            return module;
        } else {
            cwd = cwd.openDir(name, .{}) catch return self.err(
                .{ .unknown_module = .{ .name = self.ast.toSource(node.names[0]) } },
                self.ast.getSpan(node.names[0]),
            );
        }
    }

    unreachable;
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl) !void {
    const name = try self.checkName(node.name);
    var checked_type = try self.checkAndGetType(node.typ);
    const idx = self.addInstr(.{ .tag = .var_decl, .data = .{ .var_decl = undefined } });

    var initialized = false;
    var cast = false;

    if (node.value) |value| {
        const last = self.state.allow_partial;
        self.state.allow_partial = false;

        const value_type = try self.analyzeExpr(value);
        self.state.allow_partial = last;

        // Void assignment check
        if (value_type == .void) return self.err(.void_assignment, self.ast.getSpan(value));

        // If no type declared, we infer the value type
        if (checked_type == .void) {
            checked_type = value_type;
            // Else, we check for coherence
        } else if (checked_type != value_type and !self.checkEqualFnType(checked_type, value_type)) {
            // One case in wich we can coerce, int -> float
            if (checked_type == .float and value_type == .int) {
                cast = true;
                _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
            } else return self.err(
                .{ .invalid_assign_type = .{
                    .expect = self.getTypeName(checked_type),
                    .found = self.getTypeName(value_type),
                } },
                self.ast.getSpan(value),
            );
        }

        initialized = true;

        // If we assign a bound method that match the type (checked after), update type's infos
        const extra = value_type.getExtra();

        if (extra == .bound_method) {
            checked_type.setExtra(.bound_method);
        } else if (extra == .imported) {
            checked_type.setExtra(.imported);
        }
    } else {
        _ = self.addInstr(.{ .tag = .null });
    }

    const variable = try self.declareVariable(name, checked_type, initialized, idx, .variable, node.name);
    self.instructions.items(.data)[idx] = .{ .var_decl = .{ .variable = variable, .cast = cast } };

    if (variable.scope == .global) {
        self.addSymbol(name, checked_type);
    }
}

fn whileStmt(self: *Self, node: *const Ast.While) Error!void {
    _ = self.addInstr(.{ .tag = .@"while" });
    const cond_type = try self.analyzeExpr(node.condition);

    if (cond_type != .bool) return self.err(
        .{ .non_bool_cond = .{
            .what = "while",
            .found = self.getTypeName(cond_type),
        } },
        self.ast.getSpan(node.condition),
    );

    const body_type = try self.block(&node.body);

    if (body_type != .void) return self.err(
        .{ .non_void_while = .{
            .found = self.getTypeName(body_type),
        } },
        self.ast.getSpan(node.body),
    );
}

fn analyzeExpr(self: *Self, expr: *const Expr) Error!Type {
    return switch (expr.*) {
        .block => |*e| self.block(e),
        .binop => |*e| self.binop(e),
        .field => |*e| (try self.field(e)).field,
        .fn_call => |*e| self.call(e),
        .grouping => |*e| self.analyzeExpr(e.expr),
        .@"if" => |*e| self.ifExpr(e),
        .literal => |*e| self.literal(e),
        .@"return" => |*e| self.returnExpr(e),
        .struct_literal => |*e| self.structLiteral(e),
        .unary => |*e| self.unary(e),
    };
}

fn binop(self: *Self, expr: *const Ast.Binop) Error!Type {
    const op = self.ast.token_tags[expr.op];
    const idx = self.addInstr(.{ .tag = .binop });

    const lhs = try self.analyzeExpr(expr.lhs);
    const rhs = try self.analyzeExpr(expr.rhs);

    var res = lhs;

    // String operations
    if (op == .plus and lhs == .str and rhs == .str) {
        self.instructions.items(.data)[idx] = .{ .binop = .{ .op = .add_str } };
        return .str;
    } else if (op == .star) {
        if ((lhs == .str and rhs == .int) or (lhs == .int and rhs == .str)) {
            self.instructions.items(.data)[idx] = .{ .binop = .{
                .cast = if (rhs == .int) .rhs else .lhs,
                .op = .mul_str,
            } };

            return .str;
        }
    }

    // Error check
    switch (op) {
        .plus, .slash, .star, .minus, .greater_equal, .greater, .less_equal, .less => {
            try self.isNumeric(expr.lhs, lhs);
            try self.isNumeric(expr.rhs, rhs);
        },
        else => {},
    }

    var data: Instruction.Binop = .{ .op = undefined };

    switch (op) {
        // Arithmetic binop
        .plus, .slash, .star, .minus => {
            switch (op) {
                .plus => data.op = .add_float,
                .slash => data.op = .div_float,
                .star => data.op = .mul_float,
                .minus => data.op = .sub_float,
                else => unreachable,
            }

            switch (lhs) {
                .float => {
                    switch (rhs) {
                        .float => {},
                        .int => {
                            self.warn(
                                AnalyzerMsg.implicitCast("right hand side", self.getTypeName(lhs)),
                                self.ast.getSpan(expr.rhs),
                            );

                            data.cast = .rhs;
                        },
                        else => unreachable,
                    }
                },
                .int => {
                    switch (rhs) {
                        .float => {
                            self.warn(
                                AnalyzerMsg.implicitCast("left hand side", self.getTypeName(rhs)),
                                self.ast.getSpan(expr.lhs),
                            );

                            data.cast = .lhs;
                            res = .float;
                        },
                        .int => switch (op) {
                            .plus => data.op = .add_int,
                            .slash => data.op = .div_int,
                            .star => data.op = .mul_int,
                            .minus => data.op = .sub_int,
                            else => unreachable,
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .equal_equal, .bang_equal => {
            switch (op) {
                .equal_equal => data.op = switch (lhs) {
                    .bool => .eq_bool,
                    .float => .eq_float,
                    .int => .eq_int,
                    else => .eq_str,
                },
                .bang_equal => data.op = switch (lhs) {
                    .bool => .ne_bool,
                    .float => .ne_float,
                    .int => .ne_int,
                    else => .ne_str,
                },
                else => unreachable,
            }

            // If different value types
            if (lhs != rhs) {
                // Check for implicit casts
                if ((lhs == .int and rhs == .float) or (lhs == .float and rhs == .int)) {
                    if (lhs == .int) {
                        data.cast = .lhs;
                        self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
                    } else {
                        data.cast = .rhs;
                        self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
                    }

                    switch (op) {
                        .equal_equal => data.op = .eq_float,
                        .bang_equal => data.op = .ne_float,
                        else => unreachable,
                    }
                } else {
                    return self.err(
                        AnalyzerMsg.invalidCmp(self.getTypeName(lhs), self.getTypeName(rhs)),
                        self.ast.getSpan(expr),
                    );
                }
            } else {
                // Check for unsafe float comparisons or int comparison
                if (lhs == .float) {
                    self.warn(.float_equal, self.ast.getSpan(expr));
                }
            }

            res = .bool;
        },
        .greater_equal, .greater, .less_equal, .less => {
            switch (op) {
                .greater_equal => data.op = .ge_float,
                .greater => data.op = .gt_float,
                .less_equal => data.op = .le_float,
                .less => data.op = .lt_float,
                else => unreachable,
            }

            switch (lhs) {
                .float => {
                    switch (rhs) {
                        .float => self.warn(.float_equal, self.ast.getSpan(expr)),
                        .int => {
                            data.cast = .rhs;
                            self.warn(.float_equal_cast, self.ast.getSpan(expr.rhs));
                        },
                        else => unreachable,
                    }
                },
                .int => {
                    switch (rhs) {
                        .float => {
                            data.cast = .lhs;
                            self.warn(.float_equal_cast, self.ast.getSpan(expr.lhs));
                        },
                        .int => switch (op) {
                            .greater_equal => data.op = .ge_int,
                            .greater => data.op = .gt_int,
                            .less_equal => data.op = .le_int,
                            .less => data.op = .lt_int,
                            else => unreachable,
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }

            res = .bool;
        },
        // Logical binop
        .@"and", .@"or" => {
            if (lhs != .bool) return self.err(
                .{ .invalid_logical = .{ .found = self.getTypeName(lhs) } },
                self.ast.getSpan(expr.lhs),
            );

            if (rhs != .bool) return self.err(
                .{ .invalid_logical = .{ .found = self.getTypeName(rhs) } },
                self.ast.getSpan(expr.rhs),
            );

            switch (op) {
                .@"and" => data.op = .@"and",
                .@"or" => data.op = .@"or",
                else => unreachable,
            }
        },
        else => unreachable,
    }

    self.instructions.items(.data)[idx] = .{ .binop = data };

    return res;
}

fn block(self: *Self, expr: *const Ast.Block) Error!Type {
    self.scope_depth += 1;
    errdefer self.scope_depth -= 1;

    const idx = self.addInstr(.{ .tag = .block, .data = undefined });

    var final: Type = .void;

    for (expr.nodes, 0..) |*node, i| {
        final = try self.analyzeNode(node);

        if (final != .void and i != expr.nodes.len - 1) return self.err(.unused_value, expr.span);
    }

    const count = self.endScope();

    self.instructions.items(.data)[idx] = .{ .block = .{
        .length = expr.nodes.len,
        .pop_count = @intCast(count),
        .is_expr = if (final != .void) true else false,
    } };

    return final;
}

const StructAndFieldTypes = struct {
    structure: Type,
    field: Type,

    pub fn init(structure_type: Type, field_type: Type) StructAndFieldTypes {
        return .{ .structure = structure_type, .field = field_type };
    }
};

fn getStructAndFieldTypes(self: *Self, expr: *const Expr) Error!StructAndFieldTypes {
    return switch (expr.*) {
        .field => |*e| try self.field(e),
        else => .init(.void, try self.analyzeExpr(expr)),
    };
}

fn field(self: *Self, expr: *const Ast.Field) Error!StructAndFieldTypes {
    const idx = self.addInstr(.{ .tag = .member });
    const field_name = self.interner.intern(self.ast.toSource(expr.field));

    const struct_type = try self.analyzeExpr(expr.structure);
    const struct_value = struct_type.getValue();

    var found_field, const kind: Instruction.Member.Kind = switch (struct_type.getKind()) {
        .@"struct" => blk: {
            const infos = self.type_manager.type_infos.items[struct_value].@"struct";

            break :blk if (infos.fields.get(field_name)) |f|
                .{ f, .field }
            else if (infos.functions.get(field_name)) |f|
                .{ f, .bound_method }
            else
                return self.err(
                    .{ .undeclared_field_access = .{ .name = self.ast.toSource(expr.field) } },
                    self.ast.getSpan(expr.field),
                );
        },
        .module => blk: {
            const module = &self.modules.items[struct_value];
            var symbol = module.symbols.get(field_name) orelse {
                // TODO: error
                @panic("Module doesn't have declaration");
            };

            // TODO:
            if (!symbol.type.is(.func)) {
                @panic("support only function member access on imports for now");
            }

            // TODO: for now we copy it as the type manager is shared accross all sub-pipelines. We want to
            // set the `module` field only for this analyzer pass

            // Here, we get the symbol we are referencing, then we create a copy. We give it the correct
            // import index (setModule) so that we can retreive its declaration later in the self.imports
            // list. As we updated the 'module' index field, we add a new type in the type manager and
            // create a type based on its index. We update the symbol's type to refer to the new one
            var new_infos = self.type_manager.type_infos.items[symbol.type.getValue()];
            new_infos.setModule(struct_value);

            const new_idx = try self.type_manager.reserveInfo();
            self.type_manager.setInfo(new_idx, new_infos);
            const new_type = Type.create(.func, .imported, new_idx);
            symbol.type = new_type;

            break :blk .{ symbol, .symbol };
        },
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.getTypeName(struct_type) } },
            self.ast.getSpan(expr.structure),
        ),
    };

    self.instructions.items(.data)[idx] = .{ .member = .{ .index = found_field.index, .kind = kind } };

    if (kind == .bound_method)
        found_field.type.setExtra(.bound_method);

    return .init(struct_type, found_field.type);
}

fn call(self: *Self, expr: *const Ast.FnCall) Error!Type {
    const idx = self.addInstr(.{ .tag = .call, .data = .{ .call = .{
        .arity = @intCast(expr.args.len),
    } } });

    const sft = try self.getStructAndFieldTypes(expr.callee);
    const extra = sft.field.getExtra();

    const infos = if (sft.field.is(.func))
        self.type_manager.type_infos.items[sft.field.getValue()].func
    else if (sft.field.is(.@"struct")) blk: {
        const val = sft.field.getValue();
        const struct_infos = self.type_manager.type_infos.items[val].@"struct";

        if (struct_infos.functions.get(self.init_interned)) |init_fn| {
            const type_idx = init_fn.type.getValue();
            break :blk self.type_manager.type_infos.items[type_idx].func;
        } else {
            return self.err(.struct_call_but_no_init, self.ast.getSpan(expr));
        }
    } else return self.err(.invalid_call_target, self.ast.getSpan(expr));

    var call_tag: Instruction.Call.CallTag = switch (infos.tag) {
        .builtin => .builtin,
        .function => .function,
    };

    // Not just a call to an already bounded method. Two cases here:
    // 1: var bounded = p.method(); bounded()  --> here instance_type is void
    // 2: p.method()                           --> here instance_type isn't
    if (extra == .imported) {
        call_tag = if (sft.structure.getKind() == .module) .invoke_import else .import;
    } else if (extra == .bound_method) {
        if (sft.structure != .void) {
            call_tag = .invoke;

            if (infos.params.len == 0 or infos.params[0] != sft.structure) return self.err(
                .{ .missing_self_method_call = .{ .name = self.ast.toSource(expr.callee.field.field) } },
                self.ast.getSpan(expr.callee.field.field),
            );
        } else call_tag = .bound;
    }

    b: {
        if (infos.params.len == expr.args.len) {
            break :b;
        }

        const args_diff = @max(infos.params.len, expr.args.len) - @min(infos.params.len, expr.args.len);

        // For bounded method, the difference of 1 is the invisible 'self' parameter
        if (args_diff == 1 and extra == .bound_method) {
            break :b;
        }

        return self.err(
            AnalyzerMsg.wrongArgsCount(infos.params.len, expr.args.len),
            self.ast.getSpan(expr),
        );
    }

    try self.fnCall(expr.args, infos, call_tag == .bound or call_tag == .invoke);

    // To load the module's global before call. If it's an invoke_import, we know that the
    // module is already just behind
    if (call_tag == .import) {
        self.makeVariableInstr(&self.imports.items[infos.module]);
    }
    self.instructions.items(.data)[idx].call.tag = call_tag;

    return infos.return_type;
}

fn fnCall(self: *Self, args: []*Expr, infos: TypeSys.FnInfo, is_bound: bool) Error!void {
    const params = if (is_bound) infos.params[1..] else infos.params;

    for (args, 0..) |arg, i| {
        const arg_type = try self.analyzeExpr(arg);

        if (arg_type != params[i] and !self.checkEqualFnType(arg_type, params[i])) {
            if (params[i] == .float and arg_type == .int) {
                _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
            } else {
                return self.err(
                    .{ .type_mismatch = .{
                        .expect = self.getTypeName(params[i]),
                        .found = self.getTypeName(arg_type),
                    } },
                    self.ast.getSpan(arg),
                );
            }
        }
    }
}

fn literal(self: *Self, expr: *const Ast.Literal) Error!Type {
    const text = self.ast.toSource(expr);

    switch (expr.tag) {
        .bool => {
            _ = self.addInstr(.{ .tag = .bool, .data = .{
                .bool = if (self.ast.token_tags[expr.idx] == .true) true else false,
            } });

            return .bool;
        },
        .identifier, .self => {
            const variable = try self.identifier(expr.idx, true);

            return variable.typ;
        },
        .int => {
            const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                // TODO: error handling, only one possible it's invalid char
                std.debug.print("Error parsing integer\n", .{});
                break :blk 0;
            };
            _ = self.addInstr(.{ .tag = .int, .data = .{ .int = value } });

            return .int;
        },
        .float => {
            const value = std.fmt.parseFloat(f64, text) catch blk: {
                // TODO: error handling, only one possible it's invalid char or too big
                std.debug.print("Error parsing float\n", .{});
                break :blk 0.0;
            };
            _ = self.addInstr(.{ .tag = .float, .data = .{ .float = value } });

            return .float;
        },
        .null => {
            _ = self.addInstr(.{ .tag = .null });

            return .null;
        },
        .string => {
            // Removes the quotes
            const value = self.interner.intern(text[1 .. text.len - 1]);
            _ = self.addInstr(.{ .tag = .string, .data = .{ .id = value } });

            return .str;
        },
    }
}

fn identifier(self: *Self, name: Ast.TokenIndex, initialized: bool) Error!*Variable {
    const variable = try self.resolveIdentifier(name, initialized);
    self.makeVariableInstr(variable);

    return variable;
}

/// Checks if a variable is in local scope, enclosing scope or global scope. Check if its state
/// is `initialized`, otherwise return an error.
fn resolveIdentifier(self: *Self, name: Ast.TokenIndex, initialized: bool) Error!*Variable {
    const text = self.ast.toSource(name);
    const name_idx = self.interner.intern(text);

    if (self.locals.items.len > 0) {
        var idx = self.locals.items.len;

        while (idx > 0) : (idx -= 1) {
            const local = &self.locals.items[idx - 1];

            if (name_idx == local.name) {
                if (initialized and !local.initialized) {
                    return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(name));
                }

                return local;
            }
        }
    }

    // TODO: in reverse? People tend to use latest declared variables
    for (self.globals.items) |*glob| {
        if (name_idx == glob.name) {
            if (initialized and !glob.initialized) {
                return self.err(.{ .use_uninit_var = .{ .name = text } }, self.ast.getSpan(name));
            }

            return glob;
        }
    }

    return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(name));
}

/// Generates the instruction to get the current variable
fn makeVariableInstr(self: *Self, variable: *Variable) void {
    if (variable.kind == .variable) {
        self.checkCapture(variable);
        _ = self.addInstr(.{ .tag = .identifier_id, .data = .{ .id = variable.decl } });
    } else {
        // Params and imports aren't declared so we can't reference them, they just live on stack
        // TODO: scope can't be 'heap'? Just use variable.scope()? Create a to() method?
        _ = self.addInstr(
            .{ .tag = .identifier, .data = .{ .variable = .{
                .index = @intCast(variable.index),
                .scope = if (variable.depth > 0) .local else .global,
            } } },
        );
    }
}

/// Check if the variable needs to be captured and captures it if so
fn checkCapture(self: *Self, variable: *Variable) void {
    // If it's a global variable or if it's been declared in current function's frame
    // or already captured, return
    if (variable.index >= self.local_offset or variable.depth == 0 or variable.captured) return;

    variable.captured = true;
    variable.index = self.heap_count;
    self.instructions.items(.data)[variable.decl].var_decl.variable.scope = .heap;
    // TODO: protect the cast?
    self.instructions.items(.data)[variable.decl].var_decl.variable.index = @intCast(self.heap_count);
    self.heap_count += 1;
}

fn ifExpr(self: *Self, expr: *const Ast.If) Error!Type {
    const idx = self.addInstr(.{ .tag = .@"if", .data = undefined });
    var data: Instruction.If = .{ .cast = .none, .has_else = false };

    const cond_type = try self.analyzeExpr(expr.condition);

    // We can continue to analyze if the condition isn't a bool
    if (cond_type != .bool) self.err(
        .{ .non_bool_cond = .{
            .what = "if",
            .found = self.getTypeName(cond_type),
        } },
        self.ast.getSpan(expr.condition),
    ) catch {};

    var then_return: bool = false;
    var else_return: bool = false;

    const then_type = try self.analyzeNode(&expr.then);
    var final_type = then_type;

    // If we hit a return, we transfert it first to the then branch
    if (self.state.returns) {
        // Reset return  for else branch
        self.state.returns = false;
        then_return = true;
        // As we exit scope, we don't return any type
        final_type = .void;
    }

    var else_type: Type = .void;

    if (expr.@"else") |*n| {
        data.has_else = true;
        else_type = try self.analyzeNode(n);

        // If it returns
        if (self.state.returns) {
            else_return = true;
            // If not then, unmark as globally returning from scope
            if (!then_return) self.state.returns = false;
        } else if (then_return) {
            // If else only then branch returns, final_type becomes else branch
            final_type = else_type;
        }

        // Type coherence. If branches don't exit scope and branches have
        // diffrent types
        if (!then_return and !else_return and then_type != else_type) {
            if (then_type == .int and else_type == .float) {
                data.cast = .then;

                self.warn(
                    AnalyzerMsg.implicitCast("then branch", "float"),
                    self.ast.getSpan(expr.then),
                );
            } else if (then_type == .float and else_type == .int) {
                data.cast = .@"else";

                // Safe unsafe access, if there is a non void type
                // there is an else body
                self.warn(
                    AnalyzerMsg.implicitCast("else branch", "float"),
                    self.ast.getSpan(expr.@"else".?),
                );
            } else return self.err(
                .{ .incompatible_if_type = .{
                    .found1 = self.getTypeName(then_type),
                    .found2 = self.getTypeName(else_type),
                } },
                self.ast.getSpan(expr),
            );
        }
    } else if (then_type != .void and !self.state.allow_partial) {
        return self.err(
            .{ .missing_else_clause = .{ .if_type = self.getTypeName(then_type) } },
            self.ast.getSpan(expr),
        );
    }

    self.instructions.items(.data)[idx] = .{ .@"if" = data };

    return final_type;
}

fn returnExpr(self: *Self, expr: *const Ast.Return) Error!Type {
    const idx = self.addInstr(.{ .tag = .@"return", .data = .{ .@"return" = .{
        .value = false,
        .cast = false,
    } } });

    const return_type = if (expr.expr) |e| blk: {
        self.instructions.items(.data)[idx].@"return".value = true;
        break :blk try self.analyzeExpr(e);
    } else .void;

    // We check after to advance node idx
    if (!self.state.in_fn) return self.err(.return_outside_fn, self.ast.getSpan(expr));

    if (!self.checkEqualFnType(self.state.fn_type, return_type)) {
        if (self.state.fn_type == .float and return_type == .int) {
            self.instructions.items(.data)[idx].@"return".cast = true;
            _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
        } else return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(self.state.fn_type),
                .found = self.getTypeName(return_type),
            } },
            self.ast.getSpan(expr),
        );
    }

    self.state.returns = true;
    return self.state.fn_type;
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral) !Type {
    const decl = try self.resolveIdentifier(expr.name, true);

    const struct_lit_idx = self.addInstr(.{
        .tag = .struct_literal,
        .data = .{ .struct_literal = .{ .variable = decl.toVar(), .arity = expr.fields.len, .end = 0 } },
    });

    if (self.type_manager.declared.get(decl.name)) |struct_type| {
        const arity = expr.fields.len;
        const value = struct_type.getValue();
        const infos = self.type_manager.type_infos.items[value].@"struct";
        var proto = infos.proto(self.allocator);
        defer proto.deinit(self.allocator);

        const start = self.instructions.len;
        self.instructions.ensureTotalCapacity(self.allocator, self.instructions.len + arity) catch oom();

        for (0..arity) |_| {
            self.instructions.appendAssumeCapacity(.{ .tag = .member });
        }

        for (expr.fields) |*fv| {
            const field_name = self.interner.intern(self.ast.toSource(fv.name));

            if (infos.fields.get(field_name)) |f| {
                proto.putAssumeCapacity(field_name, true);
                self.instructions.items(.data)[start + f.index] = .{ .member = .{
                    .index = self.instructions.len,
                    .kind = .field,
                } };

                if (fv.value) |val| {
                    _ = try self.analyzeExpr(val);
                } else {
                    // Syntax: { x } instead of { x = x }
                    _ = try self.identifier(fv.name, true);
                }
            } else return self.err(
                .{ .unknown_struct_field = .{ .name = self.ast.toSource(fv.name) } },
                self.ast.getSpan(fv.name),
            );
        }

        if (arity != proto.size) {
            var kv = proto.iterator();
            while (kv.next()) |entry| {
                if (!entry.value_ptr.*) self.err(
                    .{ .missing_field_struct_literal = .{ .name = self.interner.getKey(entry.key_ptr.*).? } },
                    self.ast.getSpan(expr.name),
                ) catch {};
            }

            return error.Err;
        }

        // As the compiler is gonna jump around to compile in the correct order, we need a way
        // to know where to go in the list at the end to continue compiling as normal
        self.instructions.items(.data)[struct_lit_idx].struct_literal.end = self.instructions.len;

        return decl.typ;
    } else return self.err(.non_struct_struct_literal, self.ast.getSpan(expr.name));
}

fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
    const op = self.ast.token_tags[expr.op];
    const idx = self.addInstr(.{
        .tag = .unary,
        .data = .{ .unary = .{
            .op = if (op == .not) .bang else .minus,
            .typ = .float,
        } },
    });

    const rhs = try self.analyzeExpr(expr.expr);

    if (op == .not and rhs != .bool) {
        return self.err(
            .{ .invalid_unary = .{ .found = self.getTypeName(rhs) } },
            self.ast.getSpan(expr),
        );
    } else if (op == .minus and rhs != .int and rhs != .float) {
        return self.err(
            AnalyzerMsg.invalidArithmetic(self.getTypeName(rhs)),
            self.ast.getSpan(expr),
        );
    }

    if (rhs == .int) self.instructions.items(.data)[idx].unary.typ = .int;

    return rhs;
}

/// Unincrement scope depth, discards all locals and return the number
/// of discarded locals
fn endScope(self: *Self) usize {
    self.scope_depth -= 1;

    var pop_count: usize = 0;
    // Discards all the local variables
    if (self.locals.items.len > 0) {
        var i = self.locals.items.len;

        while (i > 0 and self.locals.items[i - 1].depth > self.scope_depth) {
            i -= 1;
        }

        pop_count = self.locals.items.len - i;
        self.locals.resize(self.allocator, i) catch oom();
    }

    return pop_count;
}

/// Checks if the variable name is in local or global scope
fn identInScope(self: *const Self, name: usize) bool {
    if (self.scope_depth > 0) {
        if (self.locals.items.len == 0) return false;

        var idx = self.locals.items.len;

        while (idx > 0) : (idx -= 1) {
            const local = self.locals.items[idx - 1];

            if (local.depth < self.scope_depth) break;

            // First condition might fail first avoiding string comparison
            if (name == local.name) return true;
        }
    } else {
        if (self.globals.items.len == 0) return false;

        // TODO: reverse order, user tend to used recently declared variables
        for (self.globals.items) |*glob| {
            if (name == glob.name) return true;
        }
    }

    return false;
}

/// Checks if identifier name is already declared, otherwise interns it and returns the key
fn checkName(self: *Self, token: usize) !usize {
    const name = self.interner.intern(self.ast.toSource(token));

    if (self.identInScope(name)) return self.err(
        .{ .already_declared = .{ .name = self.interner.getKey(name).? } },
        self.ast.getSpan(token),
    );

    return name;
}

/// Checks that the node is a declared type and return it's value. If node is
/// `empty`, returns `void`
fn checkAndGetType(self: *Self, typ: ?*const Ast.Type) Error!Type {
    return if (typ) |t| return switch (t.*) {
        .scalar => self.type_manager.declared.get(
            self.interner.intern(self.ast.toSource(t)),
        ) orelse return self.err(
            .{ .undeclared_type = .{ .found = self.ast.toSource(t) } },
            self.ast.getSpan(t),
        ),
        .function => |*fn_type| self.createAnonymousFnType(fn_type),
        .self => .self,
    } else .void;
}

/// Creates a type for an anonymous function, like the one defined in return types
/// of functions
fn createAnonymousFnType(self: *Self, fn_type: *const Ast.Type.Fn) Error!Type {
    const type_idx = try self.type_manager.reserveInfo();
    var params_type: ArrayListUnmanaged(Type) = .{};
    params_type.ensureTotalCapacity(self.allocator, fn_type.params.len) catch oom();

    for (fn_type.params) |p| {
        const param_type = try self.checkAndGetType(p);

        if (param_type == .void) {
            return self.err(.void_param, fn_type.span);
        }

        params_type.appendAssumeCapacity(param_type);
    }

    // Set all the informations now that we have every thing
    self.type_manager.setInfo(type_idx, .{
        .func = .{
            .params = params_type.toOwnedSlice(self.allocator) catch oom(),
            .return_type = try self.checkAndGetType(fn_type.return_type),
        },
    });

    return .create(.func, .none, type_idx);
}

/// Declares a variable either in globals or in locals based on current scope depth
fn declareVariable(
    self: *Self,
    name: usize,
    typ: Type,
    initialized: bool,
    decl_idx: usize,
    kind: Variable.Tag,
    token: Ast.TokenIndex,
) !Instruction.Variable {
    // We put the current number of instruction for the declaration index
    var variable: Variable = .{
        .name = name,
        .typ = typ,
        .decl = decl_idx,
        .depth = self.scope_depth,
        .initialized = initialized,
        .kind = kind,
    };

    // Add the variable to the correct data structure
    if (self.scope_depth == 0) {
        const index = self.globals.items.len;
        variable.index = index;

        self.globals.append(self.allocator, variable) catch oom();
        return .{ .index = @intCast(index), .scope = .global };
    } else {
        // Take function's frame into account
        const index = self.locals.items.len - self.local_offset;
        variable.index = index;

        if (index > 255) {
            return self.err(.too_many_locals, self.ast.getSpan(token));
        }

        self.locals.append(self.allocator, variable) catch oom();
        return .{ .index = @intCast(index), .scope = .local };
    }
}

fn isPure(self: *const Self, node: anytype) bool {
    return switch (@TypeOf(node)) {
        Ast.Node => switch (node) {
            inline else => |*e| self.isPure(e.*),
        },
        Ast.FnDecl, Ast.StructDecl, Ast.Use => true,
        Ast.VarDecl => return if (node.value) |val| self.isPure(val.*) else true,
        Ast.Expr => return switch (node) {
            inline else => |*sub| return self.isPure(sub.*),
        },
        Ast.Literal => return node.tag != .identifier,
        Ast.Grouping => self.isPure(node.expr.*),
        Ast.Unary => self.isPure(node.expr.*),
        Ast.Binop => self.isPure(node.lhs.*) and self.isPure(node.rhs.*),
        Ast.Assignment => self.isPure(node.value.*),
        else => false,
    };
}

/// Checks if an expression if of a certain type kind and returns the associated value or error
fn expect_type_kind(self: *Self, node: Node.Index, kind: TypeSys.Kind) !TypeSys.Value {
    const expr_type = try self.analyze_node(node);

    return if (expr_type.is(kind))
        expr_type.getValue()
    else
        self.err(
            .{ .type_mismatch = .{
                .expect = kind.toStr(),
                .found = expr_type.getKind().toStr(),
            } },
            self.to_span(node),
        );
}

/// Checks if two function types are equal. Functions' type depend on the index
/// where their infos are in the type manager, so they could be the same type
/// but with different indices. This is due to anonymus function type (like return
/// types in function definitions)
fn checkEqualFnType(self: *const Self, t1: Type, t2: Type) bool {
    if (t1 == t2) return true;
    if (!t1.is(.func) or !t2.is(.func)) return false;

    const v1 = t1.getValue();
    const infos1 = self.type_manager.type_infos.items[v1].func;

    const v2 = t2.getValue();
    const infos2 = self.type_manager.type_infos.items[v2].func;

    if (infos1.params.len == infos2.params.len and infos1.return_type == infos2.return_type) {
        for (infos1.params, infos2.params) |p1, p2| {
            if (p1 != p2) return false;
        }

        return true;
    }

    return false;
}

/// Helpers used for errors
fn getTypeName(self: *const Self, typ: Type) []const u8 {
    if (typ.is(.func)) {
        return self.getFnTypeName(typ) catch oom();
    } else {
        const index = self.type_manager.idx(typ);
        return self.interner.getKey(index).?;
    }
}

/// Helpers used for errors
fn getFnTypeName(self: *const Self, typ: Type) ![]const u8 {
    const value = typ.getValue();
    const decl = self.type_manager.type_infos.items[value].func;

    var res: std.ArrayListUnmanaged(u8) = .{};
    var writer = res.writer(self.allocator);
    try writer.writeAll("fn(");

    for (decl.params, 0..) |p, i| {
        try writer.print("{s}{s}", .{
            self.getTypeName(p),
            if (i < decl.params.len - 1) ", " else "",
        });
    }
    try writer.print(") -> {s}", .{self.getTypeName(decl.return_type)});

    return try res.toOwnedSlice(self.allocator);
}
