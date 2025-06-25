const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

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
const FnInfo = TypeSys.FnInfo;
const StructInfo = TypeSys.StructInfo;
const ArrayInfo = TypeSys.ArrayInfo;

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
imports: ArrayListUnmanaged(Instruction.Variable),
scope_depth: usize,
local_offset: usize,
heap_count: usize,
main: ?usize,
state: State,
symbols: TypeSys.Symbols,
modules: AutoArrayHashMapUnmanaged(usize, Pipeline.Module),
cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },
big_self: Variable,

arena: std.heap.ArenaAllocator,
allocator: Allocator,
repl: bool,

const Self = @This();
const Error = error{Err} || TypeManager.Error;

const Variable = struct {
    /// Index in scope
    index: usize = 0,
    /// Variable's type
    typ: Type = .void,
    /// Scope depth if it's a local
    depth: usize,
    /// Name interned
    name: usize,
    /// Index of declaration instruction
    decl: usize = 0,
    /// Is initialized
    initialized: bool = false,
    /// Is captured
    captured: bool = false,
    /// Kind of variable
    kind: Kind = .variable,

    pub const Kind = enum { variable, param, decl };

    pub fn scope(self: *const Variable) rir.Scope {
        return if (self.captured) .heap else if (self.depth == 0) .global else .local;
    }

    /// Converts analyzer `Variable` representation to IR `Variable`
    pub fn toVar(self: *const Variable) Instruction.Variable {
        return .{ .index = self.index, .scope = self.scope() };
    }
};

const State = struct {
    /// In a context that allow partially returning a value
    allow_partial: bool = true,
    /// Current function's type
    fn_type: Type = .void,
    /// Indicates if we should increment reference count
    incr_ref: bool = false,
    /// In a function
    in_fn: bool = false,
    /// Flag to tell if last statement returned from scope
    returns: bool = false,
    /// Current structure's type
    struct_type: Type = .void,
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

    self.cached_names = .{
        .empty = self.interner.intern("empty"),
        .main = self.interner.intern("main"),
        .std = self.interner.intern("std"),
        .self = self.interner.intern("self"),
        .Self = self.interner.intern("Self"),
        .init = self.interner.intern("init"),
    };
    self.big_self = .{ .name = self.cached_names.Self, .kind = .decl, .depth = 0 };

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
inline fn addInstr(self: *Self, data: Instruction.Data) void {
    self.instructions.append(self.allocator, .{ .data = data }) catch oom();
}

/// Reserves an empty slot and returns its index
inline fn reserveInstr(self: *Self) usize {
    return self.instructions.addOne(self.allocator) catch oom();
}

/// Sets the instruction at the given idnex
inline fn setInstr(self: *Self, index: usize, data: Instruction.Data) void {
    self.instructions.set(index, .{ .data = data });
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
    defer self.state.allow_partial = last;

    const index = self.reserveInstr();

    self.state.incr_ref = true;
    var value_type = try self.analyzeExpr(node.value);
    self.state.incr_ref = false;

    const assigne_type, const cow = switch (node.assigne.*) {
        .literal => |*e| b: {
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

            if (assigne.kind != .variable) return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne));

            break :b .{ assigne.typ, false };
        },
        .field => |*e| b: {
            const field_type = (try self.field(e)).field;
            if (field_type.getKind() == .func) return self.err(.assign_to_method, self.ast.getSpan(e.field));

            break :b .{ field_type, false };
        },
        .array_access => |*e| .{ try self.arrayAccess(e), true },
        else => return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne)),
    };

    // var assigne_type, const cow, const assigne_type_ptr = switch (node.assigne.*) {
    //     .literal => |*e| b: {
    //         if (e.tag != .identifier) return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne));
    //
    //         var assigne = try self.identifier(e.idx, false);
    //
    //         if (!assigne.initialized) assigne.initialized = true;
    //
    //         // Protects against assigning to declarations or parameter for example
    //         if (assigne.kind != .variable) return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne));
    //
    //         break :b .{ assigne.typ, false, &assigne.typ };
    //     },
    //     .field => |*e| b: {
    //         const field_infos = try self.field(e);
    //         const field_type = field_infos.field;
    //
    //         if (field_infos.kind != .field and field_type.getKind() == .func) {
    //             return self.err(.assign_to_method, self.ast.getSpan(e.field));
    //         }
    //
    //         break :b .{ field_type, false, if (field_infos.field_ptr) |ptr| ptr else null };
    //     },
    //     .array_access => |*e| .{ try self.arrayAccess(e), true, null },
    //     else => return self.err(.invalid_assign_target, self.ast.getSpan(node.assigne)),
    // };
    //
    // const value_instr = self.instructions.len;
    // self.state.incr_ref = true;
    // var value_type = try self.analyzeExpr(node.value);
    // self.state.incr_ref = false;
    //
    // // If we fetched a variable, we add extra informations if needed
    // if (assigne_type_ptr) |ptr| {
    //     const extra = value_type.getExtra();
    //
    //     if (extra == .bound_method) {
    //         ptr.setExtra(.bound_method);
    //     } else if (extra == .imported) {
    //         ptr.setExtra(.imported);
    //     } else ptr.setExtra(.none);
    //
    //     assigne_type = ptr.*;
    // }

    const coherence = try self.performTypeCoercion(assigne_type, value_type, false, self.ast.getSpan(node.assigne));

    // self.setInstr(index, .{ .assignment = .{ .cast = coherence.cast, .cow = cow, .value_instr = value_instr } });
    self.setInstr(index, .{ .assignment = .{ .cast = coherence.cast, .cow = cow } });
}

fn discard(self: *Self, expr: *const Expr) !void {
    self.addInstr(.{ .discard = undefined });
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

    if (self.main == null and self.scope_depth == 0 and name_idx == self.cached_names.main) {
        self.main = self.instructions.len;
    }

    const fn_idx = self.reserveInstr();
    // We add function's name for runtime access
    self.addInstr(.{ .name = name_idx });

    // We declare before body for recursion
    const type_idx = try self.type_manager.reserveInfo();
    const fn_type: Type = .create(.func, .none, type_idx);
    const fn_var = try self.declareVariable(name_idx, fn_type, true, self.instructions.len, .decl, node.name);
    self.addInstr(.{ .var_decl = .{ .variable = fn_var } });

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
    _ = try self.declareVariable(self.cached_names.empty, self.state.struct_type, true, self.instructions.len, .param, 0);

    var default_params: usize = 0;
    var params_type: AutoArrayHashMapUnmanaged(usize, FnInfo.ParamInfo) = .{};
    params_type.ensureTotalCapacity(self.allocator, node.params.len) catch oom();

    for (node.params, 0..) |*p, i| {
        const decl = self.instructions.len;
        const param_idx = self.checkName(p.name) catch |e| {
            self.errs.items[self.errs.items.len - 1] = AnalyzerReport.err(
                .{ .duplicate_param = .{ .name = self.ast.toSource(p.name) } },
                self.ast.getSpan(p.name),
            );
            return e;
        };

        const param_type, const default = if (param_idx == self.cached_names.self) blk: {
            if (self.state.struct_type == .void) {
                return self.err(.self_outside_struct, self.ast.getSpan(p.name));
            }

            self.locals.items[self.locals.items.len - 1].name = self.cached_names.self;
            break :blk .{ self.state.struct_type, true };
        } else blk: {
            var param_type = try self.checkAndGetType(p.typ);

            // Default value
            const default_value = if (p.value) |value| b: {
                if (!self.isPure(value.*)) {
                    return self.err(.{ .unpure_default = .new(.param) }, self.ast.getSpan(value));
                }

                var value_type = try self.analyzeExpr(value);

                if (param_type != .void and value_type != param_type) {
                    if (self.checkCast(param_type, value_type, true)) {
                        value_type = .float;
                    } else return self.err(
                        .{ .default_value_type_mismatch = .new(self.getTypeName(param_type), self.getTypeName(value_type), .param) },
                        self.ast.getSpan(value),
                    );
                }
                // Assign in case param type was void, we infer from value
                param_type = value_type;
                default_params += 1;
                break :b true;
            } else b: {
                if (param_type == .void) return self.err(.void_param, self.ast.getSpan(p.name));
                break :b false;
            };

            _ = try self.declareVariable(param_idx, param_type, true, decl, .param, p.name);
            break :blk .{ param_type, default_value };
        };

        params_type.putAssumeCapacity(param_idx, .{ .index = i, .type = param_type, .default = default });
    }

    const return_type = try self.checkAndGetType(node.return_type);
    self.state.fn_type = return_type;
    self.type_manager.setInfo(type_idx, .{ .func = .{ .params = params_type, .return_type = return_type } });

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
        // Usefull for 'if' for example, in this case we want all the branches to return something
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

        // If last expression produced a value and that it wasn't the last one and it wasn't a return, error
        if (body_type != .void and i != len - 1 and !self.state.returns) {
            self.err(.unused_value, self.ast.getSpan(n)) catch {};
        }
    }

    if (!body_err and body_type != return_type and !self.isFunctionTypeEqual(body_type, return_type)) {
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

    if (body_type.getExtra() == .bound_method) {
        self.type_manager.type_infos.items[type_idx].func.return_type.setExtra(.bound_method);
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
    self.setInstr(fn_idx, .{ .fn_decl = .{
        .body_len = len - deadcode_count,
        .default_params = default_params,
        .return_kind = return_kind,
    } });

    return fn_type;
}

fn print(self: *Self, expr: *const Expr) !void {
    self.addInstr(.{ .print = undefined });
    const typ = try self.analyzeExpr(expr);

    if (typ == .void)
        return self.err(.void_print, self.ast.getSpan(expr));
}

fn structDecl(self: *Self, node: *const Ast.StructDecl) !void {
    const name = self.interner.intern(self.ast.toSource(node.name));

    if (self.type_manager.isDeclared(name)) {
        return self.err(.{ .already_declared_struct = .{ .name = self.ast.toSource(node.name) } }, self.ast.getSpan(node));
    }

    // We forward declare for self referencing
    const type_idx = try self.type_manager.reserveInfo();
    const struct_type: Type = .create(.@"struct", .none, type_idx);

    self.state.struct_type = struct_type;
    defer self.state.struct_type = .void;

    errdefer self.state.struct_type = .void;
    const struct_var = try self.declareVariable(name, struct_type, true, self.instructions.len, .decl, node.name);

    // self.big_self.typ = struct_type;
    self.big_self = if (struct_var.scope == .local) self.locals.items[struct_var.index] else self.globals.items[struct_var.index];
    // Enough to enable checks on wether we are in a struct or not
    defer self.big_self.typ = .void;

    const index = self.reserveInstr();
    // We add function's name for runtime access
    self.addInstr(.{ .name = name });
    self.addInstr(.{ .var_decl = .{ .variable = struct_var, .cast = false } });

    self.scope_depth += 1;
    errdefer _ = self.endScope();

    var infos: TypeSys.StructInfo = .{ .functions = .{}, .fields = .{}, .default_value_fields = 0 };

    infos.fields.ensureTotalCapacity(self.allocator, node.fields.len) catch oom();

    for (node.fields, 0..) |*f, i| {
        var field_infos: StructInfo.MemberInfo = .{ .type = undefined, .index = i };
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
                return self.err(.{ .unpure_default = .new(.field) }, self.ast.getSpan(value));
            }

            field_infos.default = true;
            infos.default_value_fields += 1;

            break :blk try self.analyzeExpr(value);
        } else .void;

        if (field_value_type != .void and field_type != .void and field_value_type != field_type) {
            if (!self.checkCast(field_type, field_value_type, true)) return self.err(
                .{ .default_value_type_mismatch = .new(self.getTypeName(field_type), self.getTypeName(field_value_type), .field) },
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

    // TODO: protect cast
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
    self.setInstr(index, .{ .struct_decl = .{
        .fields_count = node.fields.len,
        .default_fields = infos.default_value_fields,
        .func_count = node.functions.len,
    } });
}

fn use(self: *Self, node: *const Ast.Use) !void {
    const first_name = self.interner.intern(self.ast.toSource(node.names[0]));

    // For now, "std" is interned at initialization in slot 1
    if (first_name == self.cached_names.std) {
        // const index = self.reserveInstr();
        // // TODO: For now, it keeps synchronized the different arrays of
        // // nodes/instructions
        // self.addInstr(.{ .null = undefined });
        //
        // // TODO: support real imports
        // if (node.names.len > 2) @panic("Use statements can't import more than std + one module");
        //
        // // 1 less because we parsed "std"
        // for (node.names[1..]) |n| {
        //     if (self.type_manager.importNative(self.ast.toSource(n))) |*module| {
        //         const all_fn_names = module.keys();
        //
        //         for (all_fn_names) |fn_name| {
        //             const name_idx = self.interner.intern(fn_name);
        //
        //             // TODO: Error handling
        //             const func = module.get(fn_name).?;
        //
        //             const info: TypeInfo = .{
        //                 .func = .{
        //                     // TODO: One side is fixed size, not the other one. Delete allocation?
        //                     .params = func.params[0..func.arity],
        //                     .return_type = func.return_type,
        //                     .tag = .builtin,
        //                 },
        //             };
        //
        //             // Declare the type and additional informations
        //             const typ = try self.type_manager.declare(name_idx, .func, .builtin, info);
        //             // Declare the variable
        //             const variable = try self.declareVariable(name_idx, typ, true, self.instructions.len, .decl, n);
        //             self.addInstr(.{ .imported = .{ .index = func.index, .variable = variable } });
        //         }
        //
        //         self.setInstr(index, .{ .use = all_fn_names.len });
        //
        //         return;
        //     } else return self.err(
        //         .{ .unknown_module = .{ .name = self.ast.toSource(n) } },
        //         self.ast.getSpan(n),
        //     );
        // }
    } else {
        const module, const cached = try self.importModule(node);
        const token = if (node.alias) |alias| alias else node.names[node.names.len - 1];
        const module_name = self.interner.intern(self.ast.toSource(token));
        // TODO: protect cast
        const module_type: Type = .create(.module, .none, @intCast(self.modules.count()));

        if (node.items) |items| {
            const module_index = if (!cached) b: {
                self.modules.put(self.allocator, module_name, module) catch oom();
                break :b self.modules.count() - 1;
            } else self.modules.getIndex(module_name).?;

            for (items) |item| {
                const item_name = self.interner.intern(self.ast.toSource(item.item));
                const symbol = module.symbols.get(item_name) orelse return self.err(
                    .{ .missing_symbol_in_module = .{
                        .module = self.ast.toSource(node.names[node.names.len - 1]),
                        .symbol = self.ast.toSource(item.item),
                    } },
                    self.ast.getSpan(item.item),
                );
                const value = symbol.type.getValue();

                var new_infos = self.type_manager.type_infos.items[value];
                new_infos.setModule(self.modules.count(), value);
                const kind: TypeSys.Kind = switch (new_infos) {
                    .@"struct" => .@"struct",
                    .func => .func,
                    else => @panic("Not supported yet"),
                };

                const index = try self.type_manager.reserveInfo();
                self.type_manager.setInfo(index, new_infos);
                const typ = Type.create(kind, .imported, index);

                const item_token = if (item.alias) |alias| alias else item.item;
                const alias_name = self.interner.intern(self.ast.toSource(item_token));
                const variable = try self.declareVariable(alias_name, typ, true, self.instructions.len, .decl, item_token);

                self.addInstr(.{ .item_import = .{ .module_index = module_index, .field_index = symbol.index, .scope = variable.scope } });
            }
        } else {
            if (cached) return self.err(
                .{ .already_imported_module = .{ .name = self.ast.toSource(node.names[node.names.len - 1]) } },
                self.ast.getSpan(node.names[node.names.len - 1]),
            );

            const variable = try self.declareVariable(module_name, module_type, true, self.instructions.len, .decl, token);
            self.addInstr(.{ .module_import = .{ .index = self.modules.count(), .scope = variable.scope } });
            self.modules.put(self.allocator, module_name, module) catch oom();
            self.imports.append(self.allocator, variable) catch oom();
        }
    }
}

/// Returns the module and a boolean indicating if the module has just been compiled or if
/// we used a cached one
fn importModule(self: *Self, node: *const Ast.Use) Error!struct { Pipeline.Module, bool } {
    var cwd = std.fs.cwd();

    for (node.names, 0..) |n, i| {
        const name = self.ast.toSource(n);

        if (i == node.names.len - 1) {
            // TODO: manage this better. We should hash the whole path and later manage it even better
            // because same module could be imported from different files with different path. We should
            // create a path from the root folder for every import so that all path can be compared regardless
            // of where files are
            const interned = self.interner.intern(self.ast.toSource(n));
            if (self.modules.get(interned)) |mod| return .{ mod, true };

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
            const module = pipeline.run(name, buf[0..size :0]) catch {
                std.process.exit(0);
            };

            return .{ module, false };
        } else {
            cwd = cwd.openDir(name, .{}) catch return self.err(
                .{ .unknown_module = .{ .name = self.ast.toSource(node.names[i]) } },
                self.ast.getSpan(node.names[i]),
            );
        }
    }

    unreachable;
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl) !void {
    const name = try self.checkName(node.name);
    var checked_type = try self.checkAndGetType(node.typ);
    const index = self.reserveInstr();

    var initialized = false;
    var data = Instruction.VarDecl{ .variable = undefined };

    if (node.value) |value| {
        data.has_value = true;
        initialized = true;

        const last = self.state.allow_partial;
        self.state.allow_partial = false;
        self.state.incr_ref = true;

        var value_type = try self.analyzeExpr(value);

        self.state.incr_ref = false;
        self.state.allow_partial = last;

        const coherence = try self.performTypeCoercion(checked_type, value_type, true, self.ast.getSpan(value));
        checked_type = coherence.type;
        data.cast = coherence.cast;

        // If we assign a bound method that match the type (checked after), update type's infos
        const extra = value_type.getExtra();

        if (extra == .bound_method) {
            checked_type.setExtra(.bound_method);
        } else if (extra == .imported) {
            checked_type.setExtra(.imported);
        }
    }

    data.variable = try self.declareVariable(name, checked_type, initialized, index, .variable, node.name);
    self.setInstr(index, .{ .var_decl = data });

    if (data.variable.scope == .global) {
        self.addSymbol(name, checked_type);
    }
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl) !void {
    self.addInstr(.{ .multiple_var_decl = node.decls.len });

    for (node.decls) |*n| {
        try self.varDeclaration(n);
    }
}

fn whileStmt(self: *Self, node: *const Ast.While) Error!void {
    self.addInstr(.{ .@"while" = undefined });
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
        .{ .non_void_while = .{ .found = self.getTypeName(body_type) } },
        self.ast.getSpan(node.body),
    );
}

fn analyzeExpr(self: *Self, expr: *const Expr) Error!Type {
    return switch (expr.*) {
        .array => |*e| self.array(e),
        .array_access => |*e| self.arrayAccess(e),
        .block => |*e| self.block(e),
        .binop => |*e| self.binop(e),
        .field => |*e| (try self.field(e)).field,
        .fn_call => |*e| self.call(e),
        .grouping => |*e| self.analyzeExpr(e.expr),
        .@"if" => |*e| self.ifExpr(e),
        .literal => |*e| self.literal(e),
        .named_arg => unreachable,
        .@"return" => |*e| self.returnExpr(e),
        .struct_literal => |*e| self.structLiteral(e),
        .unary => |*e| self.unary(e),
    };
}

fn array(self: *Self, expr: *const Ast.Array) Error!Type {
    const index = self.reserveInstr();
    var value_type: Type = .void;
    var cast_count: usize = 0;
    var cast_until: usize = 0;

    for (expr.values, 0..) |val, i| {
        var typ = try self.analyzeExpr(val);

        if (value_type != .void and !self.isTypeEqual(value_type, typ)) {
            // Backtrack casts without emitting an instruction
            if (cast_until == 0 and self.checkCast(typ, value_type, false)) {
                cast_until = i + 1;
            } else if (self.checkCast(value_type, typ, true)) {
                cast_count += 1;
                typ = value_type;
            } else return self.err(.{ .array_elem_different_type = .{
                .found1 = self.getTypeName(value_type),
                .found2 = self.getTypeName(typ),
            } }, self.ast.getSpan(val));
        }
        value_type = typ;
    }

    self.setInstr(index, .{ .array = .{
        .len = expr.values.len,
        .cast_count = cast_count,
        .cast_until = cast_until,
    } });

    return self.type_manager.getOrCreateArray(value_type);
}

fn arrayAccess(self: *Self, expr: *const Ast.ArrayAccess) Error!Type {
    if (expr.array.* == .array_access) {
        return self.arrayAccessChain(expr);
    }

    // If we are in an array access and that we should increment the reference, we unmark
    // the flag so that the identifier instruction doesn't trigger a reference increment
    // but only the last array access, for example:
    //   var arr = [[1, 2], [3, 4]]
    //   var arrRef = arr[0]
    //
    // Here, we don't want to increment the reference of 'arr' when resolving the identifier,
    // we want to increment the nested array's reference count
    const save_check_inr_ref = self.state.incr_ref;
    self.state.incr_ref = false;

    const idx = self.reserveInstr();
    const arr = try self.analyzeExpr(expr.array);

    if (!arr.is(.array)) return self.err(
        .{ .non_array_indexing = .{ .found = self.getTypeName(arr) } },
        self.ast.getSpan(expr.array),
    );

    const type_value = arr.getValue();
    const child = self.type_manager.type_infos.items[type_value].array.child;

    try self.expectArrayIndex(expr.index);
    self.setInstr(idx, .{ .array_access = .{ .incr_ref = save_check_inr_ref and child.isHeap() } });

    return child;
}

fn arrayAccessChain(self: *Self, expr: *const Ast.ArrayAccess) Error!Type {
    const idx = self.reserveInstr();
    // We use 1 here because we're gonna compile last index too at the end
    // of the chain, resulting in 1 more length that the chain
    var depth: usize = 1;
    var current = expr;

    while (current.array.* == .array_access) : (depth += 1) {
        try self.expectArrayIndex(current.index);
        current = &current.array.array_access;
    }

    try self.expectArrayIndex(current.index);

    const save_check_inr_ref = self.state.incr_ref;
    self.state.incr_ref = false;
    const arr = try self.analyzeExpr(current.array);

    var final_type: Type = arr;
    for (0..depth) |_| {
        const type_value = final_type.getValue();
        final_type = self.type_manager.type_infos.items[type_value].array.child;
    }

    self.setInstr(idx, .{ .array_access_chain = .{ .depth = depth, .incr_ref = save_check_inr_ref and final_type.isHeap() } });

    return final_type;
}

/// Analyze the expression and return an error if the type isn't an integer
fn expectArrayIndex(self: *Self, expr: *const Expr) Error!void {
    const index = try self.analyzeExpr(expr);

    if (index != .int) return self.err(
        .{ .non_integer_index = .{ .found = self.getTypeName(index) } },
        self.ast.getSpan(expr),
    );
}

fn binop(self: *Self, expr: *const Ast.Binop) Error!Type {
    const op = expr.op;
    const index = self.reserveInstr();

    const lhs = try self.analyzeExpr(expr.lhs);
    const rhs = try self.analyzeExpr(expr.rhs);

    var res = lhs;

    // String operations
    if (op == .plus and lhs == .str and rhs == .str) {
        self.setInstr(index, .{ .binop = .{ .op = .add_str } });
        return .str;
    } else if (op == .star) {
        if ((lhs == .str and rhs == .int) or (lhs == .int and rhs == .str)) {
            self.setInstr(index, .{ .binop = .{ .op = .mul_str, .cast = if (rhs == .int) .rhs else .lhs } });
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

    self.setInstr(index, .{ .binop = data });

    return res;
}

fn block(self: *Self, expr: *const Ast.Block) Error!Type {
    self.scope_depth += 1;
    errdefer self.scope_depth -= 1;

    const index = self.reserveInstr();
    var final: Type = .void;

    for (expr.nodes, 0..) |*node, i| {
        final = try self.analyzeNode(node);

        if (final != .void and i != expr.nodes.len - 1) return self.err(.unused_value, expr.span);
    }

    const count = self.endScope();

    self.setInstr(index, .{
        .block = .{
            .length = expr.nodes.len,
            // TODO: protect cast
            .pop_count = @intCast(count),
            .is_expr = if (final != .void) true else false,
        },
    });

    return final;
}

const StructAndFieldTypes = struct {
    structure: Type,
    is_type: bool,
    // field_ptr: ?*Type,
    field: Type,
    kind: Instruction.Field.Kind,

    // pub fn init(structure_type: Type, is_type: bool, field_ptr: ?*Type, field_type: Type, kind: Instruction.Field.Kind) StructAndFieldTypes {
    //     return .{ .structure = structure_type, .is_type = is_type, .field_ptr = field_ptr, .field = field_type, .kind = kind };
    pub fn init(structure_type: Type, is_type: bool, field_type: Type, kind: Instruction.Field.Kind) StructAndFieldTypes {
        return .{ .structure = structure_type, .is_type = is_type, .field = field_type, .kind = kind };
    }
};

fn field(self: *Self, expr: *const Ast.Field) Error!StructAndFieldTypes {
    const index = self.reserveInstr();
    const field_name = self.interner.intern(self.ast.toSource(expr.field));

    // We set it to false because we wan't to increment rhs of the field access, not the structure
    const save_check_inr_ref = self.state.incr_ref;
    self.state.incr_ref = false;

    // If the structure is only an identifier, it could be a type name
    const struct_type, const is_type = switch (expr.structure.*) {
        .literal => |*e| blk: {
            const assigne = try self.identifier(e.idx, false);
            break :blk .{ assigne.typ, assigne.kind == .decl };
        },
        else => .{ try self.analyzeExpr(expr.structure), false },
    };

    self.state.incr_ref = save_check_inr_ref;
    const struct_value = struct_type.getValue();

    var found_field, const kind: Instruction.Field.Kind = switch (struct_type.getKind()) {
        .@"struct" => blk: {
            const infos = self.type_manager.type_infos.items[struct_value].@"struct";

            break :blk if (infos.fields.get(field_name)) |f|
                .{ f, .field }
            else if (infos.functions.get(field_name)) |f|
                .{ f, if (is_type) .static_method else .method }
            else
                return self.err(
                    .{ .undeclared_field_access = .{ .name = self.ast.toSource(expr.field) } },
                    self.ast.getSpan(expr.field),
                );
        },
        .module => blk: {
            const module = &self.modules.values()[struct_value];
            var symbol = module.symbols.get(field_name) orelse return self.err(
                .{ .missing_symbol_in_module = .{ .symbol = self.ast.toSource(expr.field), .module = module.name } },
                self.ast.getSpan(expr.field),
            );
            // Here, we get the symbol we are referencing, then we create a copy. We give it the correct
            // import index (setModule) so that we can retreive its declaration later in the self.imports
            // list. As we updated the 'module' index field, we add a new type in the type manager and
            // create a type based on its index. We update the symbol's type to refer to the new one
            const value = symbol.type.getValue();
            const infos = &self.type_manager.type_infos.items[value];

            var new_infos = infos.*;
            new_infos.setModule(struct_value, value);
            const kind: TypeSys.Kind = if (new_infos == .func) .func else .@"struct";

            const new_idx = try self.type_manager.reserveInfo();
            self.type_manager.setInfo(new_idx, new_infos);
            const new_type = Type.create(kind, .imported, new_idx);
            symbol.type = new_type;

            break :blk .{ symbol, .symbol };
        },
        else => return self.err(
            .{ .non_struct_field_access = .{ .found = self.getTypeName(struct_type) } },
            self.ast.getSpan(expr.structure),
        ),
    };

    self.setInstr(index, .{
        .field = .{
            .index = found_field.index,
            .kind = kind,
            // Here, '!is_type' protects: 'geom.Point {}' to be incremented, as we are in the rhs of an assignment
            .incr_ref_count = self.state.incr_ref and found_field.type.isHeap() and !is_type,
        },
    });

    if (kind == .method) found_field.type.setExtra(.bound_method);

    // return .init(struct_type, is_type, &found_field.type, found_field.type, kind);
    return .init(struct_type, is_type, found_field.type, kind);
}

// fn getStructAndFieldTypes(self: *Self, expr: *const Expr) Error!StructAndFieldTypes {
//     return switch (expr.*) {
//         .field => |*e| try self.field(e),
//         else => .init(.void, false, &(try self.analyzeExpr(expr)), .field),
//     };
// }

fn call(self: *Self, expr: *const Ast.FnCall) Error!Type {
    const index = self.reserveInstr();

    // const sft = try self.getStructAndFieldTypes(expr.callee);
    const sft = switch (expr.callee.*) {
        .field => |*e| try self.field(e),
        // else => .init(.void, false, &(try self.analyzeExpr(expr)), .field),
        else => b: {
            const typ = try self.analyzeExpr(expr.callee);
            // break :b StructAndFieldTypes.init(.void, false, null, typ, .field);
            break :b StructAndFieldTypes.init(.void, false, typ, .field);
        },
    };
    const extra = sft.field.getExtra();
    const type_value = sft.field.getValue();

    const infos = if (sft.field.is(.func))
        self.type_manager.type_infos.items[type_value].func
    else if (sft.field.is(.@"struct")) blk: {
        const struct_infos = self.type_manager.type_infos.items[type_value].@"struct";
        const init_fn = struct_infos.functions.get(self.cached_names.init) orelse {
            return self.err(.struct_call_but_no_init, self.ast.getSpan(expr));
        };

        const type_idx = init_fn.type.getValue();
        break :blk self.type_manager.type_infos.items[type_idx].func;
    } else {
        return self.err(.invalid_call_target, self.ast.getSpan(expr));
    };

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
            call_tag = if (sft.is_type) .invoke_static else .invoke;

            if (!sft.is_type and (infos.params.count() == 0 or !self.isStructTypeEqual(infos.params.values()[0].type, sft.structure))) {
                return self.err(
                    .{ .missing_self_method_call = .{ .name = self.ast.toSource(expr.callee.field.field) } },
                    self.ast.getSpan(expr.callee.field.field),
                );
            }
        } else call_tag = .bound;
    }

    const arity = try self.fnArgsList(expr, &infos, extra == .bound_method);
    // TODO: protect cast
    self.setInstr(index, .{ .call = .{ .arity = @intCast(arity), .tag = call_tag } });

    // To load the module's global before call. If it's an invoke_import, we know that the
    // module is already just behind
    if (call_tag == .import) {
        // TODO: make declaration_id instead of full variable instruction?
        self.addInstr(.{ .identifier = self.imports.items[infos.module.?.import_index] });
    }

    return infos.return_type;
}

fn fnArgsList(self: *Self, callee: *const Ast.FnCall, infos: *const TypeSys.FnInfo, is_bound: bool) Error!usize {
    // Take 'self' into account
    const offset = @intFromBool(is_bound);
    const param_count = infos.params.count() - offset;

    if (callee.args.len > param_count) return self.err(
        AnalyzerMsg.tooManyFnArgs(param_count, callee.args.len),
        self.ast.getSpan(callee),
    );

    var proto = infos.proto(self.allocator);
    var proto_values = proto.values()[offset..];
    const start = self.instructions.len;
    self.instructions.ensureUnusedCapacity(self.allocator, param_count) catch oom();

    // We initialize all the values used for the initialization. By default, we put empty data under
    // the form of 'default_value' but we check for all real param default to mark their index (order
    // of declaration) so that the compiler can emit the right index
    var default_count: usize = 0;
    // Self is always the first parameter
    for (infos.params.values()[offset..]) |*f| {
        self.instructions.appendAssumeCapacity(.{ .data = .{ .default_value = default_count } });
        if (f.default) default_count += 1;
    }

    for (callee.args, 0..) |arg, i| {
        var cast = false;
        var value_instr: usize = 0;
        var param_info: *const FnInfo.ParamInfo = undefined;

        switch (arg.*) {
            .named_arg => |na| {
                const name = self.interner.intern(self.ast.toSource(na.name));
                proto.putAssumeCapacity(name, true);

                param_info = infos.params.getPtr(name) orelse return self.err(
                    .{ .unknown_param = .{ .name = self.ast.toSource(na.name) } },
                    self.ast.getSpan(na.name),
                );

                value_instr = self.instructions.len;
                const value_type = try self.analyzeExpr(na.value);
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(na.value))).cast;
            },
            else => {
                value_instr = self.instructions.len;
                const value_type = try self.analyzeExpr(arg);
                param_info = &infos.params.values()[i + offset];
                cast = (try self.performTypeCoercion(param_info.type, value_type, false, self.ast.getSpan(arg))).cast;
                proto_values[i] = true;
            },
        }

        // We take into account here too
        self.instructions.items(.data)[start + param_info.index - offset] = .{ .value = .{ .value_instr = value_instr, .cast = cast } };
    }

    // Check if any missing non-default parameter
    var has_err = false;
    for (proto_values, 0..) |has_value, i| if (!has_value) {
        has_err = true;

        self.err(
            .{ .missing_function_param = .{ .name = self.interner.getKey(proto.keys()[i + offset]).? } },
            self.ast.getSpan(callee),
        ) catch {};
    };

    return if (has_err) error.Err else param_count;
}

fn literal(self: *Self, expr: *const Ast.Literal) Error!Type {
    const text = self.ast.toSource(expr);

    switch (expr.tag) {
        .bool => {
            self.addInstr(.{ .bool = if (self.ast.token_tags[expr.idx] == .true) true else false });
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
            self.addInstr(.{ .int = value });
            return .int;
        },
        .float => {
            const value = std.fmt.parseFloat(f64, text) catch blk: {
                // TODO: error handling, only one possible it's invalid char or too big
                std.debug.print("Error parsing float\n", .{});
                break :blk 0.0;
            };
            self.addInstr(.{ .float = value });
            return .float;
        },
        .null => {
            self.addInstr(.{ .null = undefined });
            return .null;
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
            self.addInstr(.{ .string = value });
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

    if (name_idx == self.cached_names.Self) {
        if (self.state.struct_type == .void)
            return self.err(.big_self_outside_struct, self.ast.getSpan(name));

        return &self.big_self;
    }

    return self.err(.{ .undeclared_var = .{ .name = text } }, self.ast.getSpan(name));
}

/// Generates the instruction to get the current variable
fn makeVariableInstr(self: *Self, variable: *Variable) void {
    if (variable.kind == .variable) {
        self.checkCapture(variable);
        self.addInstr(.{ .identifier_id = .{
            .index = variable.decl,
            .incr_ref_count = self.state.incr_ref and variable.typ.isHeap(),
        } });
    } else {
        // In local scopes, we want sometimes to refer to local declarations like:
        // fn main() {
        //      struct Point {}
        //      fn getPoint() -> Point {
        //          Point {}
        //      }
        // }
        // Here, `Point` is on the stack, so when we call `getPoint`, we can't see outside
        // the function's callframe. So we first check if the variable is a declaration, if it's in
        // an outer scope and if it's not a global declaration. Then, we use its index and the compiler
        // will emit a special code to get it at runtime from the beginning of the stack, not the callframe
        if (variable.kind == .decl and self.scope_depth > variable.depth and variable.depth > 0) {
            self.addInstr(.{ .identifier_absolute = variable.index });
        } else {
            // It's a declaration
            self.addInstr(.{ .identifier = .{
                .index = variable.index,
                .scope = if (variable.depth > 0) .local else .global,
            } });
        }
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
    self.instructions.items(.data)[variable.decl].var_decl.variable.index = self.heap_count;
    self.heap_count += 1;
}

fn ifExpr(self: *Self, expr: *const Ast.If) Error!Type {
    const index = self.reserveInstr();
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
            if (self.checkCast(else_type, then_type, false)) {
                data.cast = .then;

                self.warn(
                    AnalyzerMsg.implicitCast("then branch", "float"),
                    self.ast.getSpan(expr.then),
                );
            } else if (self.checkCast(then_type, else_type, false)) {
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

    self.setInstr(index, .{ .@"if" = data });

    return final_type;
}

fn returnExpr(self: *Self, expr: *const Ast.Return) Error!Type {
    const index = self.reserveInstr();
    var data: Instruction.Return = .{ .value = false, .cast = false };

    var return_type = if (expr.expr) |e| blk: {
        data.value = true;

        break :blk try self.analyzeExpr(e);
    } else .void;

    // We check after to advance node idx
    if (!self.state.in_fn) return self.err(.return_outside_fn, self.ast.getSpan(expr));

    // We do that here because we can insert a cast
    if (!self.isFunctionTypeEqual(self.state.fn_type, return_type)) {
        if (self.checkCast(self.state.fn_type, return_type, true)) {
            data.cast = true;
            return_type = .float;
        } else return self.err(
            .{ .incompatible_fn_type = .{
                .expect = self.getTypeName(self.state.fn_type),
                .found = self.getTypeName(return_type),
            } },
            self.ast.getSpan(expr),
        );
    }

    self.state.returns = true;
    self.setInstr(index, .{ .@"return" = data });

    return return_type;
}

fn structLiteral(self: *Self, expr: *const Ast.StructLiteral) !Type {
    const index = self.reserveInstr();
    const struct_type = try self.analyzeExpr(expr.structure);

    if (!struct_type.is(.@"struct")) {
        return self.err(.non_struct_struct_literal, self.ast.getSpan(expr.structure));
    }

    const infos = self.type_manager.type_infos.items[struct_type.getValue()].@"struct";
    const arity = expr.fields.len;
    var proto = infos.proto(self.allocator);
    defer proto.deinit(self.allocator);

    const start = self.instructions.len;
    self.instructions.ensureUnusedCapacity(self.allocator, infos.fields.count()) catch oom();

    // We initialize all the values used for the initialization. By default, we put empty data under
    // the form of 'struct_default' but we check for all real struct default to mark their index (order
    // of declaration) so that the compiler can emit the right index
    var default_count: usize = 0;
    for (infos.fields.values()) |f| {
        self.instructions.appendAssumeCapacity(.{ .data = .{ .default_value = default_count } });
        if (f.default) default_count += 1;
    }

    for (expr.fields) |*fv| {
        const field_name = self.interner.intern(self.ast.toSource(fv.name));
        const f = infos.fields.get(field_name) orelse return self.err(
            .{ .unknown_struct_field = .{ .name = self.ast.toSource(fv.name) } },
            self.ast.getSpan(fv.name),
        );

        const value_instr = self.instructions.len;
        proto.putAssumeCapacity(field_name, true);

        const typ = if (fv.value) |val|
            try self.analyzeExpr(val)
        else // Syntax: { x } instead of { x = x }
            (try self.identifier(fv.name, true)).typ;

        const span = if (fv.value) |val| self.ast.getSpan(val) else self.ast.getSpan(fv.name);
        const cast = (try self.performTypeCoercion(f.type, typ, false, span)).cast;

        self.instructions.items(.data)[start + f.index] = .{ .value = .{ .value_instr = value_instr, .cast = cast } };
    }

    if (arity != proto.count()) {
        var kv = proto.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.*) self.err(
                .{ .missing_field_struct_literal = .{ .name = self.interner.getKey(entry.key_ptr.*).? } },
                self.ast.getSpan(expr.structure),
            ) catch {};
        }

        return error.Err;
    }

    self.setInstr(index, .{ .struct_literal = infos.fields.count() });

    return struct_type;
}

fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
    const op = self.ast.token_tags[expr.op];
    const index = self.reserveInstr();
    var data: Instruction.Unary = .{ .op = if (op == .not) .bang else .minus, .typ = .float };

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

    if (rhs == .int) data.typ = .int;

    self.setInstr(index, .{ .unary = data });

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
        .array => |arr_type| {
            const child = try self.checkAndGetType(arr_type.child);

            if (child == .void) {
                return self.err(.void_array, self.ast.getSpan(arr_type.child));
            }

            return self.type_manager.getOrCreateArray(child);
        },
        .fields => |fields| {
            var module: Pipeline.Module = undefined;

            for (fields[0 .. fields.len - 1]) |f| {
                const module_variable = try self.resolveIdentifier(f, true);

                if (module_variable.typ.getKind() != .module) return self.err(
                    .{ .dot_type_on_non_mod = .{ .found = self.getTypeName(module_variable.typ) } },
                    self.ast.getSpan(f),
                );

                const module_index = module_variable.typ.getValue();
                module = self.modules.values()[module_index];
            }

            const name_token = fields[fields.len - 1];
            const name = self.interner.intern(self.ast.toSource(name_token));
            const final = module.symbols.get(name) orelse return self.err(
                .{ .missing_symbol_in_module = .{ .module = module.name, .symbol = self.ast.toSource(name_token) } },
                self.ast.getSpan(name_token),
            );
            return final.type;
        },
        .function => |*fn_type| self.createAnonymousFnType(fn_type),
        .scalar => {
            const interned = self.interner.intern(self.ast.toSource(t));

            return self.type_manager.declared.get(interned) orelse {
                if (interned == self.cached_names.Self) {
                    if (self.state.struct_type == .void)
                        return self.err(.big_self_outside_struct, self.ast.getSpan(t));

                    return self.state.struct_type;
                } else return self.err(
                    .{ .undeclared_type = .{ .found = self.ast.toSource(t) } },
                    self.ast.getSpan(t),
                );
            };
        },
        .self => .self,
    } else .void;
}

/// Creates a type for an anonymous function, like the one defined in return types
/// of functions
fn createAnonymousFnType(self: *Self, fn_type: *const Ast.Type.Fn) Error!Type {
    const type_idx = try self.type_manager.reserveInfo();
    var params_type: AutoArrayHashMapUnmanaged(usize, FnInfo.ParamInfo) = .{};
    params_type.ensureTotalCapacity(self.allocator, fn_type.params.len) catch oom();

    for (fn_type.params, 0..) |p, i| {
        const param_type = try self.checkAndGetType(p);

        if (param_type == .void) {
            return self.err(.void_param, fn_type.span);
        }

        params_type.putAssumeCapacity(i, .{ .index = i, .type = param_type });
    }

    // Set all the informations now that we have every thing
    self.type_manager.setInfo(type_idx, .{
        .func = .{
            .params = params_type,
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
    kind: Variable.Kind,
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
        return .{ .index = index, .scope = .global };
    } else {
        // Take function's frame into account
        const index = self.locals.items.len - self.local_offset;
        variable.index = index;

        if (index > 255) {
            return self.err(.too_many_locals, self.ast.getSpan(token));
        }

        self.locals.append(self.allocator, variable) catch oom();
        return .{ .index = index, .scope = .local };
    }
}

fn checkCast(self: *Self, to: Type, typ: Type, emit_instr: bool) bool {
    const cast = to == .float and typ == .int;

    if (emit_instr) self.addInstr(.{ .cast = .float });

    return cast;
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
fn isFunctionTypeEqual(self: *const Self, t1: Type, t2: Type) bool {
    if (t1 == t2) return true;
    if (!t1.is(.func) or !t2.is(.func)) return false;

    const v1 = t1.getValue();
    const v2 = t2.getValue();
    if (v1 == v2) return true;

    const infos1 = self.type_manager.type_infos.items[v1].func;
    const infos2 = self.type_manager.type_infos.items[v2].func;

    if (infos1.params.count() == infos2.params.count() and infos1.return_type == infos2.return_type) {
        for (infos1.params.values(), infos2.params.values()) |p1, p2| {
            if (p1.type != p2.type) return false;
        }

        return true;
    }

    return false;
}

/// Checks if two structures are identical
fn isStructTypeEqual(self: *const Self, s1: Type, s2: Type) bool {
    // Relies on the fact that we share the same type manager between all
    // sub-pipelines. It means that if the index in the type infos field is
    // the samen it's the same type
    if (s1 == s2) return true;
    if (!s1.is(.@"struct") or !s2.is(.@"struct")) return false;

    const v1 = s1.getValue();
    const v2 = s2.getValue();
    if (v1 == v2) return true;

    const infos1 = &self.type_manager.type_infos.items[v1].@"struct";
    const infos2 = &self.type_manager.type_infos.items[v2].@"struct";

    const final1 = if (infos1.module) |*mod| mod.type_index else v1;
    const final2 = if (infos2.module) |*mod| mod.type_index else v2;

    return final1 == final2;
}

/// Check if two arrays have same dimensions and child type
fn isArrayTypeEqual(self: *const Self, arr1: Type, arr2: Type) bool {
    if (!arr1.is(.array) or !arr2.is(.array)) return false;

    const dim1, const child1 = self.type_manager.getArrayDimAndChildType(arr1);
    const dim2, const child2 = self.type_manager.getArrayDimAndChildType(arr2);

    return dim1 == dim2 and self.isTypeEqual(child1, child2);
}

fn isTypeEqual(self: *const Self, t1: Type, t2: Type) bool {
    return t1 == t2 or self.isStructTypeEqual(t1, t2) or self.isFunctionTypeEqual(t1, t2) or self.isArrayTypeEqual(t1, t2);
}

/// Helpers used for errors
fn getTypeName(self: *const Self, typ: Type) []const u8 {
    if (typ.is(.func)) {
        return self.getFnTypeName(typ) catch oom();
    } else if (typ.is(.array)) {
        return self.getArrayTypeName(typ) catch oom();
    } else {
        const index = self.type_manager.idx(typ);
        return self.interner.getKey(index).?;
    }
}

fn getArrayTypeName(self: *const Self, typ: Type) ![]const u8 {
    const value = typ.getValue();
    const child = self.type_manager.type_infos.items[value].array.child;

    var res: std.ArrayListUnmanaged(u8) = .{};
    var writer = res.writer(self.allocator);
    try writer.print("[]{s}", .{self.getTypeName(child)});

    return try res.toOwnedSlice(self.allocator);
}

/// Helpers used for errors
fn getFnTypeName(self: *const Self, typ: Type) ![]const u8 {
    const value = typ.getValue();
    const decl = self.type_manager.type_infos.items[value].func;

    var res: std.ArrayListUnmanaged(u8) = .{};
    var writer = res.writer(self.allocator);
    try writer.writeAll("fn(");

    for (decl.params.values(), 0..) |p, i| {
        try writer.print("{s}{s}", .{
            self.getTypeName(p.type),
            if (i < decl.params.count() - 1) ", " else "",
        });
    }
    try writer.print(") -> {s}", .{self.getTypeName(decl.return_type)});

    return try res.toOwnedSlice(self.allocator);
}

/// try to infer array value type from variable's declared type
fn inferArrayType(self: *Self, decl: Type, value: Type, span: Span) Error!Type {
    // Get nested item's type from: [][][]int -> int
    _, const child = self.type_manager.getArrayDimAndChildType(value);

    // Empty array like: []
    if (child == .void) {
        // No type declared and empty array like: var a = [], else infer from declaration
        return if (decl == .void) self.err(.cant_infer_arary_type, span) else decl;
    }

    return value;
}

const TypeCoherence = struct { type: Type = .void, cast: bool = false };

/// Checks for `void` values, array inference and cast. The goal is to see if the two types are equivalent
fn performTypeCoercion(self: *Self, decl: Type, value: Type, emit_cast: bool, span: Span) Error!TypeCoherence {
    var cast = false;
    var local_decl = decl;
    var local_value = value;

    if (value == .void) return self.err(.void_value, span);

    if (value.is(.array)) {
        local_value = try self.inferArrayType(decl, value, span);
    }

    if (local_decl == .void) {
        local_decl = local_value;
    } else if (!self.isTypeEqual(local_decl, local_value)) {
        // One case in wich we can coerce, int -> float
        if (self.checkCast(local_decl, local_value, emit_cast)) {
            cast = true;
        } else return self.err(
            .{ .type_mismatch = .{
                .expect = self.getTypeName(local_decl),
                .found = self.getTypeName(local_value),
            } },
            span,
        );
    }

    return .{ .type = local_decl, .cast = cast };
}
