const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const AutoHashMap = std.AutoHashMap;

const Interner = @import("../Interner.zig");
const GenReport = @import("../reporter.zig").GenReport;
const oom = @import("../utils.zig").oom;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const BA = @import("builtins_analyzer.zig");
const BuiltinAnalyzer = BA.BuiltinAnalyzer;
const FnDeclaration = BA.FnDeclaration;
const builtin_init = BA.init;
const Rir = @import("rir.zig");
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Span = @import("Lexer.zig").Span;
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;
const TypeInfo = TypeSys.TypeInfo;

// Re-export constants
pub const TypeManager = struct {
    declared: std.AutoHashMap(usize, Type),
    type_infos: ArrayList(TypeInfo),
    natives: BuiltinAnalyzer = builtin_init(),

    const Error = error{TooManyTypes};

    pub fn init(allocator: Allocator) TypeManager {
        return .{
            .declared = .init(allocator),
            .type_infos = .init(allocator),
        };
    }

    pub fn init_builtins(self: *TypeManager, interner: *Interner) void {
        self.declared.put(interner.intern("void"), .void) catch oom();
        self.declared.put(interner.intern("null"), .null) catch oom();
        self.declared.put(interner.intern("bool"), .bool) catch oom();
        self.declared.put(interner.intern("float"), .float) catch oom();
        self.declared.put(interner.intern("int"), .int) catch oom();
        self.declared.put(interner.intern("str"), .str) catch oom();
        self.declared.put(interner.intern("Self"), .self) catch oom();
    }

    pub fn deinit(self: *TypeManager) void {
        self.declared.deinit();
        self.type_infos.deinit();
    }

    /// Adds information about a type. Requires the kind and extra info, the value (aka
    /// index in information array) is computed in the function.
    /// Returns the complete type
    pub fn reserveInfo(self: *TypeManager) !TypeSys.Value {
        const count = self.type_infos.items.len;
        self.type_infos.append(undefined) catch oom();

        return if (count == std.math.maxInt(TypeSys.Value))
            error.TooManyTypes
        else
            @intCast(count);
    }

    /// Set type information at a specific index in list (index gave by *reserveInfo* method)
    pub fn setInfo(self: *TypeManager, index: usize, info: TypeInfo) void {
        self.type_infos.items[index] = info;
    }

    /// Adds a type linked associated with the name
    pub fn addType(self: *TypeManager, name: usize, typ: Type) void {
        self.declared.put(name, typ) catch oom();
    }

    /// Declares a new type built with `kind` and `extra` parameters and add the informations
    pub fn declare(self: *TypeManager, name: usize, kind: TypeSys.Kind, extra: TypeSys.Extra, info: TypeInfo) !TypeSys.Type {
        const count = self.type_infos.items.len;

        // Error
        if (count == std.math.maxInt(TypeSys.Value)) return error.TooManyTypes;

        const typ = TypeSys.create(kind, extra, @intCast(count));
        self.type_infos.append(info) catch oom();
        self.declared.put(name, typ) catch oom();

        return typ;
    }

    /// Checks if the type has already been declared
    pub fn isDeclared(self: *const TypeManager, typ: usize) bool {
        return self.declared.get(typ) != null;
    }

    /// Use natives function whose informations are gathered at compile time. Import the
    /// informations among other declared types
    pub fn importNative(self: *TypeManager, name: []const u8) ?std.StaticStringMap(FnDeclaration) {
        return self.natives.declarations.get(name);
    }

    /// Used only in error mode, no need for performance. If used in performance path
    pub fn idx(self: *const TypeManager, typ: Type) usize {
        var iter = self.declared.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.* == typ) {
                return entry.key_ptr.*;
            }
        }
        unreachable;
    }
};

ast: *const Ast,

instructions: MultiArrayList(Instruction),
warns: ArrayList(AnalyzerReport),
errs: ArrayList(AnalyzerReport),

globals: ArrayList(Variable),
locals: ArrayList(Variable),
scope_depth: usize,
/// Offset updated at each fn call, emulate the frame pointer at runtime
local_offset: usize,
heap_count: usize,
main: ?usize,
state: State,
type_manager: TypeManager,
interner: Interner,

// TODO: mettre en struct
main_interned: usize,
std_interned: usize,
self_interned: usize,
init_interned: usize,

arena: std.heap.ArenaAllocator,
allocator: Allocator,
repl: bool,

const Self = @This();
const Error = error{ Err, UndeclaredVar, UninitVar } || TypeManager.Error;

const Variable = struct {
    index: usize = 0,
    typ: Type = .void,
    depth: usize,
    name: usize,
    decl: usize = 0,
    initialized: bool = false,
    captured: bool = false,
    kind: Tag = .normal,

    pub const Tag = enum { normal, func, param, import, @"struct" };

    pub fn scope(self: *const Variable) Rir.Scope {
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
    /// Flag to tell if last statement returned from scope
    returns: bool = false,
    /// In a structure
    in_struct: bool = false,
};

pub const AnalyzerReport = GenReport(AnalyzerMsg);

pub fn init(self: *Self, allocator: Allocator, repl: bool) void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();

    self.instructions = .{};
    self.warns = .init(self.allocator);
    self.errs = .init(self.allocator);
    self.globals = .init(self.allocator);
    self.locals = .init(self.allocator);
    self.scope_depth = 0;
    self.heap_count = 0;

    self.state = .{};
    self.main = null;
    self.local_offset = 0;
    self.type_manager = TypeManager.init(self.allocator);
    self.interner = Interner.init(self.allocator);

    self.main_interned = self.interner.intern("main");
    self.std_interned = self.interner.intern("std");
    self.self_interned = self.interner.intern("self");
    self.init_interned = self.interner.intern("init");

    self.type_manager.init_builtins(&self.interner);
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
        AnalyzerMsg.invalid_arithmetic(self.getTypeName(t)),
        self.ast.getSpan(expr.*),
    );
}

fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    self.errs.append(AnalyzerReport.err(kind, span)) catch oom();
    return error.Err;
}

fn warn(self: *Self, kind: AnalyzerMsg, span: Span) void {
    self.warns.append(AnalyzerReport.warn(kind, span)) catch oom();
}

pub fn analyze(self: *Self, ast: *const Ast) !void {
    self.ast = ast;

    for (ast.nodes) |*node| {
        const node_type = self.analyzeNode(node) catch |e| {
            switch (e) {
                error.TooManyTypes => return self.err(.TooManyTypes, self.ast.getSpan(node.*)),
                error.Err => continue,
                else => return e,
            }
        };

        if (node_type != .void) {
            std.debug.print("Before to source\n", .{});
            self.err(.UnusedValue, self.ast.getSpan(node.*)) catch {};
            std.debug.print("After to source\n", .{});
        }
    }

    // In REPL mode, no need for main function
    if (self.repl)
        return
    else if (self.main == null) self.err(.NoMain, .{ .start = 0, .end = 0 }) catch {};
}

fn analyzeNode(self: *Self, node: *const Node) Error!Type {
    if (self.scope_depth == 0 and !self.repl and !self.isPure(node.*)) {
        // TODO: add block, not allowed to have local scopes in global scope
        return if (std.meta.activeTag(node.*) == .expr and std.meta.activeTag(node.expr.*) == .@"return")
            // self.err(.ReturnOutsideFn, node.getSpan(self.ast))
            self.err(.ReturnOutsideFn, self.ast.getSpan(node.*))
        else
            self.err(.UnpureInGlobal, self.ast.getSpan(node.*));
    }

    switch (node.*) {
        .assignment => |*n| try self.assignment(n),
        .discard => |n| try self.discard(n),
        .fn_decl => |*n| try self.fnDeclaration(n),
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

    var assigne_type = switch (node.assigne.*) {
        .literal => |*e| blk: {
            if (e.tag != .identifier) {
                return self.err(.InvalidAssignTarget, self.ast.getSpan(node.assigne.*));
            }

            // TODO: check if this is a function's parameter (there are constant by defninition)
            const assigne = try self.identifier(e.idx, false);

            if (!assigne.initialized) assigne.initialized = true;

            break :blk assigne.typ;
        },
        .field => |*e| try self.field(e),
        else => return self.err(.InvalidAssignTarget, self.ast.getSpan(node.assigne.*)),
    };
    const assigne_kind = TypeSys.getKind(assigne_type);

    b: {
        if (assigne_kind != .variable) {
            if (assigne_kind == .func) {
                const value = TypeSys.getValue(assigne_type);
                if (self.type_manager.type_infos.items[value].func.is_var) {
                    break :b;
                }
            }

            return self.err(.InvalidAssignTarget, self.ast.getSpan(node.assigne.*));
        }
    }
    const value_type = try self.analyzeExpr(node.value);

    if (value_type == .void) {
        return self.err(.VoidAssignment, self.ast.getSpan(node.value.*));
    }

    // If type is unknown, we update it
    if (assigne_type == .void) {
        assigne_type = value_type;
    } else if (assigne_type != value_type and !self.checkEqualFnType(assigne_type, value_type)) {
        // One case in wich we can coerce; int -> float
        if (assigne_type == .float and value_type == .int) {
            cast = true;
            _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
        } else {
            return self.err(
                .{ .InvalidAssignType = .{
                    .expect = self.getTypeName(assigne_type),
                    .found = self.getTypeName(value_type),
                } },
                self.ast.getSpan(node.assigne.*),
            );
        }
    }

    self.instructions.items(.data)[idx] = .{ .assignment = .{ .cast = cast } };
    self.state.allow_partial = last;
}

fn discard(self: *Self, expr: *const Expr) !void {
    _ = self.addInstr(.{ .tag = .discard });
    const discarded = try self.analyzeExpr(expr);

    if (discarded == .void) return self.err(.VoidDiscard, self.ast.getSpan(expr.*));
}

fn fnDeclaration(self: *Self, node: *const Ast.FnDecl) Error!void {
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

    // We declare before body for recursion and before parameters to put it as the first local
    const type_idx = try self.type_manager.reserveInfo();
    const fn_type = TypeSys.create(.func, .none, type_idx);
    const fn_var = try self.declareVariable(name_idx, fn_type, true, self.instructions.len, .func, node.name);

    _ = self.addInstr(.{ .tag = .var_decl, .data = .{ .var_decl = .{ .variable = fn_var, .cast = false } } });

    self.scope_depth += 1;
    self.local_offset = self.locals.items.len;

    // We put back allow partial at the beginning of the function in case we are the last statement of an enclosing
    // function, which has the effect of putting allow_partial to false
    self.state.in_fn = true;
    self.state.allow_partial = true;

    // We add a empty variable to anticipate the function it self on the stack
    // it's the returned address for the function
    self.locals.append(.{ .depth = self.scope_depth, .name = name_idx, .typ = fn_type, .initialized = true, .kind = .func }) catch oom();

    // TODO: ArrayList with init capacity of arity
    var params_type: ArrayListUnmanaged(Type) = .{};
    params_type.ensureTotalCapacity(self.allocator, node.params.len) catch oom();

    for (node.params) |p| {
        const decl = self.instructions.len;
        const param_idx = self.checkName(p.name) catch |e| {
            self.errs.items[self.errs.items.len - 1] = AnalyzerReport.err(
                .{ .DuplicateParam = .{ .name = self.ast.toSource(p.name) } },
                self.ast.token_spans[p.name],
            );
            return e;
        };

        const param_type = if (param_idx == self.self_interned) param_type: {
            if (!self.state.in_struct) {
                return self.err(.SelfOutsideStruct, self.ast.token_spans[p.name]);
            }

            if (name_idx == self.init_interned) {
                return self.err(.SelfInInit, self.ast.token_spans[p.name]);
            }

            break :param_type .self;
        } else try self.checkAndGetType(p.typ);

        if (param_type == .void) {
            return self.err(.VoidParam, self.ast.token_spans[p.name]);
        }

        _ = try self.declareVariable(param_idx, param_type, true, decl, .param, p.name);
        params_type.appendAssumeCapacity(param_type);
    }

    const return_type = try self.checkAndGetType(node.return_type);

    if (name_idx == self.init_interned and return_type != .self) {
        return self.err(.NonSelfInitReturn, if (node.return_type) |t|
            self.ast.getSpan(t.*)
        else
            self.ast.token_spans[node.name]);
    }

    self.state.fn_type = return_type;

    self.type_manager.setInfo(type_idx, .{ .func = .{
        .params = params_type.toOwnedSlice(self.allocator) catch oom(),
        .return_type = return_type,
    } });

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
            self.warn(.DeadCode, self.ast.getSpan(n.*));
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
            self.err(.UnusedValue, self.ast.getSpan(n.*)) catch {};
        }
    }

    if (!body_err and body_type != return_type and !self.checkEqualFnType(body_type, return_type)) {
        return self.err(
            .{ .IncompatibleFnType = .{
                .expect = self.getTypeName(return_type),
                .found = self.getTypeName(body_type),
            } },
            if (node.body.nodes.len > 0)
                self.ast.getSpan(node.body.nodes[node.body.nodes.len - 1])
            else
                self.ast.token_spans[node.name],
        );
    }

    // We strip unused instructions for them not to be compiled
    // TODO: test
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
}

fn multiVarDecl(self: *Self, node: *const Ast.MultiVarDecl) !void {
    _ = self.addInstr(.{ .tag = .multiple_var_decl, .data = .{ .id = node.decls.len } });

    for (node.decls) |*n| {
        try self.varDeclaration(n);
    }
}

fn print(self: *Self, expr: *const Expr) !void {
    _ = self.addInstr(.{ .tag = .print, .data = undefined });
    const typ = try self.analyzeExpr(expr);

    if (typ == .void)
        return self.err(.VoidPrint, self.ast.getSpan(expr.*));
}

fn structDecl(self: *Self, node: *const Ast.StructDecl) !void {
    const name = self.interner.intern(self.ast.toSource(node.name));

    if (self.type_manager.isDeclared(name)) {
        return self.err(.{ .AlreadyDeclaredStruct = .{ .name = self.ast.toSource(node.name) } }, self.ast.getSpan(node.*));
    }

    // We forward declare for self referencing
    const type_idx = try self.type_manager.reserveInfo();
    const struct_type = TypeSys.create(.@"struct", .none, type_idx);
    const struct_var = try self.declareVariable(name, struct_type, true, self.instructions.len, .@"struct", node.name);

    const struct_idx = self.addInstr(.{ .tag = .struct_decl, .data = undefined });
    // We add function's name for runtime access
    _ = self.addInstr(.{ .tag = .name, .data = .{ .id = name } });
    _ = self.addInstr(.{ .tag = .var_decl, .data = .{ .var_decl = .{ .variable = struct_var, .cast = false } } });

    self.scope_depth += 1;
    errdefer _ = self.endScope();

    var infos: TypeSys.StructInfo = .{ .functions = .{}, .fields = .{}, .default_value_fields = 0 };

    infos.fields.ensureTotalCapacity(self.allocator, @intCast(node.fields.len)) catch oom();

    for (node.fields, 0..) |*f, i| {
        var field_infos: TypeSys.FieldInfo = .{ .type = undefined, .default = false, .idx = i };
        const field_name = self.interner.intern(self.ast.toSource(f.name));

        if (infos.fields.get(field_name) != null) {
            // TODO: Error, already declared field
            std.debug.print("Already declared field\n", .{});
        }

        const field_type = try self.checkAndGetType(f.typ);

        const field_value_type = if (f.value) |value| blk: {
            // if (!self.isPure(self.node_idx)) {
            //     std.debug.print("Unpure default value\n", .{});
            //     // TODO: error, non-constant default value
            // }

            field_infos.default = true;
            infos.default_value_fields += 1;

            _ = self.addInstr(.{ .tag = .field, .data = .{ .field = i } });
            break :blk try self.analyzeExpr(value);
        } else .void;

        if (field_value_type != .void and field_type != .void and field_value_type != field_type) {
            // TODO: Error
            std.debug.print("Wrong type default value\n", .{});
        }

        // Fro mparsing, we know that there is either a type or default value. If no declared type, we take
        // the one from the default value
        field_infos.type = if (field_type == .void) field_value_type else field_type;
        infos.fields.putAssumeCapacity(field_name, field_infos);
    }

    infos.functions.ensureTotalCapacity(self.allocator, @intCast(node.functions.len)) catch oom();
    self.state.in_struct = true;

    for (node.functions) |*f| {
        const fn_name = self.interner.intern(self.ast.toSource(f.name));
        // Function's type infos will be the first added to the manager, even if types are created
        // in function's body It's safe to save it from here
        const func_idx = self.type_manager.type_infos.items.len;
        try self.fnDeclaration(f);

        if (fn_name == self.init_interned) {
            infos.init = func_idx;
        } else infos.functions.putAssumeCapacity(fn_name, func_idx);
    }

    self.state.in_struct = false;
    const type_info: TypeInfo = .{ .@"struct" = infos };
    self.type_manager.setInfo(type_idx, type_info);
    self.type_manager.addType(name, struct_type);

    _ = self.endScope();
    // _ = try self.declareVariable(name, struct_type, true, struct_idx, .@"struct");
    self.instructions.items(.data)[struct_idx] = .{ .struct_decl = .{
        .fields_count = node.fields.len,
        .default_fields = infos.default_value_fields,
        .func_count = node.functions.len,
    } };
}

fn use(self: *Self, node: *const Ast.Use) !void {
    const idx = self.addInstr(.{ .tag = .use, .data = undefined });

    const name = self.interner.intern(self.ast.toSource(node.names[0]));

    // For now, "std" is interned at initialization in slot 1
    if (name == self.std_interned) {
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
                            .builtin = true,
                        },
                    };

                    // Declare the type and additional informations
                    const typ = try self.type_manager.declare(name_idx, .func, .builtin, info);
                    // Declare the variable
                    const variable = try self.declareVariable(name_idx, typ, true, self.instructions.len, .import, n);

                    _ = self.addInstr(.{ .tag = .imported, .data = .{ .imported = .{
                        .index = func.index,
                        .variable = variable,
                    } } });
                }

                self.instructions.items(.data)[idx] = .{ .use = node.names.len };

                return;
            } else {
                return self.err(
                    .{ .UnknownModule = .{ .name = self.ast.toSource(n) } },
                    self.ast.token_spans[n],
                );
            }
        }
    } else return self.err(
        .{ .UnknownModule = .{ .name = self.ast.toSource(node.names[0]) } },
        self.ast.token_spans[node.names[0]],
    );
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
        if (value_type == .void) {
            return self.err(.VoidAssignment, self.ast.getSpan(value.*));
        }

        // If no type declared, we infer the value type
        if (checked_type == .void) {
            checked_type = value_type;
            // Else, we check for coherence
        } else if (checked_type != value_type) {
            // One case in wich we can coerce, int -> float
            if (checked_type == .float and value_type == .int) {
                cast = true;
                _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
            } else {
                return self.err(
                    .{ .InvalidAssignType = .{
                        .expect = self.getTypeName(checked_type),
                        .found = self.getTypeName(value_type),
                    } },
                    self.ast.getSpan(value.*),
                );
            }
        }

        initialized = true;
    } else {
        _ = self.addInstr(.{ .tag = .null });
    }

    // If it was a function type, we mark it as assignable. Allow to assign to variable that of a function type like:
    // var a: fn(int) -> bool
    if (TypeSys.getKind(checked_type) == .func) {
        const value = TypeSys.getValue(checked_type);
        self.type_manager.type_infos.items[value].func.is_var = true;
    }

    const variable = try self.declareVariable(name, checked_type, initialized, idx, .normal, node.name);
    self.instructions.items(.data)[idx] = .{ .var_decl = .{ .variable = variable, .cast = cast } };
}

fn whileStmt(self: *Self, node: *const Ast.While) Error!void {
    _ = self.addInstr(.{ .tag = .@"while" });
    const cond_type = try self.analyzeExpr(node.condition);

    if (cond_type != .bool) return self.err(
        .{ .NonBoolCond = .{
            .what = "while",
            .found = self.getTypeName(cond_type),
        } },
        self.ast.getSpan(node.condition.*),
    );

    const body_type = try self.block(&node.body);

    if (body_type != .void) return self.err(
        .{ .NonVoidWhile = .{
            .found = self.getTypeName(body_type),
        } },
        self.ast.getSpan(node.body),
    );
}

fn analyzeExpr(self: *Self, expr: *const Expr) Error!Type {
    return switch (expr.*) {
        .block => |*e| self.block(e),
        .binop => |*e| self.binop(e),
        .field => |*e| self.field(e),
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
                                AnalyzerMsg.implicit_cast("right hand side", self.getTypeName(lhs)),
                                self.ast.getSpan(expr.rhs.*),
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
                                AnalyzerMsg.implicit_cast("left hand side", self.getTypeName(rhs)),
                                self.ast.getSpan(expr.lhs.*),
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
                        self.warn(.FloatEqualCast, self.ast.getSpan(expr.rhs.*));
                    } else {
                        data.cast = .rhs;
                        self.warn(.FloatEqualCast, self.ast.getSpan(expr.rhs.*));
                    }

                    switch (op) {
                        .equal_equal => data.op = .eq_float,
                        .bang_equal => data.op = .ne_float,
                        else => unreachable,
                    }
                } else {
                    return self.err(
                        AnalyzerMsg.invalid_cmp(self.getTypeName(lhs), self.getTypeName(rhs)),
                        self.ast.getSpan(expr.*),
                    );
                }
            } else {
                // Check for unsafe float comparisons or int comparison
                if (lhs == .float) {
                    self.warn(.FloatEqual, self.ast.getSpan(expr.*));
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
                        .float => self.warn(.FloatEqual, self.ast.getSpan(expr.*)),
                        .int => {
                            data.cast = .rhs;
                            self.warn(.FloatEqualCast, self.ast.getSpan(expr.rhs.*));
                        },
                        else => unreachable,
                    }
                },
                .int => {
                    switch (rhs) {
                        .float => {
                            data.cast = .lhs;
                            self.warn(.FloatEqualCast, self.ast.getSpan(expr.lhs.*));
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
                .{ .InvalidLogical = .{ .found = self.getTypeName(lhs) } },
                self.ast.getSpan(expr.lhs.*),
            );

            if (rhs != .bool) return self.err(
                .{ .InvalidLogical = .{ .found = self.getTypeName(rhs) } },
                self.ast.getSpan(expr.rhs.*),
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

        if (final != .void and i != expr.nodes.len - 1) {
            return self.err(.UnusedValue, expr.span);
        }
    }

    const count = self.endScope();

    self.instructions.items(.data)[idx] = .{ .block = .{
        .length = expr.nodes.len,
        .pop_count = @intCast(count),
        .is_expr = if (final != .void) true else false,
    } };

    return final;
}

fn field(self: *Self, expr: *const Ast.Field) Error!Type {
    const idx = self.addInstr(.{ .tag = .field });
    const struct_type = try self.analyzeExpr(expr.structure);
    const field_idx = self.interner.intern(self.ast.toSource(expr.field));

    if (!TypeSys.is(struct_type, .@"struct")) {
        // TODO: error
        std.debug.print("Error, field access on a non-struct", .{});
    }

    const infos_index = TypeSys.getValue(struct_type);
    const infos = self.type_manager.type_infos.items[infos_index].@"struct";

    if (infos.fields.get(field_idx)) |f| {
        self.instructions.items(.data)[idx] = .{ .field = f.idx };

        return f.type;
    } else {
        // TODO: Error
        std.debug.print("Undeclared field during access", .{});
    }

    unreachable;
}

fn call(self: *Self, expr: *const Ast.FnCall) Error!Type {
    const idx = self.addInstr(.{ .tag = .call, .data = .{ .call = .{
        .arity = @intCast(expr.args.len),
        .builtin = undefined,
    } } });

    const callee_type = try self.analyzeExpr(expr.callee);

    const infos = if (TypeSys.is(callee_type, .func))
        self.type_manager.type_infos.items[TypeSys.getValue(callee_type)].func
    else if (TypeSys.is(callee_type, .@"struct")) blk: {
        const val = TypeSys.getValue(callee_type);
        const init_idx = self.type_manager.type_infos.items[val].@"struct".init;

        if (init_idx) |i| {
            break :blk self.type_manager.type_infos.items[i].func;
        } else return self.err(.StructCallButNoInit, self.ast.getSpan(expr.*));
    } else return self.err(.InvalidCallTarget, self.ast.getSpan(expr.*));

    if (infos.params.len != expr.args.len) {
        return self.err(
            AnalyzerMsg.wrongArgsCount(infos.params.len, expr.args.len),
            self.ast.getSpan(expr.*),
        );
    }

    self.instructions.items(.data)[idx].call.builtin = infos.builtin;
    return self.fnCall(expr.args, infos);
}

fn fnCall(self: *Self, args: []*Expr, infos: TypeSys.FnInfo) Error!Type {
    for (args, 0..) |arg, i| {
        const arg_type = try self.analyzeExpr(arg);

        if (arg_type != infos.params[i] and !self.checkEqualFnType(arg_type, infos.params[i])) {
            // If it's an implicit cast between int and float, save the
            // argument indices for compiler. Otherwise, error
            if (infos.params[i] == .float and arg_type == .int) {
                _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
            } else {
                return self.err(
                    .{ .TypeMismatch = .{
                        .expect = self.getTypeName(infos.params[i]),
                        .found = self.getTypeName(arg_type),
                    } },
                    self.ast.getSpan(arg.*),
                );
            }
        }
    }

    return infos.return_type;
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
        .identifier => {
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
            _ = self.addInstr(.{ .tag = .null, .data = undefined });

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

fn identifier(self: *Self, name: usize, initialized: bool) Error!*Variable {
    const variable = try self.resolveIdentifier(name, initialized);

    if (variable.kind == .normal) {
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

    return variable;
}

/// Checks if a variable is in local scope, enclosing scope or global scope. Check if its state
/// is `initialized`, otherwise return an error.
fn resolveIdentifier(self: *Self, name: usize, initialized: bool) Error!*Variable {
    const text = self.ast.toSource(name);
    const name_idx = self.interner.intern(text);

    // We first check in locals
    if (self.locals.items.len > 0) {
        var idx = self.locals.items.len;

        while (idx > 0) : (idx -= 1) {
            const local = &self.locals.items[idx - 1];

            if (name_idx == local.name) {
                // Checks the initialization if asked
                if (initialized and !local.initialized) {
                    return self.err(.{ .UseUninitVar = .{ .name = text } }, self.ast.token_spans[name]);
                }

                return local;
            }
        }
    }

    // TODO: in reverse? People tend to use latest declared variables
    for (self.globals.items) |*glob| {
        if (name_idx == glob.name) {
            if (initialized and !glob.initialized) {
                return self.err(.{ .UseUninitVar = .{ .name = text } }, self.ast.token_spans[name]);
            }

            return glob;
        }
    }

    // Else, it's undeclared
    return self.err(.{ .UndeclaredVar = .{ .name = text } }, self.ast.token_spans[name]);
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
        .{ .NonBoolCond = .{
            .what = "if",
            .found = self.getTypeName(cond_type),
        } },
        self.ast.getSpan(expr.condition.*),
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
                    AnalyzerMsg.implicit_cast("then branch", "float"),
                    self.ast.getSpan(expr.then),
                );
            } else if (then_type == .float and else_type == .int) {
                data.cast = .@"else";

                // Safe unsafe access, if there is a non void type
                // there is an else body
                self.warn(
                    AnalyzerMsg.implicit_cast("else branch", "float"),
                    self.ast.getSpan(expr.@"else".?),
                );
            } else return self.err(
                .{ .IncompatibleIfType = .{
                    .found1 = self.getTypeName(then_type),
                    .found2 = self.getTypeName(else_type),
                } },
                self.ast.getSpan(expr.*),
            );
        }
    } else if (then_type != .void and !self.state.allow_partial) {
        return self.err(
            .{ .MissingElseClause = .{ .if_type = self.getTypeName(then_type) } },
            self.ast.getSpan(expr.*),
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
    if (!self.state.in_fn) {
        return self.err(.ReturnOutsideFn, self.ast.getSpan(expr.*));
    }

    if (!self.checkEqualFnType(self.state.fn_type, return_type)) {
        if (self.state.fn_type == .float and return_type == .int) {
            self.instructions.items(.data)[idx].@"return".cast = true;
            _ = self.addInstr(.{ .tag = .cast, .data = .{ .cast_to = .float } });
        } else return self.err(
            .{ .IncompatibleFnType = .{
                .expect = self.getTypeName(self.state.fn_type),
                .found = self.getTypeName(return_type),
            } },
            self.ast.getSpan(expr.*),
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
        const value = TypeSys.getValue(struct_type);
        const infos = self.type_manager.type_infos.items[value].@"struct";
        var proto = infos.proto(self.allocator);
        defer proto.deinit(self.allocator);

        const start = self.instructions.len;
        self.instructions.ensureTotalCapacity(self.allocator, self.instructions.len + arity) catch oom();

        for (0..arity) |_| {
            self.instructions.appendAssumeCapacity(.{ .tag = .field, .data = undefined });
        }

        for (expr.fields) |*fv| {
            const field_name = self.interner.intern(self.ast.toSource(fv.name));

            if (infos.fields.get(field_name)) |f| {
                proto.putAssumeCapacity(field_name, true);
                self.instructions.items(.data)[start + f.idx] = .{ .field = self.instructions.len };

                if (fv.value) |val| {
                    _ = try self.analyzeExpr(val);
                } else {
                    // Syntax: { x } instead of { x = x }
                    _ = try self.identifier(fv.name, true);
                }
            } else {
                // TODO: Error
                std.debug.print("Unknown structure field\n", .{});
            }
        }

        if (arity != proto.size) {
            var kv = proto.iterator();
            while (kv.next()) |entry| {
                if (!entry.value_ptr.*) {
                    // TODO: Error for each non init field
                    std.debug.print("Uninit filed: {s}\n", .{self.interner.getKey(entry.key_ptr.*).?});
                }
            }
        }

        // As the compiler is gonna jump around to compile in the correct order, we need a way
        // to know where to go in the list at the end to continue compiling as normal
        self.instructions.items(.data)[struct_lit_idx].struct_literal.end = self.instructions.len;

        return decl.typ;
    } else {
        // TODO: error
        std.debug.print("Unknown structure type\n", .{});

        return .void;
    }
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
            .{ .InvalidUnary = .{ .found = self.getTypeName(rhs) } },
            self.ast.getSpan(expr.*),
        );
    } else if (op == .minus and rhs != .int and rhs != .float) {
        return self.err(
            AnalyzerMsg.invalid_arithmetic(self.getTypeName(rhs)),
            self.ast.getSpan(expr.*),
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
        self.locals.resize(i) catch oom();
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
        .{ .AlreadyDeclared = .{ .name = self.interner.getKey(name).? } },
        self.ast.token_spans[token],
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
            .{ .UndeclaredType = .{ .found = self.ast.toSource(t) } },
            self.ast.getSpan(t.*),
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
            return self.err(.VoidParam, fn_type.span);
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

    return TypeSys.create(.func, .none, type_idx);
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

        self.globals.append(variable) catch oom();
        return .{ .index = @intCast(index), .scope = .global };
    } else {
        // Take function's frame into account
        const index = self.locals.items.len - self.local_offset;
        variable.index = index;

        if (index > 255) {
            return self.err(.TooManyLocals, self.ast.token_spans[token]);
        }

        self.locals.append(variable) catch oom();
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

    return if (TypeSys.is(expr_type, kind))
        TypeSys.getValue(expr_type)
    else
        self.err(
            .{ .TypeMismatch = .{
                .expect = TypeSys.str_kind(kind),
                .found = TypeSys.str_kind(TypeSys.getKind(expr_type)),
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
    if (!TypeSys.is(t1, .func) or !TypeSys.is(t2, .func)) return false;

    const v1 = TypeSys.getValue(t1);
    const infos1 = self.type_manager.type_infos.items[v1].func;

    const v2 = TypeSys.getValue(t2);
    const infos2 = self.type_manager.type_infos.items[v2].func;

    if (infos1.params.len == infos2.params.len and infos1.return_type == infos2.return_type) {
        for (infos1.params, infos2.params) |p1, p2| {
            if (p1 != p2) return false;
        }

        return true;
    }

    return false;
}

// Helpers used for errors
fn getTypeName(self: *const Self, typ: Type) []const u8 {
    if (TypeSys.is(typ, .func)) {
        return self.getFnTypeName(typ) catch oom();
    } else {
        const idx = self.type_manager.idx(typ);
        return self.interner.getKey(idx).?;
    }
}

fn getFnTypeName(self: *const Self, typ: Type) ![]const u8 {
    const value = TypeSys.getValue(typ);
    const decl = self.type_manager.type_infos.items[value].func;

    var res: std.ArrayListUnmanaged(u8) = .{};
    var writer = res.writer(self.allocator);
    try writer.writeAll("fn(");

    for (decl.params, 0..) |p, i| {
        try writer.print("{s}{s}", .{
            self.interner.getKey(p.toIdx()).?,
            if (i < decl.params.len - 1) ", " else "",
        });
    }
    try writer.print(") -> {s}", .{self.getTypeName(decl.return_type)});

    return try res.toOwnedSlice(self.allocator);
}
