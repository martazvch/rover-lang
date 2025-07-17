const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;

const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const Span = @import("Lexer.zig").Span;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const oom = @import("../utils.zig").oom;
const TM2 = @import("TM2.zig");

pub const Type = union(enum) {
    void,
    int,
    float,
    bool,
    str,
    null,
    self,

    pub fn equal(self: *const Type, other: *const Type) bool {
        return std.meta.eql(self, other);
    }

    pub fn cast(self: *const Type, other: *const Type) bool {
        return if (self.* == .int and other.* == .float)
            true
        else
            false;
    }
};

const Scope = struct {
    variables: std.AutoArrayHashMapUnmanaged(InternerIdx, Variable),
    depth: usize,
    offset: usize,

    pub const empty: Scope = .{ .variables = .{}, .depth = 0, .offset = 0 };

    pub fn declare(self: *Scope, allocator: Allocator, name: InternerIdx, typ: Type, initialized: bool) error{TooManyLocals}!Instruction.Variable {
        if (self.variables.count() - self.offset > 255) {
            return error.TooManyLocals;
        }

        self.variables.put(allocator, name, .{ .typ = typ, .depth = self.depth, .initialized = initialized }) catch oom();
        return .{ .index = self.variables.count(), .scope = if (self.depth == 0) .global else .local };
    }

    pub fn isInScope(self: *const Scope, name: InternerIdx) bool {
        return if (self.variables.get(name)) |v| v.depth == self.depth else false;
    }
};

fn declareVariable(self: *Self, name: InternerIdx, typ: Type, initialized: bool, span: Span) Error!Instruction.Variable {
    return self.scope.declare(self.allocator, name, typ, initialized) catch self.err(.too_many_locals, span);
}

const Context = struct {
    side: enum { lhs, rhs } = .lhs,
    fn_type: Type = .void,
    struct_type: Type = .void,
    ref_count: bool = false,
    cow: bool = false,
    allow_partial: bool = false,

    pub fn reset(self: *Context) void {
        self.* = .{};
    }
};

const Variable = struct {
    /// Variable's type
    typ: Type = .void,
    /// Depth
    depth: usize = 0,
    /// Is initialized
    initialized: bool = false,
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
instructions: MultiArrayList(Instruction),
tm: TM2,

cached_names: struct { empty: usize, main: usize, std: usize, self: usize, Self: usize, init: usize },

pub fn init(self: *Self, allocator: Allocator, interner: *Interner, repl: bool) void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();
    self.interner = interner;
    self.scope = .empty;
    self.errs = .{};
    self.warns = .{};
    self.instructions = .{};
    self.tm = TM2.init(allocator);
    self.tm.declareBuiltinTypes(interner);

    self.cached_names = .{
        .empty = self.interner.intern(""),
        .main = self.interner.intern("main"),
        .std = self.interner.intern("std"),
        .self = self.interner.intern("self"),
        .Self = self.interner.intern("Self"),
        .init = self.interner.intern("init"),
    };

    if (repl) self.scope.depth = 1;
}

pub fn deinit(self: *Self) void {
    self.tm.deinit();
}

fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    self.errs.append(self.allocator, AnalyzerReport.err(kind, span)) catch oom();
    return error.Err;
}

/// Adds a new instruction and add it's `start` field and returns its index.
fn addInstr(self: *Self, data: Instruction.Data) void {
    self.instructions.append(self.allocator, .{ .data = data }) catch oom();
}

/// Reserves an empty slot and returns its index
fn reserveInstr(self: *Self) usize {
    return self.instructions.addOne(self.allocator) catch oom();
}

/// Sets the instruction at the given idnex
fn setInstr(self: *Self, index: usize, data: Instruction.Data) void {
    self.instructions.set(index, .{ .data = data });
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

        if (node_type != .void) {
            self.err(.unused_value, self.ast.getSpan(node)) catch {};
        }
    }

    return;
    // In REPL mode, no need for main function
    // if (self.repl)
    //     return
    // else if (self.main == null) self.err(.no_main, .{ .start = 0, .end = 0 }) catch {};
}

fn analyzeNode(self: *Self, node: *const Node, ctx: *Context) Error!Type {
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
        // .fn_decl => |*n| _ = try self.fnDeclaration(n),
        // .multi_var_decl => |*n| try self.multiVarDecl(n),
        .print => |n| try self.print(n, ctx),
        // .struct_decl => |*n| try self.structDecl(n),
        // .use => |*n| try self.use(n),
        .var_decl => |*n| try self.varDeclaration(n, ctx),
        // .@"while" => |*n| try self.whileStmt(n),
        .expr => |n| return self.analyzeExpr(n, ctx),
        else => unreachable,
    }

    return .void;
}

fn print(self: *Self, expr: *const Expr, ctx: *Context) Error!void {
    self.addInstr(.{ .print = undefined });
    const typ = try self.analyzeExpr(expr, ctx);

    if (typ == .void) {
        return self.err(.void_print, self.ast.getSpan(expr));
    }
}

fn varDeclaration(self: *Self, node: *const Ast.VarDecl, ctx: *Context) !void {
    const name = try self.internIfNotInScope(node.name);
    var checked_type = try self.checkAndGetType(node.typ, ctx);
    const index = self.reserveInstr();

    var initialized = false;
    var data = Instruction.VarDecl{ .variable = undefined };

    if (node.value) |value| {
        data.has_value = true;
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
        data.cast = coherence.cast;
    }

    // data.variable = try self.declareVariable(name, checked_type, initialized, index, .variable, node.name);
    data.variable = try self.declareVariable(name, checked_type, checked_type != .void, self.ast.getSpan(node.name));
    self.setInstr(index, .{ .var_decl = data });

    // if (data.variable.scope == .global) {
    //     self.addSymbol(name, checked_type);
    // }
}

fn analyzeExpr(self: *Self, expr: *const Expr, ctx: *Context) Error!Type {
    return switch (expr.*) {
        // .array => |*e| self.array(e),
        // .array_access => |*e| self.arrayAccess(e),
        // .block => |*e| self.block(e),
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

fn literal(self: *Self, expr: *const Ast.Literal, _: *Context) Error!Type {
    const text = self.ast.toSource(expr);

    switch (expr.tag) {
        .bool => {
            self.addInstr(.{ .bool = self.ast.token_tags[expr.idx] == .true });
            return .bool;
        },
        .identifier, .self => {
            const name_interned = self.interner.intern(text);
            self.scope.variables.getPtr(name_interned);
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

/// Checks if identifier name is already declared, otherwise interns it and returns the key
fn internIfNotInScope(self: *Self, token: usize) !usize {
    const name = self.interner.intern(self.ast.toSource(token));

    if (self.scope.isInScope(name)) return self.err(
        .{ .already_declared = .{ .name = self.interner.getKey(name).? } },
        self.ast.getSpan(token),
    );

    return name;
}

/// Checks that the node is a declared type and return it's value. If node is
/// `empty`, returns `void`
fn checkAndGetType(self: *Self, typ: ?*const Ast.Type, ctx: *const Context) Error!Type {
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

            return self.tm.declared.get(interned) orelse {
                if (interned == self.cached_names.Self) {
                    if (ctx.struct_type == .void) {
                        return self.err(.big_self_outside_struct, self.ast.getSpan(t));
                    }

                    return ctx.struct_type;
                } else return self.err(.{ .undeclared_type = .{ .found = self.ast.toSource(t) } }, self.ast.getSpan(t));
            };
        },
        .self => .self,
    } else .void;
}

const TypeCoherence = struct { type: Type = .void, cast: bool = false };

/// Checks for `void` values, array inference, cast and function type generation
/// The goal is to see if the two types are equivalent and if so, make the transformations needed
fn performTypeCoercion(self: *Self, decl: Type, value: Type, emit_cast: bool, span: Span) Error!TypeCoherence {
    var cast = false;
    var local_decl = decl;
    const local_value = value;

    if (value == .void) return self.err(.void_value, span);

    // if (value.is(.array)) {
    //     local_value = try self.inferArrayType(decl, value, span);
    // }

    // if (local_decl == .void) {
    //     // For functions, we get an anonymus type from it to not point to the same type infos
    //     // This is made to avoid getting like default values that can't be used on
    //     local_decl = try self.type_manager.createBoundedFnType(self.allocator, local_value);
    // } else
    // if (!self.isTypeEqual(local_decl, local_value)) {

    if (local_decl == .void) {
        local_decl = local_value;
    } else {
        if (!local_decl.equal(&local_value)) {
            // One case in wich we can coerce, int -> float
            // if (self.checkCast(local_decl, local_value, emit_cast)) {
            if (local_decl.cast(&local_value)) {
                cast = true;
                if (emit_cast) self.addInstr(.{ .cast = .float });
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

/// Helpers used for errors
fn getTypeName(self: *const Self, typ: Type) []const u8 {
    // if (typ.is(.function)) {
    //     return self.getFnTypeName(typ) catch oom();
    // } else if (typ.is(.array)) {
    //     return self.getArrayTypeName(typ) catch oom();
    // } else {
    const index = self.tm.getInternerIndex(typ);
    return self.interner.getKey(index).?;
    // }
}
