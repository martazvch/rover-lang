const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const AutoHashMap = std.AutoHashMap;

const Interner = @import("../interner.zig").Interner;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const BA = @import("builtins_analyzer.zig");
const BuiltinAnalyzer = BA.BuiltinAnalyzer;
const FnDeclaration = BA.FnDeclaration;
const builtin_init = BA.init;
// const Node = @import("ast_.zig").Node;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Expr = Ast.Expr;
const Rir = @import("rir.zig");
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Span = @import("Lexer.zig").Span;
// const Token = @import("Lexer.zig").Token;
// const TokenIndex = @import("ast.zig").TokenIndex;
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;
const TypeInfo = TypeSys.TypeInfo;

// Re-export constants
pub const TypeManager = struct {
    declared: std.AutoHashMap(usize, Type),
    type_infos: ArrayList(TypeInfo),
    natives: BuiltinAnalyzer = builtin_init(),

    const Error = error{TooManyTypes} || std.fmt.BufPrintError || Allocator.Error;

    pub fn init(allocator: Allocator) TypeManager {
        return .{
            .declared = .init(allocator),
            .type_infos = .init(allocator),
        };
    }

    pub fn init_builtins(self: *TypeManager, interner: *Interner) !void {
        try self.declared.put(try interner.intern("void"), .void);
        try self.declared.put(try interner.intern("null"), .null);
        try self.declared.put(try interner.intern("bool"), .bool);
        try self.declared.put(try interner.intern("float"), .float);
        try self.declared.put(try interner.intern("int"), .int);
        try self.declared.put(try interner.intern("str"), .str);
        try self.declared.put(try interner.intern("Self"), .self);
    }

    pub fn deinit(self: *TypeManager) void {
        self.declared.deinit();
        self.type_infos.deinit();
    }

    /// Adds information about a type. Requires the kind and extra info, the value (aka
    /// index in information array) is computed in the function.
    /// Returns the complete type
    pub fn reserve_info(self: *TypeManager) !TypeSys.Value {
        try self.type_infos.append(undefined);
        const count = self.type_infos.items.len - 1;

        return if (count == std.math.maxInt(TypeSys.Value))
            error.TooManyTypes
        else
            @intCast(count);
    }

    /// Set type information at a specific index in list (index gave by *reserve_info* method)
    pub fn set_info(self: *TypeManager, index: usize, info: TypeInfo) void {
        self.type_infos.items[index] = info;
    }

    /// Adds a type linked associated with the name
    pub fn add_type(self: *TypeManager, name: usize, typ: Type) !void {
        try self.declared.put(name, typ);
    }

    /// Declares a new type built with `kind` and `extra` parameters and add the informations
    pub fn declare(self: *TypeManager, name: usize, kind: TypeSys.Kind, extra: TypeSys.Extra, info: TypeInfo) !TypeSys.Type {
        const count = self.type_infos.items.len;

        // Error
        if (count == std.math.maxInt(TypeSys.Value)) return error.TooManyTypes;

        const typ = TypeSys.create(kind, extra, @intCast(count));
        try self.type_infos.append(info);

        try self.declared.put(name, typ);

        return typ;
    }

    /// Checks if the type has already been declared
    pub fn is_declared(self: *const TypeManager, typ: usize) bool {
        return self.declared.get(typ) != null;
    }

    /// Use natives function whose informations are gathered at compile time. Import the
    /// informations among other declared types
    pub fn import_natives(self: *TypeManager, name: []const u8) !?std.StaticStringMap(FnDeclaration) {
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

// source: []const u8,
// token_tags: []const Token.Tag,
// token_spans: []const Span,
// node_tags: []const Node.Tag,
// node_mains: []const TokenIndex,
// node_data: []const usize,
// node_ends: []const Node.Index,
// node_idx: usize,
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
// main: ?Node.Index,
main: ?*Node,
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
const Error = error{ Err, Overflow } || TypeManager.Error || Allocator.Error;

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
    pub fn to_var(self: *const Variable) Instruction.Variable {
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

pub fn init(self: *Self, allocator: Allocator, repl: bool) !void {
    self.arena = std.heap.ArenaAllocator.init(allocator);
    self.allocator = self.arena.allocator();

    self.instructions = .{};
    self.warns = .init(self.allocator);
    self.errs = .init(self.allocator);
    self.globals = .init(self.allocator);
    self.locals = .init(self.allocator);
    // self.node_idx = 0;
    self.scope_depth = 0;
    self.heap_count = 0;

    // self.states = .init(self.allocator);
    self.state = .{};
    self.main = null;
    self.local_offset = 0;
    self.type_manager = TypeManager.init(self.allocator);
    self.interner = Interner.init(self.allocator);

    self.main_interned = try self.interner.intern("main");
    self.std_interned = try self.interner.intern("std");
    self.self_interned = try self.interner.intern("self");
    self.init_interned = try self.interner.intern("init");

    try self.type_manager.init_builtins(&self.interner);
    self.repl = repl;
}

/// For REPL
pub fn reinit(self: *Self) void {
    // self.node_idx = 0;
    self.scope_depth = 0;
    self.local_offset = 0;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub fn analyze(
    self: *Self,
    ast: *const Ast,
    // source: []const u8,
    // tokens: *const MultiArrayList(Token),
    // nodes: *const MultiArrayList(Node),
) !void {
    // self.source = source;
    // self.token_tags = tokens.items(.tag);
    // self.token_spans = tokens.items(.span);
    // self.node_tags = nodes.items(.tag);
    // self.node_mains = nodes.items(.main);
    // self.node_data = nodes.items(.data);
    // self.node_ends = nodes.items(.end);
    self.ast = ast;

    for (ast.nodes) |*node| {
        const node_type = self.analyzeNode(node) catch |e| {
            switch (e) {
                error.TooManyTypes => return self.err(.TooManyTypes, ast.toSpan(node)),
                else => return e,
            }
        };

        if (node_type != .void) {
            self.err(.UnusedValue, ast.toSpan(node)) catch {};
        }
    }

    // while (self.node_idx < self.node_data.len) {
    //     const start = self.node_idx;
    //
    //     const node_type = self.analyze_node(self.node_idx) catch |e| {
    //         switch (e) {
    //             // If it's our own error, we continue
    //             error.Err => {
    //                 self.node_idx = self.node_ends[start];
    //                 continue;
    //             },
    //             error.TooManyTypes => return self.err(.TooManyTypes, self.to_span(self.node_idx)),
    //             else => return e,
    //         }
    //     };
    //
    //     if (node_type != .void) {
    //         self.err(.UnusedValue, self.to_span(start)) catch {};
    //     }
    // }

    // In REPL mode, no need for main function
    if (self.repl)
        return
    else if (self.main == null) self.err(.NoMain, .{ .start = 0, .end = 0 }) catch {};
}

fn analyzeNode(self: *Self, node: *const Node) Error!Type {
    // if (self.scope_depth == 0 and !self.repl and !self.is_pure(node)) {
    //     // TODO: add block, not allowed to have local scopes in global scope
    //     return if (self.node_tags[node] == .@"return")
    //         self.err(.ReturnOutsideFn, self.to_span(node))
    //     else
    //         self.err(.UnpureInGlobal, self.to_span(node));
    // }

    var final: Type = .void;

    switch (node.*) {
        .expr => |e| final = try self.analyzeExpr(e),
        else => {},
    }

    // switch (self.node_tags[node]) {
    //     .Add, .@"and", .Div, .Mul, .@"or", .Sub, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => final = try self.binop(node),
    //     .Assignment => try self.assignment(node),
    //     .Block => final = try self.block(node),
    //     .Bool => final = try self.bool_lit(node),
    //     .call => final = try self.call(node),
    //     .Discard => try self.discard(node),
    //     .Empty => self.node_idx += 1,
    //     .field => final = try self.field(),
    //     .Float => final = try self.float_lit(node),
    //     .FnDecl => try self.fn_declaration(node),
    //     .Grouping => {
    //         self.node_idx += 1;
    //         final = try self.analyze_node(self.node_idx);
    //     },
    //     .Identifier => final = (try self.identifier(node, true)).typ,
    //     .@"if" => final = try self.if_expr(node),
    //     .Int => final = try self.int_lit(node),
    //     .MultiVarDecl => try self.multi_var_decl(node),
    //     .null => final = try self.null_lit(),
    //     .print => try self.print(node),
    //     .@"return" => final = try self.return_expr(node),
    //     .string => final = try self.string(node),
    //     .StructDecl => final = try self.structure(node),
    //     .struct_literal => final = try self.struct_literal(node),
    //     .Unary => final = try self.unary(node),
    //     .use => try self.use(node),
    //     .VarDecl => try self.var_decl(node),
    //     .@"while" => try self.while_stmt(node),
    //     .Parameter, .self, .count, .Type => unreachable,
    // }

    return final;
}

fn analyzeExpr(self: *Self, expr: *const Expr) Error!Type {
    var final: Type = .void;

    switch (expr.*) {
        .literal => |*e| final = try self.literal(e),
        else => {},
    }

    return final;
}

fn toSource(self: *const Self, node: anytype) []const u8 {
    return self.ast.toSource(@ptrCast(node));
}

fn literal(self: *Self, expr: *const Ast.Literal) Error!Type {
    const span = self.ast.tokens.items(.span)[expr.idx];
    const text = self.ast.source[span.start..span.end];

    switch (expr.tag) {
        .bool => {
            _ = try self.addInstr(.{ .tag = .Bool, .data = .{
                .Bool = if (self.ast.tokens.items(.tag)[expr.idx] == .true) true else false,
            } });

            return .bool;
        },
        .int => {
            const value = std.fmt.parseInt(isize, text, 10) catch blk: {
                // TODO: error handling, only one possible it's invalid char
                std.debug.print("Error parsing integer\n", .{});
                break :blk 0;
            };
            _ = try self.addInstr(.{ .tag = .Int, .data = .{ .Int = value } });

            return .int;
        },
        .float => {
            const value = std.fmt.parseFloat(f64, text) catch blk: {
                // TODO: error handling, only one possible it's invalid char or too big
                std.debug.print("Error parsing float\n", .{});
                break :blk 0.0;
            };
            _ = try self.addInstr(.{ .tag = .Float, .data = .{ .Float = value } });

            return .float;
        },
        .null => {
            _ = try self.addInstr(.{ .tag = .Null, .data = undefined });

            return .null;
        },
        .string => {
            // Removes the quotes
            const value = try self.interner.intern(text[1 .. text.len - 1]);
            _ = try self.addInstr(.{ .tag = .String, .data = .{ .Id = value } });

            return .str;
        },
        else => unreachable,
    }
}

fn get_fn_type_name(self: *const Self, typ: Type) ![]const u8 {
    const value = TypeSys.get_value(typ);
    const decl = self.type_manager.type_infos.items[value].func;

    var res: std.ArrayListUnmanaged(u8) = .{};
    var writer = res.writer(self.allocator);
    try writer.writeAll("fn(");

    for (0..decl.arity) |i| {
        try writer.print("{s}{s}", .{
            self.interner.get_key(decl.params[i].to_idx()).?,
            if (i < decl.arity - 1) ", " else "",
        });
    }
    try writer.print(") -> {s}", .{self.get_type_name(decl.return_type)});

    return try res.toOwnedSlice(self.allocator);
}

/// Adds a new instruction and add it's `start` field and returns its index.
fn addInstr(self: *Self, instr: Instruction) !usize {
    try self.instructions.append(self.allocator, .{
        .tag = instr.tag,
        .data = instr.data,
    });
    return self.instructions.len - 1;
}

fn is_numeric(t: Type) bool {
    return t == .int or t == .float;
}

fn err(self: *Self, kind: AnalyzerMsg, span: Span) Error {
    try self.errs.append(AnalyzerReport.err(kind, span));
    return error.Err;
}

fn warn(self: *Self, kind: AnalyzerMsg, span: Span) !void {
    try self.warns.append(AnalyzerReport.warn(kind, span));
}

fn source_from_node(self: *const Self, node: Node.Index) []const u8 {
    const span = self.token_spans[self.node_mains[node]];
    return self.source[span.start..span.end];
}

// fn last_state(self: *Self) *State {
//     return &self.states.items[self.states.items.len - 1];
// }

/// Unincrement scope depth, discards all locals and return the number
/// of discarded locals
fn end_scope(self: *Self) !usize {
    self.scope_depth -= 1;

    var pop_count: usize = 0;
    // Discards all the local variables
    if (self.locals.items.len > 0) {
        var i: usize = self.locals.items.len;

        while (i > 0 and self.locals.items[i - 1].depth > self.scope_depth) {
            i -= 1;
        }

        pop_count = self.locals.items.len - i;
        try self.locals.resize(i);
    }

    return pop_count;
}

/// Checks if the variable name is in local or global scope
fn ident_in_scope(self: *const Self, name: usize) bool {
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

fn check_name(self: *Self) !usize {
    const name = try self.interner.intern(self.source_from_node(self.node_idx));

    if (self.ident_in_scope(name)) {
        return self.err(
            .{ .AlreadyDeclared = .{ .name = self.interner.get_key(name).? } },
            self.to_span(self.node_idx),
        );
    }

    self.node_idx += 1;
    return name;
}

/// Checks that the node is a declared type and return it's value. If node is
/// `empty`, returns `void`
fn check_and_get_type(self: *Self) Error!Type {
    // Function type are not declared in advance, so we declare it and return it's index
    if (self.node_tags[self.node_idx] != .Empty and self.token_tags[self.node_mains[self.node_idx]] == .@"fn") {
        return self.create_anonymus_fn_type();
    }

    defer self.node_idx += 1;

    return if (self.node_tags[self.node_idx] != .Empty)
        self.type_manager.declared.get(
            try self.interner.intern(self.source_from_node(self.node_idx)),
        ) orelse
            return self.err(
                .{ .UndeclaredType = .{ .found = self.source_from_node(self.node_idx) } },
                self.to_span(self.node_idx),
            )
    else
        .void;
}

/// Creates a type for an anonymous function, like the one defined in return types
/// of functions
fn create_anonymus_fn_type(self: *Self) Error!Type {
    const arity = self.node_data[self.node_idx];
    self.node_idx += 1;

    const type_idx = try self.type_manager.reserve_info();
    var params_type: [256]Type = undefined;

    for (0..arity) |i| {
        const param_type = try self.check_and_get_type();

        if (param_type == .void) {
            return self.err(.VoidParam, self.to_span(self.node_idx));
        }

        params_type[i] = param_type;
    }

    // Set all the informations now that we have every thing
    self.type_manager.set_info(type_idx, .{
        .func = .{
            .arity = arity,
            .params = params_type,
            .return_type = try self.check_and_get_type(),
        },
    });

    return TypeSys.create(.func, .none, type_idx);
}

/// Declares a variable either in globals or in locals based on current scope depth
fn declare_variable(
    self: *Self,
    name: usize,
    typ: Type,
    initialized: bool,
    decl_idx: usize,
    kind: Variable.Tag,
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

        try self.globals.append(variable);
        return .{ .index = @intCast(index), .scope = .global };
    } else {
        // Take function's frame into account
        const index = self.locals.items.len - self.local_offset;
        variable.index = index;

        if (index > 255) {
            // -3 because we are past the variable and its type and value
            return self.err(.TooManyLocals, self.to_span(self.node_idx - 3));
        }

        try self.locals.append(variable);
        return .{ .index = @intCast(index), .scope = .local };
    }
}

fn is_pure(self: *const Self, node: Node.Index) bool {
    // TODO: manage those
    // call: call,
    // Identifier: Identifier,
    // If: If,
    return switch (self.node_tags[node]) {
        // Skips assign node and identifier
        .Assignment => self.is_pure(node + 2),
        .Bool, .Float, .Int, .null, .string, .FnDecl, .StructDecl, .use => true,
        .Add, .Div, .Mul, .Sub, .@"and", .@"or", .Eq, .Ge, .Gt, .Le, .Lt, .Ne => {
            const lhs = self.is_pure(node + 1);
            return lhs and self.is_pure(node + 2);
        },
        .Grouping => self.is_pure(node + 1),
        .Unary => self.is_pure(node + 1),
        // Checks the value
        .VarDecl => if (self.node_tags[node + 2] == .Empty)
            true
        else
            self.is_pure(node + 2),
        else => false,
    };
}

fn assignment(self: *Self, _: Node.Index) !void {
    const last = self.state.allow_partial;
    self.state.allow_partial = false;
    errdefer self.state.allow_partial = last;

    self.node_idx += 1;
    var cast = false;

    const assigne_idx = self.node_idx;
    const idx = try self.add_instr(.{ .tag = .Assignment }, assigne_idx);

    // switch (self.node_tags[assigne_idx]) {
    //     .Identifier => {
    //         // TODO: check if this is a function's parameter (there are constant by defninition)
    //         const assigne = try self.identifier(assigne_idx, false);
    //
    //         const value_idx = self.node_idx;
    //         const value_type = try self.analyze_node(value_idx);
    //
    //         // For now, we can assign only to scalar variables
    //         // TODO: useless check?
    //         if (TypeSys.get_kind(assigne.typ) != .variable) {
    //             return self.err(.InvalidAssignTarget, self.to_span(assigne_idx));
    //         }
    //
    //         if (value_type == .void) {
    //             return self.err(.VoidAssignment, self.to_span(value_idx));
    //         }
    //
    //         // If type is unknown, we update it
    //         if (assigne.typ == .void) {
    //             assigne.typ = value_type;
    //         } else if (assigne.typ != value_type) {
    //             // One case in wich we can coerce; int -> float
    //             if (assigne.typ == .float and value_type == .int) {
    //                 cast = true;
    //                 _ = try self.add_instr(
    //                     .{ .tag = .Cast, .data = .{ .CastTo = .Float } },
    //                     assigne_idx,
    //                 );
    //             } else {
    //                 return self.err(
    //                     .{ .InvalidAssignType = .{
    //                         .expect = self.get_type_name(assigne.typ),
    //                         .found = self.get_type_name(value_type),
    //                     } },
    //                     self.to_span(assigne_idx),
    //                 );
    //             }
    //         }
    //
    //         if (!assigne.initialized) assigne.initialized = true;
    //
    //         self.instructions.items(.data)[idx] = .{ .Assignment = .{ .cast = cast } };
    //     },
    //     .field => {
    //         const assigne = try self.field();
    //
    //         const value_idx = self.node_idx;
    //         const value_type = try self.analyze_node(value_idx);
    //
    //     },
    //     // Later, manage member, pointer, ...
    //     else => return self.err(.InvalidAssignTarget, self.to_span(assigne_idx)),
    // }

    var assigne_type = switch (self.node_tags[assigne_idx]) {
        .Identifier => blk: {
            // TODO: check if this is a function's parameter (there are constant by defninition)
            const assigne = try self.identifier(assigne_idx, false);

            if (!assigne.initialized) assigne.initialized = true;

            break :blk assigne.typ;
        },
        .field => try self.field(),
        // Later, manage member, pointer, ...
        else => return self.err(.InvalidAssignTarget, self.to_span(assigne_idx)),
    };

    const value_idx = self.node_idx;
    const value_type = try self.analyze_node(value_idx);

    // For now, we can assign only to scalar variables
    // TODO: useless check?
    if (TypeSys.get_kind(assigne_type) != .variable) {
        return self.err(.InvalidAssignTarget, self.to_span(assigne_idx));
    }

    if (value_type == .void) {
        return self.err(.VoidAssignment, self.to_span(value_idx));
    }

    // If type is unknown, we update it
    if (assigne_type == .void) {
        assigne_type = value_type;
    } else if (assigne_type != value_type) {
        // One case in wich we can coerce; int -> float
        if (assigne_type == .float and value_type == .int) {
            cast = true;
            _ = try self.add_instr(
                .{ .tag = .Cast, .data = .{ .CastTo = .Float } },
                assigne_idx,
            );
        } else {
            return self.err(
                .{ .InvalidAssignType = .{
                    .expect = self.get_type_name(assigne_type),
                    .found = self.get_type_name(value_type),
                } },
                self.to_span(assigne_idx),
            );
        }
    }

    self.instructions.items(.data)[idx] = .{ .Assignment = .{ .cast = cast } };

    // Restore state
    self.state.allow_partial = last;
}

fn binop(self: *Self, node: Node.Index) Error!Type {
    const op = self.node_tags[node];
    const idx = try self.add_instr(.{ .tag = .Binop }, node);

    self.node_idx += 1;
    const lhs_index = self.node_idx;
    const lhs = try self.analyze_node(lhs_index);

    const rhs_index = self.node_idx;
    const rhs = try self.analyze_node(rhs_index);

    var res = lhs;

    // String operations
    if (op == .Add and lhs == .str and rhs == .str) {
        self.instructions.items(.data)[idx] = .{ .Binop = .{ .op = .AddStr } };
        return .str;
    } else if (op == .Mul) {
        if ((lhs == .str and rhs == .int) or (lhs == .int and rhs == .str)) {
            self.instructions.items(.data)[idx] = .{ .Binop = .{
                .cast = if (rhs == .int) .rhs else .lhs,
                .op = .MulStr,
            } };

            return .str;
        }
    }

    // Error check
    switch (op) {
        .Add, .Div, .Mul, .Sub, .Ge, .Gt, .Le, .Lt => {
            if (!is_numeric(lhs)) {
                return self.err(
                    AnalyzerMsg.invalid_arithmetic(self.get_type_name(lhs)),
                    self.to_span(lhs_index),
                );
            }

            if (!is_numeric(rhs)) {
                return self.err(
                    AnalyzerMsg.invalid_arithmetic(self.get_type_name(rhs)),
                    self.to_span(rhs_index),
                );
            }
        },
        else => {},
    }

    var data: Instruction.Binop = .{ .op = undefined };

    switch (op) {
        // Arithmetic binop
        .Add, .Div, .Mul, .Sub => {
            switch (op) {
                .Add => data.op = .AddFloat,
                .Div => data.op = .DivFloat,
                .Mul => data.op = .MulFloat,
                .Sub => data.op = .SubFloat,
                else => unreachable,
            }

            switch (lhs) {
                .float => {
                    switch (rhs) {
                        .float => {},
                        .int => {
                            try self.warn(
                                AnalyzerMsg.implicit_cast("right hand side", self.get_type_name(lhs)),
                                self.to_span(rhs_index),
                            );

                            data.cast = .rhs;
                        },
                        else => unreachable,
                    }
                },
                .int => {
                    switch (rhs) {
                        .float => {
                            try self.warn(
                                AnalyzerMsg.implicit_cast("left hand side", self.get_type_name(rhs)),
                                self.to_span(lhs_index),
                            );

                            data.cast = .lhs;
                            res = .float;
                        },
                        .int => switch (op) {
                            .Add => data.op = .AddInt,
                            .Div => data.op = .DivInt,
                            .Mul => data.op = .MulInt,
                            .Sub => data.op = .SubInt,
                            else => unreachable,
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .Eq, .Ne => {
            switch (op) {
                .Eq => data.op = switch (lhs) {
                    .bool => .EqBool,
                    .float => .EqFloat,
                    .int => .EqInt,
                    else => .EqStr,
                },
                .Ne => data.op = switch (lhs) {
                    .bool => .NeBool,
                    .float => .NeFloat,
                    .int => .NeInt,
                    else => .NeStr,
                },
                else => unreachable,
            }

            // If different value types
            if (lhs != rhs) {
                // Check for implicit casts
                if ((lhs == .int and rhs == .float) or (lhs == .float and rhs == .int)) {
                    if (lhs == .int) {
                        data.cast = .lhs;

                        try self.warn(.FloatEqualCast, self.to_span(lhs_index));
                    } else {
                        data.cast = .rhs;

                        try self.warn(.FloatEqualCast, self.to_span(rhs_index));
                    }

                    switch (op) {
                        .Eq => data.op = .EqFloat,
                        .Ne => data.op = .NeFloat,
                        else => unreachable,
                    }
                } else {
                    return self.err(
                        AnalyzerMsg.invalid_cmp(
                            self.get_type_name(lhs),
                            self.get_type_name(rhs),
                        ),
                        self.to_span(node),
                    );
                }
            } else {
                // Check for unsafe float comparisons or int comparison
                if (lhs == .float) {
                    try self.warn(.FloatEqual, self.to_span(node));
                }
            }

            res = .bool;
        },
        .Ge, .Gt, .Le, .Lt => {
            switch (op) {
                .Ge => data.op = .GeFloat,
                .Gt => data.op = .GtFloat,
                .Le => data.op = .LeFloat,
                .Lt => data.op = .LtFloat,
                else => unreachable,
            }

            switch (lhs) {
                .float => {
                    switch (rhs) {
                        .float => try self.warn(.FloatEqual, self.to_span(node)),
                        .int => {
                            try self.warn(.FloatEqualCast, self.to_span(rhs_index));

                            data.cast = .rhs;
                        },
                        else => unreachable,
                    }
                },
                .int => {
                    switch (rhs) {
                        .float => {
                            try self.warn(.FloatEqualCast, self.to_span(lhs_index));

                            data.cast = .lhs;
                        },
                        .int => switch (op) {
                            .Ge => data.op = .GeInt,
                            .Gt => data.op = .GtInt,
                            .Le => data.op = .LeInt,
                            .Lt => data.op = .LtInt,
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
            if (lhs != .bool) return self.err(.{ .InvalidLogical = .{
                .found = self.get_type_name(lhs),
            } }, self.to_span(lhs_index));

            if (rhs != .bool) return self.err(.{ .InvalidLogical = .{
                .found = self.get_type_name(rhs),
            } }, self.to_span(rhs_index));

            switch (op) {
                .@"and" => data.op = .@"and",
                .@"or" => data.op = .@"or",
                else => unreachable,
            }
        },
        else => unreachable,
    }

    self.instructions.items(.data)[idx] = .{ .Binop = data };

    return res;
}

fn block(self: *Self, node: Node.Index) Error!Type {
    const length = self.node_data[node];

    self.scope_depth += 1;
    errdefer self.scope_depth -= 1;

    const idx = try self.add_instr(.{ .tag = .Block, .data = undefined }, node);

    var final: Type = .void;
    self.node_idx += 1;

    for (0..length) |i| {
        final = try self.analyze_node(self.node_idx);

        if (final != .void and i != length - 1) {
            return self.err(.UnusedValue, self.to_span(self.node_idx));
        }
    }

    const count = try self.end_scope();

    self.instructions.items(.data)[idx] = .{ .Block = .{
        .length = length,
        .pop_count = @intCast(count),
        .is_expr = if (final != .void) true else false,
    } };

    return final;
}

// fn bool_lit(self: *Self, node: Node.Index) !Type {
//     _ = try self.add_instr(.{ .tag = .Bool, .data = .{
//         .Bool = if (self.token_tags[self.node_mains[node]] == .true) true else false,
//     } }, node);
//
//     self.node_idx += 1;
//
//     return .bool;
// }

fn call(self: *Self, node: Node.Index) Error!Type {
    const arity = self.node_data[node];
    self.node_idx += 1;

    const idx = try self.add_instr(.{ .tag = .call, .data = .{ .call = .{
        .arity = @intCast(arity),
        .builtin = undefined,
    } } }, node);

    const callee_type = try self.analyze_node(self.node_idx);

    const infos = if (TypeSys.is(callee_type, .func))
        self.type_manager.type_infos.items[TypeSys.get_value(callee_type)].func
    else if (TypeSys.is(callee_type, .@"struct")) blk: {
        const val = TypeSys.get_value(callee_type);
        const init_idx = self.type_manager.type_infos.items[val].@"struct".init;

        if (init_idx) |i| {
            break :blk self.type_manager.type_infos.items[i].func;
        } else return self.err(.StructCallButNoInit, self.to_span(node));
    } else return self.err(.InvalidCallTarget, self.to_span(node));

    if (infos.arity != arity) {
        return self.err(
            try AnalyzerMsg.wrong_args_count(infos.arity, arity),
            self.to_span(self.node_idx),
        );
    }

    return self.fn_call(idx, infos);
}

fn discard(self: *Self, node: Node.Index) !void {
    _ = try self.add_instr(.{ .tag = .Discard }, node);

    self.node_idx += 1;
    const discarded = try self.analyze_node(self.node_idx);

    if (discarded == .void) return self.err(.VoidDiscard, self.to_span(node + 1));
}

fn field(self: *Self) !Type {
    const idx = try self.add_instr(.{ .tag = .field }, self.node_idx);
    self.node_idx += 1;

    const struct_type = try self.analyze_node(self.node_idx);
    const field_idx = try self.interner.intern(self.source_from_node(self.node_idx));
    self.node_idx += 1;

    if (!TypeSys.is(struct_type, .@"struct")) {
        // TODO: error
        std.debug.print("Error, field access on a non-struct", .{});
    }

    const infos = self.type_manager.type_infos.items[TypeSys.get_value(struct_type)].@"struct";

    if (infos.fields.get(field_idx)) |f| {
        self.instructions.items(.data)[idx] = .{ .field = f.idx };

        return f.type;
    } else {
        // TODO: Error
        std.debug.print("Undeclared field during access", .{});
    }

    unreachable;
}

// fn float_lit(self: *Self, node: Node.Index) !Type {
//     const value = std.fmt.parseFloat(f64, self.source_from_node(node)) catch blk: {
//         // TODO: error handling, only one possible it's invalid char or too big
//         std.debug.print("Error parsing float\n", .{});
//         break :blk 0.0;
//     };
//
//     _ = try self.add_instr(
//         .{ .tag = .Float, .data = .{ .Float = value } },
//         node,
//     );
//     self.node_idx += 1;
//
//     return .float;
// }
fn fn_call(self: *Self, instr: usize, infos: TypeSys.FnInfo) Error!Type {
    self.instructions.items(.data)[instr].call.builtin = infos.builtin;

    for (0..infos.arity) |i| {
        const arg_idx = self.node_idx;
        const arg_type = try self.analyze_node(arg_idx);

        if (arg_type != infos.params[i] and !self.check_equal_fn_types(arg_type, infos.params[i])) {
            // If it's an implicit cast between int and float, save the
            // argument indices for compiler. Otherwise, error
            if (infos.params[i] == .float and arg_type == .int) {
                _ = try self.add_instr(
                    .{ .tag = .Cast, .data = .{ .CastTo = .Float } },
                    arg_idx,
                );
            } else {
                return self.err(
                    .{ .TypeMismatch = .{
                        .expect = self.get_type_name(infos.params[i]),
                        .found = self.get_type_name(arg_type),
                    } },
                    self.to_span(arg_idx),
                );
            }
        }
    }

    return infos.return_type;
}

/// Checks if an expression if of a certain type kind and returns the associated value or error
fn expect_type_kind(self: *Self, node: Node.Index, kind: TypeSys.Kind) !TypeSys.Value {
    const expr_type = try self.analyze_node(node);

    return if (TypeSys.is(expr_type, kind))
        TypeSys.get_value(expr_type)
    else
        self.err(
            .{ .TypeMismatch = .{
                .expect = TypeSys.str_kind(kind),
                .found = TypeSys.str_kind(TypeSys.get_kind(expr_type)),
            } },
            self.to_span(node),
        );
}

fn fn_declaration(self: *Self, node: Node.Index) Error!void {
    const save_local_offset = self.local_offset;
    const save_scope_depth = self.scope_depth;
    const save_state = self.state;

    errdefer {
        self.local_offset = save_local_offset;

        if (self.scope_depth > save_scope_depth) {
            for (0..self.scope_depth - save_scope_depth) |_| {
                _ = self.end_scope() catch @panic("oom");
            }
        }

        self.scope_depth = save_scope_depth;
        self.state = save_state;
    }

    const name_idx = try self.check_name();

    if (self.main == null and self.scope_depth == 0 and name_idx == self.main_interned) {
        self.main = self.instructions.len;
    }

    // Check in current scope
    const arity = self.node_data[node];
    const fn_idx = try self.add_instr(.{ .tag = .FnDecl, .data = undefined }, node);
    // We add function's name for runtime access
    _ = try self.add_instr(.{ .tag = .Name, .data = .{ .Id = name_idx } }, node);

    // We declare before body for recursion and before parameters to put it as the first local
    const type_idx = try self.type_manager.reserve_info();
    const fn_type = TypeSys.create(.func, .none, type_idx);
    const fn_var = try self.declare_variable(name_idx, fn_type, true, self.instructions.len, .func);

    _ = try self.add_instr(.{ .tag = .VarDecl, .data = .{ .VarDecl = .{ .variable = fn_var, .cast = false } } }, node);

    self.scope_depth += 1;
    self.local_offset = self.locals.items.len;

    // We put back allow partial at the beginning of the function in case we are the last statement of an enclosing
    // function, which has the effect of putting allow_partial to false
    self.state.in_fn = true;
    self.state.allow_partial = true;

    // We add a empty variable to anticipate the function it self on the stack
    // it's the returned address for the function
    try self.locals.append(.{ .depth = self.scope_depth, .name = name_idx, .typ = fn_type, .initialized = true, .kind = .func });

    // TODO: ArrayList with init capacity of arity
    var params_type: [256]Type = undefined;
    var param_start: usize = 0;

    // Self parameter
    if (self.node_tags[self.node_idx] == .self) {
        if (!self.state.in_struct) {
            return self.err(.SelfOutsideStruct, self.to_span(self.node_idx));
        }

        if (name_idx == self.init_interned) {
            return self.err(.SelfInInit, self.to_span(self.node_idx));
        }

        params_type[0] = .self;
        param_start = 1;
        self.node_idx += 1;
        _ = try self.declare_variable(2, .self, true, self.node_idx, .param);
    }

    for (param_start..arity) |i| {
        const decl = self.node_idx;

        const param_idx = self.check_name() catch |e| {
            self.errs.items[self.errs.items.len - 1] = AnalyzerReport.err(
                .{ .DuplicateParam = .{ .name = self.source_from_node(decl) } },
                self.to_span(decl),
            );
            return e;
        };

        const param_type = try self.check_and_get_type();

        if (param_type == .void) {
            return self.err(.VoidParam, self.to_span(decl));
        }

        _ = try self.declare_variable(param_idx, param_type, true, decl, .param);
        params_type[i] = param_type;
    }

    const return_type_idx = self.node_idx;
    const return_type = try self.check_and_get_type();

    if (name_idx == self.init_interned and return_type != .self) {
        return self.err(.NonSelfInitReturn, self.to_span(return_type_idx));
    }

    self.state.fn_type = return_type;

    self.type_manager.set_info(type_idx, .{ .func = .{
        .arity = arity,
        .params = params_type,
        .return_type = return_type,
    } });

    // ------
    //  Body
    // ------
    self.scope_depth += 1;
    const block_idx = self.node_idx;
    const length = self.node_data[block_idx];

    // We don't use block because we don't want to emit extra data from the block
    self.node_idx += 1;
    var start = self.node_idx;
    var body_err = false;
    var body_type: Type = .void;
    var deadcode_start: usize = 0;
    var deadcode_count: usize = 0;

    for (0..length) |i| {
        // If previous statement returned, it's only dead code now
        if (deadcode_start == 0 and self.state.returns) {
            try self.warn(.DeadCode, self.to_span(start));
            deadcode_start = self.instructions.len;
            deadcode_count = length - i;
        }

        // If last statement, we don't allow partial anymore (for return)
        // Usefull for 'if' for example, in this case we want all the branches
        // to return something
        if (i == length - 1) self.state.allow_partial = false;

        // We try to analyze the whole body
        start = self.node_idx;
        const typ = self.analyze_node(self.node_idx) catch |e| switch (e) {
            error.Err => {
                body_err = true;
                self.node_idx = self.node_ends[start];
                continue;
            },
            else => return e,
        };

        // If we analyze dead code, we don't update the type
        if (deadcode_start == 0) body_type = typ;

        // If last expression produced a value and that it wasn't the last one and it
        // wasn't a return, error
        if (body_type != .void and i != length - 1 and !self.state.returns) {
            self.err(.UnusedValue, self.to_span(start)) catch {};
        }
    }

    if (!body_err and body_type != return_type and !self.check_equal_fn_types(body_type, return_type)) {
        return self.err(
            .{ .IncompatibleFnType = .{
                .expect = self.get_type_name(return_type),
                .found = self.get_type_name(body_type),
            } },
            // If the block was empty, we're on 'FnDeclEnd' so we take the span
            // of the previous node
            self.to_span(if (length == 0) start - 1 else start),
        );
    }

    // We strip unused instructions for them not to be compiled
    // TODO: test
    if (deadcode_start > 0)
        self.instructions.shrinkRetainingCapacity(deadcode_start);

    // Two levels: 1 for function's name + params and another one for body
    _ = try self.end_scope();
    _ = try self.end_scope();

    // Switch back to locals before function call
    self.local_offset = save_local_offset;

    const return_kind: ReturnKind = if (self.state.returns)
        .explicit
    else if (body_type == .void)
        .implicit_void
    else
        .implicit_value;

    self.state = save_state;

    self.instructions.items(.data)[fn_idx] = .{ .FnDecl = .{
        .body_len = length - deadcode_count,
        .return_kind = return_kind,
    } };
}

/// Checks if two function types are equal. Functions' type depend on the index
/// where their infos are in the type manager, so they could be the same type
/// but with different indices. This is due to anonymus function type (like return
/// types in function definitions)
fn check_equal_fn_types(self: *const Self, t1: Type, t2: Type) bool {
    if (t1 == t2) return true;
    if (!TypeSys.is(t1, .func) or !TypeSys.is(t2, .func)) return false;

    const v1 = TypeSys.get_value(t1);
    const infos1 = self.type_manager.type_infos.items[v1].func;

    const v2 = TypeSys.get_value(t2);
    const infos2 = self.type_manager.type_infos.items[v2].func;

    if (infos1.arity == infos2.arity and infos1.return_type == infos2.return_type) {
        for (0..infos1.arity) |i| {
            if (infos1.params[i] != infos2.params[i]) return false;
        }

        return true;
    }

    return false;
}

fn identifier(self: *Self, node: Node.Index, initialized: bool) Error!*Variable {
    const variable = try self.resolve_identifier(node, initialized);

    if (variable.kind == .normal) {
        self.check_capture(variable);
        _ = try self.add_instr(.{ .tag = .IdentifierId, .data = .{ .Id = variable.decl } }, node);
    } else {
        // Params and imports aren't declared so we can't reference them, they just live on stack
        // TODO: scope can't be 'heap'? Just use variable.scope()? Create a to() method?
        _ = try self.add_instr(
            .{ .tag = .Identifier, .data = .{ .Variable = .{
                .index = @intCast(variable.index),
                .scope = if (variable.depth > 0) .local else .global,
            } } },
            node,
        );
    }

    return variable;
}

/// Checks if a variable is in local scope, enclosing scope or global scope. Check if its state
/// is `initialized`, otherwise return an error.
fn resolve_identifier(self: *Self, node: Node.Index, initialized: bool) Error!*Variable {
    self.node_idx += 1;
    const name = self.source_from_node(node);
    const name_idx = try self.interner.intern(name);

    // We first check in locals
    if (self.locals.items.len > 0) {
        var idx = self.locals.items.len;

        while (idx > 0) : (idx -= 1) {
            const local = &self.locals.items[idx - 1];

            if (name_idx == local.name) {
                // Checks the initialization if asked
                if (initialized and !local.initialized) {
                    return self.err(
                        .{ .UseUninitVar = .{ .name = self.interner.get_key(name_idx).? } },
                        self.to_span(node),
                    );
                }

                return local;
            }
        }
    }

    // TODO: in reverse? People tend to use latest declared variables
    for (self.globals.items) |*glob| {
        if (name_idx == glob.name) {
            if (initialized and !glob.initialized) {
                return self.err(
                    .{ .UseUninitVar = .{ .name = self.interner.get_key(name_idx).? } },
                    self.to_span(node),
                );
            }

            return glob;
        }
    }

    // Else, it's undeclared
    return self.err(
        .{ .UndeclaredVar = .{ .name = self.interner.get_key(name_idx).? } },
        self.to_span(node),
    );
}

/// Check if the variable needs to be captured and captures it if so
fn check_capture(self: *Self, variable: *Variable) void {
    // If it's a global variable or if it's been declared in current function's frame
    // or already captured, return
    if (variable.index >= self.local_offset or variable.depth == 0 or variable.captured) return;

    variable.captured = true;
    variable.index = self.heap_count;
    self.instructions.items(.data)[variable.decl].VarDecl.variable.scope = .heap;
    // TODO: protect the cast?
    self.instructions.items(.data)[variable.decl].VarDecl.variable.index = @intCast(self.heap_count);
    self.heap_count += 1;
}

fn if_expr(self: *Self, node: Node.Index) Error!Type {
    const idx = try self.add_instr(.{ .tag = .@"if", .data = undefined }, node);
    self.node_idx += 1;
    var data: Instruction.@"if" = .{ .cast = .none, .has_else = false };

    const cond_idx = self.node_idx;
    const cond_type = try self.analyze_node(self.node_idx);

    // We can continue to analyze if the condition isn't a bool
    if (cond_type != .bool) self.err(
        .{ .NonBoolCond = .{
            .what = "if",
            .found = self.get_type_name(cond_type),
        } },
        self.to_span(cond_idx),
    ) catch {};

    var then_return: bool = false;
    var else_return: bool = false;

    const then_idx = self.node_idx;
    const then_type = try self.analyze_node(self.node_idx);
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
    const else_idx = self.node_idx;

    if (self.node_tags[else_idx] != .Empty) {
        data.has_else = true;

        else_type = try self.analyze_node(else_idx);

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

                try self.warn(
                    AnalyzerMsg.implicit_cast("then branch", "float"),
                    self.to_span(then_idx),
                );
            } else if (then_type == .float and else_type == .int) {
                data.cast = .@"else";

                // Safe unsafe access, if there is a non void type
                // there is an else body
                try self.warn(
                    AnalyzerMsg.implicit_cast("else branch", "float"),
                    self.to_span(else_idx),
                );
            } else return self.err(
                .{ .IncompatibleIfType = .{
                    .found1 = self.get_type_name(then_type),
                    .found2 = self.get_type_name(else_type),
                } },
                self.to_span(node),
            );
        }
    } else if (then_type != .void and !self.state.allow_partial) {
        // Skips the Empty
        self.node_idx += 1;

        return self.err(
            .{ .MissingElseClause = .{ .if_type = self.get_type_name(then_type) } },
            self.to_span(node),
        );
    } else self.node_idx += 1;

    self.instructions.items(.data)[idx] = .{ .@"if" = data };

    return final_type;
}

// fn int_lit(self: *Self, node: Node.Index) !Type {
//     const value = std.fmt.parseInt(isize, self.source_from_node(node), 10) catch blk: {
//         // TODO: error handling, only one possible it's invalid char
//         std.debug.print("Error parsing integer\n", .{});
//         break :blk 0;
//     };
//
//     _ = try self.add_instr(.{ .tag = .Int, .data = .{ .Int = value } }, node);
//     self.node_idx += 1;
//
//     return .int;
// }

fn multi_var_decl(self: *Self, node: Node.Index) !void {
    const count = self.node_mains[node];
    self.node_idx += 1;
    _ = try self.add_instr(.{ .tag = .MultipleVarDecl, .data = .{ .Id = count } }, self.node_idx);

    for (0..count) |_| {
        try self.var_decl(self.node_idx);
    }
}

// fn null_lit(self: *Self) !Type {
//     _ = try self.add_instr(.{ .tag = .null, .data = undefined }, self.node_idx);
//     self.node_idx += 1;
//
//     return .null;
// }

fn print(self: *Self, _: Node.Index) !void {
    _ = try self.add_instr(.{ .tag = .print, .data = undefined }, self.node_idx);
    self.node_idx += 1;
    const expr_idx = self.node_idx;
    const typ = try self.analyze_node(self.node_idx);

    if (typ == .void)
        return self.err(.VoidPrint, self.to_span(expr_idx));
}

fn return_expr(self: *Self, node: Node.Index) Error!Type {
    self.node_idx += 1;

    const idx = try self.add_instr(.{ .tag = .@"return", .data = .{ .@"return" = .{
        .value = false,
        .cast = false,
    } } }, node);

    const value_idx = self.node_idx;

    const return_type = if (self.node_tags[self.node_idx] != .Empty) blk: {
        self.instructions.items(.data)[idx].@"return".value = true;
        break :blk try self.analyze_node(self.node_idx);
    } else blk: {
        self.node_idx += 1;
        break :blk .void;
    };

    // We check after to advance node idx
    if (!self.state.in_fn) {
        return self.err(.ReturnOutsideFn, self.to_span(node));
    }

    if (!self.check_equal_fn_types(self.state.fn_type, return_type)) {
        if (self.state.fn_type == .float and return_type == .int) {
            self.instructions.items(.data)[idx].@"return".cast = true;
            _ = try self.add_instr(.{ .tag = .Cast, .data = .{ .CastTo = .Float } }, value_idx);
        } else return self.err(
            .{ .IncompatibleFnType = .{
                .expect = self.get_type_name(self.state.fn_type),
                .found = self.get_type_name(return_type),
            } },
            self.to_span(node),
        );
    }

    self.state.returns = true;
    return self.state.fn_type;
}

// fn string(self: *Self, node: Node.Index) !Type {
//     const source = self.source_from_node(node);
//     // Removes the quotes
//     const value = try self.interner.intern(source[1 .. source.len - 1]);
//     _ = try self.add_instr(.{ .tag = .string, .data = .{ .Id = value } }, node);
//     self.node_idx += 1;
//
//     return .str;
// }

fn structure(self: *Self, node: Node.Index) !Type {
    const name = try self.interner.intern(self.source_from_node(node));
    self.node_idx += 1;

    if (self.type_manager.is_declared(name)) {
        return self.err(.{ .AlreadyDeclaredStruct = .{ .name = self.source_from_node(node) } }, self.to_span(node));
    }

    // We forward declare for self referencing
    const type_idx = try self.type_manager.reserve_info();
    const struct_type = TypeSys.create(.@"struct", .none, type_idx);
    const struct_var = try self.declare_variable(name, struct_type, true, self.instructions.len, .@"struct");

    const struct_idx = try self.add_instr(.{ .tag = .struct_decl, .data = undefined }, node);
    // We add function's name for runtime access
    _ = try self.add_instr(.{ .tag = .Name, .data = .{ .Id = name } }, node);
    _ = try self.add_instr(.{ .tag = .VarDecl, .data = .{ .VarDecl = .{ .variable = struct_var, .cast = false } } }, node);

    self.scope_depth += 1;
    errdefer _ = self.end_scope() catch @panic("oom");

    var infos: TypeSys.StructInfo = .{ .functions = .{}, .fields = .{}, .default_value_fields = 0 };

    const fields_count = self.node_data[self.node_idx];
    try infos.fields.ensureTotalCapacity(self.allocator, @intCast(fields_count));
    self.node_idx += 1;
    var default_value_fields: usize = 0;

    for (0..fields_count) |i| {
        var field_infos: TypeSys.FieldInfo = .{ .type = undefined, .default = false, .idx = i };
        const field_name = try self.interner.intern(self.source_from_node(self.node_idx));
        self.node_idx += 1;

        if (infos.fields.get(field_name) != null) {
            // TODO: Error, already declared field
            std.debug.print("Already declared field\n", .{});
        }

        const field_type = if (self.node_tags[self.node_idx] != .Empty)
            try self.check_and_get_type()
        else blk: {
            self.node_idx += 1;
            break :blk .void;
        };

        const field_value_type = blk: {
            if (self.node_tags[self.node_idx] != .Empty) {
                if (!self.is_pure(self.node_idx)) {
                    std.debug.print("Unpure default value\n", .{});
                    // TODO: error, non-constant default value
                }

                field_infos.default = true;
                default_value_fields += 1;

                _ = try self.add_instr(.{ .tag = .field, .data = .{ .field = i } }, self.node_idx);
                break :blk try self.analyze_node(self.node_idx);
            } else {
                self.node_idx += 1;
                break :blk .void;
            }
        };

        if (field_value_type != .void and field_type != .void and field_value_type != field_type) {
            // TODO: Error
            std.debug.print("Wrong type default value\n", .{});
        }

        // Fro mparsing, we know that there is either a type or default value. If no declared type, we take
        // the one from the default value
        field_infos.type = if (field_type == .void) field_value_type else field_type;
        infos.fields.putAssumeCapacity(field_name, field_infos);
    }

    infos.default_value_fields = default_value_fields;

    const func_count = self.node_data[self.node_idx];
    try infos.functions.ensureTotalCapacity(self.allocator, @intCast(func_count));
    self.node_idx += 1;

    self.state.in_struct = true;

    for (0..func_count) |_| {
        const fn_name = try self.interner.intern(self.source_from_node(self.node_idx));
        // Function's type infos will be the first added to the manager, even if types are created
        // in function's body It's safe to save it from here
        const func_idx = self.type_manager.type_infos.items.len;
        try self.fn_declaration(self.node_idx);

        if (fn_name == self.init_interned) {
            infos.init = func_idx;
        } else infos.functions.putAssumeCapacity(fn_name, func_idx);
    }

    self.state.in_struct = false;
    const type_info: TypeInfo = .{ .@"struct" = infos };
    self.type_manager.set_info(type_idx, type_info);
    try self.type_manager.add_type(name, struct_type);

    _ = try self.end_scope();
    // _ = try self.declare_variable(name, struct_type, true, struct_idx, .@"struct");
    self.instructions.items(.data)[struct_idx] = .{ .struct_decl = .{
        .fields_count = fields_count,
        .default_fields = default_value_fields,
        .func_count = func_count,
    } };

    return .void;
}

fn struct_literal(self: *Self, node: Node.Index) !Type {
    const arity = self.node_data[self.node_idx];
    self.node_idx += 1;
    const decl = try self.resolve_identifier(self.node_idx, true);

    const struct_lit_idx = try self.add_instr(.{
        .tag = .struct_literal,
        .data = .{ .struct_literal = .{ .variable = decl.to_var(), .arity = arity, .end = 0 } },
    }, node);

    if (self.type_manager.declared.get(decl.name)) |struct_type| {
        const value = TypeSys.get_value(struct_type);
        const infos = self.type_manager.type_infos.items[value].@"struct";
        var proto = infos.proto(self.allocator);
        defer proto.deinit(self.allocator);

        const start = self.instructions.len;
        try self.instructions.ensureTotalCapacity(self.allocator, self.instructions.len + arity);

        for (0..arity) |_| {
            self.instructions.appendAssumeCapacity(.{ .tag = .field, .data = undefined });
        }

        for (0..arity) |_| {
            const field_name = try self.interner.intern(self.source_from_node(self.node_idx));

            if (infos.fields.get(field_name)) |f| {
                proto.putAssumeCapacity(field_name, true);
                self.instructions.items(.data)[start + f.idx] = .{ .field = self.instructions.len };

                // Syntax: { x } instead of { x = x }
                if (self.node_tags[self.node_idx + 1] == .Empty) {
                    // We resolve the same identifier
                    _ = try self.identifier(self.node_idx, true);
                    // Skips empty
                    self.node_idx += 1;
                } else {
                    self.node_idx += 1;
                    _ = try self.analyze_node(self.node_idx);
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
                    std.debug.print("Uninit filed: {s}\n", .{self.interner.get_key(entry.key_ptr.*).?});
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

fn unary(self: *Self, node: Node.Index) Error!Type {
    const op = self.token_tags[self.node_mains[node]];
    const idx = try self.add_instr(.{
        .tag = .Unary,
        .data = .{ .Unary = .{
            .op = if (op == .not) .bang else .minus,
            .typ = .Float,
        } },
    }, node);

    self.node_idx += 1;
    const rhs = try self.analyze_node(self.node_idx);

    if (op == .not and rhs != .bool) {
        return self.err(
            .{ .InvalidUnary = .{ .found = self.get_type_name(rhs) } },
            self.to_span(node),
        );
    } else if (op == .minus and rhs != .int and rhs != .float) {
        return self.err(
            AnalyzerMsg.invalid_arithmetic(self.get_type_name(rhs)),
            self.to_span(node),
        );
    }

    if (rhs == .int) self.instructions.items(.data)[idx].Unary.typ = .Int;

    return rhs;
}

fn use(self: *Self, node: Node.Index) !void {
    const idx = try self.add_instr(.{ .tag = .use, .data = undefined }, node);
    self.node_idx += 1;

    var count: usize = 0;
    var idx_unknown: usize = 1;

    const name = try self.interner.intern(self.source_from_node(self.node_idx));

    // For now, "std" is interned at initialization in slot 1
    self.node_idx += 1;
    if (name == self.std_interned) {
        // TODO: For now, il allows to keep synchronized the different arrays of
        // nodes/instructions
        _ = try self.add_instr(.{ .tag = .null, .data = undefined }, 0);

        // TODO: support real imports
        if (self.node_data[node] > 2) @panic("Use statements can't import more than std + one module");

        // 1 less because we parsed "std"
        for (0..self.node_data[node] - 1) |_| {
            if (try self.type_manager.import_natives(self.source_from_node(self.node_idx))) |module| {
                const all_fn_names = module.keys();

                for (all_fn_names) |fn_name| {
                    const name_idx = try self.interner.intern(fn_name);

                    // TODO: Error handling
                    const func = module.get(fn_name).?;

                    const info: TypeInfo = .{ .func = .{
                        .arity = func.arity,
                        .params = func.params,
                        .return_type = func.return_type,
                        .builtin = true,
                    } };

                    // Declare the type and additional informations
                    const typ = try self.type_manager.declare(name_idx, .func, .builtin, info);
                    // Declare the variable
                    const variable = try self.declare_variable(name_idx, typ, true, self.node_idx, .import);

                    _ = try self.add_instr(.{ .tag = .Imported, .data = .{ .Imported = .{
                        .index = func.index,
                        .variable = variable,
                    } } }, node);

                    count += 1;
                }

                self.instructions.items(.data)[idx] = .{ .use = count };
                self.node_idx += 1;

                return;
            } else {
                idx_unknown = 2;
                self.node_idx += 1;
            }
        }
    } else self.node_idx += 1;

    return self.err(
        .{ .UnknownModule = .{ .name = self.source_from_node(node + idx_unknown) } },
        self.to_span(node + idx_unknown),
    );
}

fn var_decl(self: *Self, node: Node.Index) !void {
    const name = try self.check_name();
    const type_idx = self.node_idx;
    var checked_type = try self.check_and_get_type();
    const value_idx = self.node_idx;

    const idx = try self.add_instr(.{ .tag = .VarDecl, .data = .{ .VarDecl = undefined } }, node);

    var initialized = false;
    var cast = false;

    if (self.node_tags[value_idx] != .Empty) {
        const last = self.state.allow_partial;
        self.state.allow_partial = false;

        const value_type = try self.analyze_node(value_idx);
        self.state.allow_partial = last;

        // Void assignment check
        if (value_type == .void) {
            return self.err(.VoidAssignment, self.to_span(value_idx));
        }

        // If no type declared, we infer the value type
        if (checked_type == .void) {
            checked_type = value_type;
            // Else, we check for coherence
        } else if (checked_type != value_type) {
            // One case in wich we can coerce, int -> float
            if (checked_type == .float and value_type == .int) {
                cast = true;
                _ = try self.add_instr(.{ .tag = .Cast, .data = .{ .CastTo = .Float } }, type_idx);
            } else {
                return self.err(
                    .{ .InvalidAssignType = .{
                        .expect = self.get_type_name(checked_type),
                        .found = self.get_type_name(value_type),
                    } },
                    self.to_span(value_idx),
                );
            }
        }

        initialized = true;
    } else {
        _ = try self.add_instr(.{ .tag = .null }, node);
        self.node_idx += 1;
    }

    const variable = try self.declare_variable(name, checked_type, initialized, idx, .normal);
    self.instructions.items(.data)[idx] = .{ .VarDecl = .{ .variable = variable, .cast = cast } };
}

fn while_stmt(self: *Self, _: Node.Index) Error!void {
    self.node_idx += 1;
    const cond_idx = self.node_idx;
    _ = try self.add_instr(.{ .tag = .@"while" }, cond_idx);
    const cond_type = try self.analyze_node(cond_idx);

    if (cond_type != .bool) return self.err(
        .{ .NonBoolCond = .{
            .what = "while",
            .found = self.get_type_name(cond_type),
        } },
        self.to_span(cond_idx),
    );

    const body_idx = self.node_idx;
    const body_type = try self.analyze_node(body_idx);

    if (body_type != .void) return self.err(
        .{ .NonVoidWhile = .{
            .found = self.get_type_name(body_type),
        } },
        self.to_span(body_idx),
    );
}

// fn to_span(self: *const Self, node: Node.Index) Span {
//     return switch (self.node_tags[node]) {
//         .Add, .@"and", .Div, .Mul, .@"or", .Sub, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => .{
//             .start = self.token_spans[self.node_mains[node + 1]].start,
//             .end = self.to_span(node + 2).end,
//         },
//         .Assignment => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + 1]].end,
//         },
//         .Block => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node]].start + 1,
//         },
//         .Bool, .Float, .Identifier, .Int, .null, .string => self.token_spans[self.node_mains[node]],
//         .call => self.token_spans[self.node_mains[node + 1]],
//         .Discard => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + 1]].end,
//         },
//         .Empty => self.to_span(node - 1),
//         .field => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + 1]].end,
//         },
//         .FnDecl => self.token_spans[self.node_mains[node]],
//         .Grouping => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_data[node]].end,
//         },
//         .@"if" => self.token_spans[self.node_mains[node]],
//         .Parameter => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + 1]].end,
//         },
//         .print => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.to_span(node + 1).end,
//         },
//         .@"return" => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = if (self.node_tags[node + 1] == .Empty)
//                 self.token_spans[self.node_mains[node]].end
//             else
//                 self.to_span(node + 1).end,
//         },
//         .self, .StructDecl, .Type => self.token_spans[self.node_mains[node]],
//         .struct_literal => unreachable,
//         .Unary => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + 1]].end,
//         },
//         .use => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + self.node_data[node]]].end,
//         },
//         .VarDecl => .{
//             .start = self.token_spans[self.node_mains[node]].start,
//             .end = self.token_spans[self.node_mains[node + self.node_data[node]]].end,
//         },
//         .@"while" => self.token_spans[self.node_mains[node]],
//         .MultiVarDecl, .count => unreachable,
//     };
// }
//
fn get_type_name(self: *const Self, typ: Type) []const u8 {
    if (TypeSys.is(typ, .func)) {
        return self.get_fn_type_name(typ) catch @panic("oom");
    } else {
        const idx = self.type_manager.idx(typ);
        return self.interner.get_key(idx).?;
    }
}
