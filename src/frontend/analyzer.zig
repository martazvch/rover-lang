const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const AutoHashMap = std.AutoHashMap;

const Interner = @import("../interner.zig").Interner;
const GenReport = @import("../reporter.zig").GenReport;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const BA = @import("builtins_analyzer.zig");
const BuiltinAnalyzer = BA.BuiltinAnalyzer;
const FnDeclaration = BA.FnDeclaration;
const builtin_init = BA.init;
const Node = @import("ast.zig").Node;
const Rir = @import("rir.zig");
const Scope = Rir.Scope;
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Span = @import("lexer.zig").Span;
const Token = @import("lexer.zig").Token;
const TokenIndex = @import("ast.zig").TokenIndex;
const TypeSys = @import("type_system.zig");
const Type = TypeSys.Type;
const TypeInfo = TypeSys.TypeInfo;
const Void = TypeSys.Void;
const Null = TypeSys.Null;
const Int = TypeSys.Int;
const Float = TypeSys.Float;
const Bool = TypeSys.Bool;
const Str = TypeSys.Str;

// Re-export constants
pub const TypeManager = struct {
    declared: std.AutoHashMap(usize, Type),
    type_infos: ArrayList(TypeInfo),
    builtins: BuiltinAnalyzer = builtin_init(),

    const Self = @This();
    const Error = error{TooManyTypes} || std.fmt.BufPrintError || Allocator.Error;

    pub fn init(allocator: Allocator) Self {
        return .{
            .declared = AutoHashMap(usize, Type).init(allocator),
            .type_infos = ArrayList(TypeInfo).init(allocator),
        };
    }

    pub fn init_builtins(self: *Self, interner: *Interner) !void {
        try self.declared.put(try interner.intern("void"), Void);
        try self.declared.put(try interner.intern("null"), Null);
        try self.declared.put(try interner.intern("bool"), Bool);
        try self.declared.put(try interner.intern("float"), Float);
        try self.declared.put(try interner.intern("int"), Int);
        try self.declared.put(try interner.intern("str"), Str);
    }

    pub fn deinit(self: *Self) void {
        self.declared.deinit();
        self.type_infos.deinit();
    }

    /// Adds information about a type. Requires the kind and extra info, the value (aka
    /// index in information array) is computed in the function.
    /// Returns the complete type
    pub fn reserve_info(self: *Self) !TypeSys.Value {
        try self.type_infos.append(undefined);
        const count = self.type_infos.items.len - 1;

        return if (count == std.math.maxInt(TypeSys.Value))
            error.TooManyTypes
        else
            @intCast(count);
    }

    /// Set type information at a specific index in list (index gave by *reserve_info* method)
    pub fn set_info(self: *Self, index: usize, info: TypeInfo) void {
        self.type_infos.items[index] = info;
    }

    /// Declares a new type built with `kind` and `extra` parameters and add the informations
    pub fn declare(
        self: *Self,
        name: usize,
        kind: TypeSys.Kind,
        extra: TypeSys.Extra,
        info: TypeInfo,
    ) !TypeSys.Type {
        const count = self.type_infos.items.len;

        // Error
        if (count == std.math.maxInt(TypeSys.Value)) return error.TooManyTypes;

        const typ = TypeSys.create(kind, extra, @intCast(count));
        try self.type_infos.append(info);

        try self.declared.put(name, typ);

        return typ;
    }

    /// Use builtins function whose informations are gathered at compile time. Import the
    /// informations among other declared types
    pub fn import_builtins(self: *Self, name: []const u8) !?std.StaticStringMap(FnDeclaration) {
        return self.builtins.declarations.get(name);
    }

    // NOTE:
    // Used only in error mode, no need for performance. If used in
    // performance path, maybe use a ArrayHashMap to retreive with
    // index (as type == index) but every thing else is slow?
    pub fn idx(self: *const Self, typ: Type) usize {
        var iter = self.declared.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.* == typ) {
                return entry.key_ptr.*;
            }
        }
        unreachable;
    }
};

pub const Analyzer = struct {
    source: []const u8,
    token_tags: []const Token.Tag,
    token_spans: []const Span,
    node_tags: []const Node.Tag,
    node_mains: []const TokenIndex,
    node_data: []const usize,
    node_idx: usize,

    instructions: MultiArrayList(Instruction),
    warns: ArrayList(AnalyzerReport),
    errs: ArrayList(AnalyzerReport),

    globals: ArrayList(Variable),
    locals: ArrayList(Variable),
    scope_depth: usize,
    /// Offset updated at each fn call, emulate the frame pointer at runtime
    local_offset: usize,
    main: ?Node.Index,
    states: ArrayList(State),
    type_manager: TypeManager,
    interner: Interner,

    starts: []const usize,
    start_idx: usize,

    arena: std.heap.ArenaAllocator,
    allocator: Allocator,
    repl: bool,

    const Self = @This();
    const Error = error{ Err, Overflow } || TypeManager.Error || Allocator.Error;

    // Representation of a variable. Index is the declaration order
    // NOTE: use depth: isize = -1 as uninit? Saves a bool in struct. On passerait
    // de 48 à 47 bits, mais bon il y a padding
    // Voir si possible de faire autrement que de stocker le nom des vars
    const Variable = struct {
        index: usize = 0,
        typ: Type = Void,
        depth: usize,
        name: usize,
        initialized: bool = false,
    };

    const State = struct {
        /// In a context that allow partially returning a value
        allow_partial: bool = true,
        /// In a function
        in_fn: bool = false,
        /// Current function's type
        fn_type: Type = Void,
        /// Flag to tell if last statement returned from scope
        returns: bool = false,
    };

    pub const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(self: *Self, allocator: Allocator, repl: bool) !void {
        self.arena = std.heap.ArenaAllocator.init(allocator);
        self.allocator = self.arena.allocator();

        self.instructions = MultiArrayList(Instruction){};
        self.warns = ArrayList(AnalyzerReport).init(self.allocator);
        self.errs = ArrayList(AnalyzerReport).init(self.allocator);
        self.globals = ArrayList(Variable).init(self.allocator);
        self.locals = ArrayList(Variable).init(self.allocator);
        self.node_idx = 0;
        self.scope_depth = 0;

        self.states = ArrayList(State).init(self.allocator);
        self.main = null;
        self.local_offset = 0;
        self.type_manager = TypeManager.init(self.allocator);
        self.interner = Interner.init(self.allocator);

        self.start_idx = 0;

        // We reserve slot 0 for 'main'
        _ = try self.interner.intern("main");
        // Slot 1 for std
        _ = try self.interner.intern("std");
        try self.type_manager.init_builtins(&self.interner);
        self.repl = repl;
    }

    /// For REPL
    pub fn reinit(self: *Self) void {
        self.node_idx = 0;
        self.scope_depth = 0;
        self.local_offset = 0;
        self.start_idx = 0;
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn analyze(
        self: *Self,
        source: []const u8,
        tokens: *const MultiArrayList(Token),
        nodes: *const MultiArrayList(Node),
        starts: []const usize,
    ) !void {
        self.source = source;
        self.token_tags = tokens.items(.tag);
        self.token_spans = tokens.items(.span);
        self.node_tags = nodes.items(.tag);
        self.node_mains = nodes.items(.main);
        self.node_data = nodes.items(.data);

        self.starts = starts;

        // HACK: to protect a -1 access
        try self.states.append(.{});

        while (self.node_idx < self.node_data.len) : (self.start_idx += 1) {
            const start = self.node_idx;

            const node_type = self.analyze_node(self.node_idx) catch |e| {
                switch (e) {
                    // If it's our own error, we continue
                    error.Err => {
                        self.node_idx = self.starts[self.start_idx + 1];
                        continue;
                    },
                    error.TooManyTypes => return self.err(.TooManyTypes, self.to_span(self.node_idx)),
                    else => return e,
                }
            };

            if (node_type != Void) {
                self.err(.UnusedValue, self.to_span(start)) catch {};
            }
        }

        // In REPL mode, no need for main function
        if (self.repl)
            return
        else if (self.main == null) self.err(.NoMain, .{ .start = 0, .end = 0 }) catch {};
    }

    // TODO: move to other file
    fn to_span(self: *const Self, node: Node.Index) Span {
        return switch (self.node_tags[node]) {
            .Add, .And, .Div, .Mul, .Or, .Sub, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => .{
                .start = self.token_spans[self.node_mains[node + 1]].start,
                .end = self.to_span(node + 2).end,
            },
            .Assignment => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node + 1]].end,
            },
            .Block => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node]].start + 1,
            },
            .Bool, .Float, .Identifier, .Int, .Null, .String => self.token_spans[self.node_mains[node]],
            .Discard => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node + 1]].end,
            },
            .Empty => self.to_span(node - 1),
            // TODO: real spans, here we underline only the function's name
            .FnDecl => self.token_spans[self.node_mains[node]],
            .FnCall => self.token_spans[self.node_mains[node + 1]],
            .Grouping => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_data[node]].end,
            },
            .If => self.token_spans[self.node_mains[node]],
            .Link => self.token_spans[self.node_data[node]],
            .Parameter => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node + 1]].end,
            },
            .Print => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.to_span(node + 1).end,
            },
            //TODO: real span with return's expression
            .Return => self.token_spans[self.node_mains[node]],
            .Type => self.token_spans[self.node_mains[node]],
            .Unary => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node + 1]].end,
            },
            .Use => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node + self.node_data[node]]].end,
            },
            .VarDecl => .{
                .start = self.token_spans[self.node_mains[node]].start,
                .end = self.token_spans[self.node_mains[node + self.node_data[node]]].end,
            },
            .While => self.token_spans[self.node_mains[node]],
            .FnCallEnd, .FnDeclEnd, .MultiVarDecl => unreachable,
        };
    }

    fn get_type_name(self: *const Self, typ: Type) []const u8 {
        const idx = self.type_manager.idx(typ);
        return self.interner.get_key(idx).?;
    }

    /// Adds a new instruction and add it's `start` field and returns its index.
    fn add_instr(self: *Self, instr: Instruction, main: usize) !Node.Index {
        try self.instructions.append(self.allocator, .{
            .tag = instr.tag,
            .data = instr.data,
            .start = self.token_spans[self.node_mains[main]].start,
        });
        return self.instructions.len - 1;
    }

    fn is_numeric(t: Type) bool {
        return t == Int or t == Float;
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

    fn last_state(self: *Self) *State {
        return &self.states.items[self.states.items.len - 1];
    }

    /// Skips until it reaches the `to` node tag
    fn skip(self: *Self, to: Node.Tag) void {
        const opening: Node.Tag = if (to == .FnCallEnd) .FnCall else .FnDecl;
        var left: usize = 0;

        while (self.node_idx < self.node_tags.len) {
            if (self.node_tags[self.node_idx] == opening) left += 1;

            if (self.node_tags[self.node_idx] == to) {
                self.node_idx += 1;

                if (left == 0) break;
            } else self.node_idx += 1;
        }
    }

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

    /// Checks if an identifier already exists in current scope and if it's type exists
    /// Returns the type of the variable, void if none provided
    fn check_name_and_type(
        self: *Self,
        name: usize,
        name_idx: Node.Index,
        type_idx: Node.Index,
    ) !Type {
        // Name check
        if (self.ident_in_scope(name)) {
            return self.err(
                .{ .AlreadyDeclared = .{ .name = self.interner.get_key(name).? } },
                self.to_span(name_idx),
            );
        }

        return if (self.node_tags[type_idx] != .Empty)
            self.type_manager.declared.get(
                try self.interner.intern(self.source_from_node(type_idx)),
            ) orelse
                return self.err(
                    .{ .UndeclaredType = .{ .found = self.source_from_node(type_idx) } },
                    self.to_span(type_idx),
                )
        else
            Void;
    }

    /// Declares a variable either in globals or in locals based on current scope depth
    fn declare_variable(self: *Self, name: usize, typ: Type, initialized: bool) !Instruction.Variable {
        var variable: Variable = .{
            .name = name,
            .typ = typ,
            .depth = self.scope_depth,
            .initialized = initialized,
        };

        // Add the variable to the correct data structure
        if (self.scope_depth == 0) {
            const index = self.globals.items.len;
            variable.index = index;

            try self.globals.append(variable);
            return .{ .index = @intCast(index), .scope = .Global };
        } else {
            // Take function's frame into account
            const index = self.locals.items.len - self.local_offset;
            variable.index = index;

            try self.locals.append(variable);
            return .{ .index = @intCast(index), .scope = .Local };
        }
    }

    fn is_pure(self: *const Self, node: Node.Index) bool {
        // TODO: manage those
        // Block: Block,
        // FnCall: FnCall,
        // Identifier: Identifier,
        // If: If,
        return switch (self.node_tags[node]) {
            .Bool, .Float, .Int, .Null, .String, .FnDecl => true,
            .Add, .Div, .Mul, .Sub, .And, .Or, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => {
                const lhs = self.is_pure(node + 1);
                return lhs and self.is_pure(node + 2);
            },
            .Grouping => self.is_pure(node + 1),
            .Unary => self.is_pure(node + 1),
            // Skips type
            .VarDecl => self.is_pure(node + 2),
            else => false,
        };
    }

    fn analyze_node(self: *Self, node: Node.Index) Error!Type {
        if (self.scope_depth == 0 and !self.repl and !self.is_pure(node)) {
            return self.err(.UnpureInGlobal, self.to_span(node));
        }

        var final: Type = Void;

        switch (self.node_tags[node]) {
            .Add, .And, .Div, .Mul, .Or, .Sub, .Eq, .Ge, .Gt, .Le, .Lt, .Ne => final = try self.binop(node),
            .Assignment => try self.assignment(node),
            .Block => final = try self.block(node),
            .Bool => final = try self.bool_lit(node),
            .Discard => try self.discard(node),
            .Empty => self.node_idx += 1,
            .Float => final = try self.float_lit(node),
            .FnCall => final = try self.fn_call(node),
            .FnDecl => try self.fn_declaration(node),
            .Grouping => {
                self.node_idx += 1;
                final = try self.analyze_node(self.node_idx);
            },
            .Identifier => final = (try self.identifier(node, true)).typ,
            .If => final = try self.if_expr(node),
            .Int => final = try self.int_lit(node),
            .Link => {
                self.node_idx += 1;
                final = try self.analyze_node(self.node_data[node]);
            },
            .MultiVarDecl => try self.multi_var_decl(node),
            .Null => final = try self.null_lit(),
            .Print => try self.print(node),
            .Return => final = try self.return_expr(node),
            .String => final = try self.string(node),
            .Unary => final = try self.unary(node),
            .Use => try self.use(node),
            .VarDecl => try self.var_decl(node),
            .While => try self.while_stmt(node),
            .FnCallEnd, .FnDeclEnd, .Parameter, .Type => unreachable,
        }

        return final;
    }

    fn assignment(self: *Self, _: Node.Index) !void {
        const state = self.last_state();
        const last = state.allow_partial;
        state.allow_partial = false;
        errdefer state.allow_partial = last;

        self.node_idx += 1;
        var cast = false;

        const assigne_idx = self.node_idx;
        const idx = try self.add_instr(.{ .tag = .Assignment, .data = undefined }, assigne_idx);

        switch (self.node_tags[assigne_idx]) {
            .Identifier => {
                // In case of failure, skips the value
                const assigne = self.resolve_identifier(assigne_idx, false) catch |e| {
                    _ = self.analyze_node(self.node_idx) catch {};
                    return e;
                };

                const value_idx = self.node_idx;
                const value_type = try self.analyze_node(value_idx);

                // For now, we can assign only to scalar variables
                if (TypeSys.get_kind(assigne.typ) != TypeSys.Var) {
                    return self.err(.InvalidAssignTarget, self.to_span(assigne_idx));
                }

                // Restore state
                state.allow_partial = last;

                if (value_type == Void) {
                    return self.err(.VoidAssignment, self.to_span(value_idx));
                }

                // If type is unknown, we update it
                if (assigne.typ == Void) {
                    assigne.typ = value_type;
                } else if (assigne.typ != value_type) {
                    // One case in wich we can coerce; int -> float
                    if (assigne.typ == Float and value_type == Int) {
                        cast = true;
                        _ = try self.add_instr(
                            .{ .tag = .Cast, .data = .{ .CastTo = .Float } },
                            assigne_idx,
                        );
                    } else {
                        return self.err(
                            .{ .InvalidAssignType = .{
                                .expect = self.get_type_name(assigne.typ),
                                .found = self.get_type_name(value_type),
                            } },
                            self.to_span(assigne_idx),
                        );
                    }
                }

                if (!assigne.initialized) assigne.initialized = true;

                self.instructions.items(.data)[idx] = .{ .Assignment = .{
                    .variable = .{
                        .index = @intCast(assigne.index),
                        .scope = if (assigne.depth > 0) .Local else .Global,
                    },
                    .cast = cast,
                } };
            },
            // Later, manage member, pointer, ...
            else => {
                // Skips the assigne
                _ = self.analyze_node(assigne_idx) catch {};
                // Skips the value
                _ = self.analyze_node(self.node_idx) catch {};
                return self.err(.InvalidAssignTarget, self.to_span(assigne_idx));
            },
        }
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
        if (op == .Add and lhs == Str and rhs == Str) {
            self.instructions.items(.data)[idx] = .{ .Binop = .{ .op = .AddStr } };
            return Str;
        } else if (op == .Mul) {
            if ((lhs == Str and rhs == Int) or (lhs == Int and rhs == Str)) {
                self.instructions.items(.data)[idx] = .{ .Binop = .{
                    .cast = if (rhs == Int) .Rhs else .Lhs,
                    .op = .MulStr,
                } };

                return Str;
            }
        }

        // Error check
        switch (op) {
            .Add, .Div, .Mul, .Sub, .Ge, .Gt, .Le, .Lt => {
                if (!Analyzer.is_numeric(lhs)) {
                    return self.err(
                        AnalyzerMsg.invalid_arithmetic(self.get_type_name(lhs)),
                        self.to_span(lhs_index),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
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
                    Float => {
                        switch (rhs) {
                            Float => {},
                            Int => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast("right hand side", self.get_type_name(lhs)),
                                    self.to_span(rhs_index),
                                );

                                data.cast = .Rhs;
                            },
                            else => unreachable,
                        }
                    },
                    Int => {
                        switch (rhs) {
                            Float => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast("left hand side", self.get_type_name(rhs)),
                                    self.to_span(lhs_index),
                                );

                                data.cast = .Lhs;
                                res = Float;
                            },
                            Int => switch (op) {
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
                // TODO: Error handling for non int, float and str
                switch (op) {
                    .Eq => data.op = switch (lhs) {
                        Bool => .EqBool,
                        Float => .EqFloat,
                        Int => .EqInt,
                        else => .EqStr,
                    },
                    .Ne => data.op = switch (lhs) {
                        Bool => .NeBool,
                        Float => .NeFloat,
                        Int => .NeInt,
                        else => .NeStr,
                    },
                    else => unreachable,
                }

                // If different value types
                if (lhs != rhs) {
                    // Check for implicit casts
                    if ((lhs == Int and rhs == Float) or (lhs == Float and rhs == Int)) {
                        if (lhs == Int) {
                            data.cast = .Lhs;

                            try self.warn(.FloatEqualCast, self.to_span(lhs_index));
                        } else {
                            data.cast = .Rhs;

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
                    if (lhs == Float) {
                        try self.warn(.FloatEqual, self.to_span(node));
                    }
                }

                res = Bool;
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
                    Float => {
                        switch (rhs) {
                            Float => try self.warn(.FloatEqual, self.to_span(node)),
                            Int => {
                                try self.warn(.FloatEqualCast, self.to_span(rhs_index));

                                data.cast = .Rhs;
                            },
                            else => unreachable,
                        }
                    },
                    Int => {
                        switch (rhs) {
                            Float => {
                                try self.warn(.FloatEqualCast, self.to_span(lhs_index));

                                data.cast = .Lhs;
                            },
                            Int => switch (op) {
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

                res = Bool;
            },
            // Logical binop
            .And, .Or => {
                if (lhs != Bool) return self.err(.{ .InvalidLogical = .{
                    .found = self.get_type_name(lhs),
                } }, self.to_span(lhs_index));

                if (rhs != Bool) return self.err(.{ .InvalidLogical = .{
                    .found = self.get_type_name(rhs),
                } }, self.to_span(rhs_index));

                switch (op) {
                    .And => data.op = .And,
                    .Or => data.op = .Or,
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

        var final: Type = Void;
        self.node_idx += 1;

        for (0..length) |i| {
            final = try self.analyze_node(self.node_idx);

            if (final != Void and i != length - 1) {
                return self.err(.UnusedValue, self.to_span(self.node_idx));
            }
        }

        const count = try self.end_scope();
        if (count > 255) return self.err(.TooManyLocals, self.to_span(self.node_idx));

        self.instructions.items(.data)[idx] = .{ .Block = .{
            .length = length,
            .pop_count = @intCast(count),
            .is_expr = if (final != Void) true else false,
        } };

        return final;
    }

    fn bool_lit(self: *Self, node: Node.Index) !Type {
        _ = try self.add_instr(.{ .tag = .Bool, .data = .{
            .Bool = if (self.token_tags[self.node_mains[node]] == .True) true else false,
        } }, node);

        self.node_idx += 1;

        return Bool;
    }

    fn discard(self: *Self, node: Node.Index) !void {
        _ = try self.add_instr(.{ .tag = .Discard, .data = undefined }, node);

        self.node_idx += 1;
        const discarded = try self.analyze_node(self.node_idx);

        if (discarded == Void) return self.err(.VoidDiscard, self.to_span(node + 1));
    }

    fn float_lit(self: *Self, node: Node.Index) !Type {
        const value = std.fmt.parseFloat(f64, self.source_from_node(node)) catch blk: {
            // TODO: error handling, only one possible it's invalid char
            std.debug.print("Error parsing float\n", .{});
            break :blk 0.0;
        };

        _ = try self.add_instr(
            .{ .tag = .Float, .data = .{ .Float = value } },
            node,
        );
        self.node_idx += 1;

        return Float;
    }

    fn fn_call(self: *Self, node: Node.Index) Error!Type {
        errdefer self.skip(.FnCallEnd);
        const arity = self.node_data[node];
        self.node_idx += 1;

        const idx = try self.add_instr(.{ .tag = .FnCall, .data = .{ .FnCall = .{
            .arity = @intCast(arity),
            .builtin = undefined,
        } } }, node);

        // Resolve the callee
        const type_value = try self.expect_type_kind(self.node_idx, TypeSys.Fn);
        const type_info = self.type_manager.type_infos.items[type_value].Fn;

        if (type_info.arity != arity) {
            return self.err(
                try AnalyzerMsg.wrong_args_count(type_info.arity, arity),
                self.to_span(self.node_idx),
            );
        }

        self.instructions.items(.data)[idx].FnCall.builtin = type_info.builtin;

        for (0..arity) |i| {
            const arg_idx = self.node_idx;
            const arg_type = try self.analyze_node(arg_idx);

            if (arg_type != type_info.params[i]) {
                // If it's an implicit cast between int and float, save the
                // argument indices for compiler. Otherwise, error
                if (type_info.params[i] == Float and arg_type == Int) {
                    _ = try self.add_instr(
                        .{ .tag = .Cast, .data = .{ .CastTo = .Float } },
                        arg_idx,
                    );
                } else {
                    return self.err(
                        .{ .TypeMismatch = .{
                            .expect = self.get_type_name(type_info.params[i]),
                            .found = self.get_type_name(arg_type),
                        } },
                        self.to_span(arg_idx),
                    );
                }
            }
        }

        // Skips the FnCallEnd
        self.node_idx += 1;

        return type_info.return_type;
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
        errdefer self.skip(.FnDeclEnd);
        const name_idx = try self.interner.intern(self.source_from_node(node));

        if (self.main == null and self.scope_depth == 0 and name_idx == 0) {
            self.main = self.instructions.len;
        }

        // Check in current scope
        const arity = self.node_data[node];
        const return_type = try self.check_name_and_type(name_idx, node, node + 1 + arity * 2);
        const fn_idx = try self.add_instr(.{ .tag = .FnDecl, .data = undefined }, node);

        // We add function's name for runtime access
        _ = try self.add_instr(.{ .tag = .FnName, .data = .{ .Id = name_idx } }, node);

        // We declare before body for recursion. We need to correct type to check those recursions
        const type_idx = try self.type_manager.reserve_info();
        const fn_type = TypeSys.create(TypeSys.Fn, 0, type_idx);
        const fn_var = try self.declare_variable(name_idx, fn_type, true);

        _ = try self.add_instr(.{ .tag = .Identifier, .data = .{ .Variable = fn_var } }, node);

        self.scope_depth += 1;
        errdefer self.scope_depth -= 1;

        // Stores the previous offset
        const local_offset_save = self.local_offset;
        self.local_offset = self.locals.items.len;

        // Switch back to locals before function call
        errdefer self.local_offset = local_offset_save;

        // We add a empty variable to anticipate the function it self on the stack
        // it's the returned address for the function
        try self.locals.append(.{
            .depth = self.scope_depth,
            .name = name_idx,
            .typ = fn_type,
            .initialized = true,
        });

        // Skips function's node
        self.node_idx += 1;
        var params_type: [256]Type = undefined;

        for (0..arity) |i| {
            // Check on parameter
            const param_idx = try self.interner.intern(self.source_from_node(self.node_idx));

            // Skips param's name
            self.node_idx += 1;

            const param_type = self.check_name_and_type(
                param_idx,
                self.node_idx - 1,
                self.node_idx,
            ) catch |e| switch (e) {
                error.Err => {
                    // We replace the error with a more explicit one for parameters
                    if (self.errs.items[self.errs.items.len - 1].report == .AlreadyDeclared) {
                        self.errs.items[self.errs.items.len - 1] = AnalyzerReport.err(
                            .{ .DuplicateParam = .{ .name = self.source_from_node(self.node_idx - 1) } },
                            self.to_span(self.node_idx - 1),
                        );
                    }

                    return e;
                },
                else => return e,
            };

            if (param_type == Void) {
                return self.err(.VoidParam, self.to_span(self.node_idx - 1));
            }

            _ = try self.declare_variable(param_idx, param_type, true);
            params_type[i] = param_type;

            // Skips param's type
            self.node_idx += 1;
        }

        // Set all the informations now that we have every thing
        self.type_manager.set_info(type_idx, .{ .Fn = .{
            .arity = arity,
            .params = params_type,
            .return_type = return_type,
        } });

        // We skip the return type, it has already been analyzed
        self.node_idx += 1;

        // ------
        //  Body
        // ------
        try self.states.append(.{ .in_fn = true, .fn_type = return_type });
        const prev_state = self.last_state();

        self.scope_depth += 1;
        errdefer self.scope_depth -= 1;

        const block_idx = self.node_idx;
        const length = self.node_data[block_idx];

        // We don't use block because we don't want to emit extra data from the block
        self.node_idx += 1;
        var start = self.node_idx;
        var body_err = false;
        var body_type: Type = Void;
        var deadcode_start: usize = 0;
        var deadcode_count: usize = 0;

        for (0..length) |i| {
            // If previous statement returned, it's only dead code now
            if (deadcode_start == 0 and prev_state.returns) {
                try self.warn(.DeadCode, self.to_span(start));
                deadcode_start = self.instructions.len;
                deadcode_count = length - i;
            }

            // If last statement, we don't allow partial anymore (for return)
            // Usefull for 'if' for example, in this case we want all the branches
            // to return something
            if (i == length - 1) prev_state.allow_partial = false;

            // We try to analyze the whole body
            start = self.node_idx;
            const typ = self.analyze_node(self.node_idx) catch |e| switch (e) {
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
            if (body_type != Void and i != length - 1 and !prev_state.returns) {
                return self.err(.UnusedValue, self.to_span(start));
            }
        }

        if (!body_err and body_type != return_type) {
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
        if (deadcode_start > 0)
            self.instructions.shrinkRetainingCapacity(deadcode_start);

        // Two levels: 1 for function's name + params and another one for body
        _ = try self.end_scope();
        _ = try self.end_scope();

        // Switch back to locals before function call
        self.local_offset = local_offset_save;

        const state = self.states.pop().?;

        const return_kind: ReturnKind = if (state.returns)
            .Explicit
        else if (body_type == Void)
            .ImplicitVoid
        else
            .ImplicitValue;

        // Skips the FnDeclEnd
        self.node_idx += 1;

        self.instructions.items(.data)[fn_idx] = .{ .FnDecl = .{
            .body_len = length - deadcode_count,
            .return_kind = return_kind,
        } };
    }

    fn identifier(self: *Self, node: Node.Index, initialized: bool) Error!*Variable {
        const variable = try self.resolve_identifier(node, initialized);

        _ = try self.add_instr(.{ .tag = .Identifier, .data = .{ .Variable = .{
            .scope = if (variable.depth > 0) .Local else .Global,
            .index = @intCast(variable.index),
        } } }, node);

        return variable;
    }

    fn resolve_identifier(self: *Self, node: Node.Index, initialized: bool) Error!*Variable {
        self.node_idx += 1;
        const name = self.source_from_node(node);
        const name_idx = try self.interner.intern(name);

        // We first check in locals
        if (self.locals.items.len > 0) {
            var idx = self.locals.items.len;

            // while (idx > 0) : (idx -= 1) {
            // NOTE: for now, can't see outside function's frame
            while (idx > self.local_offset) : (idx -= 1) {
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

    fn if_expr(self: *Self, node: Node.Index) Error!Type {
        errdefer self.node_idx = self.node_data[node];

        const idx = try self.add_instr(.{ .tag = .If, .data = undefined }, node);
        self.node_idx += 1;
        var data: Instruction.If = .{ .cast = .None, .has_else = false };

        const cond_idx = self.node_idx;
        const cond_type = try self.analyze_node(self.node_idx);

        // We can continue to analyze if the condition isn't a bool
        if (cond_type != Bool) self.err(
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

        // State managment
        const state = self.last_state();

        // If we hit a return, we transfert it first to the then branch
        if (state.returns) {
            // Reset return  for else branch
            state.returns = false;
            then_return = true;
            // As we exit scope, we don't return any type
            final_type = Void;
        }

        var else_type: Type = Void;
        const else_idx = self.node_idx;

        if (self.node_tags[else_idx] != .Empty) {
            data.has_else = true;

            else_type = try self.analyze_node(else_idx);

            // If it returns
            if (state.returns) {
                else_return = true;
                // If not then, unmark as globally returning from scope
                if (!then_return) state.returns = false;
            } else if (then_return) {
                // If else only then branch returns, final_type becomes else branch
                final_type = else_type;
            }

            // Type coherence. If branches don't exit scope and branches have
            // diffrent types
            if (!then_return and !else_return and then_type != else_type) {
                if (then_type == Int and else_type == Float) {
                    data.cast = .Then;

                    try self.warn(
                        AnalyzerMsg.implicit_cast("then branch", "float"),
                        self.to_span(then_idx),
                    );
                } else if (then_type == Float and else_type == Int) {
                    data.cast = .Else;

                    // Safe unsafe access, if there is a non void type
                    // there is an else body
                    try self.warn(
                        AnalyzerMsg.implicit_cast("else branch", "float"),
                        self.to_span(else_idx),
                    );
                } else {
                    return self.err(
                        .{ .IncompatibleIfType = .{
                            .found1 = self.get_type_name(then_type),
                            .found2 = self.get_type_name(else_type),
                        } },
                        self.to_span(node),
                    );
                }
            }
        } else if (then_type != Void and !state.allow_partial) {
            // Skips the Empty
            self.node_idx += 1;

            return self.err(
                .{ .MissingElseClause = .{ .if_type = self.get_type_name(then_type) } },
                self.to_span(node),
            );
        } else self.node_idx += 1;

        self.instructions.items(.data)[idx] = .{ .If = data };

        return final_type;
    }

    fn int_lit(self: *Self, node: Node.Index) !Type {
        const value = std.fmt.parseInt(isize, self.source_from_node(node), 10) catch blk: {
            // TODO: error handling, only one possible it's invalid char
            std.debug.print("Error parsing integer\n", .{});
            break :blk 0;
        };

        _ = try self.add_instr(.{ .tag = .Int, .data = .{ .Int = value } }, node);
        self.node_idx += 1;

        return Int;
    }

    fn multi_var_decl(self: *Self, node: Node.Index) !void {
        const count = self.node_mains[node];
        self.node_idx += 1;
        _ = try self.add_instr(.{ .tag = .MultipleVarDecl, .data = .{ .Id = count } }, self.node_idx);

        for (0..count) |_| {
            try self.var_decl(self.node_idx);
        }
    }

    fn null_lit(self: *Self) !Type {
        _ = try self.add_instr(.{ .tag = .Null, .data = undefined }, self.node_idx);
        self.node_idx += 1;

        return Null;
    }

    fn print(self: *Self, node: Node.Index) !void {
        errdefer self.node_idx = self.node_data[node];

        _ = try self.add_instr(.{ .tag = .Print, .data = undefined }, self.node_idx);
        self.node_idx += 1;
        _ = try self.analyze_node(self.node_idx);
    }

    fn return_expr(self: *Self, node: Node.Index) Error!Type {
        self.node_idx += 1;
        var state = self.last_state();

        const idx = try self.add_instr(.{ .tag = .Return, .data = .{ .Return = .{
            .value = false,
            .cast = false,
        } } }, node);

        const value_idx = self.node_idx;

        const return_type = if (self.node_tags[self.node_idx] != .Empty) blk: {
            self.instructions.items(.data)[idx].Return.value = true;
            break :blk try self.analyze_node(self.node_idx);
        } else blk: {
            self.node_idx += 1;
            break :blk Void;
        };

        // We check after to advance node idx
        if (!state.in_fn) {
            return self.err(.ReturnOutsideFn, self.to_span(node));
        }

        if (state.fn_type != return_type) {
            if (TypeSys.get_value(state.fn_type) == Float and
                TypeSys.get_value(return_type) == Int)
            {
                self.instructions.items(.data)[idx].Return.cast = true;
                _ = try self.add_instr(.{ .tag = .Cast, .data = .{ .CastTo = .Float } }, value_idx);
            } else return self.err(
                .{ .IncompatibleFnType = .{
                    .expect = self.get_type_name(state.fn_type),
                    .found = self.get_type_name(return_type),
                } },
                self.to_span(node),
            );
        }

        state.returns = true;
        return state.fn_type;
    }

    fn string(self: *Self, node: Node.Index) !Type {
        const source = self.source_from_node(node);
        // Removes the quotes
        const value = try self.interner.intern(source[1 .. source.len - 1]);
        _ = try self.add_instr(.{ .tag = .String, .data = .{ .Id = value } }, node);
        self.node_idx += 1;

        return Str;
    }

    fn unary(self: *Self, node: Node.Index) Error!Type {
        const op = self.token_tags[self.node_mains[node]];
        const idx = try self.add_instr(.{
            .tag = .Unary,
            .data = .{ .Unary = .{
                .op = if (op == .Not) .Bang else .Minus,
                .typ = .Float,
            } },
        }, node);

        self.node_idx += 1;
        const rhs = try self.analyze_node(self.node_idx);

        if (op == .Not and rhs != Bool) {
            return self.err(
                .{ .InvalidUnary = .{ .found = self.get_type_name(rhs) } },
                self.to_span(node),
            );
        } else if (op == .Minus and rhs != Int and rhs != Float) {
            return self.err(
                AnalyzerMsg.invalid_arithmetic(self.get_type_name(rhs)),
                self.to_span(node),
            );
        }

        if (rhs == Int) self.instructions.items(.data)[idx].Unary.typ = .Int;

        return rhs;
    }

    fn use(self: *Self, node: Node.Index) !void {
        const idx = try self.add_instr(.{ .tag = .Use, .data = undefined }, node);
        self.node_idx += 1;

        var count: usize = 0;
        var idx_unknown: usize = 1;

        const name = try self.interner.intern(self.source_from_node(self.node_idx));

        // For now, "std" is interned at initialization in slot 1
        self.node_idx += 1;
        if (name == 1) {
            // TODO: For now, il allows to keep synchronized the different arrays of
            // nodes/instructions
            _ = try self.add_instr(.{ .tag = .Null, .data = undefined, .start = 0 }, 0);

            // TODO: support real imports
            if (self.node_data[node] > 2) @panic("Use statements can't import more than std + one module");

            // 1 less because we parsed "std"
            for (0..self.node_data[node] - 1) |_| {
                if (try self.type_manager.import_builtins(self.source_from_node(self.node_idx))) |module| {
                    const all_fn_names = module.keys();

                    for (all_fn_names) |fn_name| {
                        const name_idx = try self.interner.intern(fn_name);

                        // TODO: Error handling
                        const func = module.get(fn_name).?;

                        const info: TypeInfo = .{ .Fn = .{
                            .arity = func.arity,
                            .params = func.params,
                            .return_type = func.return_type,
                            .builtin = true,
                        } };

                        // Declare the type and additional informations
                        const typ = try self.type_manager.declare(name_idx, TypeSys.Fn, TypeSys.Builtin, info);
                        // Declare the variable
                        const variable = try self.declare_variable(name_idx, typ, true);

                        _ = try self.add_instr(.{ .tag = .Imported, .data = .{ .Imported = .{
                            .index = func.index,
                            .variable = variable,
                        } } }, node);

                        count += 1;
                    }

                    self.instructions.items(.data)[idx] = .{ .Use = count };
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

    fn resolve_link(self: *const Self, node: Node.Index) Node.Index {
        return if (self.node_tags[node] == .Link)
            self.node_data[node]
        else
            node;
    }

    fn var_decl(self: *Self, node: Node.Index) !void {
        // In case we propagate an error, we advance the counter to avoid
        // infinite loop
        self.node_idx += 1;
        const type_idx = self.resolve_link(self.node_idx);
        self.node_idx += 1;
        const value_idx = self.resolve_link(self.node_idx);

        const idx = try self.add_instr(.{ .tag = .VarDecl, .data = undefined }, node);

        const name = try self.interner.intern(self.source_from_node(node));
        var checked_type = self.check_name_and_type(name, node, type_idx) catch |e| {
            // Skips the value
            _ = try self.analyze_node(self.node_idx);
            return e;
        };

        var initialized = false;
        var cast = false;

        if (self.node_tags[value_idx] != .Empty) {
            const state = self.last_state();
            const last = state.allow_partial;
            state.allow_partial = false;

            const value_type = try self.analyze_node(value_idx);
            state.allow_partial = last;

            // Void assignment check
            if (value_type == Void) {
                return self.err(.VoidAssignment, self.to_span(value_idx));
            }

            // If no type declared, we infer the value type
            if (checked_type == Void) {
                checked_type = value_type;
                // Else, we check for coherence
            } else if (checked_type != value_type) {
                // One case in wich we can coerce, int -> float
                if (checked_type == Float and value_type == Int) {
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
            _ = try self.add_instr(.{ .tag = .Null }, node);
            self.node_idx += 1;
        }

        const variable = try self.declare_variable(name, checked_type, initialized);
        self.instructions.items(.data)[idx] = .{ .VarDecl = .{ .variable = variable, .cast = cast } };
    }

    fn while_stmt(self: *Self, node: Node.Index) Error!void {
        errdefer self.node_idx = self.node_data[node];

        self.node_idx += 1;
        const cond_idx = self.node_idx;
        _ = try self.add_instr(.{ .tag = .While }, cond_idx);
        const cond_type = try self.analyze_node(cond_idx);

        if (cond_type != Bool) return self.err(
            .{ .NonBoolCond = .{
                .what = "while",
                .found = self.get_type_name(cond_type),
            } },
            self.to_span(cond_idx),
        );

        const body_idx = self.node_idx;
        const body_type = try self.analyze_node(body_idx);

        if (body_type != Void) return self.err(
            .{ .NonVoidWhile = .{
                .found = self.get_type_name(body_type),
            } },
            self.to_span(body_idx),
        );
    }
};
