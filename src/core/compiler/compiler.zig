const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;
const FieldEnum = std.meta.FieldEnum;

const Disassembler = @import("Disassembler.zig");
const rir = @import("../ir/rir.zig");
const Instruction = rir.Instruction;
const Obj = @import("../runtime/Obj.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;

const misc = @import("misc");
const Interner = misc.Interner;
const GenReport = misc.reporter.GenReport;
const oom = misc.oom;

pub const CompiledModule = struct {
    // TODO: useless?
    name: []const u8,
    function: *Obj.Function,
    globals: []Value,
    symbols: []Value,

    pub fn init(allocator: Allocator, name: []const u8, globals_count: usize, symbols_count: usize) CompiledModule {
        return .{
            .name = name,
            .function = undefined,
            .globals = allocator.alloc(Value, globals_count) catch oom(),
            .symbols = allocator.alloc(Value, symbols_count) catch oom(),
        };
    }

    pub fn deinit(self: *CompiledModule, allocator: Allocator) void {
        allocator.free(self.globals);
        allocator.free(self.symbols);
    }
};

pub const CompilationManager = struct {
    allocator: Allocator,
    vm: *Vm,
    interner: *const Interner,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    instr_data: []const Instruction.Data,
    instr_lines: []const usize,
    render_mode: Disassembler.RenderMode,
    module: CompiledModule,
    line: usize,

    const Self = @This();
    const Error = error{err} || Chunk.Error || std.posix.WriteError;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        allocator: Allocator,
        name: []const u8,
        vm: *Vm,
        interner: *const Interner,
        render_mode: Disassembler.RenderMode,
        global_count: usize,
        symbol_count: usize,
    ) Self {
        return .{
            .allocator = allocator,
            .vm = vm,
            .interner = interner,
            .compiler = undefined,
            .errs = .empty,
            .instr_data = undefined,
            .instr_lines = undefined,
            .render_mode = render_mode,
            .module = .init(allocator, name, global_count, symbol_count),
            .line = 0,
        };
    }

    pub fn compile(
        self: *Self,
        instr_data: []const Instruction.Data,
        roots: []const rir.Index,
        instr_lines: []const usize,
        main_index: ?usize,
        module_index: usize,
    ) !CompiledModule {
        self.instr_data = instr_data;
        self.instr_lines = instr_lines;

        if (self.render_mode != .none) {
            var buf: [256]u8 = undefined;
            var stdout = std.fs.File.stdout().writer(&buf);
            stdout.interface.print("//---- {s} ----\n\n", .{self.module.name}) catch oom();
            stdout.interface.flush() catch oom();
        }

        self.compiler = Compiler.init(self, "global scope", 0, module_index);

        for (roots) |root| {
            try self.compiler.compileInstr(root);
        }

        if (main_index) |idx| {
            // TODO: protect
            self.compiler.writeOpAndByte(.load_sym, @intCast(idx));
            self.compiler.writeOpAndByte(.call, 0);
        } else {
            self.compiler.getChunk().writeOp(.exit_repl, 0);
        }

        self.module.function = try self.compiler.end();
        return self.module;
    }
};

const Compiler = struct {
    manager: *CompilationManager,
    function: *Obj.Function,
    block_stack: BlockStack,
    state: State = .{},

    const Self = @This();
    const BlockStack = struct {
        stack: ArrayList(Breaks),

        const Breaks = ArrayList(usize);
        pub const empty: BlockStack = .{ .stack = .empty };

        pub fn open(self: *BlockStack, allocator: Allocator) void {
            self.stack.append(allocator, .empty) catch oom();
        }

        pub fn close(self: *BlockStack) Breaks {
            return self.stack.pop().?;
        }

        /// Depth is generated from bottom to top in *Analyzer*
        pub fn add(self: *BlockStack, allocator: Allocator, instr: usize, depth: usize) void {
            self.stack.items[self.stack.items.len - 1 - depth].append(allocator, instr) catch oom();
        }
    };
    const Error = error{Err} || Chunk.Error || std.posix.WriteError;

    const CompilerReport = GenReport(CompilerMsg);
    // TODO: could make a generic construct with only flags like this (share with other contexts)
    const State = struct {
        cow: bool = false,

        pub fn setAndGetPrev(self: *State, comptime f: FieldEnum(State), value: @FieldType(State, @tagName(f))) @TypeOf(value) {
            const prev = @field(self, @tagName(f));
            @field(self, @tagName(f)) = value;

            return prev;
        }
    };
    const FnKind = enum { global, @"fn", method };

    pub fn init(manager: *CompilationManager, name: []const u8, default_count: usize, module_index: usize) Self {
        return .{
            .manager = manager,
            .function = Obj.Function.create(
                manager.vm,
                Obj.String.copy(manager.vm, name),
                default_count,
                module_index,
            ),
            .block_stack = .empty,
        };
    }

    fn eof(self: *const Self) bool {
        return self.manager.instr_idx == self.manager.instr_data.len;
    }

    fn getChunk(self: *Self) *Chunk {
        return &self.function.chunk;
    }

    fn setInstrIndexGetPrev(self: *Self, index: usize) usize {
        const variable_instr = self.manager.instr_idx;
        self.manager.instr_idx = index;
        return variable_instr;
    }

    /// Writes an OpCode to the current chunk
    fn writeOp(self: *Self, op: OpCode) void {
        self.getChunk().writeOp(op, self.manager.line);
    }

    /// Writes a byte to the current chunk
    fn writeByte(self: *Self, byte: u8) void {
        self.getChunk().writeByte(byte, self.manager.line);
    }

    /// Writes an OpCode and a byte to the current chunk
    fn writeOpAndByte(self: *Self, op: OpCode, byte: u8) void {
        self.writeOp(op);
        self.writeByte(byte);
    }

    fn emitConstant(self: *Self, value: Value) Error!void {
        // TODO: error
        self.writeOpAndByte(
            .constant,
            self.getChunk().writeConstant(value) catch |err| {
                std.debug.print("Too many constants in chunk\n", .{});
                return err;
            },
        );
    }

    fn addGlobal(self: *Self, index: usize, global: Value) void {
        self.manager.module.globals[index] = global;
    }

    fn addSymbol(self: *Self, index: usize, symbol: Value) void {
        self.manager.module.symbols[index] = symbol;
    }

    /// Emits the corresponding `get_global` or `get_local` with the correct index
    fn emitGetVar(self: *Self, variable: *const Instruction.Variable) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        self.writeOpAndByte(
            if (variable.scope == .local)
                if (self.state.cow) .get_local_cow else .get_local
            else if (variable.scope == .global)
                if (self.state.cow) .get_global_cow else .get_global
            else
                unreachable,
            @intCast(variable.index),
        );
    }

    /// Define the variable with value on top of stack for global variables.
    /// For locals, they are already sitting on top of stack
    fn defineVariable(self: *Self, infos: Instruction.Variable) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        if (infos.scope == .global) {
            self.writeOpAndByte(.def_global, @intCast(infos.index));
        }
    }

    fn emitJump(self: *Self, kind: OpCode) usize {
        const chunk = self.getChunk();
        chunk.writeOp(kind, self.manager.line);
        chunk.writeByte(0xff, self.manager.line);
        chunk.writeByte(0xff, self.manager.line);

        return chunk.code.items.len - 2;
    }

    fn patchJump(self: *Self, offset: usize) Error!void {
        const chunk = self.getChunk();
        // -2 for the two 8bits jump value (cf emit jump)
        const jump = chunk.code.items.len - offset - 2;

        // TODO: proper error handling
        if (jump > std.math.maxInt(u16)) {
            std.debug.print("Too much code to jump over", .{});
            return error.Err;
        }

        chunk.code.items[offset] = @as(u8, @intCast(jump >> 8));
        chunk.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emitLoop(self: *Self, loop_start: usize) Error!void {
        const offset = self.manager.line;
        const chunk = self.getChunk();
        chunk.writeOp(.loop, offset);
        // +2 for loop own operands (jump offset on 16bits)
        const jump_offset = chunk.code.items.len - loop_start + 2;

        // TODO: Error handling
        if (jump_offset > std.math.maxInt(u16)) {
            @panic("loop body too large\n");
        }

        chunk.writeByte(@as(u8, @intCast(jump_offset >> 8)) & 0xff, offset);
        chunk.writeByte(@intCast(jump_offset & 0xff), offset);
    }

    pub fn end(self: *Self) Error!*Obj.Function {
        if (self.manager.render_mode != .none) {
            var alloc_writer: std.Io.Writer.Allocating = .init(self.manager.allocator);
            defer alloc_writer.deinit();

            var dis = Disassembler.init(
                &self.function.chunk,
                &self.manager.module,
                self.manager.render_mode,
            );
            dis.disChunk(&alloc_writer.writer, self.function.name.chars);

            var buf: [1024]u8 = undefined;
            var stdout_writer = std.fs.File.stdout().writer(&buf);
            const stdout = &stdout_writer.interface;

            stdout.print("{s}\n", .{alloc_writer.writer.buffered()}) catch oom();
            stdout.flush() catch oom();
        }

        return self.function;
    }

    fn compileInstr(self: *Self, instr: rir.Index) Error!void {
        self.manager.line = self.manager.instr_lines[instr];

        try switch (self.manager.instr_data[instr]) {
            .array => |*data| self.array(data),
            .array_access => |*data| self.arrayAccess(data),
            .assignment => |*data| self.assignment(data),
            .binop => |*data| self.binop(data),
            .block => |*data| self.block(data),
            .bool => |data| self.boolInstr(data),
            .box => |index| self.wrappedInstr(.box, index),
            .bound_method => |data| self.boundMethod(data),
            .@"break" => |data| self.breakInstr(data),
            .call => |*data| self.fnCall(data),
            .discard => |index| self.wrappedInstr(.pop, index),

            // In case of an extractor, we don't replace top of stack with the bool result of
            // comparison because if it's true, it's gonna be popped and so last value on stack
            // will be the one extracted, it acts as if we just declared the value in scope
            .extractor => |index| self.wrappedInstr(.ne_null_push, index),

            .field => |*data| self.field(data),
            .float => |data| self.floatInstr(data),
            .fn_decl => |*data| self.compileFn(data),
            .identifier => |*data| self.identifier(data),
            .@"if" => |*data| self.ifInstr(data),
            .int => |data| self.intInstr(data),
            .incr_rc => |index| self.wrappedInstr(.incr_ref, index),
            // TODO: protect the cast
            .load_builtin => |index| self.writeOpAndByte(.load_builtin, @intCast(index)),
            .load_symbol => |*data| self.loadSymbol(data),
            .multiple_var_decl => |*data| self.multipleVarDecl(data),
            .null => self.nullInstr(),
            .pop => |index| self.wrappedInstr(.pop, index),
            .print => |index| self.wrappedInstr(.print, index),
            .@"return" => |*data| self.returnInstr(data),
            .string => |data| self.stringInstr(data),
            .struct_decl => |*data| self.structDecl(data),
            .struct_literal => |*data| self.structLiteral(data),
            .unary => |*data| self.unary(data),
            .unbox => |index| self.wrappedInstr(.unbox, index),
            .var_decl => |*data| self.varDecl(data),
            .@"while" => |data| self.whileInstr(data),

            .noop => {},
        };
    }

    fn wrappedInstr(self: *Self, op: OpCode, index: usize) Error!void {
        try self.compileInstr(index);
        self.writeOp(op);
    }

    fn array(self: *Self, data: *const Instruction.Array) Error!void {
        for (data.values) |value| {
            try self.compileInstr(value);
        }
        // TODO: protect cast
        self.writeOpAndByte(.array_new, @intCast(data.values.len));
    }

    fn arrayAccess(self: *Self, data: *const Instruction.ArrayAccess) Error!void {
        try self.compileInstr(data.array);

        // Index, we deactivate cow for indicies because never wanted but could be triggered by a multiple array
        // access inside an array assignment
        const prev = self.state.setAndGetPrev(.cow, false);
        for (data.indicies) |index| {
            try self.compileInstr(index);
        }
        self.state.cow = prev;

        // TODO: protect the cast
        if (data.indicies.len == 1) {
            self.writeOp(.array_get);
        } else {
            self.writeOpAndByte(if (self.state.cow) .array_get_chain_cow else .array_get_chain, @intCast(data.indicies.len));
        }
    }

    fn arrayAssign(self: *Self, data: *const Instruction.ArrayAccess) Error!void {
        const prev = self.state.setAndGetPrev(.cow, true);
        try self.compileInstr(data.array);
        self.state.cow = prev;

        for (data.indicies) |index| {
            try self.compileInstr(index);
        }

        // TODO: protect the cast
        if (data.indicies.len == 1) {
            self.writeOp(.array_set);
        } else {
            self.writeOpAndByte(.array_set_chain, @intCast(data.indicies.len));
        }
    }

    fn assignment(self: *Self, data: *const Instruction.Assignment) Error!void {
        try self.compileInstr(data.value);

        // TODO: no use of data.cow?
        const variable_data, const unbox = switch (self.manager.instr_data[data.assigne]) {
            .identifier => |*variable| .{ variable, false },
            .array_access => |*arr_data| return self.arrayAssign(arr_data),
            .field => |*field_data| return self.fieldAssignment(field_data),
            .unbox => |index| .{ &self.manager.instr_data[index].identifier, true },
            else => unreachable,
        };

        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        self.writeOpAndByte(
            if (variable_data.scope == .global)
                .set_global
            else if (variable_data.scope == .local)
                if (unbox) .set_local_box else .set_local
            else
                unreachable,
            @intCast(variable_data.index),
        );
    }

    fn fieldAssignment(self: *Self, data: *const Instruction.Field) Error!void {
        self.state.cow = true;
        defer self.state.cow = false;
        try self.compileInstr(data.structure);
        self.writeOpAndByte(.set_field, @intCast(data.index));
    }

    fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
        if (data.op == .@"and" or data.op == .@"or") return self.logicalBinop(data);

        try self.compileInstr(data.lhs);
        try self.compileInstr(data.rhs);

        self.writeOp(
            switch (data.op) {
                .add_float => .add_float,
                .add_int => .add_int,
                .add_str => .str_cat,
                .div_float => .div_float,
                .div_int => .div_int,
                .eq_bool => .eq_bool,
                .eq_float => .eq_float,
                .eq_int => .eq_int,
                .eq_null => .eq_null,
                .eq_str => .eq_str,
                .ge_float => .ge_float,
                .ge_int => .ge_int,
                .gt_float => .gt_float,
                .gt_int => .gt_int,
                .le_float => .le_float,
                .le_int => .le_int,
                .lt_float => .lt_float,
                .lt_int => .lt_int,
                .mul_float => .mul_float,
                .mul_int => .mul_int,
                .mul_str => .str_mul,
                .ne_bool => .ne_bool,
                .ne_float => .ne_float,
                .ne_int => .ne_int,
                .ne_null => .ne_null,
                .ne_str => .ne_str,
                .sub_float => .sub_float,
                .sub_int => .sub_int,
                else => unreachable,
            },
        );
    }

    fn logicalBinop(self: *Self, data: *const Instruction.Binop) Error!void {
        switch (data.op) {
            .@"and" => {
                try self.compileInstr(data.lhs);
                const end_jump = self.emitJump(.jump_false);
                // If true, pop the value, else the 'false' remains on top of stack
                self.writeOp(.pop);
                try self.compileInstr(data.rhs);
                try self.patchJump(end_jump);
            },
            .@"or" => {
                try self.compileInstr(data.lhs);
                const else_jump = self.emitJump(.jump_true);
                self.writeOp(.pop);
                try self.compileInstr(data.rhs);
                try self.patchJump(else_jump);
            },
            else => unreachable,
        }
    }

    fn block(self: *Self, data: *const Instruction.Block) Error!void {
        self.block_stack.open(self.manager.allocator);

        for (data.instrs) |instr| {
            try self.compileInstr(instr);
        }

        for (self.block_stack.close().items) |b| {
            try self.patchJump(b);
        }

        // PERF: horrible perf, just emit a stack.top -= count
        for (0..data.pop_count) |_| {
            self.writeOp(.pop);
        }

        if (data.is_expr) self.writeOp(.load_blk_val);
    }

    fn boolInstr(self: *Self, value: bool) Error!void {
        self.writeOp(if (value) .push_true else .push_false);
    }

    // TODO: protext cast
    fn boundMethod(self: *Self, data: Instruction.BoundMethod) Error!void {
        try self.compileInstr(data.structure);
        // Get method duplicates instance on top of stack and it's used by closure
        self.writeOpAndByte(.get_method, @intCast(data.index));
        self.writeOpAndByte(.closure, 1);
    }

    fn breakInstr(self: *Self, data: Instruction.Break) Error!void {
        if (data.instr) |instr| {
            try self.compileInstr(instr);
            self.writeOp(.store_blk_val);
        }

        self.block_stack.add(self.manager.allocator, self.emitJump(.jump), data.depth);
    }

    // TODO: protect cast
    fn capture(self: *Self, data: *const Instruction.FnDecl.Capture) Error!void {
        self.writeOpAndByte(if (data.local) .get_capt_local else .get_capt_frame, @intCast(data.index));
    }

    fn cast(self: *Self, typ: rir.Type) Error!void {
        switch (typ) {
            .float => self.writeOp(.cast_to_float, self.getLineNumber()),
            .int => unreachable,
        }
    }

    fn field(self: *Self, data: *const Instruction.Field) Error!void {
        try self.compileInstr(data.structure);

        // Get field/bound method of first value on the stack
        self.writeOpAndByte(
            if (data.kind == .field)
                if (self.state.cow) .get_field_cow else .get_field
            else if (data.kind == .symbol)
                .load_sym
            else if (data.kind == .method)
                .get_method
            else
                .get_static_method,
            @intCast(data.index),
        );
    }

    fn floatInstr(self: *Self, value: f64) Error!void {
        try self.emitConstant(Value.makeFloat(value));
    }

    fn fnCall(self: *Self, data: *const Instruction.Call) Error!void {
        try self.compileInstr(data.callee);
        try self.compileArgs(data.args);
        // TODO: protect cast
        self.writeOpAndByte(
            if (data.native) .call_native else .call,
            @as(u8, @intCast(data.args.len)) + @intFromBool(data.implicit_first),
        );
    }

    fn compileArgs(self: *Self, args: []const Instruction.Arg) Error!void {
        for (args) |arg| {
            switch (arg) {
                .default => |index| self.writeOpAndByte(.get_default, @intCast(index)),
                .instr => |instr| try self.compileInstr(instr),
            }
        }
    }

    fn compileCallable(self: *Self, name: []const u8, data: *const Instruction.FnDecl) Error!*Obj.Function {
        // TODO: protect cast
        var compiler = Compiler.init(self.manager, name, @intCast(data.defaults.len), self.function.module_index);

        for (data.defaults, 0..) |def, i| {
            compiler.function.default_values[i] = try self.compileDefaultValue(def);
        }

        for (data.body) |instr| {
            try compiler.compileInstr(instr);
        }

        // If the function doesn't return by itself, we emit one naked return
        if (!data.returns) compiler.writeOp(.ret_naked);
        return compiler.end();
    }

    fn compileFn(self: *Self, data: *const Instruction.FnDecl) Error!void {
        const fn_name = if (data.name) |idx| self.manager.interner.getKey(idx).? else "Script";

        const index = switch (data.kind) {
            .symbol => |idx| idx,
            .closure => return self.compileClosure(data, fn_name),
        };

        const func = try self.compileCallable(fn_name, data);
        self.addSymbol(index, Value.makeObj(func.asObj()));
    }

    fn compileClosure(self: *Self, data: *const Instruction.FnDecl, name: ?[]const u8) Error!void {
        const func = try self.compileCallable(name orelse "anonymus", data);
        try self.emitConstant(Value.makeObj(func.asObj()));

        for (data.captures) |*capt| {
            try self.capture(capt);
        }

        self.writeOpAndByte(.closure, @intCast(data.captures.len));
    }

    fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
        self.emitGetVar(data);
    }

    fn intInstr(self: *Self, value: isize) Error!void {
        try self.emitConstant(Value.makeInt(value));
    }

    fn ifInstr(self: *Self, data: *const Instruction.If) Error!void {
        const is_extractor = self.manager.instr_data[data.cond] == .extractor;
        try self.compileInstr(data.cond);

        const then_jump = self.emitJump(.jump_false);
        // Pops the condition
        self.writeOp(.pop);

        // Then body
        try self.compileInstr(data.then);

        // Exits the if expression
        const else_jump = self.emitJump(.jump);
        try self.patchJump(then_jump);

        // If we go in the else branch, we pop the condition too
        self.writeOp(.pop);
        // If the condition was an extractor, the variable tested against `null` is still on
        // top of stack so we have to remove it (see `extractor` function)
        if (is_extractor) self.writeOp(.pop);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.@"else") |instr| {
            try self.compileInstr(instr);
        }

        try self.patchJump(else_jump);
    }

    // TODO: protect the casts
    fn loadSymbol(self: *Self, data: *const Instruction.LoadSymbol) Error!void {
        if (data.module_index) |mod| {
            self.writeOpAndByte(.load_extern_sym, @intCast(mod));
            self.writeByte(data.symbol_index);
        } else {
            self.writeOpAndByte(.load_sym, data.symbol_index);
        }
    }

    fn multipleVarDecl(self: *Self, data: *const Instruction.MultiVarDecl) Error!void {
        for (data.decls) |decl| {
            try self.compileInstr(decl);
        }
    }

    fn nullInstr(self: *Self) Error!void {
        self.writeOp(.push_null);
    }

    fn returnInstr(self: *Self, data: *const Instruction.Return) Error!void {
        if (data.value) |val| {
            try self.compileInstr(val);
            self.writeOp(.ret);
        } else self.writeOp(.ret_naked);
    }

    fn stringInstr(self: *Self, index: usize) Error!void {
        try self.emitConstant(
            Value.makeObj(Obj.String.copy(
                self.manager.vm,
                self.manager.interner.getKey(index).?,
            ).asObj()),
        );
    }

    fn structDecl(self: *Self, data: *const Instruction.StructDecl) Error!void {
        // TODO: just fetch symbol in symbol table?
        var structure = Obj.Structure.create(
            self.manager.vm,
            Obj.String.copy(self.manager.vm, self.manager.interner.getKey(data.name).?),
            data.fields_count,
            data.default_fields.len,
            &.{},
        );

        // We forward declare the structure in the globals because when disassembling the
        // structure's method, they need to refer to the object. Only the name can be refered to
        // TODO: Create a placeholder that has only the name?
        self.addSymbol(data.index, Value.makeObj(structure.asObj()));

        // We compile each default value and as we know there are pure
        for (data.default_fields, 0..) |def, i| {
            structure.default_values[i] = try self.compileDefaultValue(def);
        }

        var funcs: ArrayList(*Obj.Function) = .empty;
        funcs.ensureTotalCapacity(self.manager.vm.allocator, data.functions.len) catch oom();

        for (data.functions) |func| {
            const fn_data = self.manager.instr_data[func].fn_decl;
            const fn_name = if (fn_data.name) |idx| self.manager.interner.getKey(idx).? else "anonymus";
            funcs.appendAssumeCapacity(try self.compileCallable(fn_name, &fn_data));
        }

        structure.methods = funcs.toOwnedSlice(self.manager.vm.allocator) catch oom();
        const struct_obj = Value.makeObj(structure.asObj());
        self.addSymbol(data.index, struct_obj);
    }

    fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) Error!void {
        try self.compileInstr(data.structure);
        try self.compileArgs(data.values);
        // TODO: protect cast
        self.writeOpAndByte(.struct_lit, @intCast(data.values.len));
    }

    fn unary(self: *Self, data: *const Instruction.Unary) Error!void {
        try self.compileInstr(data.instr);

        if (data.op == .minus) {
            self.writeOp(if (data.typ == .int) .neg_int else .neg_float);
        } else {
            self.writeOp(.not);
        }
    }

    fn varDecl(self: *Self, data: *const Instruction.VarDecl) Error!void {
        if (data.value) |val| {
            try self.compileInstr(val);
        } else {
            self.writeOp(.push_null);
        }

        // TODO: Fix this, just to avoid accessing an empty slot at runtime
        // If we are top level, value should be pure and compile time known
        // The purpose is to initialize the slot so when accessed like self.globals[idx] we don't segfault
        if (data.variable.scope == .global) {
            self.addGlobal(data.variable.index, .null_);
        } else if (data.box) {
            self.writeOp(.box);
        }

        self.defineVariable(data.variable);
    }

    fn whileInstr(self: *Self, data: Instruction.While) Error!void {
        const is_extractor = self.manager.instr_data[data.cond] == .extractor;
        self.block_stack.open(self.manager.allocator);

        const loop_start = self.getChunk().code.items.len;

        try self.compileInstr(data.cond);
        const exit_jump = self.emitJump(.jump_false);

        // If true
        self.writeOp(.pop);

        const body = self.manager.instr_data[data.body].block;

        for (body.instrs) |instr| {
            try self.compileInstr(instr);
        }

        for (0..body.pop_count) |_| {
            self.writeOp(.pop);
        }

        try self.emitLoop(loop_start);

        try self.patchJump(exit_jump);
        // If false
        self.writeOp(.pop);
        // If the condition was an extractor, the variable tested against `null` is still on
        // top of stack so we have to remove it (see `extractor` function)
        if (is_extractor) self.writeOp(.pop);

        for (self.block_stack.close().items) |b| {
            try self.patchJump(b);
        }
    }

    fn compileDefaultValue(self: *Self, instr: rir.Index) Error!Value {
        return switch (self.manager.instr_data[instr]) {
            .int => |val| Value.makeInt(val),
            .float => |val| Value.makeFloat(val),
            .bool => |val| Value.makeBool(val),
            .string => |val| Value.makeObj(Obj.String.copy(
                self.manager.vm,
                self.manager.interner.getKey(val).?,
            ).asObj()),
            else => @panic("Default value type not supported yet"),
        };
    }
};
