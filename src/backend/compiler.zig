const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;
const FieldEnum = std.meta.FieldEnum;

const Disassembler = @import("../backend/Disassembler.zig");
const rir = @import("../frontend/rir.zig");
const Instruction = rir.Instruction;
const Interner = @import("../Interner.zig");
const ModuleInterner = @import("../ModuleInterner.zig");
const GenReport = @import("../reporter.zig").GenReport;
const Obj = @import("../runtime/Obj.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");
// const NativeFn = @import("../std/meta.zig").NativeFn;
const oom = @import("../utils.zig").oom;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;

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
    // natives: []const NativeFn,
    interner: *const Interner,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    instr_data: []const Instruction.Data,
    instr_lines: []const usize,
    instr_idx: usize,
    render_mode: Disassembler.RenderMode,
    module: CompiledModule,

    const Self = @This();
    const Error = error{err} || Chunk.Error || std.posix.WriteError;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        allocator: Allocator,
        name: []const u8,
        vm: *Vm,
        interner: *const Interner,
        // natives: []const NativeFn,
        render_mode: Disassembler.RenderMode,
        global_count: usize,
        symbol_count: usize,
    ) Self {
        return .{
            .allocator = allocator,
            .vm = vm,
            .interner = interner,
            // .natives = natives,
            .compiler = undefined,
            .errs = .empty,
            .instr_idx = 0,
            .instr_data = undefined,
            .instr_lines = undefined,
            .render_mode = render_mode,
            .module = .init(allocator, name, global_count, symbol_count),
        };
    }

    pub fn compile(
        self: *Self,
        instr_start: usize,
        instr_data: []const Instruction.Data,
        instr_lines: []const usize,
        main_index: ?usize,
        module_index: usize,
    ) !CompiledModule {
        self.instr_idx = instr_start;
        self.instr_data = instr_data;
        self.instr_lines = instr_lines;

        if (self.render_mode != .none) {
            var buf: [256]u8 = undefined;
            var stdout = std.fs.File.stdout().writer(&buf);
            stdout.interface.print("//---- {s} ----\n\n", .{self.module.name}) catch oom();
            stdout.interface.flush() catch oom();
        }

        self.compiler = Compiler.init(self, "global scope", 0, module_index);

        while (self.instr_idx < self.instr_data.len) {
            try self.compiler.compileInstr();
        }

        if (main_index) |idx| {
            // TODO: protect
            self.compiler.writeOpAndByte(.load_sym, @intCast(idx), 0);
            self.compiler.writeOpAndByte(.call, 0, 0);
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
    state: State = .{},

    const Self = @This();
    const Error = error{Err} || Chunk.Error || std.posix.WriteError;

    const CompilerReport = GenReport(CompilerMsg);
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
        };
    }

    fn at(self: *const Self) *const Instruction.Data {
        return &self.manager.instr_data[self.manager.instr_idx];
    }

    fn next(self: *Self) Instruction.Data {
        defer self.manager.instr_idx += 1;
        return self.manager.instr_data[self.manager.instr_idx];
    }

    fn eof(self: *const Self) bool {
        return self.manager.instr_idx == self.manager.instr_data.len;
    }

    fn getChunk(self: *Self) *Chunk {
        return &self.function.chunk;
    }

    // TODO: useless? same as `at`
    fn getData(self: *const Self) Instruction.Data {
        return self.at().*;
    }

    /// Get the line number of previous instruction
    fn getLineNumber(self: *const Self) usize {
        return self.manager.instr_lines[self.manager.instr_idx - 1];
    }

    fn setInstrIndexGetPrev(self: *Self, index: usize) usize {
        const variable_instr = self.manager.instr_idx;
        self.manager.instr_idx = index;
        return variable_instr;
    }

    /// Writes an OpCode to the current chunk
    fn writeOp(self: *Self, op: OpCode, offset: usize) void {
        self.getChunk().writeOp(op, offset);
    }

    /// Writes a byte to the current chunk
    fn writeByte(self: *Self, byte: u8, offset: usize) void {
        self.getChunk().writeByte(byte, offset);
    }

    /// Writes an OpCode and a byte to the current chunk
    fn writeOpAndByte(self: *Self, op: OpCode, byte: u8, offset: usize) void {
        self.writeOp(op, offset);
        self.writeByte(byte, offset);
    }

    fn emitConstant(self: *Self, value: Value, offset: usize) Error!void {
        // TODO: error
        self.writeOpAndByte(
            .constant,
            self.getChunk().writeConstant(value) catch |err| {
                std.debug.print("Too many constants in chunk\n", .{});
                return err;
            },
            offset,
        );
    }

    fn addGlobal(self: *Self, index: usize, global: Value) void {
        self.manager.module.globals[index] = global;
    }

    fn addSymbol(self: *Self, index: usize, symbol: Value) void {
        self.manager.module.symbols[index] = symbol;
    }

    /// Emits the corresponding `get_global` or `get_local` with the correct index
    fn emitGetVar(self: *Self, variable: *const Instruction.Variable, offset: usize) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        self.writeOpAndByte(
            if (variable.scope == .local)
                if (self.state.cow) .get_local_cow else .get_local
            else if (variable.scope == .global)
                if (self.state.cow) .get_global_cow else .get_global
            else
                unreachable,
            @intCast(variable.index),
            offset,
        );

        if (variable.unbox) self.writeOp(.unbox, offset);
    }

    /// Define the variable with value on top of stack for global variables.
    /// For locals, they are already sitting on top of stack
    fn defineVariable(self: *Self, infos: Instruction.Variable, offset: usize) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        if (infos.scope == .global) {
            self.writeOpAndByte(.def_global, @intCast(infos.index), offset);
        }
    }

    fn emitJump(self: *Self, kind: OpCode, offset: usize) usize {
        const chunk = self.getChunk();
        chunk.writeOp(kind, offset);
        chunk.writeByte(0xff, offset);
        chunk.writeByte(0xff, offset);

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

    fn emitLoop(self: *Self, loop_start: usize, offset: usize) Error!void {
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

    fn compileInstr(self: *Self) Error!void {
        try switch (self.next()) {
            .array => |*data| self.array(data),
            .array_access => self.arrayAccess(),
            .array_access_chain => |*data| self.arrayAccessChain(data),
            .assignment => |*data| self.assignment(data),
            .binop => |*data| self.binop(data),
            .block => |*data| self.block(data),
            .bool => |data| self.boolInstr(data),
            .bound_method => |data| self.boundMethod(data),
            .call => |*data| self.fnCall(data),
            .capture => unreachable,
            .cast => |data| self.cast(data),
            .default_value => unreachable,
            .discard => self.discard(),
            .extractor => self.extractor(),
            // .extractor => unreachable,
            .field => |*data| self.field(data),
            .float => |data| self.floatInstr(data),
            .fn_decl => |*data| self.compileFn(data),
            .identifier => |*data| self.identifier(data),
            .@"if" => |*data| self.ifInstr(data),
            .int => |data| self.intInstr(data),
            .load_symbol => |*data| self.loadSymbol(data),
            .multiple_var_decl => |data| self.multipleVarDecl(data),
            .name => unreachable,
            .null => self.nullInstr(),
            .pop => self.writeOp(.pop, self.getLineNumber()),
            .print => self.print(),
            .@"return" => |*data| self.returnInstr(data),
            .string => |data| self.stringInstr(data),
            .struct_decl => |*data| self.structDecl(data),
            .struct_literal => |*data| self.structLiteral(data),
            .unary => |*data| self.unary(data),
            .value => unreachable,
            .var_decl => |*data| self.varDecl(data),
            .@"while" => self.whileInstr(),
        };
    }

    fn array(self: *Self, data: *const Instruction.Array) Error!void {
        const line = self.getLineNumber();

        for (data.elems) |elem| {
            try self.compileInstr();

            if (elem.cast) {
                self.writeOp(.cast_to_float, line);
            } else if (elem.incr_rc) {
                self.writeOp(.incr_ref, line);
            }
        }
        // TODO: protect cast
        self.writeOpAndByte(.array_new, @intCast(data.elems.len), line);
    }

    fn arrayAccess(self: *Self) Error!void {
        const line = self.getLineNumber();
        // Variable
        try self.compileInstr();

        // Index, we deactivate cow for indicies because never wanted but could be triggered by a multiple array
        // access inside an array assignment
        const prev = self.state.setAndGetPrev(.cow, false);
        try self.compileInstr();
        self.state.cow = prev;

        self.writeOp(.array_get, line);
    }

    fn arrayAccessChain(self: *Self, data: *const Instruction.ArrayAccessChain) Error!void {
        const line = self.getLineNumber();
        // Indicies
        const prev = self.state.setAndGetPrev(.cow, false);
        for (0..data.depth) |_| {
            try self.compileInstr();
        }
        self.state.cow = prev;

        // Variable
        try self.compileInstr();
        // get_chain_cow is used when the chain is in the middle of assigne like: foo.bar[0][1].baz = 1
        self.writeOpAndByte(if (self.state.cow) .array_get_chain_cow else .array_get_chain, @intCast(data.depth), line);
    }

    fn arrayAssign(self: *Self, line: usize) Error!void {
        // Variable
        const prev = self.state.setAndGetPrev(.cow, true);
        try self.compileInstr();
        self.state.cow = prev;

        // Index
        try self.compileInstr();
        self.writeOp(.array_set, line);
    }

    fn arrayAssignChain(self: *Self, data: *const Instruction.ArrayAccessChain, line: usize) Error!void {
        // Indicies
        for (0..data.depth) |_| {
            try self.compileInstr();
        }

        // Variable
        const prev = self.state.setAndGetPrev(.cow, true);
        try self.compileInstr();
        self.state.cow = prev;

        // TODO: protect the cast
        self.writeOpAndByte(.array_set_chain, @intCast(data.depth), line);
    }

    fn assignment(self: *Self, data: *const Instruction.Assignment) Error!void {
        const line = self.getLineNumber();

        // Value
        try self.compileInstr();

        // We cast the value on top of stack if needed
        if (data.cast) {
            self.writeOp(.cast_to_float, line);
        } else if (data.incr_rc) {
            self.writeOp(.incr_ref, line);
        }

        // TODO: no use of data.cow?
        const variable_data = switch (self.next()) {
            .identifier => |*variable| variable,
            .array_access => return self.arrayAssign(line),
            .array_access_chain => |*array_data| return self.arrayAssignChain(array_data, line),
            .field => |*field_data| return self.fieldAssignment(field_data, line),
            else => unreachable,
        };

        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        // TODO: protect cow
        self.writeOpAndByte(
            if (variable_data.scope == .global)
                .set_global
            else if (variable_data.scope == .local)
                if (variable_data.unbox) .set_local_box else .set_local
            else
                unreachable,
            @intCast(variable_data.index),
            line,
        );
    }

    fn fieldAssignment(self: *Self, data: *const Instruction.Field, line: usize) Error!void {
        // Variable
        self.state.cow = true;
        defer self.state.cow = false;
        try self.compileInstr();
        self.writeOpAndByte(.set_field, @intCast(data.index), line);
    }

    fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
        const line = self.getLineNumber();

        // Special handle for logicals
        if (data.op == .@"and" or data.op == .@"or") return self.logicalBinop(line, data);

        try self.compileInstr();
        if (data.cast == .lhs and data.op != .mul_str and data.op != .eq_null and data.op != .ne_null) self.writeOp(.cast_to_float, line);

        try self.compileInstr();
        if (data.cast == .rhs and data.op != .mul_str and data.op != .eq_null and data.op != .ne_null) self.writeOp(.cast_to_float, line);

        // Special handle for instructions using 'cast' field for another purpose
        // If the left side is marked as the operand, we swap their position for the VM
        if (data.op == .mul_str or data.op == .eq_null or data.op == .ne_null) {
            if (data.cast == .lhs) self.writeOp(.swap, line);
        }

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
            line,
        );
    }

    fn logicalBinop(self: *Self, line: usize, data: *const Instruction.Binop) Error!void {
        switch (data.op) {
            .@"and" => {
                try self.compileInstr();
                const end_jump = self.emitJump(.jump_false, line);
                // If true, pop the value, else the 'false' remains on top of stack
                self.writeOp(.pop, line);
                try self.compileInstr();
                try self.patchJump(end_jump);
            },
            .@"or" => {
                try self.compileInstr();
                const else_jump = self.emitJump(.jump_true, line);
                self.writeOp(.pop, line);
                try self.compileInstr();
                try self.patchJump(else_jump);
            },
            else => unreachable,
        }
    }

    fn block(self: *Self, data: *const Instruction.Block) Error!void {
        const line = self.getLineNumber();

        for (0..data.length) |_| {
            try self.compileInstr();

            if (self.manager.instr_idx < self.manager.instr_data.len and self.at().* == .pop) {
                self.writeOp(.pop, 0);
                self.manager.instr_idx += 1;
            }
        }

        if (data.is_expr) {
            self.writeOpAndByte(.ret_scope, data.pop_count, line);
        } else {
            // PERF: horrible perf, just emit a stack.top -= count
            for (0..data.pop_count) |_| {
                self.getChunk().writeOp(.pop, line);
            }
        }
    }

    fn boolInstr(self: *Self, value: bool) Error!void {
        const op: OpCode = if (value) .push_true else .push_false;
        self.writeOp(op, self.getLineNumber());
    }

    // TODO: protext cast
    fn boundMethod(self: *Self, field_index: usize) Error!void {
        const line = self.getLineNumber();
        // Variable
        try self.compileInstr();
        // Get method duplicates instance on top of stack and it's used by closure
        self.writeOpAndByte(.get_method, @intCast(field_index), line);
        self.writeOpAndByte(.closure, 1, line);
    }

    fn box(self: *Self) Error!void {
        self.writeOp(.box, self.getLineNumber());
    }

    // TODO: protect cast
    fn capture(self: *Self, data: *const Instruction.Capture, line: usize) Error!void {
        self.writeOpAndByte(
            if (data.is_local) .get_capt_local else .get_capt_frame,
            @intCast(data.index),
            line,
        );
    }

    fn cast(self: *Self, typ: rir.Type) Error!void {
        switch (typ) {
            .float => self.writeOp(.cast_to_float, self.getLineNumber()),
            .int => unreachable,
        }
    }

    fn discard(self: *Self) Error!void {
        try self.compileInstr();
        self.writeOp(.pop, 0);
    }

    fn field(self: *Self, data: *const Instruction.Field) Error!void {
        const line = self.getLineNumber();

        // Member first
        try self.compileInstr();

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
            line,
        );
    }

    fn floatInstr(self: *Self, value: f64) Error!void {
        try self.emitConstant(Value.makeFloat(value), self.getLineNumber());
    }

    fn fnCall(self: *Self, data: *const Instruction.Call) Error!void {
        const line = self.getLineNumber();
        // Callee
        try self.compileInstr();
        try self.compileArgs(data.arity);
        self.writeOpAndByte(.call, data.arity + @intFromBool(data.implicit_first), line);
    }

    fn compileArgs(self: *Self, arity: usize) Error!void {
        const line = self.getLineNumber();
        var last: usize = 0;

        for (0..arity) |_| {
            switch (self.next()) {
                .value => |data| {
                    const save = self.setInstrIndexGetPrev(data.value_instr);
                    defer self.manager.instr_idx = save;
                    try self.compileInstr();
                    // Arguments may not be in the same order as the declaration, we could be
                    // resolving the first value during the last iteration
                    last = @max(last, self.manager.instr_idx);

                    // TODO: maybe define an union, because it can't be several at the same time
                    if (data.cast) self.writeOp(.cast_to_float, line);
                    if (data.box) self.writeOp(.box, line);
                    if (data.incr_rc) self.writeOp(.incr_ref, line);
                },
                .default_value => |idx| self.writeOpAndByte(.get_default, @intCast(idx), line),
                else => unreachable,
            }
        }

        if (last > self.manager.instr_idx) self.manager.instr_idx = last;
    }

    fn compileCallable(self: *Self, name: []const u8, data: *const Instruction.FnDecl) Error!*Obj.Function {
        var compiler = Compiler.init(self.manager, name, data.default_params, self.function.module_index);

        for (0..data.default_params) |i| {
            compiler.function.default_values[i] = try self.compileDefaultValue();
        }

        for (0..data.body_len) |_| {
            try compiler.compileInstr();
        }
        if (data.cast) try compiler.compileInstr();

        // Giving line 0 won't print any number as it will be less than current line as if it were on last line
        if (data.return_kind == .implicit_value) {
            compiler.writeOp(.ret, 0);
        } else if (data.return_kind == .implicit_void) {
            compiler.writeOp(.ret_naked, 0);
        }

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
        const line = self.getLineNumber();

        const func = try self.compileCallable(name orelse "anonymus", data);
        try self.emitConstant(Value.makeObj(func.asObj()), line);

        for (0..data.captures_count) |_| {
            try self.capture(&self.next().capture, line);
        }

        self.writeOpAndByte(.closure, @intCast(data.captures_count), line);
    }

    fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
        self.emitGetVar(data, self.getLineNumber());
    }

    fn intInstr(self: *Self, value: isize) Error!void {
        try self.emitConstant(Value.makeInt(value), self.getLineNumber());
    }

    fn ifInstr(self: *Self, data: *const Instruction.If) Error!void {
        const line = self.getLineNumber();

        // Condition
        const is_extractor = self.at().* == .extractor;
        try self.compileInstr();

        const then_jump = self.emitJump(.jump_false, line);
        // Pops the condition
        self.writeOp(.pop, line);

        // Then body
        try self.compileInstr();
        // TODO: can only one at the time?
        if (data.cast == .then) self.writeOp(.cast_to_float, line);
        if (data.incr_rc_then) self.writeOp(.incr_ref, line);

        // Exits the if expression
        const else_jump = self.emitJump(.jump, line);
        try self.patchJump(then_jump);

        // If we go in the else branch, we pop the condition too
        self.writeOp(.pop, line);
        // If the condition was an extractor, the variable tested against `null` is still on
        // top of stack so we have to remove it (see `extractor` function)
        if (is_extractor) self.writeOp(.pop, line);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.has_else) {
            try self.compileInstr();
            if (data.cast == .@"else") self.writeOp(.cast_to_float, line);
            if (data.incr_rc_else) self.writeOp(.incr_ref, line);
        }

        try self.patchJump(else_jump);
    }

    fn extractor(self: *Self) Error!void {
        const line = self.getLineNumber();
        try self.compileInstr();

        // In case of an extractor, we don't replace top of stack with the bool result of
        // comparison because if it's true, it's gonna be popped and so last value on stack
        // will be the one extracted, it acts as if we just declared the value in scope
        self.writeOp(.ne_null_push, line);
    }

    // TODO: protect the casts
    fn loadSymbol(self: *Self, data: *const Instruction.LoadSymbol) Error!void {
        const line = self.getLineNumber();
        if (data.module_index) |mod| {
            self.writeOpAndByte(.load_extern_sym, @intCast(mod), line);
            self.writeByte(data.symbol_index, line);
        } else {
            self.writeOpAndByte(.load_sym, data.symbol_index, line);
        }
    }

    fn multipleVarDecl(self: *Self, count: usize) Error!void {
        for (0..count) |_| {
            try self.compileInstr();
        }
    }

    fn nullInstr(self: *Self) Error!void {
        self.writeOp(.push_null, self.getLineNumber());
    }

    fn print(self: *Self) Error!void {
        const line = self.getLineNumber();
        try self.compileInstr();
        self.getChunk().writeOp(.print, line);
    }

    fn returnInstr(self: *Self, data: *const Instruction.Return) Error!void {
        const line = self.getLineNumber();

        if (data.value) {
            try self.compileInstr();
            if (data.cast) try self.compileInstr();

            self.writeOp(.ret, line);
        } else self.writeOp(.ret_naked, line);
    }

    fn stringInstr(self: *Self, index: usize) Error!void {
        try self.emitConstant(
            Value.makeObj(Obj.String.copy(
                self.manager.vm,
                self.manager.interner.getKey(index).?,
            ).asObj()),
            self.getLineNumber(),
        );
    }

    fn structDecl(self: *Self, data: *const Instruction.StructDecl) Error!void {
        const name = self.next().name;
        var structure = Obj.Structure.create(
            self.manager.vm,
            Obj.String.copy(self.manager.vm, self.manager.interner.getKey(name).?),
            data.fields_count,
            data.default_fields,
            &.{},
        );

        // We forward declare the structure in the globals because when disassembling the
        // structure's method, they need to refer to the object. Only the name can be refered to
        // TODO: Create a placeholder that has only the name?
        self.addSymbol(data.index, Value.makeObj(structure.asObj()));

        // We compile each default value and as we know there are pure, we can extract them
        // from the constants (they aren't global either). As we do that, we delete compiled
        // code to not execute it at runtime
        for (0..data.default_fields) |i| {
            structure.default_values[i] = try self.compileDefaultValue();
        }

        var funcs: ArrayList(*Obj.Function) = .empty;
        funcs.ensureTotalCapacity(self.manager.vm.allocator, data.func_count) catch oom();

        for (0..data.func_count) |_| {
            const fn_data = self.next().fn_decl;
            const fn_name = if (fn_data.name) |idx| self.manager.interner.getKey(idx).? else "anonymus";
            const func = try self.compileCallable(fn_name, &fn_data);
            funcs.appendAssumeCapacity(func);
        }

        structure.methods = funcs.toOwnedSlice(self.manager.vm.allocator) catch oom();
        const struct_obj = Value.makeObj(structure.asObj());
        self.addSymbol(data.index, struct_obj);
    }

    fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) Error!void {
        const line = self.getLineNumber();
        // Structure
        try self.compileInstr();
        try self.compileArgs(data.fields_count);
        self.writeOpAndByte(.struct_lit, @intCast(data.fields_count), line);
    }

    fn unary(self: *Self, data: *const Instruction.Unary) Error!void {
        const line = self.getLineNumber();
        try self.compileInstr();

        if (data.op == .minus) {
            self.writeOp(
                if (data.typ == .int) .neg_int else .neg_float,
                line,
            );
        } else self.writeOp(.not, line);
    }

    fn varDecl(self: *Self, data: *const Instruction.VarDecl) Error!void {
        const line = self.getLineNumber();

        if (data.has_value) {
            try self.compileInstr();
            if (data.cast) {
                try self.compileInstr();
            } else if (data.incr_rc) {
                self.writeOp(.incr_ref, line);
            }
        } else {
            self.writeOp(.push_null, line);
        }

        // TODO: Fix this, just to avoid accessing an empty slot at runtime
        // If we are top level, value should be pure and compile time known
        // The purpose is to initialize the slot so when accessed like self.globals[idx] we don't segfault
        if (data.variable.scope == .global) {
            self.addGlobal(data.variable.index, .null_);
        } else if (data.box) {
            self.writeOp(.box, line);
        }

        self.defineVariable(data.variable, line);
    }

    fn whileInstr(self: *Self) Error!void {
        const line = self.getLineNumber();

        const chunk = self.getChunk();
        const loop_start = chunk.code.items.len;

        try self.compileInstr();
        const exit_jump = self.emitJump(.jump_false, line);

        // If true
        chunk.writeOp(.pop, line);
        try self.compileInstr();
        try self.emitLoop(loop_start, line);

        try self.patchJump(exit_jump);
        // If false
        chunk.writeOp(.pop, line);
    }

    fn compileDefaultValue(self: *Self) Error!Value {
        const code_start = self.function.chunk.code.items.len;
        defer self.function.chunk.code.shrinkRetainingCapacity(code_start);
        defer self.function.chunk.offsets.shrinkRetainingCapacity(code_start);
        try self.compileInstr();

        return switch (@as(OpCode, @enumFromInt(self.function.chunk.code.items[code_start]))) {
            .constant => b: {
                self.function.chunk.constant_count -= 1;
                var val = self.function.chunk.constants[self.function.chunk.constant_count];

                if (self.manager.instr_idx < self.manager.instr_data.len and self.at().* == .cast) {
                    self.manager.instr_idx += 1;
                    val = Value.makeFloat(@floatFromInt(val.int));
                }

                break :b val;
            },
            .push_false => Value.false_,
            .push_true => Value.true_,
            // Because we know it's pure
            else => unreachable,
        };
    }
};
