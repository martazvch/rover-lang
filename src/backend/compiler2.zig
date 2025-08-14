const std = @import("std");
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Disassembler = @import("../backend/Disassembler2.zig");
const rir = @import("../frontend/rir2.zig");
const Instruction = rir.Instruction;
const Module = @import("../Pipeline2.zig").Module;
const GenReport = @import("../reporter.zig").GenReport;
const Obj = @import("../runtime/Obj2.zig");
const Value = @import("../runtime/values2.zig").Value;
const Vm = @import("../runtime/Vm2.zig");
const NativeFn = @import("../std/meta.zig").NativeFn;
const oom = @import("../utils.zig").oom;
const Chunk = @import("Chunk2.zig");
const OpCode = Chunk.OpCode;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;

pub const CompilationManager = struct {
    vm: *Vm,
    natives: []const NativeFn,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    instr_data: []const Instruction.Data,
    instr_lines: []const usize,
    instr_idx: usize,
    render_mode: Disassembler.RenderMode,
    globals: *ArrayListUnmanaged(Value),
    symbols: []Value,
    modules: []Module,

    const Self = @This();
    const Error = error{err} || Chunk.Error || std.posix.WriteError;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        vm: *Vm,
        natives: []const NativeFn,
        render_mode: Disassembler.RenderMode,
        globals: *ArrayListUnmanaged(Value),
        symbol_count: usize,
        modules: []Module,
    ) Self {
        const symbols = vm.allocator.alloc(Value, symbol_count) catch oom();

        return .{
            .vm = vm,
            .natives = natives,
            .compiler = undefined,
            .errs = .init(vm.allocator),
            .instr_idx = undefined,
            .instr_data = undefined,
            .instr_lines = undefined,
            .render_mode = render_mode,
            .globals = globals,
            .symbols = symbols,
            .modules = modules,
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
    }

    pub fn compile(
        self: *Self,
        file_name: []const u8,
        instr_start: usize,
        instr_data: []const Instruction.Data,
        instr_lines: []const usize,
        main_index: ?usize,
        repl: bool,
    ) !*Obj.Function {
        self.instr_idx = instr_start;
        self.instr_data = instr_data;
        self.instr_lines = instr_lines;

        if (self.render_mode != .none) {
            const stdout = std.io.getStdOut().writer();
            try stdout.print("//---- {s} ----\n\n", .{file_name});
        }

        self.compiler = Compiler.init(self, "global scope", 0);

        while (self.instr_idx < self.instr_data.len) {
            try self.compiler.compileInstr();
        }

        if (!repl) {
            // Insert a call to main with arity of 0 for now
            // self.compiler.writeOpAndByte(.get_global, self.main_index.?, 0);
            // TODO: protect
            self.compiler.writeOpAndByte(.get_symbol, @intCast(main_index.?), 0);
            self.compiler.writeOpAndByte(.call, 0, 0);
        } else {
            self.compiler.getChunk().writeOp(.exit_repl, 0);
        }

        return self.compiler.end();
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
        /// Triggers a cow
        cow: bool = false,
    };

    const FnKind = enum { global, @"fn", method };

    pub fn init(manager: *CompilationManager, name: []const u8, default_count: usize) Self {
        return .{
            .manager = manager,
            .function = Obj.Function.create(manager.vm, Obj.String.copy(manager.vm, name), default_count),
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

    fn addGlobal(self: *Self, global: Value) u8 {
        self.manager.globals.append(self.manager.vm.allocator, global) catch oom();
        // TODO: protect
        return @intCast(self.manager.globals.items.len - 1);
    }

    fn addSymbol(self: *Self, index: usize, symbol: Value) void {
        self.manager.symbols[index] = symbol;
    }

    /// Emits the corresponding `get_heap`, `get_global` or `get_local` with the correct index
    fn emitGetVar(self: *Self, variable: *const Instruction.Variable, offset: usize) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        self.writeOpAndByte(
            if (variable.scope == .local)
                if (self.state.cow) .get_local_cow else .get_local
            else if (variable.scope == .global)
                .get_global
            else
                unreachable,
            @intCast(variable.index),
            offset,
        );

        if (variable.unbox) self.writeOp(.unbox, offset);
    }

    /// Declare the variable based on informations coming from Analyzer. Declares
    /// either in global scope or heap, as for local it's already living
    /// on the stack
    // TODO: rename as it can only act on global?
    fn defineVariable(self: *Self, infos: Instruction.Variable, offset: usize) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        if (infos.scope == .global)
            self.writeOpAndByte(.define_global, @intCast(infos.index), offset);
        // else if (infos.scope == .heap)
        //     self.writeOpAndByte(.define_heap_var, @intCast(infos.index), offset);
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
            var dis = Disassembler.init(
                self.manager.vm.allocator,
                &self.function.chunk,
                self.manager.globals.items,
                self.manager.render_mode,
            );
            defer dis.deinit();
            dis.disChunk(if (self.function.name) |n| n.chars else "Script") catch oom();
            const stdout = std.io.getStdOut().writer();
            stdout.print("{s}\n", .{dis.disassembled.items}) catch oom();
        }

        return self.function;
    }

    fn compileInstr(self: *Self) Error!void {
        try switch (self.next()) {
            .array => |*data| self.array(data),
            .array_access => |*data| self.arrayAccess(data, self.getLineNumber()),
            .array_access_chain => |*data| self.arrayAccessChain(data, self.getLineNumber()),
            .assignment => |*data| self.assignment(data),
            .binop => |*data| self.binop(data),
            .block => |*data| self.block(data),
            .bool => |data| self.boolInstr(data),
            .bound_method => |data| self.boundMethod(data),
            .call => |*data| self.fnCall(data),
            .cast => |data| self.cast(data),
            .default_value => unreachable,
            .discard => self.discard(),
            .field => |*data| self.field(data),
            .float => |data| self.floatInstr(data),
            .fn_decl => |*data| self.compileFn(data),
            .identifier => |*data| self.identifier(data),
            .@"if" => |*data| self.ifInstr(data),
            .imported => unreachable,
            .int => |data| self.intInstr(data),
            .item_import => |*data| self.itemImport(data),
            .module_import => |*data| self.moduleImport(data),
            .multiple_var_decl => |data| self.multipleVarDecl(data),
            .name => unreachable,
            .null => self.nullInstr(),
            .print => self.print(),
            .@"return" => |*data| self.returnInstr(data),
            .string => |data| self.stringInstr(data),
            .struct_decl => |*data| self.structDecl(data),
            .struct_literal => |*data| self.structLiteral(data),
            .symbol_id => |data| self.symbolId(data),
            .unary => |*data| self.unary(data),
            .use => |data| self.use(data),
            .value => unreachable,
            .var_decl => |*data| self.varDecl(data),
            .@"while" => self.whileInstr(),
        };
    }

    fn array(self: *Self, data: *const Instruction.Array) Error!void {
        const start = self.getLineNumber();
        var cast_count: usize = 0;

        for (0..data.len) |i| {
            try self.compileInstr();

            if (data.cast_until > 0 and i < data.cast_until - 1) {
                self.writeOp(.cast_to_float, start);
            }

            if (!self.eof() and self.at().* == .cast and cast_count < data.cast_count) {
                cast_count += 1;
                try self.compileInstr();
            }
        }
        // TODO: protect cast
        self.writeOpAndByte(.array, @intCast(data.len), start);
    }

    fn arrayAccess(self: *Self, data: *const Instruction.ArrayAccess, start: usize) Error!void {
        // Variable
        try self.compileInstr();
        try self.compileInstr();
        self.writeOp(.array_access, start);

        if (data.incr_ref) self.writeOp(.incr_ref_count, start);
    }

    fn arrayAccessChain(self: *Self, data: *const Instruction.ArrayAccessChain, start: usize) Error!void {
        // Indicies
        for (0..data.depth) |_| {
            try self.compileInstr();
        }

        // Variable
        // self.state.to_reg = true;
        // const reg = self.putChainResultInReg();

        try self.compileInstr();

        // self.writeOpAndByte(if (reg) .array_access_chain_reg else .array_access_chain, @intCast(data.depth), start);
        if (data.incr_ref) self.writeOp(.incr_ref_count, start);
    }

    fn arrayAssign(self: *Self, start: usize) Error!void {
        // Variable
        self.state.cow = true;
        defer self.state.cow = false;
        try self.compileInstr();

        // Index
        try self.compileInstr();

        // If it's a simple identifier, when we fetch the variable we check for cow, so we don't place
        // the variable in a register as it is already cloned if needed on the stack
        // For more complex array expressions, it will be in a register
        self.writeOp(.array_assign, start);
    }

    fn arrayAssignChain(self: *Self, data: *const Instruction.ArrayAccessChain, start: usize) Error!void {
        // Indicies
        for (0..data.depth) |_| {
            try self.compileInstr();
        }

        // Variable
        const prev = self.setToRegAndGetPrevious(true);
        defer self.state.to_reg = prev;
        try self.compileInstr();

        // TODO: protect the cast
        self.writeOpAndByte(.array_assign_chain, @intCast(data.depth), start);
    }

    fn assignment(self: *Self, data: *const Instruction.Assignment) Error!void {
        const start = self.getLineNumber();

        // Value
        try self.compileInstr();

        // We cast the value on top of stack if needed
        if (data.cast) {
            self.writeOp(.cast_to_float, start);
        } else if (data.incr_rc) {
            self.writeOp(.incr_ref_count, start);
        }

        // TODO: no use of data.cow?
        const variable_data = switch (self.next()) {
            .identifier => |*variable| variable,
            .array_access => return self.arrayAssign(start),
            // .array_access_chain => |*array_data| return self.arrayAssignChain(array_data, start),
            .field => |*field_data| return self.fieldAssignment(field_data, start),
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
            start,
        );
    }

    fn fieldAssignment(self: *Self, data: *const Instruction.Field, start: usize) Error!void {
        // Variable
        self.state.cow = true;
        defer self.state.cow = false;
        try self.compileInstr();
        self.writeOpAndByte(.set_field, @intCast(data.index), start);
    }

    fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
        const start = self.getLineNumber();

        // Special handle for logicals
        if (data.op == .@"and" or data.op == .@"or") return self.logicalBinop(start, data);

        try self.compileInstr();
        if (data.cast == .lhs and data.op != .mul_str) self.writeOp(.cast_to_float, start);

        try self.compileInstr();
        if (data.cast == .rhs and data.op != .mul_str) self.writeOp(.cast_to_float, start);

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
                .mul_str => if (data.cast == .rhs) .str_mul_r else .str_mul_l,
                .ne_bool => .ne_bool,
                .ne_float => .ne_float,
                .ne_int => .ne_int,
                .ne_str => .ne_str,
                .sub_float => .sub_float,
                .sub_int => .sub_int,
                else => unreachable,
            },
            start,
        );
    }

    fn logicalBinop(self: *Self, start: usize, data: *const Instruction.Binop) Error!void {
        switch (data.op) {
            .@"and" => {
                try self.compileInstr();
                const end_jump = self.emitJump(.jump_if_false, start);
                // If true, pop the value, else the 'false' remains on top of stack
                self.writeOp(.pop, start);
                try self.compileInstr();
                try self.patchJump(end_jump);
            },
            .@"or" => {
                try self.compileInstr();
                const else_jump = self.emitJump(.jump_if_true, start);
                self.writeOp(.pop, start);
                try self.compileInstr();
                try self.patchJump(else_jump);
            },
            else => unreachable,
        }
    }

    fn block(self: *Self, data: *const Instruction.Block) Error!void {
        const start = self.getLineNumber();

        for (0..data.length) |_| {
            try self.compileInstr();
        }

        if (data.is_expr) {
            self.writeOpAndByte(.scope_return, data.pop_count, start);
        } else {
            // PERF: horrible perf, just emit a stack.top -= count
            for (0..data.pop_count) |_| {
                self.getChunk().writeOp(.pop, start);
            }
        }
    }

    fn boolInstr(self: *Self, value: bool) Error!void {
        const op: OpCode = if (value) .true else .false;
        self.writeOp(op, self.getLineNumber());
    }

    // TODO: protext cast
    fn boundMethod(self: *Self, field_index: usize) Error!void {
        const start = self.getLineNumber();
        // Variable
        try self.compileInstr();
        self.writeOp(.dup, start);
        self.writeOpAndByte(.get_method, @intCast(field_index), start);
        self.writeOp(.swap, start);
        self.writeOpAndByte(.closure, 1, start);
    }

    fn box(self: *Self) Error!void {
        self.writeOp(.box, self.getLineNumber());
    }

    fn cast(self: *Self, typ: rir.Type) Error!void {
        const start = self.getLineNumber();

        switch (typ) {
            .float => self.writeOp(.cast_to_float, start),
            .int => unreachable,
        }
    }

    fn discard(self: *Self) Error!void {
        try self.compileInstr();
        self.writeOp(.pop, 0);
    }

    fn field(self: *Self, data: *const Instruction.Field) Error!void {
        const member_data = self.getData();
        _ = member_data; // autofix
        const start = self.getLineNumber();

        // Member first
        try self.compileInstr();

        // Get field/bound method of first value on the stack
        self.writeOpAndByte(
            if (data.kind == .field)
                .get_field
            else if (data.kind == .symbol)
                .get_symbol
            else if (data.kind == .method)
                .get_method
            else
                .get_static_method,
            @intCast(data.index),
            start,
        );
    }

    fn floatInstr(self: *Self, value: f64) Error!void {
        try self.emitConstant(Value.makeFloat(value), self.getLineNumber());
    }

    fn fnCall(self: *Self, data: *const Instruction.Call) Error!void {
        const start = self.getLineNumber();

        try self.compileInstr();
        if (data.default_count > 0) self.writeOp(.load_fn_default, start);
        try self.compileArgs(data.arity);

        self.writeOpAndByte(.call, data.arity, start);
    }

    fn compileArgs(self: *Self, arity: usize) Error!void {
        const start = self.getLineNumber();
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

                    if (data.cast) self.writeOp(.cast_to_float, start);
                    if (data.box) self.writeOp(.box, start);
                },
                .default_value => |idx| self.writeOpAndByte(.get_default, @intCast(idx), start),
                else => unreachable,
            }
        }

        if (last > self.manager.instr_idx) self.manager.instr_idx = last;
    }

    fn compileCallable(self: *Self, name: []const u8, data: *const Instruction.FnDecl) Error!*Obj.Function {
        var compiler = Compiler.init(self.manager, name, data.default_params);

        for (0..data.default_params) |i| {
            compiler.function.default_values[i] = try self.compileDefaultValue();
        }

        for (0..data.body_len) |_| {
            try compiler.compileInstr();
        }

        if (data.return_kind == .implicit_value) {
            compiler.writeOp(.@"return", self.getLineNumber());
        } else if (data.return_kind == .implicit_void) {
            compiler.writeOp(.naked_return, self.getLineNumber());
        }

        return compiler.end();
    }

    fn compileFn(self: *Self, data: *const Instruction.FnDecl) Error!void {
        const fn_name = if (data.name) |idx| self.manager.vm.interner.getKey(idx).? else "anonymus";

        const index = switch (data.kind) {
            .symbol => |idx| idx,
            .closure => return self.compileClosure(data, fn_name),
        };

        const func = try self.compileCallable(fn_name, data);
        self.addSymbol(index, Value.makeObj(func.asObj()));
    }

    fn compileClosure(self: *Self, data: *const Instruction.FnDecl, name: ?[]const u8) Error!void {
        const start = self.getLineNumber();

        const func = try self.compileCallable(name orelse "anonymus", data);
        try self.emitConstant(Value.makeObj(func.asObj()), start);

        for (0..data.captures_count) |_| {
            self.emitGetVar(&self.next().identifier, start);
        }

        self.writeOpAndByte(.closure, @intCast(data.captures_count), start);
    }

    fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
        self.emitGetVar(data, self.getLineNumber());
    }

    fn intInstr(self: *Self, value: isize) Error!void {
        try self.emitConstant(Value.makeInt(value), self.getLineNumber());
    }

    fn ifInstr(self: *Self, data: *const Instruction.If) Error!void {
        const start = self.getLineNumber();

        // Condition
        try self.compileInstr();
        const then_jump = self.emitJump(.jump_if_false, start);
        // Pops the condition, no longer needed
        self.writeOp(.pop, start);

        // Then body
        try self.compileInstr();
        if (data.cast == .then) self.writeOp(.cast_to_float, start);

        // Exits the if expression
        const else_jump = self.emitJump(.jump, start);
        try self.patchJump(then_jump);

        // If we go in the else branch, we pop the condition too
        self.writeOp(.pop, start);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.has_else) {
            try self.compileInstr();
            if (data.cast == .@"else") self.writeOp(.cast_to_float, start);
        }

        try self.patchJump(else_jump);
    }

    fn itemImport(self: *Self, data: *const Instruction.ItemImport) Error!void {
        const module_ref = &self.manager.modules[data.module_index];
        const module = Obj.ObjModule.create(self.manager.vm, module_ref);
        const value = Value.makeObj(Obj.BoundImport.create(
            self.manager.vm,
            module,
            module_ref.globals[data.field_index].obj,
        ).asObj());

        if (data.scope == .global) {
            _ = self.addGlobal(value);
        } else {
            try self.emitConstant(value, self.getLineNumber());
        }
    }

    fn moduleImport(self: *Self, data: *const Instruction.ModuleImport) Error!void {
        if (data.scope == .global) {
            _ = self.addGlobal(Value.makeObj(Obj.ObjModule.create(self.manager.vm, &self.manager.modules[data.index]).asObj()));
        } else {
            self.writeOpAndByte(.push_module, @intCast(data.index), self.getLineNumber());
        }
    }

    fn multipleVarDecl(self: *Self, count: usize) Error!void {
        for (0..count) |_| {
            try self.compileInstr();
        }
    }

    fn nullInstr(self: *Self) Error!void {
        self.writeOp(.null, self.getLineNumber());
    }

    fn print(self: *Self) Error!void {
        const start = self.getLineNumber();
        try self.compileInstr();
        self.getChunk().writeOp(.print, start);
    }

    fn returnInstr(self: *Self, data: *const Instruction.Return) Error!void {
        const start = self.getLineNumber();

        if (data.value) {
            try self.compileInstr();
            if (data.cast) try self.compileInstr();

            self.writeOp(.@"return", start);
        } else self.writeOp(.naked_return, start);
    }

    fn stringInstr(self: *Self, index: usize) Error!void {
        try self.emitConstant(
            Value.makeObj(Obj.String.copy(
                self.manager.vm,
                self.manager.vm.interner.getKey(index).?,
            ).asObj()),
            self.getLineNumber(),
        );
    }

    fn structDecl(self: *Self, data: *const Instruction.StructDecl) Error!void {
        const name = self.next().name;
        var structure = Obj.Structure.create(
            self.manager.vm,
            Obj.String.copy(self.manager.vm, self.manager.vm.interner.getKey(name).?),
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

        var funcs: ArrayListUnmanaged(*Obj.Function) = .{};
        funcs.ensureTotalCapacity(self.manager.vm.allocator, data.func_count) catch oom();

        for (0..data.func_count) |_| {
            const fn_data = self.next().fn_decl;
            const fn_name = if (fn_data.name) |idx| self.manager.vm.interner.getKey(idx).? else "anonymus";
            const func = try self.compileCallable(fn_name, &fn_data);
            funcs.appendAssumeCapacity(func);
        }

        structure.methods = funcs.toOwnedSlice(self.manager.vm.allocator) catch oom();
        const struct_obj = Value.makeObj(structure.asObj());
        self.addSymbol(data.index, struct_obj);
    }

    fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) Error!void {
        const start = self.getLineNumber();
        // Compile structure
        try self.compileInstr();
        if (data.default_count > 0) self.writeOp(.load_struct_def, start);
        try self.compileArgs(data.fields_count);
        self.writeOpAndByte(.struct_literal, @intCast(data.fields_count), start);
    }

    fn symbolId(self: *Self, index: u8) Error!void {
        self.writeOpAndByte(.get_symbol, index, self.getLineNumber());
    }

    fn unary(self: *Self, data: *const Instruction.Unary) Error!void {
        const start = self.getLineNumber();
        try self.compileInstr();

        if (data.op == .minus) {
            self.writeOp(
                if (data.typ == .int) .negate_int else .negate_float,
                start,
            );
        } else self.writeOp(.not, start);
    }

    fn use(self: *Self, count: usize) Error!void {
        // NOTE: For now, analyzer places an empty import
        // to skip "std" by placing a Null instruction. Needs a rework
        self.manager.instr_idx += 1;

        for (0..count) |_| {
            const start = self.getLineNumber();
            const imported = self.next().imported;

            try self.emitConstant(
                Value.makeObj(Obj.NativeFunction.create(
                    self.manager.vm,
                    self.manager.natives[imported.index],
                ).asObj()),
                start,
            );
            self.defineVariable(imported.variable, start);
        }
    }

    fn varDecl(self: *Self, data: *const Instruction.VarDecl) Error!void {
        const start = self.getLineNumber();

        if (data.has_value) {
            try self.compileInstr();
            if (data.cast) {
                try self.compileInstr();
            } else if (data.incr_rc) {
                self.writeOp(.incr_ref_count, start);
            }
        } else {
            self.writeOp(.null, start);
        }

        // TODO: Fix this, just to avoid accessing an empty slot at runtime
        // If we are top level, value should be pure and compile time known
        // The purpose is to initialize the slot so when accessed like self.globals[idx] we don't segfault
        if (data.variable.scope == .global) {
            _ = self.addGlobal(.null_);
        } else if (data.box) {
            self.writeOp(.box, start);
        }

        self.defineVariable(data.variable, start);
    }

    fn whileInstr(self: *Self) Error!void {
        const start = self.getLineNumber();

        const chunk = self.getChunk();
        const loop_start = chunk.code.items.len;

        try self.compileInstr();
        const exit_jump = self.emitJump(.jump_if_false, start);

        // If true
        chunk.writeOp(.pop, start);
        try self.compileInstr();
        try self.emitLoop(loop_start, start);

        try self.patchJump(exit_jump);
        // If false
        chunk.writeOp(.pop, start);
    }

    fn compileDefaultValue(self: *Self) Error!Value {
        const code_start = self.function.chunk.code.items.len;
        defer self.function.chunk.code.shrinkRetainingCapacity(code_start);
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
            .false => Value.false_,
            .true => Value.true_,
            // Because we know it's pure
            else => unreachable,
        };
    }
};
