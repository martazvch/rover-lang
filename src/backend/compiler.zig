const std = @import("std");
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Disassembler = @import("../backend/Disassembler.zig");
const Rir = @import("../frontend/rir.zig");
const Scope = Rir.Scope;
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Symbols = @import("../frontend/type_system.zig").Symbols;
const Module = @import("../Pipeline.zig").Module;
const GenReport = @import("../reporter.zig").GenReport;
const ObjString = @import("../runtime/Obj.zig").ObjString;
const ObjFunction = @import("../runtime/Obj.zig").ObjFunction;
const ObjNativeFn = @import("../runtime/Obj.zig").ObjNativeFn;
const ObjStruct = @import("../runtime/Obj.zig").ObjStruct;
const ObjModule = @import("../runtime/Obj.zig").ObjModule;
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");
const NativeFn = @import("../std/meta.zig").NativeFn;
const oom = @import("../utils.zig").oom;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;

pub const CompilationManager = struct {
    file_name: []const u8,
    vm: *Vm,
    natives: []const NativeFn,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    instr_data: []const Instruction.Data,
    instr_offsets: []const usize,
    instr_idx: usize,
    modules: []Module,
    render_mode: Disassembler.RenderMode,
    main: usize,
    main_index: ?u8 = null,
    repl: bool,
    heap_count: usize = 0,
    globals: ArrayListUnmanaged(Value) = .{},

    const Self = @This();
    const Error = error{err} || Chunk.Error || std.posix.WriteError;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        file_name: []const u8,
        vm: *Vm,
        natives: []const NativeFn,
        instr_start: usize,
        instructions: *const MultiArrayList(Instruction),
        modules: []Module,
        render_mode: Disassembler.RenderMode,
        main: usize,
        repl: bool,
    ) Self {
        return .{
            .file_name = file_name,
            .vm = vm,
            .natives = natives,
            .compiler = undefined,
            .errs = .init(vm.allocator),
            .instr_data = instructions.items(.data),
            .instr_offsets = instructions.items(.offset),
            .modules = modules,
            .instr_idx = instr_start,
            .render_mode = render_mode,
            .main = main,
            .repl = repl,
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
    }

    pub fn compile(self: *Self, symbols_count: usize) !*ObjFunction {
        if (self.render_mode != .none) {
            const stdout = std.io.getStdOut().writer();
            try stdout.print("//---- {s} ----\n\n", .{self.file_name});
        }

        self.compiler = Compiler.init(self, "global scope", 0);
        self.globals.ensureTotalCapacity(self.vm.allocator, symbols_count) catch oom();

        while (self.instr_idx < self.instr_data.len) {
            try self.compiler.compileInstr();
        }

        if (!self.repl) {
            // We init the heap variables into the Vm
            // NOTE: maybe return the count as we return the function and let the Vm allocate?
            self.vm.heap_vars = try self.vm.allocator.alloc(Value, self.heap_count);

            // Insert a call to main with arity of 0 for now
            self.compiler.writeOpAndByte(.get_global, self.main_index.?, 0);
            self.compiler.writeOpAndByte(.call, 0, 0);
        } else {
            self.compiler.getChunk().writeOp(.exit_repl, 0);
        }

        return self.compiler.end();
    }
};

const Compiler = struct {
    manager: *CompilationManager,
    function: *ObjFunction,
    state: State = .{},

    const Self = @This();
    const Error = error{Err} || Chunk.Error || std.posix.WriteError;

    const CompilerReport = GenReport(CompilerMsg);

    const State = struct {
        /// In an assignment. Used to determine if the end of an access chain like:
        /// 'foo.bar[0].baz' should be left in a register instead of the top of the stack
        in_assign: bool = false,
        /// Used to know if we're the last part of a chain like: 'foo.bar[0].baz'. Being the last
        /// part means that we push the result on top of stack
        end_of_chain: bool = false,
        /// Writes variable getters to the register
        to_reg: bool = false,
    };

    const FnKind = enum {
        global,
        @"fn",
        method,
    };

    pub fn init(manager: *CompilationManager, name: []const u8, default_count: usize) Self {
        return .{
            .manager = manager,
            .function = ObjFunction.create(manager.vm, ObjString.copy(manager.vm, name), default_count),
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

    fn getData(self: *const Self) Instruction.Data {
        return self.manager.instr_data[self.manager.instr_idx];
    }

    fn getStart(self: *const Self) usize {
        const idx = if (self.manager.instr_idx >= self.manager.instr_data.len)
            self.manager.instr_idx - 1
        else
            self.manager.instr_idx;
        return self.manager.instr_offsets[idx];
    }

    fn setInstrIndexGetPrev(self: *Self, index: usize) usize {
        const variable_instr = self.manager.instr_idx;
        self.manager.instr_idx = index;
        return variable_instr;
    }

    /// Set the state's flag `to_reg` to true and return previous value
    fn setToRegAndGetPrevious(self: *Self, state: bool) bool {
        defer self.state.to_reg = state;
        return self.state.to_reg;
    }

    fn consumeEndChainState(self: *Self) bool {
        defer self.state.end_of_chain = false;
        return self.state.end_of_chain;
    }

    /// Determines if the result of the Op code should be left in a register instead
    /// of top of the stack. In an assignment, always true
    fn shouldPutResultInReg(self: *Self) bool {
        return self.state.in_assign or !self.consumeEndChainState();
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

    fn addGlobal(self: *Self, global: Value) void {
        self.manager.globals.appendAssumeCapacity(global);
    }

    /// Emits the corresponding `get_heap`, `get_global` or `get_local` with the correct index
    fn emitGetVar(self: *Self, variable: *const Instruction.Variable, offset: usize) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        // TODO: more frequent paths should be first
        self.writeOpAndByte(
            if (variable.scope == .heap)
                .get_heap
            else if (variable.scope == .global)
                if (self.state.to_reg) .get_global_reg else .get_global
            else if (self.state.to_reg)
                .get_local_reg
            else
                .get_local,
            @intCast(variable.index),
            offset,
        );
    }

    /// Declare the variable based on informations coming from Analyzer. Declares
    /// either in global scope or heap, as for local it's already living
    /// on the stack
    fn defineVariable(self: *Self, infos: Instruction.Variable, offset: usize) void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        if (infos.scope == .global)
            self.writeOpAndByte(.define_global, @intCast(infos.index), offset)
        else if (infos.scope == .heap)
            self.writeOpAndByte(.define_heap_var, @intCast(infos.index), offset);
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

    pub fn end(self: *Self) Error!*ObjFunction {
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
            .array_access => |*data| self.arrayAccess(data.incr_ref, self.getStart()),
            .array_access_chain => |*data| self.arrayAccessChain(data, self.getStart()),
            .assignment => |*data| self.assignment(data),
            .binop => |*data| self.binop(data),
            .block => |*data| self.block(data),
            .bool => |data| self.boolInstr(data),
            .call => |*data| self.fnCall(data),
            .cast => |data| self.cast(data),
            .default_value => unreachable,
            .discard => self.discard(),
            .field => |*data| self.getField(data),
            .float => |data| self.floatInstr(data),
            .fn_decl => |*data| self.fnDecl(data),
            .identifier => |*data| self.identifier(data),
            .identifier_id => |*data| self.identifierId(data),
            .identifier_absolute => |data| self.identifierAbsolute(data),
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
            // .struct_literal => |data| self.structLiteral(data),
            .struct_literal => |*data| self.structLiteral(data),
            .unary => |*data| self.unary(data),
            .use => |data| self.use(data),
            .value => unreachable,
            .var_decl => |*data| self.varDecl(data),
            .@"while" => self.whileInstr(),
        };
    }

    fn array(self: *Self, data: *const Instruction.Array) Error!void {
        const start = self.getStart();
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

    fn arrayAccess(self: *Self, incr_ref: bool, start: usize) Error!void {
        const prev = self.setToRegAndGetPrevious(true);
        defer self.state.to_reg = prev;
        const reg = self.shouldPutResultInReg();

        // Variable
        try self.compileInstr();
        // Index, we want to leave it on the stack
        self.state.to_reg = false;
        try self.compileInstr();

        self.writeOp(if (reg) .array_access_reg else .array_access, start);
        if (incr_ref) self.writeOp(.incr_ref_count, start);
    }

    fn arrayAccessChain(self: *Self, data: *const Instruction.ArrayAccessChain, start: usize) Error!void {
        // We want indecies to be on stack
        const prev = self.setToRegAndGetPrevious(false);
        defer self.state.to_reg = prev;

        // Indicies
        for (0..data.depth) |_| {
            try self.compileInstr();
        }

        // Variable
        self.state.to_reg = true;
        const reg = self.shouldPutResultInReg();

        try self.compileInstr();

        self.writeOpAndByte(if (reg) .array_access_chain_reg else .array_access_chain, @intCast(data.depth), start);
        if (data.incr_ref) self.writeOp(.incr_ref_count, start);
    }

    fn arrayAssign(self: *Self, start: usize) Error!void {
        const prev = self.setToRegAndGetPrevious(true);
        defer self.state.to_reg = prev;

        // Variable
        try self.compileInstr();

        // Index, we want to leave it on the stack
        self.state.to_reg = false;
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
        const start = self.getStart();

        // Value
        // const variable_instr = self.setInstrIndexGetPrev(data.value_instr);
        try self.compileInstr();
        // const end_instr = self.setInstrIndexGetPrev(variable_instr);
        // defer self.manager.instr_idx = end_instr;

        // We cast the value on top of stack if needed
        if (data.cast) self.writeOp(.cast_to_float, start);

        // Used by `getVar` in case we fetch a simple identifier
        self.state.in_assign = true;
        defer self.state.in_assign = false;

        const variable_data = switch (self.next()) {
            .identifier => |*variable| variable,
            .identifier_id => |*ident_data| &self.manager.instr_data[ident_data.index].var_decl.variable,
            .array_access => return self.arrayAssign(start),
            .array_access_chain => |*array_data| return self.arrayAssignChain(array_data, start),
            .field => |*field| return self.fieldAssignment(field, data.cow, start),
            else => unreachable,
        };

        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        // TODO: protect cow
        self.writeOpAndByte(
            if (variable_data.scope == .global)
                .set_global
            else if (variable_data.scope == .heap)
                .set_heap
            else
                .set_local,
            @intCast(variable_data.index),
            start,
        );
    }

    fn fieldAssignment(self: *Self, data: *const Instruction.Field, cow: bool, start: usize) Error!void {
        try self.getField(data);
        self.writeOp(if (cow) .reg_assign_cow else .reg_assign, start);
    }

    fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
        const start = self.getStart();

        // Special handle for logicals
        if (data.op == .@"and" or data.op == .@"or") return self.logicalBinop(start, data);

        self.state.end_of_chain = true;
        try self.compileInstr();
        if (data.cast == .lhs and data.op != .mul_str) self.writeOp(.cast_to_float, start);

        self.state.end_of_chain = true;
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
        const start = self.getStart();

        for (0..data.length) |_| {
            self.state.end_of_chain = true;
            try self.compileInstr();
        }

        if (data.is_expr) {
            self.writeOpAndByte(.scope_return, data.pop_count, start);
        } else {
            for (0..data.pop_count) |_| {
                self.getChunk().writeOp(.pop, start);
            }
        }
    }

    fn boolInstr(self: *Self, value: bool) Error!void {
        const op: OpCode = if (value) .true else .false;
        self.writeOp(op, self.getStart());
    }

    fn cast(self: *Self, typ: Rir.Type) Error!void {
        const start = self.getStart();

        switch (typ) {
            .float => self.writeOp(.cast_to_float, start),
            .int => unreachable,
        }
    }

    fn discard(self: *Self) Error!void {
        try self.compileInstr();
        self.writeOp(.pop, 0);
    }

    fn floatInstr(self: *Self, value: f64) Error!void {
        try self.emitConstant(Value.makeFloat(value), self.getStart());
    }

    fn fnCall(self: *Self, data: *const Instruction.Call) Error!void {
        const start = self.getStart();

        // Compiles the identifier. We want every thing on the stack
        // TODO: see if there are no side effects if arguments are chained fields/array accesses
        const prev = self.state.to_reg;
        defer self.state.to_reg = prev;
        self.state.to_reg = false;

        if (data.invoke)
            return self.invoke(data, start);

        const load_op: OpCode, const call_op: OpCode = switch (data.call_conv) {
            .free_function => .{ .load_fn_def, .call },
            .builtin => .{ .load_fn_def, .call_native },
            .import => .{ .load_fn_import_def, .call_import },
            else => .{ .load_fn_bound_def, .call_bound_method },
        };

        try self.compileInstr();
        if (data.default_count > 0) self.writeOp(load_op, start);
        try self.compileArgs(data.arity);

        self.writeOpAndByte(call_op, data.arity, start);

        if (data.call_conv == .import) {
            // At the end of the call, we unload it
            self.writeOp(.unload_module, start);
        }
    }

    fn invoke(self: *Self, data: *const Instruction.Call, start: usize) Error!void {
        // We do not compile the member as we invoke it (it does not go on the stack)
        const member_data = self.getData().field;
        self.manager.instr_idx += 1;

        const load_op: OpCode, const call_op: OpCode = switch (data.call_conv) {
            .bound => .{ .load_invoke_def, .invoke },
            .import => .{ .load_invoke_import_def, .invoke_import },
            .static => .{ .load_invoke_static_def, .invoke_static },
            else => unreachable,
        };

        // Compiles the receiver
        try self.compileInstr();
        if (data.default_count > 0) self.writeOpAndByte(load_op, @intCast(member_data.index), start);
        try self.compileArgs(data.arity);

        self.writeOpAndByte(call_op, data.arity, start);
        self.writeByte(@intCast(member_data.index), start);

        if (data.call_conv == .import) self.writeOp(.unload_module, start);
    }

    fn compileArgs(self: *Self, arity: usize) Error!void {
        const start = self.getStart();
        var last: usize = 0;

        for (0..arity) |_| {
            switch (self.next()) {
                .value => |data| {
                    const save = self.setInstrIndexGetPrev(data.value_instr);
                    defer self.manager.instr_idx = save;

                    // To push to stack each arguments
                    self.state.end_of_chain = true;

                    try self.compileInstr();
                    // Arguments may not be in the same order as the declaration, we could be
                    // resolving the first value during the last iteration
                    last = @max(last, self.manager.instr_idx);

                    if (data.cast) self.writeOp(.cast_to_float, start);
                },
                .default_value => |idx| self.writeOpAndByte(.get_default, @intCast(idx), start),
                else => unreachable,
            }
        }

        if (last > self.manager.instr_idx) self.manager.instr_idx = last;
    }

    fn fnDecl(self: *Self, data: *const Instruction.FnDecl) Error!void {
        const idx = self.manager.instr_idx - 1;
        const name_idx = self.next().name;
        const fn_name = self.manager.vm.interner.getKey(name_idx).?;
        const fn_var = self.next().var_decl.variable;

        const func = try self.compileFn(fn_name, data);

        if (fn_var.scope == .global) {
            self.addGlobal(Value.makeObj(func.asObj()));
        } else {
            try self.emitConstant(Value.makeObj(func.asObj()), 0);
            self.defineVariable(fn_var, 0);
        }

        // Check for main function
        if (idx == self.manager.main) {
            self.manager.main_index = @intCast(fn_var.index);
        }
    }

    fn compileFn(self: *Self, name: []const u8, data: *const Instruction.FnDecl) Error!*ObjFunction {
        var compiler = Compiler.init(self.manager, name, data.default_params);

        for (0..data.default_params) |i| {
            compiler.function.default_values[i] = try self.compileDefaultValue();
        }

        for (0..data.body_len) |_| {
            compiler.state.end_of_chain = true;
            try compiler.compileInstr();
        }

        if (data.return_kind == .implicit_value) {
            compiler.writeOp(.@"return", self.manager.instr_offsets[self.manager.instr_idx - 1]);
        } else if (data.return_kind == .implicit_void) {
            compiler.writeOp(.naked_return, self.manager.instr_offsets[self.manager.instr_idx - 1]);
        }

        return compiler.end();
    }

    fn getField(self: *Self, data: *const Instruction.Field) Error!void {
        const prev = self.setToRegAndGetPrevious(true);
        defer self.state.to_reg = prev;

        const member_data = self.getData();
        const start = self.getStart();

        // As we compile member first, we preshot the value
        const reg = self.shouldPutResultInReg();
        // We compile the identifier/call/array access first
        try self.compileInstr();

        // If we're in a member access, the field access that occurs after the call will check
        // the register, not the stack, so we pop the result from the stack
        if (member_data == .call) self.writeOp(.reg_push, start);

        // Get field/bound method of first value on the stack
        self.writeOpAndByte(
            if (data.kind == .field)
                if (reg) .get_field_reg else .get_field
            else if (data.kind == .symbol)
                .bound_import
            else if (data.kind == .method)
                .bound_method
            else
                .get_static_method,
            @intCast(data.index),
            start,
        );

        if (data.incr_ref_count) self.writeOp(.incr_ref_count, start);
    }

    fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
        self.emitGetVar(data, self.getStart());
    }

    fn identifierId(self: *Self, data: *const Instruction.IdentifierId) Error!void {
        const start = self.getStart();
        const variable_data = &self.manager.instr_data[data.index].var_decl.variable;
        self.emitGetVar(variable_data, start);
        if (data.incr_ref_count) self.writeOp(.incr_ref_count, start);
    }

    fn identifierAbsolute(self: *Self, data: usize) Error!void {
        self.writeOpAndByte(.get_local_absolute, @intCast(data), self.getStart());
    }

    fn intInstr(self: *Self, value: isize) Error!void {
        try self.emitConstant(Value.makeInt(value), self.getStart());
    }

    fn ifInstr(self: *Self, data: *const Instruction.If) Error!void {
        const start = self.getStart();

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
        if (data.scope == .global)
            self.addGlobal(self.manager.modules[data.module_index].globals[data.field_index])
        else {
            const start = self.getStart();
            // TODO: protect cast
            self.writeOpAndByte(.import_item, @intCast(data.module_index), start);
            self.writeByte(@intCast(data.field_index), start);
        }
    }

    fn moduleImport(self: *Self, data: *const Instruction.ModuleImport) Error!void {
        if (data.scope == .global) {
            self.addGlobal(Value.makeObj(ObjModule.create(self.manager.vm, &self.manager.modules[data.index]).asObj()));
        } else {
            self.writeOpAndByte(.push_module, @intCast(data.index), self.getStart());
        }
    }

    fn multipleVarDecl(self: *Self, count: usize) Error!void {
        for (0..count) |_| {
            try self.compileInstr();
        }
    }

    fn nullInstr(self: *Self) Error!void {
        self.writeOp(.null, self.getStart());
    }

    fn print(self: *Self) Error!void {
        const start = self.getStart();
        try self.compileInstr();
        self.getChunk().writeOp(.print, start);
    }

    fn returnInstr(self: *Self, data: *const Instruction.Return) Error!void {
        const start = self.getStart();

        if (data.value) {
            try self.compileInstr();
            if (data.cast) try self.compileInstr();

            self.writeOp(.@"return", start);
        } else self.writeOp(.naked_return, start);
    }

    fn stringInstr(self: *Self, index: usize) Error!void {
        try self.emitConstant(
            Value.makeObj(ObjString.copy(
                self.manager.vm,
                self.manager.vm.interner.getKey(index).?,
            ).asObj()),
            self.getStart(),
        );
    }

    fn structDecl(self: *Self, data: *const Instruction.StructDecl) Error!void {
        const start = self.getStart();
        const name = self.next().name;
        const struct_var = self.next().var_decl;

        var structure = ObjStruct.create(
            self.manager.vm,
            ObjString.copy(self.manager.vm, self.manager.vm.interner.getKey(name).?),
            data.fields_count,
            data.default_fields,
            &.{},
        );

        // We forward declare the structure in the globals because when disassembling the
        // structure's method, they need to refer to the object. Only the name can be refered to
        const idx = if (struct_var.variable.scope == .global) b: {
            self.addGlobal(Value.makeObj(structure.asObj()));
            break :b self.manager.globals.items.len - 1;
        } else 0;

        // We compile each default value and as we know there are pure, we can extract them
        // from the constants (they aren't global either). As we do that, we delete compiled
        // code to not execute it at runtime
        for (0..data.default_fields) |i| {
            structure.default_values[i] = try self.compileDefaultValue();
        }

        var funcs: ArrayListUnmanaged(*ObjFunction) = .{};
        funcs.ensureTotalCapacity(self.manager.vm.allocator, data.func_count) catch oom();

        for (0..data.func_count) |_| {
            const fn_data = self.next().fn_decl;
            const fn_name = self.manager.vm.interner.getKey(self.next().name).?;
            // Skip `variable` cause not needed here
            self.manager.instr_idx += 1;

            const func = try self.compileFn(fn_name, &fn_data);
            funcs.appendAssumeCapacity(func);
        }

        structure.methods = funcs.toOwnedSlice(self.manager.vm.allocator) catch oom();
        const struct_obj = Value.makeObj(structure.asObj());

        if (struct_var.variable.scope == .global) {
            self.manager.globals.items[idx] = struct_obj;
        } else {
            try self.emitConstant(struct_obj, self.getStart());
            self.defineVariable(struct_var.variable, start);
        }
    }

    fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) Error!void {
        const start = self.getStart();
        // Compile structure
        try self.compileInstr();
        if (data.default_count > 0) self.writeOp(.load_struct_def, start);
        try self.compileArgs(data.fields_count);
        self.writeOpAndByte(if (data.imported) .struct_literal_import else .struct_literal, @intCast(data.fields_count), start);
    }

    fn unary(self: *Self, data: *const Instruction.Unary) Error!void {
        const start = self.getStart();
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
            const start = self.getStart();
            const imported = self.next().imported;

            try self.emitConstant(
                Value.makeObj(ObjNativeFn.create(
                    self.manager.vm,
                    self.manager.natives[imported.index],
                ).asObj()),
                start,
            );
            self.defineVariable(imported.variable, start);
        }
    }

    fn varDecl(self: *Self, data: *const Instruction.VarDecl) Error!void {
        const start = self.getStart();

        if (data.has_value) {
            try self.compileInstr();
            if (data.cast) try self.compileInstr();
        } else {
            self.writeOp(.null, start);
        }

        // TODO: Fix this, just to avoid accessing an empty slot at runtime
        // If we are top level, value should be pure and compile time known
        if (data.variable.scope == .global) self.addGlobal(.null_);
        self.defineVariable(data.variable, start);

        if (data.variable.scope == .heap) {
            self.manager.heap_count += 1;
            // We place a 'tombstone' value here because during static analyzis,
            // the locals list is synchronized to all following declaration. Instead of
            // back patching all locals declaration past a heap variable, we make the
            // compiler place a tombstone.
            self.writeOp(.null, start);
        }
    }

    fn whileInstr(self: *Self) Error!void {
        const start = self.getStart();

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

                if (self.manager.instr_data[self.manager.instr_idx] == .cast) {
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
