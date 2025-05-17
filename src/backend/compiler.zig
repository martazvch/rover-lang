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

        self.compiler = Compiler.init(self, "global scope");

        // TODO: maybe separate globals and put it in the manager as every other spawned compilers
        // will be for local functions
        self.globals.ensureTotalCapacity(self.vm.allocator, symbols_count) catch oom();

        while (self.instr_idx < self.instr_data.len) {
            try self.compiler.compileInstr();
        }

        if (!self.repl) {
            // We init the heap variables into the Vm
            // NOTE: maybe return the count as we return the function and let the Vm allocate?
            self.vm.heap_vars = try self.vm.allocator.alloc(Value, self.heap_count);

            // Insert a call to main with arity of 0 for now
            self.compiler.writeOpAndByte(
                .get_global,
                self.main_index.?,
                0,
            );
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

    const Self = @This();
    const Error = error{Err} || Chunk.Error || std.posix.WriteError;

    const CompilerReport = GenReport(CompilerMsg);

    const FnKind = enum {
        global,
        @"fn",
        method,
    };

    // TODO: error handling?
    pub fn init(manager: *CompilationManager, name: []const u8) Self {
        return .{
            .manager = manager,
            .function = ObjFunction.create(manager.vm, ObjString.copy(manager.vm, name)),
        };
    }

    fn next(self: *Self) Instruction.Data {
        defer self.manager.instr_idx += 1;

        return self.manager.instr_data[self.manager.instr_idx];
    }

    inline fn getChunk(self: *Self) *Chunk {
        return &self.function.chunk;
    }

    inline fn getData(self: *const Self) Instruction.Data {
        return self.manager.instr_data[self.manager.instr_idx];
    }

    inline fn getStart(self: *const Self) usize {
        const idx = if (self.manager.instr_idx >= self.manager.instr_data.len)
            self.manager.instr_idx - 1
        else
            self.manager.instr_idx;
        return self.manager.instr_offsets[idx];
    }

    /// Writes an OpCode to the current chunk
    inline fn writeOp(self: *Self, op: OpCode, offset: usize) void {
        self.getChunk().writeOp(op, offset);
    }

    /// Writes a byte to the current chunk
    inline fn writeByte(self: *Self, byte: u8, offset: usize) void {
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
        self.writeOpAndByte(
            if (variable.scope == .heap) .get_heap else if (variable.scope == .global) .get_global else .get_local,
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

        // TODO: I think I don't need the first & 0xff
        chunk.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
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
            // return error.Err;
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
            .assignment => |*data| self.assignment(data),
            .binop => |*data| self.binop(data),
            .block => |*data| self.block(data),
            .bool => |data| self.boolInstr(data),
            .call => |*data| self.fnCall(data),
            .cast => |data| self.cast(data),
            .discard => self.discard(),
            .float => |data| self.floatInstr(data),
            .fn_decl => |*data| self.fnDecl(data),
            .identifier => |*data| self.identifier(data),
            .identifier_id => |data| self.identifier(&self.manager.instr_data[data].var_decl.variable),
            .@"if" => |*data| self.ifInstr(data),
            .imported => unreachable,
            .int => |data| self.intInstr(data),
            .member => |*data| self.getMember(data),
            .module_import => |*data| self.moduleImport(data),
            .multiple_var_decl => |data| self.multipleVarDecl(data),
            .name => unreachable,
            .null => self.nullInstr(),
            .print => self.print(),
            .@"return" => |*data| self.returnInstr(data),
            .string => |data| self.stringInstr(data),
            .struct_decl => |*data| self.structDecl(data),
            .struct_literal => |*data| self.structLiteral(data),
            .unary => |*data| self.unary(data),
            .use => |data| self.use(data),
            .var_decl => |*data| self.varDecl(data),
            .@"while" => self.whileInstr(),
        };
    }

    fn assignment(self: *Self, data: *const Instruction.Assignment) Error!void {
        const start = self.getStart();

        // Value
        try self.compileInstr();

        // We cast the value on top of stack if needed
        if (data.cast) self.writeOp(.cast_to_float, start);

        const variable_data = switch (self.next()) {
            .identifier => |*variable| variable,
            .identifier_id => |idx| &self.manager.instr_data[idx].var_decl.variable,
            .member => |*member| return self.fieldAssignment(member, start),
            else => unreachable,
        };

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
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

    fn fieldAssignment(self: *Self, data: *const Instruction.Member, start: usize) Error!void {
        self.writeOp(.field_assign, start);
        try self.getMember(data);
    }

    fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
        const start = self.getStart();

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
        const start = self.getStart();

        for (0..data.length) |_| try self.compileInstr();

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

    fn getMember(self: *Self, data: *const Instruction.Member) Error!void {
        self.writeOpAndByte(
            if (data.kind == .field)
                .get_field
            else if (data.kind == .symbol)
                .get_symbol
            else
                .bound_method,
            @intCast(data.index),
            self.getStart(),
        );
        try self.compileInstr();
    }

    fn floatInstr(self: *Self, value: f64) Error!void {
        try self.emitConstant(Value.makeFloat(value), self.getStart());
    }

    fn fnCall(self: *Self, data: *const Instruction.Call) Error!void {
        const start = self.getStart();

        if (data.tag == .invoke)
            return self.invoke(data, start)
        else if (data.tag == .invoke_import)
            return self.invokeImport(data, start);

        // Compiles the identifier
        try self.compileInstr();
        try self.compileArgs(data.arity);

        self.writeOpAndByte(
            switch (data.tag) {
                .function => .call,
                .builtin => .native_fn_call,
                .import => .import_call,
                else => .bound_method_call,
            },
            data.arity,
            start,
        );

        // For imports, we generate op code to load/unload the module
        if (data.tag == .import) {
            // Compiles the index + scope of the module to load it
            try self.compileInstr();
            self.writeOp(.unload_module, start);
        }
    }

    fn invoke(self: *Self, data: *const Instruction.Call, start: usize) Error!void {
        // We do not compile the 'bound_method' op code
        const member_data = self.getData().member;
        self.manager.instr_idx += 1;
        // Compiles the receiver
        try self.compileInstr();
        try self.compileArgs(data.arity);

        self.writeOpAndByte(.invoke, data.arity, start);
        self.writeByte(@intCast(member_data.index), start);
    }

    fn invokeImport(self: *Self, data: *const Instruction.Call, start: usize) Error!void {
        const symbol_data = self.getData().member;
        self.manager.instr_idx += 1;
        // Compiles the receiver
        try self.compileInstr();
        try self.compileArgs(data.arity);

        self.writeOpAndByte(.invoke_import, data.arity, start);
        self.writeByte(@intCast(symbol_data.index), start);
        self.writeOp(.unload_module, start);
    }

    fn compileArgs(self: *Self, arity: usize) Error!void {
        for (0..arity) |_| {
            try self.compileInstr();

            if (self.manager.instr_idx < self.manager.instr_data.len and
                self.manager.instr_data[self.manager.instr_idx] == .cast)
                try self.compileInstr();
        }
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
            // TODO: check if put on the stack, should be no need
            try self.emitConstant(Value.makeObj(func.asObj()), 0);
            self.defineVariable(fn_var, 0);
        }

        // Check for main function
        if (idx == self.manager.main) {
            self.manager.main_index = @intCast(fn_var.index);
        }
    }

    // TODO: Check if *kind* is really needed
    fn compileFn(self: *Self, name: []const u8, data: *const Instruction.FnDecl) Error!*ObjFunction {
        var compiler = Compiler.init(self.manager, name);

        for (0..data.body_len) |_| {
            try compiler.compileInstr();
        }

        if (data.return_kind == .implicit_value) {
            compiler.writeOp(.@"return", self.manager.instr_offsets[self.manager.instr_idx - 1]);
        } else if (data.return_kind == .implicit_void) {
            compiler.writeOp(.naked_return, self.manager.instr_offsets[self.manager.instr_idx - 1]);
        }

        return compiler.end();
    }

    fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
        self.emitGetVar(data, self.getStart());
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

    fn moduleImport(self: *Self, data: *const Instruction.ModuleImport) Error!void {
        if (data.scope == .global) {
            self.addGlobal(.makeModule(&self.manager.modules[data.index]));
        } else {
            const start = self.getStart();
            self.writeOpAndByte(.push_module, @intCast(data.index), start);
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

        const structure = Value.makeObj(ObjStruct.create(
            self.manager.vm,
            ObjString.copy(self.manager.vm, self.manager.vm.interner.getKey(name).?),
            data.fields_count,
            funcs.toOwnedSlice(self.manager.vm.allocator) catch oom(),
        ).asObj());

        if (struct_var.variable.scope == .global) {
            self.addGlobal(structure);
        } else {
            try self.emitConstant(
                structure,
                self.getStart(),
            );
            self.defineVariable(struct_var.variable, start);
        }
    }

    fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) Error!void {
        const start = self.getStart();

        // Compile structure
        try self.compileInstr();

        for (0..data.arity) |_| {
            const save = self.manager.instr_idx;
            const field_value_start = self.next().member;
            self.manager.instr_idx = field_value_start.index;
            try self.compileInstr();
            // Jumps the `field` tag
            self.manager.instr_idx = save + 1;
        }

        self.manager.instr_idx = data.end;

        self.writeOpAndByte(.struct_literal, @intCast(data.arity), start);
        // self.emitGetVar(&data.variable, start);
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
};
