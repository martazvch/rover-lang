const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Disassembler = @import("../backend/Disassembler.zig");
const Rir = @import("../frontend/rir.zig");
const Scope = Rir.Scope;
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Interner = @import("../Interner.zig");
const GenReport = @import("../reporter.zig").GenReport;
const ObjString = @import("../runtime/Obj.zig").ObjString;
const ObjFunction = @import("../runtime/Obj.zig").ObjFunction;
const ObjNativeFn = @import("../runtime/Obj.zig").ObjNativeFn;
const ObjStruct = @import("../runtime/Obj.zig").ObjStruct;
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");
const NativeFn = @import("../std/meta.zig").NativeFn;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;

pub const CompilationManager = struct {
    vm: *Vm,
    natives: []const NativeFn,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    interner: *const Interner,
    instr_tags: []const Instruction.Tag,
    instr_data: []const Instruction.Data,
    instr_offsets: []const usize,
    instr_idx: usize,
    render_mode: Disassembler.RenderMode,
    main: usize,
    main_index: ?u8 = null,
    repl: bool,
    heap_count: usize = 0,

    const Self = @This();
    const Error = error{err} || Chunk.Error || std.posix.WriteError;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        vm: *Vm,
        natives: []const NativeFn,
        interner: *const Interner,
        instr_start: usize,
        instructions: *const MultiArrayList(Instruction),
        render_mode: Disassembler.RenderMode,
        main: usize,
        repl: bool,
    ) Self {
        return .{
            .vm = vm,
            .natives = natives,
            .compiler = undefined,
            .errs = .init(vm.allocator),
            .interner = interner,
            .instr_tags = instructions.items(.tag),
            .instr_data = instructions.items(.data),
            .instr_offsets = instructions.items(.offset),
            .instr_idx = instr_start,
            .render_mode = render_mode,
            .main = main,
            .repl = repl,
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
    }

    pub fn compile(self: *Self) !*ObjFunction {
        self.compiler = Compiler.init(self, null, .global, "global scope");

        while (self.instr_idx < self.instr_tags.len) {
            try self.compiler.compileInstr();
        }

        if (!self.repl) {
            // We init the heap variables into the Vm
            // NOTE: maybe return the count as we return the function and let the Vm allocate?
            self.vm.heap_vars = try self.vm.allocator.alloc(Value, self.heap_count);

            // Insert a call to main with arity of 0 for now
            try self.compiler.writeOpAndByte(
                .GetGlobal,
                self.main_index.?,
                0,
            );
            try self.compiler.writeOpAndByte(.call, 0, 0);
        } else {
            try self.compiler.getChunk().writeOp(.ExitRepl, 0);
        }

        return self.compiler.end();
    }
};

const Compiler = struct {
    manager: *CompilationManager,
    // TODO: need an enclosing? Not used for now. If not, Need a Compilation manager?
    enclosing: ?*Compiler,
    function: *ObjFunction,
    // TODO: useless information
    fn_kind: FnKind,

    const Self = @This();
    const Error = error{Err} || Chunk.Error || std.posix.WriteError;

    const CompilerReport = GenReport(CompilerMsg);

    const FnKind = enum {
        global,
        @"fn",
        method,
    };

    // TODO: error handling?
    pub fn init(manager: *CompilationManager, enclosing: ?*Compiler, fn_kind: FnKind, name: []const u8) Self {
        return .{
            .manager = manager,
            .enclosing = enclosing,
            .function = ObjFunction.create(manager.vm, ObjString.copy(manager.vm, name) catch unreachable) catch unreachable,
            .fn_kind = fn_kind,
        };
    }

    inline fn getChunk(self: *Self) *Chunk {
        return &self.function.chunk;
    }

    inline fn getData(self: *const Self) Instruction.Data {
        return self.manager.instr_data[self.manager.instr_idx];
    }

    inline fn getStart(self: *const Self) usize {
        return self.manager.instr_offsets[self.manager.instr_idx];
    }

    /// Writes an OpCode to the current chunk
    inline fn writeOp(self: *Self, op: OpCode, offset: usize) Error!void {
        try self.getChunk().writeOp(op, offset);
    }

    /// Writes a byte to the current chunk
    inline fn writeByte(self: *Self, byte: u8, offset: usize) Error!void {
        try self.getChunk().writeByte(byte, offset);
    }

    /// Writes an OpCode and a byte to the current chunk
    fn writeOpAndByte(self: *Self, op: OpCode, byte: u8, offset: usize) !void {
        try self.writeOp(op, offset);
        try self.writeByte(byte, offset);
    }

    fn emitConstant(self: *Self, value: Value, offset: usize) !void {
        // TODO: error
        self.writeOpAndByte(.Constant, try self.getChunk().writeConstant(value), offset) catch |err| {
            std.debug.print("Too many constants in chunk\n", .{});
            return err;
        };
    }

    /// Emits the corresponding `GetHeap`, `GetGlobal` or `GetLocal` with the correct index
    fn emitGetVar(self: *Self, variable: Instruction.Variable, offset: usize) !void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        try self.writeOpAndByte(
            if (variable.scope == .heap) .GetHeap else if (variable.scope == .global) .GetGlobal else .GetLocal,
            @intCast(variable.index),
            offset,
        );
    }

    /// Declare the variable based on informations coming from Analyzer. Declares
    /// either in global scope or heap, as for local it's already living
    /// on the stack
    fn defineVariable(self: *Self, infos: Instruction.Variable, offset: usize) !void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        if (infos.scope == .global)
            try self.writeOpAndByte(.DefineGlobal, @intCast(infos.index), offset)
        else if (infos.scope == .heap)
            try self.writeOpAndByte(.DefineHeapVar, @intCast(infos.index), offset);
    }

    fn emitJump(self: *Self, kind: OpCode, offset: usize) !usize {
        const chunk = self.getChunk();
        try chunk.writeOp(kind, offset);
        try chunk.writeByte(0xff, offset);
        try chunk.writeByte(0xff, offset);

        return chunk.code.items.len - 2;
    }

    fn patchJump(self: *Self, offset: usize) !void {
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

    fn emitLoop(self: *Self, loop_start: usize, offset: usize) !void {
        const chunk = self.getChunk();
        try chunk.writeOp(.Loop, offset);
        // +2 for loop own operands (jump offset on 16bits)
        const jump_offset = chunk.code.items.len - loop_start + 2;

        // TODO: Error handling
        if (jump_offset > std.math.maxInt(u16)) {
            std.debug.print("Loop body too large\n", .{});
            return error.Err;
        }

        try chunk.writeByte(@as(u8, @intCast(jump_offset >> 8)) & 0xff, offset);
        try chunk.writeByte(@intCast(jump_offset & 0xff), offset);
    }

    pub fn end(self: *Self) Error!*ObjFunction {
        // Disassembler
        if (self.manager.render_mode != .none) {
            var dis = Disassembler.init(&self.function.chunk, self.manager.vm.allocator, self.manager.render_mode);
            defer dis.deinit();
            dis.disChunk(if (self.function.name) |n| n.chars else "Script") catch unreachable;
            const stdout = std.io.getStdOut().writer();
            try stdout.print("{s}", .{dis.disassembled.items});
            if (self.enclosing != null) try stdout.writeAll("\n");
        }

        return self.function;
    }

    fn compileInstr(self: *Self) Error!void {
        try switch (self.manager.instr_tags[self.manager.instr_idx]) {
            .assignment => self.assignment(),
            .binop => self.binop(),
            .block => self.block(),
            .bool => self.boolInstr(),
            .cast => self.cast(),
            .discard => self.discard(),
            .field => self.getField(),
            .float => self.floatInstr(),
            .call => self.fnCall(),
            .fn_decl => self.fnDecl(),
            .name => unreachable,
            .identifier => self.identifier(false),
            .identifier_id => self.identifier(true),
            .imported => unreachable,
            .int => self.intInstr(),
            .@"if" => self.ifInstr(),
            .multiple_var_decl => self.multipleVarDecl(),
            .null => self.nullInstr(),
            .print => self.print(),
            .@"return" => self.returnInstr(),
            .string => self.stringInstr(),
            .struct_decl => self.structDecl(),
            .struct_literal => self.structLiteral(),
            .unary => self.unary(),
            .use => self.use(),
            .var_decl => self.varDecl(),
            .@"while" => self.whileInstr(),
        };
    }

    fn assignment(self: *Self) Error!void {
        const assign_data = self.getData().assignment;
        const start = self.getStart();
        self.manager.instr_idx += 1;
        const data_idx = self.manager.instr_idx;

        const data = if (self.manager.instr_tags[data_idx] == .identifier_id)
            self.manager.instr_data[self.manager.instr_data[data_idx].id].var_decl.variable
        else if (self.manager.instr_tags[data_idx] == .field) {
            return self.fieldAssignment(assign_data, start);
        } else self.manager.instr_data[data_idx].variable;

        self.manager.instr_idx += 1;

        // Value
        try self.compileInstr();

        // We cast the value on top of stack if needed
        if (assign_data.cast) try self.compileInstr();

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.writeOpAndByte(
            if (data.scope == .global)
                .SetGlobal
            else if (data.scope == .heap)
                .SetHeap
            else
                .SetLocal,
            @intCast(data.index),
            start,
        );
    }

    fn fieldAssignment(self: *Self, assign_data: Rir.Instruction.Assignment, start: usize) Error!void {
        try self.writeOp(.field_assign, start);
        try self.getField();
        // Value
        try self.compileInstr();
        // We cast the value on top of stack if needed
        if (assign_data.cast) try self.compileInstr();
    }

    fn binop(self: *Self) !void {
        const start = self.getStart();
        const data = self.getData().binop;
        self.manager.instr_idx += 1;

        // Special handle for logicals
        if (data.op == .@"and" or data.op == .@"or") return self.logicalBinop(start, data);

        try self.compileInstr();
        if (data.cast == .lhs and data.op != .mul_str) try self.writeOp(.CastToFloat, start);

        try self.compileInstr();
        if (data.cast == .rhs and data.op != .mul_str) try self.writeOp(.CastToFloat, start);

        try self.writeOp(
            switch (data.op) {
                .add_float => .AddFloat,
                .add_int => .AddInt,
                .add_str => .StrCat,
                .div_float => .DivFloat,
                .div_int => .DivInt,
                .eq_bool => .EqBool,
                .eq_float => .EqFloat,
                .eq_int => .EqInt,
                .eq_str => .EqStr,
                .ge_float => .GeFloat,
                .ge_int => .GeInt,
                .gt_float => .GtFloat,
                .gt_int => .GtInt,
                .le_float => .LeFloat,
                .le_int => .LeInt,
                .lt_float => .LtFloat,
                .lt_int => .LtInt,
                .mul_float => .MulFloat,
                .mul_int => .MulInt,
                .mul_str => if (data.cast == .rhs) .StrMulR else .StrMulL,
                .ne_bool => .NeBool,
                .ne_float => .NeFloat,
                .ne_int => .NeInt,
                .ne_str => .NeStr,
                .sub_float => .SubFloat,
                .sub_int => .SubInt,
                else => unreachable,
            },
            start,
        );
    }

    fn cast(self: *Self) Error!void {
        const data = self.getData();
        const start = self.getStart();
        self.manager.instr_idx += 1;

        try switch (data.cast_to) {
            .float => self.writeOp(.CastToFloat, start),
            .int => unreachable,
        };
    }

    fn logicalBinop(self: *Self, start: usize, data: Instruction.Binop) !void {
        switch (data.op) {
            .@"and" => {
                try self.compileInstr();
                const end_jump = try self.emitJump(.JumpIfFalse, start);
                // If true, pop the value, else the 'false' remains on top of stack
                try self.writeOp(.Pop, start);
                try self.compileInstr();
                try self.patchJump(end_jump);
            },
            .@"or" => {
                try self.compileInstr();
                const else_jump = try self.emitJump(.JumpIfTrue, start);
                try self.writeOp(.Pop, start);
                try self.compileInstr();
                try self.patchJump(else_jump);
            },
            else => unreachable,
        }
    }

    fn block(self: *Self) Error!void {
        const data = self.getData().block;
        const start = self.getStart();
        self.manager.instr_idx += 1;

        for (0..data.length) |_| try self.compileInstr();

        if (data.is_expr) {
            try self.writeOpAndByte(.ScopeReturn, data.pop_count, start);
        } else {
            for (0..data.pop_count) |_| {
                try self.getChunk().writeOp(.Pop, start);
            }
        }
    }

    fn boolInstr(self: *Self) Error!void {
        const op: OpCode = if (self.getData().bool) .true else .false;
        try self.writeOp(op, self.getStart());
        self.manager.instr_idx += 1;
    }

    fn discard(self: *Self) Error!void {
        self.manager.instr_idx += 1;
        try self.compileInstr();
        try self.writeOp(.Pop, 0);
    }

    fn getField(self: *Self) Error!void {
        const data = self.getData().field;
        const start = self.getStart();
        self.manager.instr_idx += 1;

        try self.writeOpAndByte(.get_field, @intCast(data), start);
        try self.compileInstr();
    }

    fn floatInstr(self: *Self) !void {
        try self.emitConstant(Value.float(self.getData().float), self.getStart());
        self.manager.instr_idx += 1;
    }

    fn fnCall(self: *Self) !void {
        const data = self.getData().call;
        const start = self.getStart();
        self.manager.instr_idx += 1;

        // Compiles the identifier
        try self.compileInstr();

        for (0..data.arity) |_| {
            try self.compileInstr();

            if (self.manager.instr_idx < self.manager.instr_tags.len and
                self.manager.instr_tags[self.manager.instr_idx] == .cast)
                try self.compileInstr();
        }

        try self.writeOpAndByte(
            if (data.builtin) .NativeFnCall else .call,
            data.arity,
            start,
        );
    }

    fn fnDecl(self: *Self) Error!void {
        const idx = self.manager.instr_idx;

        const data = self.getData().fn_decl;
        self.manager.instr_idx += 1;
        const fn_name = self.manager.interner.getKey(self.getData().id).?;
        self.manager.instr_idx += 1;
        const fn_var = self.getData().var_decl.variable;
        self.manager.instr_idx += 1;

        try self.compileFn(.@"fn", fn_name, data);
        try self.defineVariable(fn_var, 0);

        // Check for main function
        if (idx == self.manager.main) {
            self.manager.main_index = @intCast(fn_var.index);
        }
    }

    // TODO: Check if *kind* is really needed
    fn compileFn(
        self: *Self,
        kind: FnKind,
        name: []const u8,
        data: Instruction.FnDecl,
    ) Error!void {
        var compiler = Compiler.init(self.manager, self, kind, name);

        for (0..data.body_len) |_| {
            try compiler.compileInstr();
        }

        if (data.return_kind == .implicit_value) {
            try compiler.writeOp(
                .@"return",
                self.manager.instr_offsets[self.manager.instr_idx - 1],
            );
        } else if (data.return_kind == .implicit_void) {
            try compiler.writeOp(
                .NakedReturn,
                self.manager.instr_offsets[self.manager.instr_idx - 1],
            );
        }

        const func = try compiler.end();
        try self.emitConstant(Value.obj(func.asObj()), 0);
    }

    fn identifier(self: *Self, is_id: bool) !void {
        const data = if (is_id) blk: {
            const id = self.getData().id;
            break :blk self.manager.instr_data[id].var_decl.variable;
        } else self.getData().variable;

        try self.emitGetVar(data, self.getStart());
        self.manager.instr_idx += 1;
    }

    fn intInstr(self: *Self) !void {
        try self.emitConstant(Value.int(self.getData().int), self.getStart());
        self.manager.instr_idx += 1;
    }

    fn ifInstr(self: *Self) !void {
        const data = self.getData().@"if";
        const start = self.getStart();
        self.manager.instr_idx += 1;

        // Condition
        try self.compileInstr();
        const then_jump = try self.emitJump(.JumpIfFalse, start);
        // Pops the condition, no longer needed
        try self.writeOp(.Pop, start);

        // Then body
        try self.compileInstr();
        if (data.cast == .then) try self.writeOp(.CastToFloat, start);

        // Exits the if expression
        const else_jump = try self.emitJump(.Jump, start);
        try self.patchJump(then_jump);

        // If we go in the else branch, we pop the condition too
        try self.writeOp(.Pop, start);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.has_else) {
            try self.compileInstr();
            if (data.cast == .@"else") try self.writeOp(.CastToFloat, start);
        }

        try self.patchJump(else_jump);
    }

    fn multipleVarDecl(self: *Self) !void {
        const data = self.getData().id;
        self.manager.instr_idx += 1;

        for (0..data) |_| {
            try self.compileInstr();
        }
    }

    fn nullInstr(self: *Self) !void {
        try self.writeOp(.null, self.getStart());
        self.manager.instr_idx += 1;
    }

    fn print(self: *Self) !void {
        const start = self.getStart();
        self.manager.instr_idx += 1;
        try self.compileInstr();
        try self.getChunk().writeOp(.print, start);
    }

    fn returnInstr(self: *Self) !void {
        const data = self.getData().@"return";
        const start = self.getStart();
        self.manager.instr_idx += 1;

        if (data.value) {
            try self.compileInstr();
            if (data.cast) try self.compileInstr();

            try self.writeOp(.@"return", start);
        } else try self.writeOp(.NakedReturn, start);
    }

    fn stringInstr(self: *Self) !void {
        try self.emitConstant(
            Value.obj((try ObjString.copy(
                self.manager.vm,
                self.manager.interner.getKey(self.getData().id).?,
            )).asObj()),
            self.getStart(),
        );
        self.manager.instr_idx += 1;
    }

    fn structDecl(self: *Self) !void {
        const start = self.getStart();
        const data = self.getData().struct_decl;
        self.manager.instr_idx += 1;
        const name = self.getData().id;
        self.manager.instr_idx += 1;
        const struct_var = self.getData().var_decl;

        try self.emitConstant(
            Value.obj((try ObjStruct.create(
                self.manager.vm,
                try ObjString.copy(self.manager.vm, self.manager.interner.getKey(name).?),
                data.fields_count,
            )).asObj()),
            self.getStart(),
        );

        if (struct_var.variable.scope != .local)
            try self.defineVariable(struct_var.variable, start);

        self.manager.instr_idx += 1;
    }

    fn structLiteral(self: *Self) !void {
        const start = self.getStart();
        const data = self.getData().struct_literal;
        self.manager.instr_idx += 1;

        for (0..data.arity) |_| {
            const save = self.manager.instr_idx;
            const field_value_start = self.getData().field;
            self.manager.instr_idx = field_value_start;
            try self.compileInstr();
            // Jumps the `field` tag
            self.manager.instr_idx = save + 1;
        }

        self.manager.instr_idx = data.end;

        try self.writeOpAndByte(.struct_literal, @intCast(data.arity), start);
        try self.emitGetVar(data.variable, start);
    }

    fn unary(self: *Self) !void {
        const start = self.getStart();
        const data = self.getData().unary;
        self.manager.instr_idx += 1;

        try self.compileInstr();

        if (data.op == .minus) {
            try self.writeOp(
                if (data.typ == .int) .NegateInt else .NegateFloat,
                start,
            );
        } else {
            try self.writeOp(.not, start);
        }
    }

    fn use(self: *Self) !void {
        const data = self.getData().use;
        self.manager.instr_idx += 1;

        // NOTE: For now, analyzer places an empty import
        // to skip "std" by placing a Null instruction. Needs a rework
        self.manager.instr_idx += 1;

        for (0..data) |_| {
            const imported = self.getData().imported;
            const start = self.getStart();
            self.manager.instr_idx += 1;

            try self.emitConstant(
                Value.obj(
                    (try ObjNativeFn.create(
                        self.manager.vm,
                        self.manager.natives[imported.index],
                    )).asObj(),
                ),
                start,
            );
            try self.defineVariable(imported.variable, start);
        }
    }

    fn varDecl(self: *Self) !void {
        const start = self.getStart();
        const data = self.getData().var_decl;
        self.manager.instr_idx += 1;

        if (self.manager.instr_tags[self.manager.instr_idx] == .null) {
            try self.writeOp(.null, start);
            self.manager.instr_idx += 1;
        } else {
            try self.compileInstr();

            if (data.cast) try self.compileInstr();
        }

        try self.defineVariable(data.variable, start);

        if (data.variable.scope == .heap) {
            self.manager.heap_count += 1;
            // We place a 'tombstone' value here because during static analyzis,
            // the locals list is synchronized to all following declaration. Instead of
            // back patching all locals declaration past a heap variable, we make the
            // compiler place a tombstone.
            try self.writeOp(.null, start);
        }
    }

    fn whileInstr(self: *Self) Error!void {
        const start = self.getStart();
        self.manager.instr_idx += 1;

        const chunk = self.getChunk();
        const loop_start = chunk.code.items.len;

        try self.compileInstr();
        const exit_jump = try self.emitJump(.JumpIfFalse, start);

        // If true
        try chunk.writeOp(.Pop, start);
        try self.compileInstr();
        try self.emitLoop(loop_start, start);

        try self.patchJump(exit_jump);
        // If false
        try chunk.writeOp(.Pop, start);
    }
};
