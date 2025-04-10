const std = @import("std");
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;

const Disassembler = @import("../backend/disassembler.zig").Disassembler;
const Rir = @import("../frontend/rir.zig");
const Scope = Rir.Scope;
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Interner = @import("../interner.zig").Interner;
const GenReport = @import("../reporter.zig").GenReport;
const ObjString = @import("../runtime/obj.zig").ObjString;
const ObjFunction = @import("../runtime/obj.zig").ObjFunction;
const ObjNativeFn = @import("../runtime/obj.zig").ObjNativeFn;
const ObjStruct = @import("../runtime/obj.zig").ObjStruct;
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/vm.zig").Vm;
const NativeFn = @import("../std/meta.zig").NativeFn;
const Chunk = @import("chunk.zig").Chunk;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const OpCode = @import("chunk.zig").OpCode;

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
            try self.compiler.compile_instr();
        }

        if (!self.repl) {
            // We init the heap variables into the Vm
            // NOTE: maybe return the count as we return the function and let the Vm allocate?
            self.vm.heap_vars = try self.vm.allocator.alloc(Value, self.heap_count);

            // Insert a call to main with arity of 0 for now
            try self.compiler.write_op_and_byte(
                .GetGlobal,
                self.main_index.?,
                0,
            );
            try self.compiler.write_op_and_byte(.call, 0, 0);
        } else {
            try self.compiler.get_chunk().write_op(.ExitRepl, 0);
        }

        return self.compiler.end();
    }
};

const Compiler = struct {
    manager: *CompilationManager,
    enclosing: ?*Compiler,
    function: *ObjFunction,
    // TODO: useless information
    fn_kind: FnKind,

    const Self = @This();
    const Error = error{err} || Chunk.Error || std.posix.WriteError;

    const CompilerReport = GenReport(CompilerMsg);

    const FnKind = enum {
        global,
        Fn,
        Method,
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

    inline fn get_chunk(self: *Self) *Chunk {
        return &self.function.chunk;
    }

    inline fn get_data(self: *const Self) Instruction.Data {
        return self.manager.instr_data[self.manager.instr_idx];
    }

    inline fn get_start(self: *const Self) usize {
        return self.manager.instr_offsets[self.manager.instr_idx];
    }

    /// Writes an OpCode to the current chunk
    inline fn write_op(self: *Self, op: OpCode, offset: usize) Error!void {
        try self.get_chunk().write_op(op, offset);
    }

    /// Writes a byte to the current chunk
    inline fn write_byte(self: *Self, byte: u8, offset: usize) Error!void {
        try self.get_chunk().write_byte(byte, offset);
    }

    /// Writes an OpCode and a byte to the current chunk
    fn write_op_and_byte(self: *Self, op: OpCode, byte: u8, offset: usize) !void {
        try self.write_op(op, offset);
        try self.write_byte(byte, offset);
    }

    fn emit_constant(self: *Self, value: Value, offset: usize) !void {
        // TODO: error
        self.write_op_and_byte(.Constant, try self.get_chunk().write_constant(value), offset) catch |err| {
            std.debug.print("Too many constants in chunk\n", .{});
            return err;
        };
    }

    /// Emits the corresponding `GetHeap`, `GetGlobal` or `GetLocal` with the correct index
    fn emit_get_var(self: *Self, variable: Instruction.Variable, offset: usize) !void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        try self.write_op_and_byte(
            if (variable.scope == .heap) .GetHeap else if (variable.scope == .global) .GetGlobal else .GetLocal,
            @intCast(variable.index),
            offset,
        );
    }

    /// Declare the variable based on informations coming from Analyzer. Declares
    /// either in global scope or heap, as for local it's already living
    /// on the stack
    fn define_variable(self: *Self, infos: Instruction.Variable, offset: usize) !void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup for now
        if (infos.scope == .global)
            try self.write_op_and_byte(.DefineGlobal, @intCast(infos.index), offset)
        else if (infos.scope == .heap)
            try self.write_op_and_byte(.DefineHeapVar, @intCast(infos.index), offset);
    }

    fn emit_jump(self: *Self, kind: OpCode, offset: usize) !usize {
        const chunk = self.get_chunk();
        try chunk.write_op(kind, offset);
        try chunk.write_byte(0xff, offset);
        try chunk.write_byte(0xff, offset);

        return chunk.code.items.len - 2;
    }

    fn patch_jump(self: *Self, offset: usize) !void {
        const chunk = self.get_chunk();
        // -2 for the two 8bits jump value (cf emit jump)
        const jump = chunk.code.items.len - offset - 2;

        // TODO: proper error handling
        if (jump > std.math.maxInt(u16)) {
            std.debug.print("Too much code to jump over", .{});
            return Error.err;
        }

        // TODO: I think I don't need the first & 0xff
        chunk.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
        chunk.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emit_loop(self: *Self, loop_start: usize, offset: usize) !void {
        const chunk = self.get_chunk();
        try chunk.write_op(.Loop, offset);
        // +2 for loop own operands (jump offset on 16bits)
        const jump_offset = chunk.code.items.len - loop_start + 2;

        // TODO: Error handling
        if (jump_offset > std.math.maxInt(u16)) {
            std.debug.print("Loop body too large\n", .{});
            return Error.err;
        }

        try chunk.write_byte(@as(u8, @intCast(jump_offset >> 8)) & 0xff, offset);
        try chunk.write_byte(@intCast(jump_offset & 0xff), offset);
    }

    pub fn end(self: *Self) Error!*ObjFunction {
        // Disassembler
        if (self.manager.render_mode != .none) {
            var dis = Disassembler.init(&self.function.chunk, self.manager.vm.allocator, self.manager.render_mode);
            defer dis.deinit();
            dis.dis_chunk(if (self.function.name) |n| n.chars else "Script") catch unreachable;
            const stdout = std.io.getStdOut().writer();
            try stdout.print("{s}", .{dis.disassembled.items});
            if (self.enclosing != null) try stdout.writeAll("\n");
        }

        return self.function;
    }

    fn compile_instr(self: *Self) Error!void {
        try switch (self.manager.instr_tags[self.manager.instr_idx]) {
            .Assignment => self.assignment(),
            .Binop => self.binop(),
            .Block => self.block(),
            .Bool => self.bool_instr(),
            .Cast => self.cast(),
            .Discard => self.discard(),
            .field => unreachable,
            .Float => self.float_instr(),
            .call => self.fn_call(),
            .FnDecl => self.fn_decl(),
            .Name => unreachable,
            .Identifier => self.identifier(false),
            .IdentifierId => self.identifier(true),
            .Imported => unreachable,
            .Int => self.int_instr(),
            .If => self.if_instr(),
            .MultipleVarDecl => self.multiple_var_decl(),
            .Null => self.null_instr(),
            .Print => self.print_instr(),
            .Return => self.return_instr(),
            .String => self.string_instr(),
            .struct_decl => self.struct_decl(),
            .struct_literal => self.struct_literal(),
            .Unary => self.unary(),
            .Use => self.use(),
            .VarDecl => self.var_decl(),
            .While => self.while_instr(),
        };
    }

    fn assignment(self: *Self) Error!void {
        const assign_data = self.get_data().Assignment;
        const start = self.get_start();
        const data_idx = self.manager.instr_idx + 1;

        const data = if (self.manager.instr_tags[data_idx] == .IdentifierId)
            self.manager.instr_data[self.manager.instr_data[data_idx].Id].VarDecl.variable
        else
            self.manager.instr_data[data_idx].Variable;

        self.manager.instr_idx += 2;

        // Value
        try self.compile_instr();

        // We cast the value on top of stack if needed
        if (assign_data.cast) try self.compile_instr();

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
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

    fn binop(self: *Self) !void {
        const start = self.get_start();
        const data = self.get_data().Binop;
        self.manager.instr_idx += 1;

        // Special handle for logicals
        if (data.op == .And or data.op == .Or) return self.logical_binop(start, data);

        try self.compile_instr();
        if (data.cast == .lhs and data.op != .MulStr) try self.write_op(.CastToFloat, start);

        try self.compile_instr();
        if (data.cast == .rhs and data.op != .MulStr) try self.write_op(.CastToFloat, start);

        try self.write_op(
            switch (data.op) {
                .AddFloat => .AddFloat,
                .AddInt => .AddInt,
                .AddStr => .StrCat,
                .DivFloat => .DivFloat,
                .DivInt => .DivInt,
                .EqBool => .EqBool,
                .EqFloat => .EqFloat,
                .EqInt => .EqInt,
                .EqStr => .EqStr,
                .GeFloat => .GeFloat,
                .GeInt => .GeInt,
                .GtFloat => .GtFloat,
                .GtInt => .GtInt,
                .LeFloat => .LeFloat,
                .LeInt => .LeInt,
                .LtFloat => .LtFloat,
                .LtInt => .LtInt,
                .MulFloat => .MulFloat,
                .MulInt => .MulInt,
                .MulStr => if (data.cast == .rhs) .StrMulR else .StrMulL,
                .NeBool => .NeBool,
                .NeFloat => .NeFloat,
                .NeInt => .NeInt,
                .NeStr => .NeStr,
                .SubFloat => .SubFloat,
                .SubInt => .SubInt,
                else => unreachable,
            },
            start,
        );
    }

    fn cast(self: *Self) Error!void {
        const data = self.get_data();
        const start = self.get_start();
        self.manager.instr_idx += 1;

        try switch (data.CastTo) {
            .Float => self.write_op(.CastToFloat, start),
            .Int => unreachable,
        };
    }

    fn logical_binop(self: *Self, start: usize, data: Instruction.Binop) !void {
        switch (data.op) {
            .And => {
                try self.compile_instr();
                const end_jump = try self.emit_jump(.JumpIfFalse, start);
                // If true, pop the value, else the 'false' remains on top of stack
                try self.write_op(.Pop, start);
                try self.compile_instr();
                try self.patch_jump(end_jump);
            },
            .Or => {
                try self.compile_instr();
                const else_jump = try self.emit_jump(.JumpIfTrue, start);
                try self.write_op(.Pop, start);
                try self.compile_instr();
                try self.patch_jump(else_jump);
            },
            else => unreachable,
        }
    }

    fn block(self: *Self) Error!void {
        const data = self.get_data().Block;
        const start = self.get_start();
        self.manager.instr_idx += 1;

        for (0..data.length) |_| try self.compile_instr();

        if (data.is_expr) {
            try self.write_op_and_byte(.ScopeReturn, data.pop_count, start);
        } else {
            for (0..data.pop_count) |_| {
                try self.get_chunk().write_op(.Pop, start);
            }
        }
    }

    fn bool_instr(self: *Self) Error!void {
        const op: OpCode = if (self.get_data().Bool) .True else .False;
        try self.write_op(op, self.get_start());
        self.manager.instr_idx += 1;
    }

    fn discard(self: *Self) Error!void {
        self.manager.instr_idx += 1;
        try self.compile_instr();
        try self.write_op(.Pop, 0);
    }

    fn float_instr(self: *Self) !void {
        try self.emit_constant(Value.float(self.get_data().Float), self.get_start());
        self.manager.instr_idx += 1;
    }

    fn fn_call(self: *Self) !void {
        const data = self.get_data().call;
        const start = self.get_start();
        self.manager.instr_idx += 1;

        // Compiles the identifier
        try self.compile_instr();

        for (0..data.arity) |_| {
            try self.compile_instr();

            if (self.manager.instr_idx < self.manager.instr_tags.len and
                self.manager.instr_tags[self.manager.instr_idx] == .Cast)
                try self.compile_instr();
        }

        try self.write_op_and_byte(
            if (data.builtin) .NativeFnCall else .call,
            data.arity,
            start,
        );
    }

    fn fn_decl(self: *Self) Error!void {
        const idx = self.manager.instr_idx;

        const data = self.get_data().FnDecl;
        self.manager.instr_idx += 1;
        const fn_name = self.manager.interner.get_key(self.get_data().Id).?;
        self.manager.instr_idx += 1;
        const fn_var = self.get_data().VarDecl.variable;
        self.manager.instr_idx += 1;

        try self.compile_function(.Fn, fn_name, data);
        try self.define_variable(fn_var, 0);

        // Check for main function
        if (idx == self.manager.main) {
            self.manager.main_index = @intCast(fn_var.index);
        }
    }

    // TODO: Check if *kind* is really needed
    fn compile_function(
        self: *Self,
        kind: FnKind,
        name: []const u8,
        data: Instruction.FnDecl,
    ) Error!void {
        var compiler = Compiler.init(self.manager, self, kind, name);

        for (0..data.body_len) |_| {
            try compiler.compile_instr();
        }

        if (data.return_kind == .implicit_value) {
            try compiler.write_op(
                .Return,
                self.manager.instr_offsets[self.manager.instr_idx - 1],
            );
        } else if (data.return_kind == .implicit_void) {
            try compiler.write_op(
                .NakedReturn,
                self.manager.instr_offsets[self.manager.instr_idx - 1],
            );
        }

        const func = try compiler.end();
        try self.emit_constant(Value.obj(func.as_obj()), 0);
    }

    fn identifier(self: *Self, is_id: bool) !void {
        const data = if (is_id) blk: {
            const id = self.get_data().Id;
            break :blk self.manager.instr_data[id].VarDecl.variable;
        } else self.get_data().Variable;

        try self.emit_get_var(data, self.get_start());
        self.manager.instr_idx += 1;
    }

    fn int_instr(self: *Self) !void {
        try self.emit_constant(Value.int(self.get_data().Int), self.get_start());
        self.manager.instr_idx += 1;
    }

    fn if_instr(self: *Self) !void {
        const data = self.get_data().If;
        const start = self.get_start();
        self.manager.instr_idx += 1;

        // Condition
        try self.compile_instr();
        const then_jump = try self.emit_jump(.JumpIfFalse, start);
        // Pops the condition, no longer needed
        try self.write_op(.Pop, start);

        // Then body
        try self.compile_instr();
        if (data.cast == .then) try self.write_op(.CastToFloat, start);

        // Exits the if expression
        const else_jump = try self.emit_jump(.Jump, start);
        try self.patch_jump(then_jump);

        // If we go in the else branch, we pop the condition too
        try self.write_op(.Pop, start);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.has_else) {
            try self.compile_instr();
            if (data.cast == .@"else") try self.write_op(.CastToFloat, start);
        }

        try self.patch_jump(else_jump);
    }

    fn multiple_var_decl(self: *Self) !void {
        const data = self.get_data().Id;
        self.manager.instr_idx += 1;

        for (0..data) |_| {
            try self.compile_instr();
        }
    }

    fn null_instr(self: *Self) !void {
        try self.write_op(.Null, self.get_start());
        self.manager.instr_idx += 1;
    }

    fn print_instr(self: *Self) !void {
        const start = self.get_start();
        self.manager.instr_idx += 1;
        try self.compile_instr();
        try self.get_chunk().write_op(.Print, start);
    }

    fn return_instr(self: *Self) !void {
        const data = self.get_data().Return;
        const start = self.get_start();
        self.manager.instr_idx += 1;

        if (data.value) {
            try self.compile_instr();
            if (data.cast) try self.compile_instr();

            try self.write_op(.Return, start);
        } else try self.write_op(.NakedReturn, start);
    }

    fn string_instr(self: *Self) !void {
        try self.emit_constant(
            Value.obj((try ObjString.copy(
                self.manager.vm,
                self.manager.interner.get_key(self.get_data().Id).?,
            )).as_obj()),
            self.get_start(),
        );
        self.manager.instr_idx += 1;
    }

    fn struct_decl(self: *Self) !void {
        const start = self.get_start();
        const data = self.get_data().struct_decl;
        self.manager.instr_idx += 1;
        const name = self.get_data().Id;
        self.manager.instr_idx += 1;
        const struct_var = self.get_data().VarDecl;

        try self.emit_constant(
            Value.obj((try ObjStruct.create(
                self.manager.vm,
                try ObjString.copy(self.manager.vm, self.manager.interner.get_key(name).?),
                data.fields_count,
            )).as_obj()),
            self.get_start(),
        );
        try self.define_variable(struct_var.variable, start);

        self.manager.instr_idx += 1;
    }

    fn struct_literal(self: *Self) !void {
        const start = self.get_start();
        const data = self.get_data().struct_literal;
        self.manager.instr_idx += 1;

        var list: std.ArrayListUnmanaged(usize) = .{};
        try list.ensureTotalCapacity(self.manager.vm.allocator, data.arity);
        defer list.deinit(self.manager.vm.allocator);

        for (0..data.arity) |_| {
            const field_idx = self.get_data().field;
            list.appendAssumeCapacity(field_idx);
            self.manager.instr_idx += 1;
            try self.compile_instr();
        }

        try self.write_op_and_byte(.struct_literal, @intCast(data.arity), start);
        try self.emit_get_var(data.variable, start);

        for (list.items) |field| {
            try self.write_byte(@intCast(field), start);
        }
    }

    fn unary(self: *Self) !void {
        const start = self.get_start();
        const data = self.get_data().Unary;
        self.manager.instr_idx += 1;

        try self.compile_instr();

        if (data.op == .minus) {
            try self.write_op(
                if (data.typ == .Int) .NegateInt else .NegateFloat,
                start,
            );
        } else {
            try self.write_op(.Not, start);
        }
    }

    fn use(self: *Self) !void {
        const data = self.get_data().Use;
        self.manager.instr_idx += 1;

        // NOTE: For now, analyzer places an empty import
        // to skip "std" by placing a Null instruction. Needs a rework
        self.manager.instr_idx += 1;

        for (0..data) |_| {
            const imported = self.get_data().Imported;
            const start = self.get_start();
            self.manager.instr_idx += 1;

            try self.emit_constant(
                Value.obj(
                    (try ObjNativeFn.create(
                        self.manager.vm,
                        self.manager.natives[imported.index],
                    )).as_obj(),
                ),
                start,
            );
            try self.define_variable(imported.variable, start);
        }
    }

    fn var_decl(self: *Self) !void {
        const start = self.get_start();
        const data = self.get_data().VarDecl;
        self.manager.instr_idx += 1;

        if (self.manager.instr_tags[self.manager.instr_idx] == .Null) {
            try self.write_op(.Null, start);
            self.manager.instr_idx += 1;
        } else {
            try self.compile_instr();

            if (data.cast) try self.compile_instr();
        }

        try self.define_variable(data.variable, start);

        if (data.variable.scope == .heap) {
            self.manager.heap_count += 1;
            // We place a 'tombstone' value here because during static analyzis,
            // the locals list is synchronized to all following declaration. Instead of
            // back patching all locals declaration past a heap variable, we make the
            // compiler place a tombstone.
            try self.write_op(.Null, start);
        }
    }

    fn while_instr(self: *Self) Error!void {
        const start = self.get_start();
        self.manager.instr_idx += 1;

        const chunk = self.get_chunk();
        const loop_start = chunk.code.items.len;

        try self.compile_instr();
        const exit_jump = try self.emit_jump(.JumpIfFalse, start);

        // If true
        try chunk.write_op(.Pop, start);
        try self.compile_instr();
        try self.emit_loop(loop_start, start);

        try self.patch_jump(exit_jump);
        // If false
        try chunk.write_op(.Pop, start);
    }
};
