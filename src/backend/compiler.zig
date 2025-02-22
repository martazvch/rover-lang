const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
// const Ast = @import("../frontend/ast.zig");
// const Stmt = Ast.Stmt;
// const Expr = Ast.Expr;
const Vm = @import("../runtime/vm.zig").Vm;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const GenReport = @import("../reporter.zig").GenReport;
const Value = @import("../runtime/values.zig").Value;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const ObjString = @import("../runtime/obj.zig").ObjString;
const ObjFunction = @import("../runtime/obj.zig").ObjFunction;
const ObjNativeFn = @import("../runtime/obj.zig").ObjNativeFn;
// const AnalyzedAst = @import("../frontend/analyzed_ast.zig");
// const AnalyzedStmt = AnalyzedAst.AnalyzedStmt;
// const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;
// const TypeSys = @import("../frontend/type_system.zig");
const Disassembler = @import("../backend/disassembler.zig").Disassembler;
const NativeFn = @import("../std/meta.zig").NativeFn;
const Rir = @import("../frontend/rir.zig");
const Scope = Rir.Scope;
const ReturnKind = Rir.ReturnKind;
const Instruction = Rir.Instruction;
const Interner = @import("../interner.zig").Interner;

// const Null = TypeSys.Null;
// const Int = TypeSys.Int;
// const Float = TypeSys.Float;
// const Bool = TypeSys.Bool;
// const Str = TypeSys.Str;

pub const CompilationManager = struct {
    vm: *Vm,
    natives: []const NativeFn,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    interner: *const Interner,
    instr_tags: []const Instruction.Tag,
    instr_data: []const Instruction.Data,
    instr_starts: []const usize,
    instr_idx: usize,
    // stmts: []const Ast.Stmt,
    // analyzed_stmts: UnsafeIter(AnalyzedStmt),
    print_bytecode: bool,
    // main: *const Ast.FnDecl,
    main: usize,
    main_index: ?u8,
    repl: bool,

    const Self = @This();
    const Error = error{err} || Chunk.Error;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        vm: *Vm,
        natives: []const NativeFn,
        interner: *const Interner,
        instr_tags: []const Instruction.Tag,
        instr_data: []const Instruction.Data,
        instr_starts: []const usize,
        // stmts: []const Ast.Stmt,
        // analyzed_stmts: []const AnalyzedStmt,
        print_bytecode: bool,
        main: usize,
        // main: *const Ast.FnDecl,
        repl: bool,
    ) Self {
        return .{
            .vm = vm,
            .natives = natives,
            .compiler = undefined,
            .errs = ArrayList(CompilerReport).init(vm.allocator),
            .interner = interner,
            .instr_tags = instr_tags,
            .instr_data = instr_data,
            .instr_starts = instr_starts,
            .instr_idx = 0,
            // .stmts = stmts,
            // .analyzed_stmts = UnsafeIter(AnalyzedStmt).init(analyzed_stmts),
            .print_bytecode = print_bytecode,
            .main = main,
            // .main = main,
            .main_index = null,
            .repl = repl,
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
    }

    pub fn compile(self: *Self) !*ObjFunction {
        self.compiler = Compiler.init(self, null, .Global, "Global scope");

        while (self.instr_idx < self.instr_tags.len) {
            try self.compiler.compile_instr();
        }

        if (!self.repl) {
            // Insert a call to main with arity of 0 for now
            try self.compiler.write_op_and_byte(
                .GetGlobal,
                self.main_index.?,
                0,
            );
            try self.compiler.write_op_and_byte(.FnCall, 0, 0);
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
    const Error = error{err} || Chunk.Error;

    const CompilerReport = GenReport(CompilerMsg);

    const FnKind = enum {
        Global,
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
        return self.manager.instr_starts[self.manager.instr_idx];
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
        self.write_op_and_byte(.Constant, try self.get_chunk().write_constant(value), offset) catch |err| {
            std.debug.print("Too many constants in chunk\n", .{});
            return err;
        };
    }

    /// Declare the variable based on informations coming from Analyzer. Declares
    /// either in global scope or do nothing, as for local it's already living
    /// on the stack
    fn define_variable(self: *Self, infos: Instruction.Variable, offset: usize) !void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        if (infos.scope == .Global) {
            try self.write_op_and_byte(.DefineGlobal, @intCast(infos.index), offset);
        }
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

        if (jump_offset > std.math.maxInt(u16)) {
            std.debug.print("Loop body too large\n", .{});
            return Error.err;
        }

        try chunk.write_byte(@as(u8, @intCast(jump_offset >> 8)) & 0xff, offset);
        try chunk.write_byte(@intCast(jump_offset & 0xff), offset);
    }

    /// Emits a `Null` and `Return` op code
    fn emit_return(self: *Self, offset: usize) Error!void {
        try self.write_op(.Null, offset);
        try self.write_op(.Return, offset);
    }

    pub fn end(self: *Self) !*ObjFunction {
        // Disassembler
        if (self.manager.print_bytecode) {
            var dis = Disassembler.init(&self.function.chunk, self.manager.vm.allocator, false);
            defer dis.deinit();
            dis.dis_chunk(if (self.function.name) |n| n.chars else "Script") catch unreachable;
            std.debug.print("\n{s}", .{dis.disassembled.items});
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
            .Float => self.float_instr(),
            .FnCall => self.fn_call(),
            .FnDecl => self.fn_decl(),
            .FnName => unreachable,
            .Identifier => self.identifier(),
            .Imported => unreachable,
            .Int => self.int_instr(),
            .If => self.if_instr(),
            .Null => self.null_instr(),
            .Print => self.print_instr(),
            .Return => self.return_instr(),
            .String => self.string_instr(),
            .Unary => self.unary(),
            .Use => self.use(),
            .VarDecl => self.var_decl(),
            .While => self.while_instr(),
        };
    }

    fn assignment(self: *Self) Error!void {
        const data = self.get_data().Assignment;
        const start = self.get_start();
        self.manager.instr_idx += 1;

        try self.compile_instr();

        // We cast the value on top of stack if needed
        if (data.cast) try self.compile_instr();

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
            if (data.variable.scope == .Global) .SetGlobal else .SetLocal,
            @intCast(data.variable.index),
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
        if (data.cast == .Lhs and data.op != .MulStr) try self.write_op(.CastToFloat, start);

        try self.compile_instr();
        if (data.cast == .Rhs and data.op != .MulStr) try self.write_op(.CastToFloat, start);

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
                .MulStr => if (data.cast == .Rhs) .StrMulR else .StrMulL,
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

        // TODO: protect the @intCast
        if (data.is_expr) {
            try self.write_op_and_byte(.ScopeReturn, @intCast(data.pop_count), start);
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
        const data = self.get_data().FnCall;
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
            if (data.builtin) .NativeFnCall else .FnCall,
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
        const fn_var = self.get_data().Variable;
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

        if (data.return_kind == .ImplicitValue) {
            try compiler.write_op(.Return, self.get_start());
        } else if (data.return_kind == .ImplicitVoid) {
            try compiler.emit_return(0);
        }

        const func = try compiler.end();
        try self.emit_constant(Value.obj(func.as_obj()), 0);
    }

    fn identifier(self: *Self) !void {
        const data = self.get_data().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
            if (data.scope == .Global) .GetGlobal else .GetLocal,
            @intCast(data.index),
            self.get_start(),
        );
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
        if (data.cast == .Then) try self.write_op(.CastToFloat, start);

        // Exits the if expression
        const else_jump = try self.emit_jump(.Jump, start);
        try self.patch_jump(then_jump);

        // If we go in the else branch, we pop the condition too
        try self.write_op(.Pop, start);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (data.has_else) {
            try self.compile_instr();
            if (data.cast == .Else) try self.write_op(.CastToFloat, start);
        }

        try self.patch_jump(else_jump);
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

        if (data) {
            try self.compile_instr();
            try self.write_op(.Return, start);
        } else try self.emit_return(start);
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

    fn unary(self: *Self) !void {
        const start = self.get_start();
        const data = self.get_data().Unary;
        self.manager.instr_idx += 1;

        try self.compile_instr();

        if (data.op == .Minus) {
            try self.write_op(
                if (data.type_ == .Int) .NegateInt else .NegateFloat,
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

    // -----------------------------------------------------

    // fn statement(self: *Self, stmt: *const Stmt) !void {
    //     try switch (stmt.*) {
    //         .Assignment => |*s| self.assignment(s),
    //         .Discard => |*s| {
    //             try self.expression(s.data);
    //             try self.get_chunk().write_op(.Pop, stmt.span().start);
    //         },
    //         .FnDecl => |*s| self.fn_declaration(s),
    //         .Print => |*s| self.print_stmt(s),
    //         .Use => |*s| self.use_stmt(s),
    //         .VarDecl => |*s| self.var_declaration(s),
    //         .While => |*s| self.while_stmt(s),
    //         .Expr => |expr| self.expression(expr),
    //     };
    // }

    // fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
    //     try self.expression(stmt.value);
    //     const offset = stmt.assigne.span().start;
    //
    //     // We cast the value on top of stack if needed
    //     const assign_extra = self.get_next_analyzed().Assignment;
    //     if (assign_extra.cast == .Yes) try self.get_chunk().write_op(.CastToFloat, offset);
    //
    //     // Scope and index resolution
    //     const extra = self.get_next_analyzed().Variable;
    //
    //     // BUG: Protect the cast, we can't have more than 256 variable to lookup
    //     // for now
    //     try self.write_op_and_byte(
    //         if (extra.scope == .Global) .SetGlobal else .SetLocal,
    //         @intCast(extra.index),
    //         offset,
    //     );
    // }

    // fn fn_declaration(self: *Self, stmt: *const Ast.FnDecl) !void {
    //     const extra = self.get_next_analyzed().FnDecl;
    //
    //     try self.compile_function(.Fn, stmt, &extra);
    //     try self.define_variable(&extra.variable, stmt.name.start);
    //
    //     // Check for main function
    //     if (self.manager.main_index == null and !self.manager.repl and stmt == self.manager.main) {
    //         self.manager.main_index = @intCast(extra.variable.index);
    //     }
    // }

    // TODO: Check if *kind* is really needed
    // fn compile_function(
    //     self: *Self,
    //     kind: FnKind,
    //     stmt: *const Ast.FnDecl,
    //     extra: *const AnalyzedAst.FnDecl,
    // ) Error!void {
    //     var compiler = Compiler.init(self.manager, self, kind, stmt.name.text);
    //
    //     for (stmt.body.stmts) |*s| {
    //         try compiler.statement(s);
    //     }
    //
    //     if (extra.return_kind == .ImplicitValue) {
    //         try compiler.get_chunk().write_op(.Return, stmt.name.start);
    //     } else if (extra.return_kind == .ImplicitVoid) {
    //         try compiler.emit_return(stmt.name.start);
    //     }
    //
    //     const func = try compiler.end();
    //     try self.emit_constant(Value.obj(func.as_obj()), stmt.name.start);
    // }

    // fn print_stmt(self: *Self, stmt: *const Ast.Print) !void {
    //     try self.expression(stmt.expr);
    //     try self.get_chunk().write_op(.Print, stmt.expr.span().start);
    // }

    // fn use_stmt(self: *Self, stmt: *const Ast.Use) !void {
    //     const extra = self.get_next_analyzed().Use;
    //
    //     for (0..extra.indices.items.len) |i| {
    //         try self.emit_constant(
    //             Value.obj(
    //                 (try ObjNativeFn.create(
    //                     self.manager.vm,
    //                     self.manager.natives[extra.indices.items[i]],
    //                 )).as_obj(),
    //             ),
    //             stmt.span.start,
    //         );
    //         try self.define_variable(&extra.variables.items[i], stmt.span.start);
    //     }
    // }

    // fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
    //     const c = self.get_chunk();
    //     const offset = stmt.name.start;
    //
    //     if (stmt.value) |v| {
    //         try self.expression(v);
    //         const extra = self.get_next_analyzed().Assignment;
    //
    //         if (extra.cast == .Yes) try c.write_op(.CastToFloat, offset);
    //     } else try c.write_op(.Null, offset);
    //
    //     try self.define_variable(&self.get_next_analyzed().Variable, offset);
    // }

    // fn while_stmt(self: *Self, stmt: *const Ast.While) Error!void {
    //     const c = self.get_chunk();
    //     const offset = stmt.condition.span().start;
    //
    //     const loop_start = c.code.items.len;
    //
    //     try self.expression(stmt.condition);
    //     const exit_jump = try self.emit_jump(.JumpIfFalse, offset);
    //
    //     // If true
    //     try c.write_op(.Pop, offset);
    //     try self.statement(stmt.body);
    //     try self.emit_loop(loop_start, offset);
    //
    //     try self.patch_jump(exit_jump);
    //     // If false
    //     try c.write_op(.Pop, offset);
    // }

    // fn expression(self: *Self, expr: *const Expr) Error!void {
    //     try switch (expr.*) {
    //         .Block => |*e| self.block(e),
    //         .BoolLit => |*e| self.bool_lit(e),
    //         .BinOp => |*e| self.binop(e),
    //         .FloatLit => |*e| self.float_lit(e),
    //         .FnCall => |*e| self.fn_call(e),
    //         .Grouping => |*e| self.grouping(e),
    //         .Identifier => |*e| self.ident_expr(e.span.start),
    //         .If => |*e| self.if_expr(e),
    //         .IntLit => |*e| self.int_lit(e),
    //         .NullLit => |*e| self.null_lit(e.span.start),
    //         .Return => |*e| self.return_expr(e),
    //         .StringLit => |*e| self.string_lit(e),
    //         .Unary => |*e| self.unary(e),
    //     };
    // }

    // fn block(self: *Self, expr: *const Ast.Block) Error!void {
    //     const extra = self.get_next_analyzed().Block;
    //     const offset = expr.span.start;
    //
    //     for (expr.stmts) |*stmt| {
    //         try self.statement(stmt);
    //     }
    //
    //     // TODO: protect the @intCast
    //     if (extra.is_expr) {
    //         try self.write_op_and_byte(.ScopeReturn, @intCast(extra.pop_count), offset);
    //     } else {
    //         for (0..extra.pop_count) |_| {
    //             try self.get_chunk().write_op(.Pop, offset);
    //         }
    //     }
    // }

    // fn binop(self: *Self, expr: *const Ast.BinOp) !void {
    //     const c = self.get_chunk();
    //     const offset = expr.span.start;
    //
    //     const extra = self.get_next_analyzed().Binop;
    //
    //     // Special handle for logicals
    //     if (extra.type_ == Bool) return self.logical_binop(expr);
    //
    //     try self.expression(expr.lhs);
    //
    //     // For Str, the cast field is used in another way
    //     if (extra.cast == .Lhs and extra.type_ != Str) {
    //         try c.write_op(.CastToFloat, offset);
    //     }
    //
    //     try self.expression(expr.rhs);
    //
    //     if (extra.cast == .Rhs and extra.type_ != Str) {
    //         try c.write_op(.CastToFloat, offset);
    //     }
    //
    //     try switch (extra.type_) {
    //         Int => switch (expr.op) {
    //             .Plus => c.write_op(.AddInt, offset),
    //             .Minus => c.write_op(.SubtractInt, offset),
    //             .Star => c.write_op(.MultiplyInt, offset),
    //             .Slash => c.write_op(.DivideInt, offset),
    //             .EqualEqual => c.write_op(.EqualInt, offset),
    //             .BangEqual => c.write_op(.DifferentInt, offset),
    // //             .Greater => c.write_op(.GreaterInt, offset),
    //             .GreaterEqual => c.write_op(.GreaterEqualInt, offset),
    //             .Less => c.write_op(.LessInt, offset),
    //             .LessEqual => c.write_op(.LessEqualInt, offset),
    //             else => unreachable,
    //         },
    //         Float => switch (expr.op) {
    //             .Plus => c.write_op(.AddFloat, offset),
    //             .Minus => c.write_op(.SubtractFloat, offset),
    //             .Star => c.write_op(.MultiplyFloat, offset),
    //             .Slash => c.write_op(.DivideFloat, offset),
    //             .EqualEqual => c.write_op(.EqualFloat, offset),
    //             .BangEqual => c.write_op(.DifferentFloat, offset),
    //             .Greater => c.write_op(.GreaterFloat, offset),
    //             .GreaterEqual => c.write_op(.GreaterEqualFloat, offset),
    //             .Less => c.write_op(.LessFloat, offset),
    //             .LessEqual => c.write_op(.LessEqualFloat, offset),
    //             else => unreachable,
    //         },
    //         Str => switch (expr.op) {
    //             .EqualEqual => c.write_op(.EqualStr, offset),
    //             .Plus => c.write_op(.StrCat, offset),
    //             .Star => {
    //                 // We use the cast info to determine where is the integer
    //                 // for the multiplication
    //                 const op: OpCode = if (extra.cast == .Lhs) .StrMulL else .StrMulR;
    //                 try c.write_op(op, offset);
    //             },
    //             else => unreachable,
    //         },
    //         // If result is bool or none, there is nothing special to do
    //         else => {},
    //     };
    // }

    // fn logical_binop(self: *Self, expr: *const Ast.BinOp) !void {
    //     const c = self.get_chunk();
    //     const offset = expr.span.start;
    //
    //     switch (expr.op) {
    //         .And => {
    //             try self.expression(expr.lhs);
    //             const end_jump = try self.emit_jump(.JumpIfFalse, offset);
    //             // If true, pop the value, else the 'false' remains on top of stack
    //             try c.write_op(.Pop, offset);
    //             try self.expression(expr.rhs);
    //             try self.patch_jump(end_jump);
    //         },
    //         .Or => {
    //             try self.expression(expr.lhs);
    //             const else_jump = try self.emit_jump(.JumpIfTrue, offset);
    //             try c.write_op(.Pop, offset);
    //             try self.expression(expr.rhs);
    //             try self.patch_jump(else_jump);
    //         },
    //         else => unreachable,
    //     }
    // }

    // fn bool_lit(self: *Self, expr: *const Ast.BoolLit) !void {
    //     const op: OpCode = if (expr.value) .True else .False;
    //     try self.get_chunk().write_op(op, expr.span.start);
    // }

    // fn float_lit(self: *Self, expr: *const Ast.FloatLit) !void {
    //     try self.emit_constant(Value.float(expr.value), expr.span.start);
    // }

    // fn fn_call(self: *Self, expr: *const Ast.FnCall) !void {
    //     try self.expression(expr.callee);
    //
    //     var counter: usize = 0;
    //     const extra = self.get_next_analyzed().FnCall;
    //
    //     for (0..expr.arity) |i| {
    //         try self.expression(expr.args[i]);
    //
    //         // Check one by one if we must implicit cast the argument
    //         if (extra.casts.len > 0 and extra.casts.buffer[counter] == i) {
    //             try self.get_chunk().write_op(.CastToFloat, expr.span.start);
    //             counter += 1;
    //         }
    //     }
    //
    //     try self.write_op_and_byte(
    //         if (extra.builtin) .NativeFnCall else .FnCall,
    //         @intCast(expr.arity),
    //         expr.span.start,
    //     );
    // }

    // fn grouping(self: *Self, expr: *const Ast.Grouping) !void {
    //     try self.expression(expr.expr);
    // }

    // fn ident_expr(self: *Self, offset: usize) !void {
    //     const extra = self.get_next_analyzed().Variable;
    //
    //     // BUG: Protect the cast, we can't have more than 256 variable to lookup
    //     // for now
    //     try self.write_op_and_byte(
    //         if (extra.scope == .Global) .GetGlobal else .GetLocal,
    //         @intCast(extra.index),
    //         offset,
    //     );
    // }

    // fn if_expr(self: *Self, expr: *const Ast.If) !void {
    //     const c = self.get_chunk();
    //     const offset = expr.span.start;
    //     const extra = self.get_next_analyzed().If;
    //
    //     try self.expression(expr.condition);
    //     const then_jump = try self.emit_jump(.JumpIfFalse, offset);
    //     // Pops the condition, no longer needed
    //     try c.write_op(.Pop, offset);
    //
    //     try self.statement(&expr.then_body);
    //     if (extra.cast == .Then) try c.write_op(.CastToFloat, offset);
    //
    //     // Exits the if expression
    //     const else_jump = try self.emit_jump(.Jump, offset);
    //     try self.patch_jump(then_jump);
    //
    //     // If we go in the else branch, we pop the condition too
    //     try c.write_op(.Pop, offset);
    //
    //     // We insert a jump in the then body to be able to jump over the else branch
    //     // Otherwise, we just patch the then_jump
    //     if (expr.else_body) |*body| {
    //         try self.statement(body);
    //         if (extra.cast == .Else) try c.write_op(.CastToFloat, offset);
    //     }
    //
    //     try self.patch_jump(else_jump);
    // }

    // fn int_lit(self: *Self, expr: *const Ast.IntLit) !void {
    //     try self.emit_constant(Value.int(expr.value), expr.span.start);
    // }

    // fn null_lit(self: *Self, offset: usize) !void {
    //     try self.get_chunk().write_op(.Null, offset);
    // }

    // fn return_expr(self: *Self, expr: *const Ast.Return) !void {
    //     if (expr.expr) |e| {
    //         try self.expression(e);
    //         try self.get_chunk().write_op(.Return, expr.span.start);
    //     } else try self.emit_return(expr.span.start);
    // }

    // fn string_lit(self: *Self, expr: *const Ast.StringLit) !void {
    //     try self.emit_constant(
    //         Value.obj((try ObjString.copy(self.manager.vm, expr.value)).as_obj()),
    //         expr.span.start,
    //     );
    // }

    // fn unary(self: *Self, expr: *const Ast.Unary) !void {
    //     const c = self.get_chunk();
    //     const offset = expr.span.start;
    //
    //     const extra = self.get_next_analyzed().Unary;
    //     try self.expression(expr.rhs);
    //
    //     if (expr.op == .Minus) {
    //         try switch (extra.type_) {
    //             Int => c.write_op(.NegateInt, offset),
    //             Float => c.write_op(.NegateFloat, offset),
    //             else => unreachable,
    //         };
    //     } else {
    //         try c.write_op(.Not, offset);
    //     }
    // }
};

// Tests
test Compiler {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_compiler.zig").get_test_data;

    const Tester = GenericTester("compiler", CompilerMsg, get_test_data);
    try Tester.run();
}
