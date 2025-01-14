const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Ast = @import("../frontend/ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Vm = @import("../runtime/vm.zig").Vm;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const GenReport = @import("../reporter.zig").GenReport;
const Value = @import("../runtime/values.zig").Value;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const ObjString = @import("../runtime/obj.zig").ObjString;
const ObjFunction = @import("../runtime/obj.zig").ObjFunction;
const AnalyzedAst = @import("../frontend/analyzed_ast.zig");
const AnalyzedStmt = AnalyzedAst.AnalyzedStmt;
const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;
const TypeSys = @import("../frontend/type_system.zig");
const Disassembler = @import("../backend/disassembler.zig").Disassembler;

const Null = TypeSys.Null;
const Int = TypeSys.Int;
const Float = TypeSys.Float;
const Bool = TypeSys.Bool;
const Str = TypeSys.Str;

pub const CompilationManager = struct {
    vm: *Vm,
    compiler: Compiler,
    errs: ArrayList(CompilerReport),
    stmts: []const Ast.Stmt,
    analyzed_stmts: UnsafeIter(AnalyzedStmt),
    print_bytecode: bool,

    const Self = @This();
    const Error = error{err} || Chunk.Error;
    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(
        vm: *Vm,
        stmts: []const Ast.Stmt,
        analyzed_stmts: []const AnalyzedStmt,
        print_bytecode: bool,
    ) Self {
        return .{
            .vm = vm,
            .compiler = undefined,
            .errs = ArrayList(CompilerReport).init(vm.allocator),
            .stmts = stmts,
            .analyzed_stmts = UnsafeIter(AnalyzedStmt).init(analyzed_stmts),
            .print_bytecode = print_bytecode,
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
    }

    pub fn compile(self: *Self) !*ObjFunction {
        self.compiler = Compiler.init(self, null, .Global, "Script");

        for (self.stmts) |*stmt| {
            try self.compiler.statement(stmt);
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

    inline fn get_next_analyzed(self: *Self) *const AnalyzedStmt {
        return self.manager.analyzed_stmts.next();
    }

    fn write_op_and_byte(self: *Self, op: OpCode, byte: u8, offset: usize) !void {
        const c = self.get_chunk();
        try c.write_op(op, offset);
        try c.write_byte(byte, offset);
    }

    fn emit_constant(self: *Self, value: Value, offset: usize) !void {
        self.write_op_and_byte(.Constant, try self.get_chunk().write_constant(value), offset) catch |err| {
            // The idea is to collect errors as TreeSpan index. It means the parser is
            // going to have to generate an array list of spans and we have to keep track
            // of which we are at current to let reporter take the index to have span
            // infos for report. For now, I'm gonna see if this is necessary as there
            // will probably not be that much of errors here at compile stage.
            // See if analyzer needs same mechanics?
            // const report = Report.err_at_tree_index(.TooManyConst, );
            // try self.errs.append(report);
            std.debug.print("Too many constants in chunk\n", .{});
            return err;
        };
    }

    /// Declare the variable based on informations coming from Analyzer. Declares
    /// either in global scope or do nothing, as for local it's already living
    /// on the stack
    fn define_variable(self: *Self, infos: *const AnalyzedAst.Variable, offset: usize) !void {
        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        if (infos.scope == .Global) {
            try self.write_op_and_byte(.DefineGlobal, @intCast(infos.index), offset);
        }
    }

    fn emit_jump(self: *Self, kind: OpCode, offset: usize) !usize {
        const c = self.get_chunk();
        try c.write_op(kind, offset);
        try c.write_byte(0xff, offset);
        try c.write_byte(0xff, offset);

        return c.code.items.len - 2;
    }

    fn patch_jump(self: *Self, offset: usize) !void {
        const c = self.get_chunk();
        // -2 for the two 8bits jump value (cf emit jump)
        const jump = c.code.items.len - offset - 2;

        // TODO: proper error handling
        if (jump > std.math.maxInt(u16)) {
            std.debug.print("Too much code to jump over", .{});
            return Error.err;
        }

        // TODO: I think I don't need the first & 0xff
        c.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
        c.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emit_loop(self: *Self, loop_start: usize, offset: usize) !void {
        const c = self.get_chunk();
        try c.write_op(.Loop, offset);
        // +2 for loop own operands (jump offset on 16bits)
        const jump_offset = c.code.items.len - loop_start + 2;

        if (jump_offset > std.math.maxInt(u16)) {
            std.debug.print("Loop body too large\n", .{});
            return Error.err;
        }

        try c.write_byte(@as(u8, @intCast(jump_offset >> 8)) & 0xff, offset);
        try c.write_byte(@intCast(jump_offset & 0xff), offset);
    }

    /// Emits a *Null* and *Return* op code
    fn emit_return(self: *Self, offset: usize) Error!void {
        const c = self.get_chunk();
        try c.write_op(.Null, offset);
        try c.write_op(.Return, offset);
    }

    pub fn end(self: *Self) !*ObjFunction {
        // TODO: real offset
        try self.emit_return(0);

        // Disassembler
        if (self.manager.print_bytecode) {
            var dis = Disassembler.init(&self.function.chunk, self.manager.vm.allocator, false);
            defer dis.deinit();
            dis.dis_chunk(if (self.function.name) |n| n.chars else "Script") catch unreachable;
            std.debug.print("\n{s}", .{dis.disassembled.items});
        }

        return self.function;
    }

    fn statement(self: *Self, stmt: *const Stmt) !void {
        try switch (stmt.*) {
            .Assignment => |*s| self.assignment(s),
            .Discard => |*s| {
                try self.expression(s.expr);
                try self.get_chunk().write_op(.Pop, stmt.span().start);
            },
            .FnDecl => |*s| self.fn_declaration(s),
            .Print => |*s| self.print_stmt(s),
            .VarDecl => |*s| self.var_declaration(s),
            .While => |*s| self.while_stmt(s),
            .Expr => |expr| self.expression(expr),
        };
    }

    fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
        try self.expression(stmt.value);
        const offset = stmt.assigne.span().start;

        // We cast the value on top of stack if needed
        const assign_extra = self.get_next_analyzed().Assignment;
        if (assign_extra.cast == .Yes) try self.get_chunk().write_op(.CastToFloat, offset);

        // Scope and index resolution
        const extra = self.get_next_analyzed().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
            if (extra.scope == .Global) .SetGlobal else .SetLocal,
            @intCast(extra.index),
            offset,
        );
    }

    fn fn_declaration(self: *Self, stmt: *const Ast.FnDecl) !void {
        const extra = self.get_next_analyzed();
        try self.compile_function(.Fn, stmt);
        try self.define_variable(&extra.FnDecl.variable, stmt.name.start);
    }

    // TODO: Check if *kind* is really needed
    fn compile_function(self: *Self, kind: FnKind, stmt: *const Ast.FnDecl) !void {
        var compiler = Compiler.init(self.manager, self, kind, stmt.name.text);
        try compiler.block(&stmt.body);

        const func = try compiler.end();
        try self.emit_constant(Value.obj(func.as_obj()), stmt.name.start);
    }

    fn print_stmt(self: *Self, stmt: *const Ast.Print) !void {
        try self.expression(stmt.expr);
        try self.get_chunk().write_op(.Print, stmt.expr.span().start);
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
        const c = self.get_chunk();
        const offset = stmt.name.start;

        if (stmt.value) |v| {
            try self.expression(v);
            const extra = self.get_next_analyzed().Assignment;

            if (extra.cast == .Yes) try c.write_op(.CastToFloat, offset);
        } else try c.write_op(.Null, offset);

        try self.define_variable(&self.get_next_analyzed().Variable, offset);
    }

    fn while_stmt(self: *Self, stmt: *const Ast.While) Error!void {
        const c = self.get_chunk();
        const offset = stmt.condition.span().start;

        const loop_start = c.code.items.len;

        try self.expression(stmt.condition);
        const exit_jump = try self.emit_jump(.JumpIfFalse, offset);

        // If true
        try c.write_op(.Pop, offset);
        try self.statement(stmt.body);
        try self.emit_loop(loop_start, offset);

        try self.patch_jump(exit_jump);
        // If false
        try c.write_op(.Pop, offset);
    }

    fn expression(self: *Self, expr: *const Expr) Error!void {
        try switch (expr.*) {
            .Block => |*e| self.block(e),
            .BoolLit => |*e| self.bool_lit(e),
            .BinOp => |*e| self.binop(e),
            .FloatLit => |*e| self.float_lit(e),
            .FnCall => |*e| self.fn_call(e),
            .Grouping => |*e| self.grouping(e),
            .Identifier => |*e| self.ident_expr(e.span.start),
            .If => |*e| self.if_expr(e),
            .IntLit => |*e| self.int_lit(e),
            .NullLit => |*e| self.null_lit(e.span.start),
            .Return => |*e| self.return_expr(e),
            .StringLit => |*e| self.string_lit(e),
            .Unary => |*e| self.unary(e),
        };
    }

    fn block(self: *Self, expr: *const Ast.Block) Error!void {
        const extra = self.get_next_analyzed().Block;
        const offset = expr.span.start;

        for (expr.stmts) |*stmt| {
            try self.statement(stmt);
        }

        // TODO: protect the @intCast
        if (extra.is_expr) {
            try self.write_op_and_byte(.ScopeReturn, @intCast(extra.pop_count), offset);
        } else {
            for (0..extra.pop_count) |_| {
                try self.get_chunk().write_op(.Pop, offset);
            }
        }
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) !void {
        const c = self.get_chunk();
        const offset = expr.span.start;

        const extra = self.get_next_analyzed().Binop;

        // Special handle for logicals
        if (extra.type_ == Bool) return self.logical_binop(expr);

        try self.expression(expr.lhs);

        // For Str, the cast field is used in another way
        if (extra.cast == .Lhs and extra.type_ != Str) {
            try c.write_op(.CastToFloat, offset);
        }

        try self.expression(expr.rhs);

        if (extra.cast == .Rhs and extra.type_ != Str) {
            try c.write_op(.CastToFloat, offset);
        }

        try switch (extra.type_) {
            Int => switch (expr.op) {
                .Plus => c.write_op(.AddInt, offset),
                .Minus => c.write_op(.SubtractInt, offset),
                .Star => c.write_op(.MultiplyInt, offset),
                .Slash => c.write_op(.DivideInt, offset),
                .EqualEqual => c.write_op(.EqualInt, offset),
                .BangEqual => c.write_op(.DifferentInt, offset),
                .Greater => c.write_op(.GreaterInt, offset),
                .GreaterEqual => c.write_op(.GreaterEqualInt, offset),
                .Less => c.write_op(.LessInt, offset),
                .LessEqual => c.write_op(.LessEqualInt, offset),
                else => unreachable,
            },
            Float => switch (expr.op) {
                .Plus => c.write_op(.AddFloat, offset),
                .Minus => c.write_op(.SubtractFloat, offset),
                .Star => c.write_op(.MultiplyFloat, offset),
                .Slash => c.write_op(.DivideFloat, offset),
                .EqualEqual => c.write_op(.EqualFloat, offset),
                .BangEqual => c.write_op(.DifferentFloat, offset),
                .Greater => c.write_op(.GreaterFloat, offset),
                .GreaterEqual => c.write_op(.GreaterEqualFloat, offset),
                .Less => c.write_op(.LessFloat, offset),
                .LessEqual => c.write_op(.LessEqualFloat, offset),
                else => unreachable,
            },
            Str => switch (expr.op) {
                .EqualEqual => c.write_op(.EqualStr, offset),
                .Plus => c.write_op(.StrCat, offset),
                .Star => {
                    // We use the cast info to determine where is the integer
                    // for the multiplication
                    const op: OpCode = if (extra.cast == .Lhs) .StrMulL else .StrMulR;
                    try c.write_op(op, offset);
                },
                else => unreachable,
            },
            // If result is bool or none, there is nothing special to do
            else => {},
        };
    }

    fn logical_binop(self: *Self, expr: *const Ast.BinOp) !void {
        const c = self.get_chunk();
        const offset = expr.span.start;

        switch (expr.op) {
            .And => {
                try self.expression(expr.lhs);
                const end_jump = try self.emit_jump(.JumpIfFalse, offset);
                // If true, pop the value, else the 'false' remains on top of stack
                try c.write_op(.Pop, offset);
                try self.expression(expr.rhs);
                try self.patch_jump(end_jump);
            },
            .Or => {
                try self.expression(expr.lhs);
                const else_jump = try self.emit_jump(.JumpIfTrue, offset);
                try c.write_op(.Pop, offset);
                try self.expression(expr.rhs);
                try self.patch_jump(else_jump);
            },
            else => unreachable,
        }
    }

    fn bool_lit(self: *Self, expr: *const Ast.BoolLit) !void {
        const op: OpCode = if (expr.value) .True else .False;
        try self.get_chunk().write_op(op, expr.span.start);
    }

    fn float_lit(self: *Self, expr: *const Ast.FloatLit) !void {
        try self.emit_constant(Value.float(expr.value), expr.span.start);
    }

    fn fn_call(self: *Self, expr: *const Ast.FnCall) !void {
        try self.expression(expr.callee);

        for (0..expr.arity) |i| {
            try self.expression(expr.args[i]);
        }

        try self.write_op_and_byte(.CallFn, @intCast(expr.arity), expr.span.start);
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) !void {
        try self.expression(expr.expr);
    }

    fn ident_expr(self: *Self, offset: usize) !void {
        const extra = self.get_next_analyzed().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
            if (extra.scope == .Global) .GetGlobal else .GetLocal,
            @intCast(extra.index),
            offset,
        );
    }

    fn if_expr(self: *Self, expr: *const Ast.If) !void {
        const c = self.get_chunk();
        const offset = expr.span.start;
        const extra = self.get_next_analyzed().If;

        try self.expression(expr.condition);
        const then_jump = try self.emit_jump(.JumpIfFalse, offset);
        // Pops the condition, no longer needed
        try c.write_op(.Pop, offset);

        try self.statement(&expr.then_body);
        if (extra.cast == .Then) try c.write_op(.CastToFloat, offset);

        // Exits the if expression
        const else_jump = try self.emit_jump(.Jump, offset);
        try self.patch_jump(then_jump);

        // If we go in the else branch, we pop the condition too
        try c.write_op(.Pop, offset);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (expr.else_body) |*body| {
            try self.statement(body);
            if (extra.cast == .Else) try c.write_op(.CastToFloat, offset);
        }

        try self.patch_jump(else_jump);
    }

    fn int_lit(self: *Self, expr: *const Ast.IntLit) !void {
        try self.emit_constant(Value.int(expr.value), expr.span.start);
    }

    fn null_lit(self: *Self, offset: usize) !void {
        try self.get_chunk().write_op(.Null, offset);
    }

    fn return_expr(self: *Self, expr: *const Ast.Return) !void {
        if (expr.expr) |e| {
            try self.expression(e);
            try self.get_chunk().write_op(.Return, expr.span.start);
        } else try self.emit_return(expr.span.start);
    }

    fn string_lit(self: *Self, expr: *const Ast.StringLit) !void {
        try self.emit_constant(
            Value.obj((try ObjString.copy(self.manager.vm, expr.value)).as_obj()),
            expr.span.start,
        );
    }

    fn unary(self: *Self, expr: *const Ast.Unary) !void {
        const c = self.get_chunk();
        const offset = expr.span.start;

        const extra = self.get_next_analyzed().Unary;
        try self.expression(expr.rhs);

        if (expr.op == .Minus) {
            try switch (extra.type_) {
                Int => c.write_op(.NegateInt, offset),
                Float => c.write_op(.NegateFloat, offset),
                else => unreachable,
            };
        } else {
            try c.write_op(.Not, offset);
        }
    }
};

// Tests
test Compiler {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_compiler.zig").get_test_data;

    const Tester = GenericTester("compiler", CompilerMsg, get_test_data);
    try Tester.run();
}
