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

const Null = AnalyzedAst.Null;
const Int = AnalyzedAst.Int;
const Float = AnalyzedAst.Float;
const Bool = AnalyzedAst.Bool;
const Str = AnalyzedAst.Str;

pub const Compiler = struct {
    vm: *Vm,
    function: *ObjFunction,
    fn_kind: FnKind,
    errs: ArrayList(CompilerReport),
    analyzed_stmts: UnsafeIter(AnalyzedStmt),

    const Self = @This();
    const Error = error{err} || Chunk.Error;

    const CompilerReport = GenReport(CompilerMsg);

    const FnKind = enum {
        Global,
        Fn,
        Method,
    };

    pub fn init(vm: *Vm, fn_kind: FnKind) Self {
        // TODO: clean this up
        return .{
            .vm = vm,
            .function = ObjFunction.create(vm, ObjString.copy(vm, "Global") catch unreachable) catch unreachable,
            .fn_kind = fn_kind,
            .errs = ArrayList(CompilerReport).init(vm.allocator),
            .analyzed_stmts = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
    }

    inline fn get_chunk(self: *Self) *Chunk {
        return &self.function.chunk;
    }

    fn write_op_and_byte(self: *Self, op: OpCode, byte: u8) !void {
        const c = self.get_chunk();
        try c.write_op(op);
        try c.write_byte(byte);
    }

    fn emit_constant(self: *Self, value: Value) !void {
        self.write_op_and_byte(.Constant, try self.get_chunk().write_constant(value)) catch |err| {
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

    fn emit_jump(self: *Self, kind: OpCode) !usize {
        const c = self.get_chunk();
        try c.write_op(kind);
        try c.write_byte(0xff);
        try c.write_byte(0xff);

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

        c.code.items[offset] = @as(u8, @intCast(jump >> 8)) & 0xff;
        c.code.items[offset + 1] = @intCast(jump & 0xff);
    }

    fn emit_loop(self: *Self, loop_start: usize) !void {
        const c = self.get_chunk();
        try c.write_op(.Loop);
        // +2 for loop own operands (jump offset on 16bits)
        const offset = c.code.items.len - loop_start + 2;

        if (offset > std.math.maxInt(u16)) {
            std.debug.print("Loop body too large\n", .{});
            return Error.err;
        }

        try c.write_byte(@as(u8, @intCast(offset >> 8)) & 0xff);
        try c.write_byte(@intCast(offset & 0xff));
    }

    pub fn compile(self: *Self, stmts: []const Stmt, analyzed_stmts: []const AnalyzedStmt) !*ObjFunction {
        self.analyzed_stmts = UnsafeIter(AnalyzedStmt).init(analyzed_stmts);

        for (stmts) |*stmt| {
            // From here, we always want to pop results from expressions
            try self.statement(stmt);
        }

        try self.get_chunk().write_op(.Return);
        return self.function;
    }

    fn statement(self: *Self, stmt: *const Stmt) !void {
        try switch (stmt.*) {
            .Assignment => |*s| self.assignment(s),
            .Discard => |*s| {
                try self.expression(s.expr);
                try self.get_chunk().write_op(.Pop);
            },
            .Print => |*s| self.print_stmt(s),
            .VarDecl => |*s| self.var_declaration(s),
            .While => |*s| self.while_stmt(s),
            .Expr => |expr| self.expression(expr),
        };
    }

    fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
        try self.expression(stmt.value);

        // We cast the value on top of stack if needed
        const assign_extra = self.analyzed_stmts.next().Assignment;
        if (assign_extra.cast == .Yes) try self.get_chunk().write_op(.CastToFloat);

        // Scope and index resolution
        const extra = self.analyzed_stmts.next().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
            if (extra.scope == .Global) .SetGlobal else .SetLocal,
            @intCast(extra.index),
        );
    }

    fn print_stmt(self: *Self, stmt: *const Ast.Print) !void {
        try self.expression(stmt.expr);
        try self.get_chunk().write_op(.Print);
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
        const c = self.get_chunk();

        if (stmt.value) |v| {
            try self.expression(v);
            const extra = self.analyzed_stmts.next().Assignment;

            if (extra.cast == .Yes) try c.write_op(.CastToFloat);
        } else try c.write_op(.Null);

        const extra = self.analyzed_stmts.next().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        if (extra.scope == .Global) {
            try self.write_op_and_byte(.DefineGlobal, @intCast(extra.index));
        }
        // No else, if local variable value sits on top of stack
    }

    fn while_stmt(self: *Self, stmt: *const Ast.While) Error!void {
        const c = self.get_chunk();

        const loop_start = c.code.items.len;

        try self.expression(stmt.condition);
        const exit_jump = try self.emit_jump(.JumpIfFalse);

        // If true
        try c.write_op(.Pop);
        try self.statement(stmt.body);
        try self.emit_loop(loop_start);

        try self.patch_jump(exit_jump);
        // If false
        try c.write_op(.Pop);
    }

    fn expression(self: *Self, expr: *const Expr) Error!void {
        try switch (expr.*) {
            .Block => |*e| self.block(e),
            .BoolLit => |*e| self.bool_lit(e),
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => |*e| self.float_lit(e),
            .Identifier => self.ident_expr(),
            .If => |*e| self.if_expr(e),
            .IntLit => |*e| self.int_lit(e),
            .NullLit => self.null_lit(),
            .StringLit => |*e| self.string_lit(e),
            .Unary => |*e| self.unary(e),
        };
    }

    fn block(self: *Self, expr: *const Ast.Block) Error!void {
        const extra = self.analyzed_stmts.next().Block;

        for (expr.stmts) |*stmt| {
            try self.statement(stmt);
        }

        // TODO: protect the @intCast
        if (extra.is_expr) {
            try self.write_op_and_byte(.ScopeReturn, @intCast(extra.pop_count));
        } else {
            for (0..extra.pop_count) |_| {
                try self.get_chunk().write_op(.Pop);
            }
        }
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) !void {
        const c = self.get_chunk();

        const extra = self.analyzed_stmts.next().Binop;

        // Special handle for logicals
        if (extra.type_ == Bool) return self.logical_binop(expr);

        try self.expression(expr.lhs);

        // For Str, the cast field is used in another way
        if (extra.cast == .Lhs and extra.type_ != Str) {
            try c.write_op(.CastToFloat);
        }

        try self.expression(expr.rhs);

        if (extra.cast == .Rhs and extra.type_ != Str) {
            try c.write_op(.CastToFloat);
        }

        try switch (extra.type_) {
            Int => switch (expr.op) {
                .Plus => c.write_op(.AddInt),
                .Minus => c.write_op(.SubtractInt),
                .Star => c.write_op(.MultiplyInt),
                .Slash => c.write_op(.DivideInt),
                .EqualEqual => c.write_op(.EqualInt),
                .BangEqual => c.write_op(.DifferentInt),
                .Greater => c.write_op(.GreaterInt),
                .GreaterEqual => c.write_op(.GreaterEqualInt),
                .Less => c.write_op(.LessInt),
                .LessEqual => c.write_op(.LessEqualInt),
                else => unreachable,
            },
            Float => switch (expr.op) {
                .Plus => c.write_op(.AddFloat),
                .Minus => c.write_op(.SubtractFloat),
                .Star => c.write_op(.MultiplyFloat),
                .Slash => c.write_op(.DivideFloat),
                .EqualEqual => c.write_op(.EqualFloat),
                .BangEqual => c.write_op(.DifferentFloat),
                .Greater => c.write_op(.GreaterFloat),
                .GreaterEqual => c.write_op(.GreaterEqualFloat),
                .Less => c.write_op(.LessFloat),
                .LessEqual => c.write_op(.LessEqualFloat),
                else => unreachable,
            },
            Str => switch (expr.op) {
                .EqualEqual => c.write_op(.EqualStr),
                .Plus => c.write_op(.StrCat),
                .Star => {
                    // We use the cast info to determine where is the integer
                    // for the multiplication
                    const op: OpCode = if (extra.cast == .Lhs) .StrMulL else .StrMulR;
                    try c.write_op(op);
                },
                else => unreachable,
            },
            // If result is bool or none, there is nothing special to do
            else => {},
        };
    }

    fn logical_binop(self: *Self, expr: *const Ast.BinOp) !void {
        const c = self.get_chunk();

        switch (expr.op) {
            .And => {
                try self.expression(expr.lhs);
                const end_jump = try self.emit_jump(.JumpIfFalse);
                // If true, pop the value, else the 'false' remains on top of stack
                try c.write_op(.Pop);
                try self.expression(expr.rhs);
                try self.patch_jump(end_jump);
            },
            .Or => {
                try self.expression(expr.lhs);
                const else_jump = try self.emit_jump(.JumpIfTrue);
                try c.write_op(.Pop);
                try self.expression(expr.rhs);
                try self.patch_jump(else_jump);
            },
            else => unreachable,
        }
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) !void {
        try self.expression(expr.expr);
    }

    fn bool_lit(self: *Self, expr: *const Ast.BoolLit) !void {
        const op: OpCode = if (expr.value) .True else .False;
        try self.get_chunk().write_op(op);
    }

    fn float_lit(self: *Self, expr: *const Ast.FloatLit) !void {
        try self.emit_constant(Value.float(expr.value));
    }

    fn ident_expr(self: *Self) !void {
        const extra = self.analyzed_stmts.next().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        try self.write_op_and_byte(
            if (extra.scope == .Global) .GetGlobal else .GetLocal,
            @intCast(extra.index),
        );
    }

    fn if_expr(self: *Self, expr: *const Ast.If) !void {
        const c = self.get_chunk();

        const extra = self.analyzed_stmts.next().If;

        try self.expression(expr.condition);
        const then_jump = try self.emit_jump(.JumpIfFalse);
        // Pops the condition, no longer needed
        try c.write_op(.Pop);

        try self.statement(&expr.then_body);
        if (extra.cast == .Then) try c.write_op(.CastToFloat);

        // Exits the if expression
        const else_jump = try self.emit_jump(.Jump);
        try self.patch_jump(then_jump);

        // If we go in the else branch, we pop the condition too
        try c.write_op(.Pop);

        // We insert a jump in the then body to be able to jump over the else branch
        // Otherwise, we just patch the then_jump
        if (expr.else_body) |*body| {
            try self.statement(body);
            if (extra.cast == .Else) try c.write_op(.CastToFloat);
        }

        try self.patch_jump(else_jump);
    }

    fn int_lit(self: *Self, expr: *const Ast.IntLit) !void {
        try self.emit_constant(Value.int(expr.value));
    }

    fn null_lit(self: *Self) !void {
        try self.get_chunk().write_op(.Null);
    }

    fn string_lit(self: *Self, expr: *const Ast.StringLit) !void {
        try self.emit_constant(Value.obj((try ObjString.copy(self.vm, expr.value)).as_obj()));
    }

    fn unary(self: *Self, expr: *const Ast.Unary) !void {
        const c = self.get_chunk();

        const extra = self.analyzed_stmts.next().Unary;
        try self.expression(expr.rhs);

        if (expr.op == .Minus) {
            try switch (extra.type_) {
                Int => c.write_op(.NegateInt),
                Float => c.write_op(.NegateFloat),
                else => unreachable,
            };
        } else {
            try c.write_op(.Not);
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
