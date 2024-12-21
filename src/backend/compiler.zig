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
    chunk: Chunk,
    errs: ArrayList(CompilerReport),
    analyzed_stmts: UnsafeIter(AnalyzedStmt),

    const Self = @This();
    const Error = Chunk.Error;

    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(vm: *Vm) Self {
        return .{
            .vm = vm,
            .chunk = Chunk.init(vm.allocator),
            .errs = ArrayList(CompilerReport).init(vm.allocator),
            .analyzed_stmts = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        self.chunk.deinit();
        self.errs.deinit();
    }

    fn write_op_and_byte(self: *Self, op: OpCode, byte: u8) !void {
        try self.chunk.write_op(op);
        try self.chunk.write_byte(byte);
    }

    fn emit_constant(self: *Self, value: Value) !void {
        self.write_op_and_byte(.Constant, try self.chunk.write_constant(value)) catch |err| {
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

    pub fn compile(self: *Self, stmts: []const Stmt, analyzed_stmts: []const AnalyzedStmt) !void {
        self.analyzed_stmts = UnsafeIter(AnalyzedStmt).init(analyzed_stmts);

        for (stmts) |*stmt| {
            try self.statement(stmt);
        }

        try self.chunk.write_op(.Return);
    }

    fn statement(self: *Self, stmt: *const Stmt) !void {
        try switch (stmt.*) {
            .Assignment => |*s| self.assignment(s),
            .Discard => |*s| self.expression_statement(s.expr),
            .Print => |*s| self.print_stmt(s),
            .VarDecl => |*s| self.var_declaration(s),
            .Expr => |expr| self.expression_statement(expr),
        };
    }

    fn assignment(self: *Self, stmt: *const Ast.Assignment) !void {
        try self.expression(stmt.value);

        // We cast the value on top of stack if needed
        const assign_extra = self.analyzed_stmts.next().Assignment;
        if (assign_extra.cast == .Yes) try self.chunk.write_op(.CastToFloat);

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
        try self.chunk.write_op(.Print);
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !void {
        if (stmt.value) |v| {
            try self.expression(v);
            const extra = self.analyzed_stmts.next().Assignment;

            if (extra.cast == .Yes) try self.chunk.write_op(.CastToFloat);
        } else try self.chunk.write_op(.Null);

        const extra = self.analyzed_stmts.next().Variable;

        // BUG: Protect the cast, we can't have more than 256 variable to lookup
        // for now
        if (extra.scope == .Global) {
            try self.write_op_and_byte(.DefineGlobal, @intCast(extra.index));
        }
        // No else, if local variable value sits on top of stack
    }

    /// Expressions used as statements, e.g.:
    ///  3 + 4
    /// without any assignment. To avoid having the value sitting on top
    /// of the stack, we discard it in cases where the expression is in
    /// stabdalone like the example
    fn expression_statement(self: *Self, expr: *const Expr) !void {
        try self.expression(expr);
        try self.chunk.write_op(.Pop);
    }

    fn expression(self: *Self, expr: *const Expr) Error!void {
        try switch (expr.*) {
            .Block => |*e| self.block(e),
            .BoolLit => |*e| self.bool_lit(e),
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => |*e| self.float_lit(e),
            .Identifier => self.ident_expr(),
            .If => unreachable,
            .IntLit => |*e| self.int_lit(e),
            .NullLit => self.null_lit(),
            .StringLit => |*e| self.string_lit(e),
            .Unary => |*e| self.unary(e),
        };
    }

    fn block(self: *Self, expr: *const Ast.Block) Error!void {
        const extra = self.analyzed_stmts.next().Block;

        for (expr.stmts, 0..) |*stmt, i| {
            // If this is the last stmt and the block returns a value
            // we compile with "expression" to avoid compiling a POP
            // with expression_statement
            if (i == expr.stmts.len - 1 and extra.returns_value) {
                try self.expression(stmt.Expr);
            } else try self.statement(stmt);
        }

        // If we don't return any value, then we brain the POP from
        // expression_statement
        // The only case in which we don't return a value and the
        // analyzer is ok with that is in cases where blocks are
        // written outside expressions, so always called with
        // expression statement
        // TODO: protect the @intCast
        if (!extra.returns_value) {
            // try self.chunk.write_op(.Null);

            // -1 because we know here we are called by expression_statement
            // which is gonna emit a POP
            for (0..extra.pop_count - 1) |_| {
                // for (0..extra.pop_count) |_| {
                try self.chunk.write_op(.Pop);
            }
        } else try self.write_op_and_byte(.ScopeReturn, @intCast(extra.pop_count));
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) !void {
        const extra = self.analyzed_stmts.next().Binop;

        try self.expression(expr.lhs);

        // For Str, the cast field is used in another way
        if (extra.cast == .Lhs and extra.type_ != Str) {
            try self.chunk.write_op(.CastToFloat);
        }

        try self.expression(expr.rhs);

        if (extra.cast == .Rhs and extra.type_ != Str) {
            try self.chunk.write_op(.CastToFloat);
        }

        try switch (extra.type_) {
            Int => switch (expr.op) {
                .Plus => self.chunk.write_op(.AddInt),
                .Minus => self.chunk.write_op(.SubtractInt),
                .Star => self.chunk.write_op(.MultiplyInt),
                .Slash => self.chunk.write_op(.DivideInt),
                .EqualEqual => self.chunk.write_op(.EqualInt),
                .BangEqual => self.chunk.write_op(.DifferentInt),
                .Greater => self.chunk.write_op(.GreaterInt),
                .GreaterEqual => self.chunk.write_op(.GreaterEqualInt),
                .Less => self.chunk.write_op(.LessInt),
                .LessEqual => self.chunk.write_op(.LessEqualInt),
                else => unreachable,
            },
            Float => switch (expr.op) {
                .Plus => self.chunk.write_op(.AddFloat),
                .Minus => self.chunk.write_op(.SubtractFloat),
                .Star => self.chunk.write_op(.MultiplyFloat),
                .Slash => self.chunk.write_op(.DivideFloat),
                .EqualEqual => self.chunk.write_op(.EqualFloat),
                .BangEqual => self.chunk.write_op(.DifferentFloat),
                .Greater => self.chunk.write_op(.GreaterFloat),
                .GreaterEqual => self.chunk.write_op(.GreaterEqualFloat),
                .Less => self.chunk.write_op(.LessFloat),
                .LessEqual => self.chunk.write_op(.LessEqualFloat),
                else => unreachable,
            },
            Str => switch (expr.op) {
                .EqualEqual => self.chunk.write_op(.EqualStr),
                .Plus => self.chunk.write_op(.StrCat),
                .Star => {
                    // We use the cast info to determine where is the integer
                    // for the multiplication
                    const op: OpCode = if (extra.cast == .Lhs) .StrMulL else .StrMulR;
                    try self.chunk.write_op(op);
                },
                else => unreachable,
            },
            // If result is bool or none, there is nothing special to do
            else => {},
        };
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) !void {
        try self.expression(expr.expr);
    }

    fn bool_lit(self: *Self, expr: *const Ast.BoolLit) !void {
        const op: OpCode = if (expr.value) .True else .False;
        try self.chunk.write_op(op);
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

    fn int_lit(self: *Self, expr: *const Ast.IntLit) !void {
        try self.emit_constant(Value.int(expr.value));
    }

    fn null_lit(self: *Self) !void {
        try self.chunk.write_op(.Null);
    }

    fn string_lit(self: *Self, expr: *const Ast.StringLit) !void {
        try self.emit_constant(Value.obj((try ObjString.copy(self.vm, expr.value)).as_obj()));
    }

    fn unary(self: *Self, expr: *const Ast.Unary) !void {
        const extra = self.analyzed_stmts.next().Unary;
        try self.expression(expr.rhs);

        if (expr.op == .Minus) {
            try switch (extra.type_) {
                Int => self.chunk.write_op(.NegateInt),
                Float => self.chunk.write_op(.NegateFloat),
                else => unreachable,
            };
        } else {
            try self.chunk.write_op(.Not);
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
