const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Ast = @import("../frontend/ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const BinOpType = @import("chunk.zig").BinOpType;
const GenReport = @import("../reporter.zig").GenReport;
const Value = @import("../runtime/values.zig").Value;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const AnalyzedBinop = @import("../frontend/analyzer.zig").Analyzer.AnalyzedBinOp;
const UnsafeIter = @import("../unsafe_iter.zig").UnsafeIter;

pub const Compiler = struct {
    chunk: Chunk,
    errs: ArrayList(CompilerReport),
    analyzed_binops: UnsafeIter(AnalyzedBinop),

    const Self = @This();
    const Error = Chunk.Error;

    const CompilerReport = GenReport(CompilerMsg);

    pub fn init(allocator: Allocator, binop_casts: []const AnalyzedBinop) Self {
        return .{
            .chunk = Chunk.init(allocator),
            .errs = ArrayList(CompilerReport).init(allocator),
            .analyzed_binops = UnsafeIter(AnalyzedBinop).init(binop_casts),
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

    pub fn compile(self: *Self, stmts: []const Stmt) !void {
        for (stmts) |*stmt| {
            try switch (stmt.*) {
                .VarDecl => @panic("not implemented yet"),
                .Expr => |expr| self.expression(expr),
            };
        }

        try self.chunk.write_op(.Return);
    }

    fn expression(self: *Self, expr: *const Expr) Error!void {
        try switch (expr.*) {
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => |*e| self.float_lit(e),
            .IntLit => |*e| self.int_lit(e),
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) !void {
        if (expr.op == .Plus or expr.op == .Minus or expr.op == .Star or expr.op == .Slash) {
            return self.arithmetic_binop(expr);
        }

        try self.expression(expr.lhs);
        try self.expression(expr.rhs);

        try switch (expr.op) {
            else => unreachable,
        };
    }

    fn arithmetic_binop(self: *Self, expr: *const Ast.BinOp) !void {
        const analyzed = self.analyzed_binops.next();

        try self.expression(expr.lhs);

        if (analyzed.cast == .Lhs) {
            switch (analyzed.type_) {
                .Int => try self.chunk.write_op(.CastToInt),
                .Float => try self.chunk.write_op(.CastToFloat),
                else => unreachable,
            }
        }

        try self.expression(expr.rhs);

        if (analyzed.cast == .Rhs) {
            switch (analyzed.type_) {
                .Int => try self.chunk.write_op(.CastToInt),
                .Float => try self.chunk.write_op(.CastToFloat),
                else => unreachable,
            }
        }

        try switch (analyzed.type_) {
            .Int => switch (expr.op) {
                .Plus => self.chunk.write_op(.AddInt),
                .Minus => self.chunk.write_op(.SubtractInt),
                .Star => self.chunk.write_op(.MultiplyInt),
                .Slash => self.chunk.write_op(.DivideInt),
                else => unreachable,
            },
            .Float => switch (expr.op) {
                .Plus => self.chunk.write_op(.AddFloat),
                .Minus => self.chunk.write_op(.SubtractFloat),
                .Star => self.chunk.write_op(.MultiplyFloat),
                .Slash => self.chunk.write_op(.DivideFloat),
                else => unreachable,
            },
            else => unreachable,
        };
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) !void {
        try self.expression(expr.expr);
    }

    fn float_lit(self: *Self, expr: *const Ast.FloatLit) !void {
        try self.emit_constant(Value.float(expr.value));
    }

    fn int_lit(self: *Self, expr: *const Ast.IntLit) !void {
        try self.emit_constant(Value.int(expr.value));
    }

    fn unary(self: *Self, expr: *const Ast.Unary) !void {
        try self.expression(expr.rhs);

        try switch (expr.op) {
            .Minus => self.chunk.write_op(.NegateInt),
            .Not => self.chunk.write_op(.Not),
            else => unreachable,
        };
    }
};
