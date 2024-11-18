const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Ast = @import("../frontend/ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const BinOpType = @import("chunk.zig").BinOpType;
const Report = @import("../reporter.zig").Report;
const Value = @import("../runtime/values.zig").Value;

pub const Compiler = struct {
    chunk: Chunk,
    errs: ArrayList(Report),

    const Self = @This();
    const Error = Chunk.Error;

    pub fn init(allocator: Allocator) Self {
        return .{
            .chunk = Chunk.init(allocator),
            .errs = ArrayList(Report).init(allocator),
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
            .IntLit => |*e| self.int_lit(e),
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) !void {
        try self.expression(expr.lhs);
        try self.expression(expr.rhs);

        try switch (expr.op.kind) {
            .Plus => self.chunk.write_op(.Add),
            .Minus => self.chunk.write_op(.Subtract),
            .Star => self.chunk.write_op(.Multiply),
            .Slash => self.chunk.write_op(.Divide),
            else => unreachable,
        };

        try self.chunk.write_byte(@intFromEnum(BinOpType.IntInt));
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) !void {
        try self.expression(expr.expr);
    }

    fn int_lit(self: *Self, expr: *const Ast.IntLit) !void {
        try self.emit_constant(Value.int(expr.value));
    }

    fn unary(self: *Self, expr: *const Ast.Unary) !void {
        try switch (expr.op.kind) {
            .Minus => self.chunk.write_op(.Negate),
            .Not => self.chunk.write_op(.Not),
            else => unreachable,
        };
    }
};
