const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const activeTag = std.meta.activeTag;
const Ast = @import("ast.zig");
const Stmt = Ast.Stmt;
const Expr = Ast.Expr;
const Span = Ast.Span;
const AnalyzedAst = @import("analyzed_ast.zig");
const AnalyzedStmt = AnalyzedAst.AnalyzedStmt;
const GenReport = @import("../reporter.zig").GenReport;

const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;

// pub const Type = union(enum) {
//     Bool,
//     Float,
//     Int,
//     Null,
//     Str,
//     Struct: []const u8,
//
//     const Self = @This();
//
//     const types = std.StaticStringMap(Type).initComptime(.{
//         .{ "bool", .Bool },
//         .{ "float", .Float },
//         .{ "int", .Int },
//         .{ "null", .Null },
//         .{ "str", .Str },
//     });
//
//     pub fn get_type(name: []const u8) Type {
//         if (types.get(name)) |t| {
//             return t;
//         } else {
//             return .{ .Struct = name };
//         }
//     }
//
//     pub fn str(self: Self) []const u8 {
//         return switch (self) {
//             .Bool => "bool",
//             .Float => "float",
//             .Int => "int",
//             .Null => "null",
//             .Str => "string",
//             .Struct => |t| t,
//         };
//     }
// };

// TODO: change to a struct containing an union. This manoeuvre makes
// every element 8 bytes large Type = u32), as the union does. For
// redability, better to do this:
//
// const Size = union(enum) {
//     A,
//     B,
//     C,
// };
//
// const Size2 = union(enum) {
//     A, B,
//     C: u32,
// };
//
// pub fn main() !void {
//     print("Size1: {}\n", .{@sizeOf(Size)});
//     print("Size2: {}\n", .{@sizeOf(Size2)}); ==> 8
// }

const Type = u32;
const Null: Type = 0;
const Int: Type = 1;
const Float: Type = 2;
const Bool: Type = 3;
const Str: Type = 4;

pub const TypeManager = struct {
    declared: StringHashMap(Type),

    const Self = @This();

    const builtins = std.StaticStringMap(Type).initComptime(.{
        .{ "bool", Bool },
        .{ "float", Float },
        .{ "int", Int },
        .{ "null", Null },
        .{ "str", Str },
    });

    pub fn init(allocator: Allocator) Self {
        return .{ .declared = StringHashMap(Type).init(allocator) };
    }

    pub fn init_builtins(self: *Self) !void {
        try self.declared.put("bool", Bool);
        try self.declared.put("float", Float);
        try self.declared.put("int", Int);
        try self.declared.put("str", Str);
    }

    pub fn deinit(self: *Self) void {
        self.declared.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.declared.clearRetainingCapacity();
    }

    pub fn str(self: *const Self, type_: Type) []const u8 {
        while (self.declared.iterator().next()) |entry| {
            if (entry.value_ptr == type_) {
                return entry.key_ptr.*;
            }
        }
    }
};

pub const Analyzer = struct {
    errs: ArrayList(AnalyzerReport),
    warns: ArrayList(AnalyzerReport),
    globals: StringHashMap(Type),
    analyzed_stmts: ArrayList(AnalyzedStmt),
    type_manager: TypeManager,

    const Self = @This();
    const Error = error{Err} || Allocator.Error;

    const AnalyzerReport = GenReport(AnalyzerMsg);

    pub fn init(allocator: Allocator) Self {
        return .{
            .errs = ArrayList(AnalyzerReport).init(allocator),
            .warns = ArrayList(AnalyzerReport).init(allocator),
            .globals = StringHashMap(Type).init(allocator),
            .analyzed_stmts = ArrayList(AnalyzedStmt).init(allocator),
            .type_manager = TypeManager.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errs.deinit();
        self.warns.deinit();
        self.globals.deinit();
        self.analyzed_stmts.deinit();
        self.type_manager.deinit();
    }

    pub fn reinit(self: *Self) void {
        self.errs.clearRetainingCapacity();
        self.warns.clearRetainingCapacity();
        self.globals.clearRetainingCapacity();
        self.analyzed_stmts.clearRetainingCapacity();
        self.type_manager.clearRetainingCapacity();
    }

    fn is_numeric(t: Type) bool {
        return (t == Int or t == Float);
    }

    fn err(self: *Self, kind: AnalyzerMsg, span: Span) !void {
        const report = AnalyzerReport.err(kind, span);
        try self.errs.append(report);
        return error.Err;
    }

    fn warn(self: *Self, kind: AnalyzerMsg, span: Span) !void {
        const report = AnalyzerReport.warn(kind, span);
        try self.warns.append(report);
    }

    pub fn analyze(self: *Self, stmts: []const Stmt) !void {
        for (stmts) |*stmt| {
            _ = self.statement(stmt) catch |e| {
                switch (e) {
                    // If it's our own error, we continue
                    error.Err => continue,
                    else => return e,
                }
            };
        }
    }

    fn statement(self: *Self, stmt: *const Stmt) !Type {
        return switch (stmt.*) {
            .Print => |*s| {
                _ = try self.expression(s.expr);
                return Null;
            },
            .VarDecl => |*s| self.var_declaration(s),
            .Expr => |e| self.expression(e),
        };
    }

    fn var_declaration(self: *Self, stmt: *const Ast.VarDecl) !Type {
        const id = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var var_decl_extra: AnalyzedAst.Variable = .{};

        var final_type = Null;

        // If a type was declared
        if (stmt.type_) |t| {
            final_type = t;
        }

        if (stmt.value) |v| {
            const value_type = try self.expression(v);

            // If no type declared, we infer the value type
            if (final_type == Null) {
                final_type = value_type;
                // Else, we check for coherence
            } else if (final_type != value_type) {
                // One case in wich we can coerce; int -> float
                if (final_type == Float and value_type == Int) {} else {
                    try self.err(
                        .{ .InvalidVarDeclType = .{
                            .expect = final_type.str(),
                            .found = value_type.str(),
                        } },
                        v.span(),
                    );
                }
            }
        }

        try self.globals.put(stmt.name, final_type);
        var_decl_extra.index = self.globals.count();
        self.analyzed_stmts.items[id] = var_decl_extra;

        return 0;
    }

    fn expression(self: *Self, expr: *const Expr) !Type {
        return switch (expr.*) {
            .BoolLit => .Bool,
            .BinOp => |*e| self.binop(e),
            .Grouping => |*e| self.grouping(e),
            .FloatLit => .Float,
            .IntLit => .Int,
            .NullLit => .Null,
            .StringLit => .Str,
            .Unary => |*e| self.unary(e),
        };
    }

    fn binop(self: *Self, expr: *const Ast.BinOp) Error!Type {
        // We reserve the slot because of recursion
        const id = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var binop_extra: AnalyzedAst.BinOp = .{};

        const lhs = try self.expression(expr.lhs);
        const rhs = try self.expression(expr.rhs);

        binop_extra.type_ = lhs;

        var res = lhs;

        // String operations
        if (expr.op == .Plus and lhs == Str and rhs == Str) {
            return Str;
        } else if (expr.op == .Star) {
            if ((lhs == Str and rhs == Int) or (lhs == Int and rhs == Str)) {
                binop_extra.type_ = Str;

                // For string concatenation, we use the cast information to tell
                // on wich side is the integer (for the compiler)
                binop_extra.cast = if (rhs == Int) .Rhs else .Lhs;
                return Str;
            }
        }

        switch (expr.op) {
            // Arithmetic binop
            .Plus, .Minus, .Star, .Slash => {
                if (!Analyzer.is_numeric(lhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(lhs.str()),
                        expr.lhs.span(),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(rhs.str()),
                        expr.rhs.span(),
                    );
                }

                switch (lhs) {
                    Float => {
                        switch (rhs) {
                            Float => {},
                            Int => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast(.Rhs, lhs.str()),
                                    expr.rhs.span(),
                                );

                                binop_extra.cast = .Rhs;
                            },
                            else => unreachable,
                        }
                    },
                    Int => {
                        switch (rhs) {
                            Float => {
                                try self.warn(
                                    AnalyzerMsg.implicit_cast(.Lhs, rhs.str()),
                                    expr.lhs.span(),
                                );

                                binop_extra.type_ = Float;
                                binop_extra.cast = .Lhs;
                                res = Float;
                            },
                            Int => {},
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .EqualEqual, .BangEqual => {
                // If different value types
                if (lhs != rhs) {
                    // Check for implicit casts
                    if ((lhs == Int and rhs == Float) or (lhs == Float and rhs == Int)) {
                        if (lhs == Int) {
                            binop_extra.cast = .Lhs;

                            try self.warn(.FloatEqualCast, expr.lhs.span());
                        } else {
                            binop_extra.cast = .Rhs;

                            try self.warn(.FloatEqualCast, expr.rhs.span());
                        }

                        binop_extra.type_ = Float;
                    } else {
                        try self.err(
                            AnalyzerMsg.invalid_cmp(lhs.str(), rhs.str()),
                            expr.span,
                        );
                    }
                } else {
                    // Check for unsafe float comparisons
                    if (lhs == Float) {
                        try self.warn(.FloatEqual, expr.span);
                    }
                }

                res = Bool;
            },

            .Greater, .GreaterEqual, .Less, .LessEqual => {
                if (!Analyzer.is_numeric(lhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(lhs.str()),
                        expr.lhs.span(),
                    );
                }

                if (!Analyzer.is_numeric(rhs)) {
                    try self.err(
                        AnalyzerMsg.invalid_arithmetic(rhs.str()),
                        expr.rhs.span(),
                    );
                }

                switch (lhs) {
                    Float => switch (rhs) {
                        Float => try self.warn(.FloatEqual, expr.span),
                        Int => {
                            try self.warn(.FloatEqualCast, expr.rhs.span());

                            binop_extra.cast = .Rhs;
                        },
                        else => unreachable,
                    },
                    Int => switch (rhs) {
                        Float => {
                            try self.warn(.FloatEqualCast, expr.lhs.span());

                            binop_extra.cast = .Lhs;
                            binop_extra.type_ = Float;
                        },
                        Int => {},
                        else => unreachable,
                    },
                    else => unreachable,
                }

                res = Bool;
            },

            // Logical binop
            .And, .Or => unreachable,
            else => unreachable,
        }

        self.analyzed_stmts.items[id] = .{ .Binop = binop_extra };
        return res;
    }

    fn grouping(self: *Self, expr: *const Ast.Grouping) Error!Type {
        return self.expression(expr.expr);
    }

    fn unary(self: *Self, expr: *const Ast.Unary) Error!Type {
        const id = self.analyzed_stmts.items.len;
        try self.analyzed_stmts.append(undefined);
        var unary_extra: AnalyzedAst.Unary = .{};

        const rhs = try self.expression(expr.rhs);

        if (expr.op == .Not and rhs != Bool) {
            try self.err(
                .{ .InvalidUnary = .{ .found = rhs.str() } },
                expr.rhs.span(),
            );
        } else if (expr.op == .Minus and rhs != Int and rhs != Float) {
            try self.err(
                AnalyzerMsg.invalid_arithmetic(rhs.str()),
                expr.rhs.span(),
            );
        }

        unary_extra.type_ = rhs;

        self.analyzed_stmts.items[id] = .{ .Unary = unary_extra };
        return rhs;
    }
};

// Test
test Analyzer {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_analyzer.zig").get_test_data;

    const Tester = GenericTester("analyzer", AnalyzerMsg, get_test_data);
    try Tester.run();
}
