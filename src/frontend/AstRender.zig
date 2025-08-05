const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");
const Span = @import("Lexer.zig").Span;

allocator: Allocator = undefined,
ast: *const Ast,

output: std.ArrayListUnmanaged(u8) = .{},
writer: std.ArrayListUnmanaged(u8).Writer = undefined,
indent_level: usize = 1,

const Self = @This();
const Error = std.ArrayListUnmanaged(u8).Writer.Error;
const spaces: []const u8 = " " ** 1024;
const INDENT_SIZE = 4;

pub fn init(allocator: Allocator, ast: *const Ast) Self {
    return .{ .allocator = allocator, .ast = ast };
}

pub fn render(self: *Self) !void {
    self.writer = self.output.writer(self.allocator);
    try self.writer.writeAll("{\n");

    for (self.ast.nodes, 0..) |*node, i| {
        try self.renderNode(node, i != self.ast.nodes.len - 1);
    }

    self.indent_level -= 1;
    try self.writer.writeAll("}\n");
}

fn renderNode(self: *Self, node: *const Ast.Node, comma: bool) Error!void {
    switch (node.*) {
        .assignment => |*n| {
            try self.openKey(@tagName(node.*), .block);
            try self.openKey("assignee", .block);
            try self.renderExpr(n.assigne, false);
            try self.closeKey(.block, true);
            try self.openKey("value", .block);
            try self.renderExpr(n.value, false);
            try self.closeKey(.block, false);
            try self.closeKey(.block, comma);
        },
        .discard => |n| {
            try self.openKey(@tagName(node.*), .block);
            try self.renderExpr(n, false);
            try self.closeKey(.block, comma);
        },
        .fn_decl => |*n| try self.renderFnDecl(n, comma),
        .multi_var_decl => |n| {
            try self.openKey(@tagName(node.*), .list);
            for (n.decls, 0..) |*decl, i| {
                try self.openKey("var_decl", .block);
                try self.renderNameTypeValue(decl, i != n.decls.len - 1);
                try self.closeKey(.block, comma);
            }
            try self.closeKey(.list, comma);
        },
        .print => |n| {
            try self.openKey(@tagName(node.*), .block);
            try self.renderExpr(n, false);
            try self.closeKey(.block, comma);
        },
        .struct_decl => |n| {
            try self.openKey(@tagName(node.*), .block);
            try self.pushKeyValue("name", self.spanToSrc(n.name), true);

            if (n.fields.len == 0) {
                try self.emptyKey("fields", .list, true);
            } else {
                try self.openKey("fields", .list);
                for (n.fields, 0..) |*v, i| {
                    const last = i != n.fields.len - 1;
                    try self.openBrace();
                    try self.renderNameTypeValue(v, false);
                    try self.closeBrace(last);
                }
                try self.closeKey(.list, true);
            }

            if (n.functions.len == 0) {
                try self.emptyKey("functions", .list, false);
            } else {
                try self.openKey("functions", .list);
                for (n.functions, 0..) |*f, i| {
                    const last = i != n.functions.len - 1;
                    try self.renderFnDecl(f, last);
                }
                try self.closeKey(.list, false);
            }

            try self.closeKey(.block, comma);
        },
        .use => |n| {
            try self.openKey("use", .block);

            try self.openKey("path", .list);
            for (n.names, 0..) |name, i| {
                const last = i != n.names.len - 1;
                try self.indent();
                try self.writer.print("\"{s}\"", .{self.spanToSrc(name)});
                try self.finishPush(last);
            }
            try self.closeKey(.list, true);

            if (n.items) |items| {
                try self.openKey("items", .list);
                for (items, 0..) |item, i| {
                    const last = i != items.len - 1;
                    try self.openBrace();
                    try self.pushKeyValue("item", self.spanToSrc(item.item), true);
                    try self.pushKeyValue("alias", if (item.alias) |alias| self.spanToSrc(alias) else "", false);
                    try self.closeBrace(last);
                }
                try self.closeKey(.list, true);
            } else try self.emptyKey("items", .list, true);

            try self.pushKeyValue("alias", if (n.alias) |alias| self.spanToSrc(alias) else "", false);
            try self.closeKey(.block, comma);
        },
        .var_decl => |*n| {
            try self.openKey("var_decl", .block);
            try self.renderNameTypeValue(n, false);
            try self.closeKey(.block, comma);
        },
        .@"while" => |*n| {
            try self.openKey("while", .block);
            try self.openKey("condition", .block);
            try self.renderExpr(n.condition, false);
            try self.closeKey(.block, true);
            try self.renderBlock(&n.body, false);
            try self.closeKey(.block, comma);
        },
        .expr => |n| try self.renderExpr(n, comma),
    }
}

fn renderFnDecl(self: *Self, decl: *const Ast.FnDecl, comma: bool) !void {
    try self.renderCallableDecl(self.spanToSrc(decl.name), decl.params, decl.return_type, decl.body, false, comma);
}

fn renderClosureDecl(self: *Self, decl: *const Ast.Closure, comma: bool) !void {
    try self.renderCallableDecl("", decl.params, decl.return_type, decl.body, true, comma);
}

fn renderCallableDecl(
    self: *Self,
    name: []const u8,
    params: []Ast.Param,
    return_type: ?*Ast.Type,
    body: Ast.Block,
    is_closure: bool,
    comma: bool,
) !void {
    try self.openKey(if (is_closure) "closure_decl" else "fn_decl", .block);
    try self.pushKeyValue("name", name, true);

    if (params.len == 0) {
        try self.emptyKey("params", .list, true);
    } else {
        try self.openKey("params", .list);
        for (params, 0..) |p, i| {
            const last = i != params.len - 1;
            try self.openBrace();
            try self.pushKeyValue("name", self.spanToSrc(p.name), true);
            if (p.typ) |typ| {
                try self.pushKeyValue("type", try self.renderType(typ), true);
            } else try self.pushKeyValue("type", "void", true);
            if (p.value) |val| {
                try self.openKey("value", .block);
                try self.renderExpr(val, false);
                try self.closeKey(.block, false);
            } else try self.emptyKey("value", .block, false);
            try self.closeBrace(last);
        }
        try self.closeKey(.list, true);
    }

    try self.pushKeyValue("return_type", if (return_type) |ret| try self.renderType(ret) else "void", true);
    try self.renderBlock(&body, false);
    try self.closeKey(.block, comma);
}

fn renderNameTypeValue(self: *Self, decl: *const Ast.VarDecl, comma: bool) !void {
    try self.pushKeyValue("name", self.spanToSrc(decl.name), true);

    if (decl.typ) |t| {
        try self.pushKeyValue("type", try self.renderType(t), true);
    } else try self.emptyKey("type", .block, true);

    if (decl.value) |val| {
        try self.openKey("value", .block);
        try self.renderExpr(val, false);
        try self.closeKey(.block, comma);
    } else try self.emptyKey("value", .block, comma);
}

fn renderType(self: *Self, typ: ?*Ast.Type) Error![]const u8 {
    if (typ == null) return "";

    var buf: std.ArrayListUnmanaged(u8) = .{};

    switch (typ.?.*) {
        .array => |t| {
            try buf.appendSlice(self.allocator, "[]");
            try buf.appendSlice(self.allocator, try self.renderType(t.child));
        },
        .fields => |fields| {
            for (fields, 0..) |f, i| {
                try buf.appendSlice(self.allocator, self.spanToSrc(f));
                if (i < fields.len - 1) {
                    try buf.appendSlice(self.allocator, ".");
                }
            }
        },
        .function => |t| {
            try buf.appendSlice(self.allocator, "fn(");

            if (t.params.len != 0) {
                for (t.params, 0..) |p, i| {
                    try buf.appendSlice(self.allocator, try self.renderType(p));
                    if (i != t.params.len - 1) {
                        try buf.appendSlice(self.allocator, ", ");
                    }
                }
            }

            try buf.appendSlice(self.allocator, ") -> ");
            if (t.return_type) |ret| {
                try buf.appendSlice(self.allocator, try self.renderType(ret));
            } else try buf.appendSlice(self.allocator, "void");
        },
        .scalar => |t| try buf.appendSlice(self.allocator, self.spanToSrc(t)),
        .self => try buf.appendSlice(self.allocator, "Self"),
    }

    return try buf.toOwnedSlice(self.allocator);
}

fn renderExpr(self: *Self, expr: *const Ast.Expr, comma: bool) Error!void {
    switch (expr.*) {
        .array => |*e| {
            if (e.values.len == 0)
                try self.emptyKey("array", .list, comma)
            else {
                try self.openKey("array", .list);
                for (e.values, 0..) |val, i| {
                    try self.renderExpr(val, i != e.values.len - 1);
                }
                try self.closeKey(.list, comma);
            }
        },
        .array_access => |*e| {
            try self.openKey("array access", .block);
            try self.openKey("array", .block);
            try self.renderExpr(e.array, false);
            try self.closeKey(.block, true);
            try self.openKey("index", .block);
            try self.renderExpr(e.index, false);
            try self.closeKey(.block, false);
            try self.closeKey(.block, comma);
        },
        .block => |*e| try self.renderBlock(e, comma),
        .binop => |*e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.openKey("lhs", .block);
            try self.renderExpr(e.lhs, false);
            try self.closeKey(.block, true);
            try self.openKey("rhs", .block);
            try self.renderExpr(e.rhs, false);
            try self.closeKey(.block, true);
            try self.pushKeyValue("op", switch (e.op) {
                .greater => ">",
                .greater_equal => ">=",
                .less => "<",
                .less_equal => "<=",
                .bang_equal => "!=",
                .equal_equal => "==",
                .@"and", .@"or" => |tag| @tagName(tag),
                .plus => "+",
                .minus => "-",
                .star => "*",
                .slash => "/",
                else => unreachable,
            }, false);
            try self.closeKey(.block, comma);
        },
        .closure => |*e| try self.renderClosureDecl(e, comma),
        .field => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.openKey("structure", .block);
            try self.renderExpr(e.structure, false);
            try self.closeKey(.block, true);
            try self.pushKeyValue("field_name", self.spanToSrc(e.field), false);
            try self.closeKey(.block, comma);
        },
        .fn_call => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.openKey("callee", .block);
            try self.renderExpr(e.callee, false);
            try self.closeKey(.block, true);

            if (e.args.len == 0) {
                try self.emptyKey("args", .list, false);
            } else {
                try self.openKey("args", .list);
                for (e.args, 0..) |arg, i| {
                    try self.renderExpr(arg, i != e.args.len - 1);
                }
                try self.closeKey(.list, false);
            }

            try self.closeKey(.block, comma);
        },
        .grouping => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.renderExpr(e.expr, false);
            try self.closeKey(.block, comma);
        },
        .@"if" => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.openKey("condition", .block);
            try self.renderExpr(e.condition, false);
            try self.closeKey(.block, true);
            try self.openKey("then", .block);
            try self.renderNode(&e.then, false);
            try self.closeKey(.block, true);
            if (e.@"else") |*data| {
                try self.openKey("else", .block);
                try self.renderNode(data, false);
                try self.closeKey(.block, comma);
            }
            try self.closeKey(.block, comma);
        },
        .literal => |e| {
            const text = self.spanToSrc(e.idx);
            const final = if (e.tag == .string) text[1 .. text.len - 1] else text;
            try self.pushKeyValue(@tagName(expr.*), final, comma);
        },
        .named_arg => |e| {
            try self.openKey("named arg", .block);
            try self.pushKeyValue("name", self.ast.toSource(e.name), true);
            try self.openKey("value", .block);
            try self.renderExpr(e.value, false);
            try self.closeKey(.block, false);
            try self.closeKey(.block, false);
        },
        .@"return" => |e| {
            if (e.expr) |data| {
                try self.openKey(@tagName(expr.*), .block);
                try self.renderExpr(data, false);
                try self.closeKey(.block, comma);
            } else try self.emptyKey("return", .block, comma);
        },
        .struct_literal => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.openKey("structure", .block);
            try self.renderExpr(e.structure, false);
            try self.closeKey(.block, true);

            if (e.fields.len == 0) {
                try self.emptyKey("fields_values", .list, false);
            } else {
                try self.openKey("fields_values", .list);
                for (e.fields, 0..) |fv, i| {
                    const last = i != e.fields.len - 1;
                    try self.openBrace();
                    try self.pushKeyValue("name", self.spanToSrc(fv.name), true);
                    if (fv.value) |v| {
                        try self.openKey("value", .block);
                        try self.renderExpr(v, false);
                        try self.closeKey(.block, false);
                    } else try self.emptyKey("value", .block, false);

                    try self.closeBrace(last);
                }
                try self.closeKey(.list, false);
            }
            try self.closeKey(.block, comma);
        },
        .unary => |e| {
            try self.openKey(@tagName(expr.*), .block);
            try self.pushKeyValue("op", self.spanToSrc(e.op), true);
            try self.openKey("expr", .block);
            try self.renderExpr(e.expr, false);
            try self.closeKey(.block, false);
            try self.closeKey(.block, comma);
        },
    }
}

fn renderBlock(self: *Self, block: *const Ast.Block, comma: bool) !void {
    if (block.nodes.len == 0) {
        try self.emptyKey("block", .list, comma);
    } else {
        try self.openKey("block", .list);
        for (block.nodes, 0..) |*data, i| {
            try self.renderNode(data, i != block.nodes.len - 1);
        }
        try self.closeKey(.list, comma);
    }
}

fn spanToSrc(self: *Self, idx: usize) []const u8 {
    const span = self.ast.token_spans[idx];
    return self.ast.source[span.start..span.end];
}

const KeyTag = enum {
    block,
    list,

    pub fn toOpenStr(self: KeyTag) []const u8 {
        return switch (self) {
            .block => "{",
            .list => "[",
        };
    }

    pub fn toCloseStr(self: KeyTag) []const u8 {
        return switch (self) {
            .block => "}",
            .list => "]",
        };
    }
};

fn openKey(self: *Self, key: []const u8, tag: KeyTag) !void {
    try self.indent();
    try self.writer.print("\"{s}\": {s}\n", .{ key, tag.toOpenStr() });
    self.indent_level += 1;
}

fn closeKey(self: *Self, tag: KeyTag, comma: bool) !void {
    self.indent_level -= 1;
    try self.indent();
    try self.writer.print("{s}", .{tag.toCloseStr()});
    try self.finishPush(comma);
}

fn emptyKey(self: *Self, key: []const u8, tag: KeyTag, comma: bool) !void {
    try self.indent();
    try self.writer.print("\"{s}\": {s}{s}", .{ key, tag.toOpenStr(), tag.toCloseStr() });
    try self.finishPush(comma);
}

fn openBrace(self: *Self) !void {
    try self.indent();
    try self.writer.writeAll("{\n");
    self.indent_level += 1;
}

fn closeBrace(self: *Self, comma: bool) !void {
    self.indent_level -= 1;
    try self.indent();
    try self.writer.print("}}{s}\n", .{if (comma) "," else ""});
}

fn pushKeyValue(self: *Self, key: []const u8, value: []const u8, comma: bool) !void {
    try self.indent();
    try self.writer.print("\"{s}\": \"{s}\"", .{ key, value });
    try self.finishPush(comma);
}

fn finishPush(self: *Self, comma: bool) !void {
    try self.writer.print("{s}\n", .{if (comma) "," else ""});
}

fn indent(self: *Self) !void {
    assert(self.indent_level * 2 < 1024);
    try self.output.appendSlice(self.allocator, spaces[0 .. self.indent_level * INDENT_SIZE]);
}
