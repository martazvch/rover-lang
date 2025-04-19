const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");
const Span = @import("Lexer.zig").Span;

allocator: Allocator = undefined,
source: [:0]const u8,
output: std.ArrayListUnmanaged(u8) = .{},
writer: std.ArrayListUnmanaged(u8).Writer = undefined,
indent_level: usize = 1,
token_spans: []const Span,

const Self = @This();
const Error = std.ArrayListUnmanaged(u8).Writer.Error;
const spaces: []const u8 = " " ** 1024;
const INDENT_SIZE = 4;

pub fn init(allocator: Allocator, source: [:0]const u8, spans: []const Span) Self {
    return .{ .allocator = allocator, .source = source, .output = .{}, .token_spans = spans };
}

pub fn render(self: *Self, ast: *const Ast) !void {
    self.writer = self.output.writer(self.allocator);
    try self.writer.writeAll("{\n");

    for (ast.nodes, 0..) |*node, i| {
        try self.renderNode(node, i != ast.nodes.len - 1);
    }

    self.indent_level -= 1;
    try self.writer.writeAll("}\n");
}

fn renderNode(self: *Self, node: *const Ast.Node, comma: bool) Error!void {
    switch (node.*) {
        .assignment => |n| {
            try self.openKey("asignment", .block);
            try self.openKey("assignee", .block);
            try self.renderExpr(n.assigne, false);
            try self.closeKey(.block, true);
            try self.openKey("value", .block);
            try self.renderExpr(n.value, false);
            try self.closeKey(.block, false);
            try self.closeKey(.block, comma);
        },
        .print => |n| {
            try self.openKey("print", .block);
            try self.renderExpr(n, false);
            try self.closeKey(.block, comma);
        },
        .expr => |n| try self.renderExpr(n, comma),
    }
}

fn renderExpr(self: *Self, expr: *const Ast.Expr, comma: bool) Error!void {
    switch (expr.*) {
        .block => |e| {
            try self.openKey("block", .list);
            for (e.exprs, 0..) |*data, i| {
                try self.renderNode(data, i != e.exprs.len - 1);
            }
            try self.closeKey(.list, comma);
        },
        .binop => |e| {
            try self.openKey("binop", .block);
            try self.openKey("lhs", .block);
            try self.renderExpr(e.lhs, false);
            try self.closeKey(.block, true);
            try self.openKey("lhs", .block);
            try self.renderExpr(e.rhs, false);
            try self.closeKey(.block, true);
            try self.pushKeyValue("op", self.spanToSrc(e.op), false);
            try self.closeKey(.block, comma);
        },
        .field => |e| {
            try self.openKey("field", .block);
            try self.openKey("structre", .block);
            try self.renderExpr(e.structure, false);
            try self.closeKey(.block, true);
            try self.pushKeyValue("field", self.spanToSrc(e.field), false);
            try self.closeKey(.block, comma);
        },
        .fn_call => |e| {
            try self.openKey("call", .block);
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
            try self.openKey("grouping", .block);
            if (e.expr) |data| try self.renderExpr(data, false);
            try self.closeKey(.block, comma);
        },
        .@"if" => |e| {
            try self.openKey("if", .block);
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
            try self.pushKeyValue("literal", self.spanToSrc(e.idx), comma);
        },
        .@"return" => |e| {
            try self.openKey("return", .block);
            if (e.expr) |data| try self.renderExpr(data, false);
            try self.closeKey(.block, comma);
        },
        .struct_literal => |e| {
            try self.openKey("struct_literal", .block);
            try self.pushKeyValue("name", self.spanToSrc(e.name), true);

            if (e.fields.len == 0) {
                try self.emptyKey("fields_values", .list, false);
            } else {
                try self.openKey("fields_values", .list);
                for (e.fields, 0..) |fv, i| {
                    try self.pushKeyValue("name", self.spanToSrc(fv.name), true);
                    if (fv.value) |v| {
                        try self.renderExpr(v, i != e.fields.len - 1);
                    }
                }
                try self.closeKey(.list, false);
            }
            try self.closeKey(.block, comma);
        },
        .unary => |e| {
            try self.openKey("unary", .block);
            try self.pushKeyValue("op", self.spanToSrc(e.op), true);
            try self.renderExpr(e.expr, false);
            try self.closeKey(.block, comma);
        },
    }
}

fn spanToSrc(self: *Self, idx: usize) []const u8 {
    const span = self.token_spans[idx];
    return self.source[span.start..span.end];
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

fn pushKeyValue(self: *Self, key: []const u8, value: []const u8, comma: bool) !void {
    try self.indent();
    try self.writer.print("\"{s}\": \"{s}\"", .{ key, value });
    try self.finishPush(comma);
}

fn pushDict(self: *Self, format: []const u8, args: anytype) !void {
    try self.indent();
    try self.writer.print(format, args);
}

fn finishPush(self: *Self, comma: bool) !void {
    try self.writer.print("{s}\n", .{if (comma) "," else ""});
}

fn indent(self: *Self) !void {
    assert(self.indent_level * 2 < 1024);
    try self.output.appendSlice(self.allocator, spaces[0 .. self.indent_level * INDENT_SIZE]);
}
