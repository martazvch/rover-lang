const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const Span = @import("lexer.zig").Span;

allocator: Allocator = undefined,
source: [:0]const u8,
output: std.ArrayListUnmanaged(u8) = .{},
writer: std.ArrayListUnmanaged(u8).Writer = undefined,
indentLevel: usize = 1,
tokenSpans: []const Span,

const AstRenderer = @This();
const spaces: []const u8 = " " ** 1024;
const INDENT_SIZE = 4;

pub fn init(allocator: Allocator, source: [:0]const u8, spans: []const Span) AstRenderer {
    return .{ .allocator = allocator, .source = source, .output = .{}, .tokenSpans = spans };
}

pub fn render(self: *AstRenderer, ast: *const Ast) !void {
    self.writer = self.output.writer(self.allocator);
    try self.writer.writeAll("{\n");

    for (ast.nodes) |*node| {
        try self.renderNode(node);
    }

    self.indentLevel -= 1;
    try self.writer.writeAll("}\n");
}

fn renderNode(self: *AstRenderer, node: *const Ast.Node) !void {
    switch (node.*) {
        .assignment => |n| {
            _ = n; // autofix
        },
        .expr => |n| try self.renderExpr(n),
    }
}

fn renderExpr(self: *AstRenderer, expr: *const Ast.Expr) !void {
    switch (expr.*) {
        .fnCall => |e| {
            _ = e; // autofix
        },
        .grouping => |e| {
            _ = e; // autofix
        },
        .literal => |e| {
            try self.pushKeyValueNc("literal", self.spanToSrc(e.idx));
        },
        .unary => |e| {
            try self.openKey("unary", .block);
            try self.pushKeyValue("op", self.spanToSrc(e.op));
            try self.renderExpr(e.expr);
            try self.closeKey(.block);
        },
    }
}

fn spanToSrc(self: *AstRenderer, tkIdx: usize) []const u8 {
    const span = self.tokenSpans[tkIdx];
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

fn openKey(self: *AstRenderer, key: []const u8, tag: KeyTag) !void {
    try self.indent();
    try self.writer.print("\"{s}\": {s}\n", .{ key, tag.toOpenStr() });
    self.indentLevel += 1;
}

fn closeKey(self: *AstRenderer, tag: KeyTag) !void {
    self.indentLevel -= 1;
    try self.indent();
    try self.writer.print("{s}\n", .{tag.toCloseStr()});
}

fn pushKeyValue(self: *AstRenderer, key: []const u8, value: []const u8) !void {
    try self.pushKeyValueFmt(key, "{s}", .{value}, true);
}

fn pushKeyValueNc(self: *AstRenderer, key: []const u8, value: []const u8) !void {
    try self.pushKeyValueFmt(key, "{s}", .{value}, false);
}

fn pushKeyValueFmt(self: *AstRenderer, key: []const u8, comptime valFmt: []const u8, valArgs: anytype, comma: bool) !void {
    try self.indent();
    try self.writer.print("\"{s}\": \"", .{key});
    try self.writer.print(valFmt, valArgs);
    try self.writer.print("\"{s}\n", .{if (comma) "," else ""});
}

fn pushDict(self: *AstRenderer, format: []const u8, args: anytype) !void {
    try self.indent();
    try self.writer.print(format, args);
}

// Old
fn pushTextNl(self: *AstRenderer, text: []const u8) !void {
    try self.pushText(text);
    try self.pushRawText("\n");
}

fn pushText(self: *AstRenderer, text: []const u8) !void {
    try self.indent();
    try self.pushRawText(text);
}

fn pushRawText(self: *AstRenderer, text: []const u8) !void {
    try self.output.appendSlice(self.allocator, text);
}

fn indent(self: *AstRenderer) !void {
    assert(self.indentLevel * 2 < 1024);
    try self.output.appendSlice(self.allocator, spaces[0 .. self.indentLevel * INDENT_SIZE]);
}
