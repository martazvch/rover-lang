const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;

const GenReport = @import("../reporter.zig").GenReport;
const Ast = @import("Ast.zig");
const Expr = Ast.Expr;
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

source: []const u8,
errs: ArrayList(ParserReport),
arena: ArenaAllocator,
allocator: Allocator,
token_tags: []const Token.Tag,
token_spans: []const Span,
token_idx: usize,
nodes: ArrayListUnmanaged(Node),
panic_mode: bool,

const Self = @This();
pub const ParserReport = GenReport(ParserMsg);
const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;

pub const empty: Self = .{
    .source = undefined,
    .errs = undefined,
    .arena = undefined,
    .allocator = undefined,
    .token_tags = undefined,
    .token_spans = undefined,
    .token_idx = 0,
    .nodes = .{},
    .panic_mode = false,
};

pub fn init(self: *Self, allocator: Allocator) void {
    self.arena = .init(allocator);
    self.allocator = self.arena.allocator();
    self.errs = .init(self.allocator);
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

/// Parses the token stream
pub fn parse(self: *Self, source: [:0]const u8, tokens: *const MultiArrayList(Token)) !Ast {
    self.source = source;
    self.token_tags = tokens.items(.tag);
    self.token_spans = tokens.items(.span);

    self.skipNewLines();

    while (!self.match(.eof)) {
        const stmt = self.declaration() catch |e| switch (e) {
            // If it's our own error, we continue on parsing
            Error.err => {
                self.synchronize();
                continue;
            },
            else => return e,
        };
        try self.nodes.append(self.allocator, stmt);

        // If EOF, exit
        if (self.match(.eof)) break;

        // After each nodes we expect a new line
        if (!self.check(.new_line)) {
            // Could be that user wrote: Foo{} instead of Foo.{}, it's identifier + block
            // without a new line instead of structure literal
            if (self.token_tags[self.token_idx - 1] == .identifier and self.token_tags[self.token_idx] == .left_brace) {
                std.debug.print("Could be trying to do structure literal? Missing '.'\n", .{});
                // TODO: Error
            }

            const start = self.token_spans[self.token_idx - 1].end;

            self.errAtSpan(
                .{ .start = start, .end = start + 1 },
                .expect_newl_ine,
            ) catch {};

            self.synchronize();
        }

        self.skipNewLines();
    }

    return .{ .source = source, .tokens = tokens, .nodes = try self.nodes.toOwnedSlice(self.allocator) };
}

const TokenField = enum { span, tag };

fn TokenFieldType(tkField: TokenField) type {
    return switch (tkField) {
        .span => Span,
        .tag => Token.Tag,
    };
}

inline fn prev(self: *const Self, comptime tkField: TokenField) TokenFieldType(tkField) {
    return self.getTkField(tkField, self.token_idx - 1);
}

inline fn current(self: *const Self, comptime tkField: TokenField) TokenFieldType(tkField) {
    return self.getTkField(tkField, self.token_idx);
}

inline fn getTkField(self: *const Self, comptime tkField: TokenField, idx: usize) TokenFieldType(tkField) {
    return switch (tkField) {
        .span => self.token_spans[idx],
        .tag => self.token_tags[idx],
    };
}

inline fn advance(self: *Self) void {
    self.token_idx += 1;
}

/// Returns *true* if the current token is of the asked type and
/// advance the `current` field to next one. Otherwise, returns *false*
fn match(self: *Self, kind: Token.Tag) bool {
    if (!self.check(kind)) {
        return false;
    }

    self.advance();
    return true;
}

fn matchAndSkip(self: *Self, kind: Token.Tag) bool {
    const res = self.match(kind);
    self.skipNewLines();
    return res;
}

/// Checks if we currently are at a token
fn check(self: *const Self, kind: Token.Tag) bool {
    return self.token_tags[self.token_idx] == kind;
}

fn skipNewLines(self: *Self) void {
    while (self.check(.new_line)) {
        self.advance();
    }
}

/// Expect a specific type, otherwise it's an error and we enter
/// *panic* mode
fn expect(self: *Self, kind: Token.Tag, error_kind: ParserMsg) !void {
    if (self.match(kind)) {
        return;
    }

    return self.errAtCurrent(error_kind);
}

/// Expect a specific type, otherwise it's an error and we enter
/// *panic* mode. Error will mark the previous token
fn expectOrErrAtPrev(self: *Self, kind: Token.Tag, error_kind: ParserMsg) !void {
    return self.expectOrErrAtToken(kind, error_kind, self.token_idx - 1);
}

/// Expect a specific token tag, otherwise it's an error and we enter
/// *panic* mode. Allow to pass a token to be marked as initial error
/// (usefull for example when unclosed parenthesis, we give the first)
fn expectOrErrAtToken(self: *Self, kind: Token.Tag, error_kind: ParserMsg, tk: TokenIndex) !void {
    if (self.match(kind)) return;

    return self.errAt(tk, error_kind);
}

fn errAtCurrent(self: *Self, error_kind: ParserMsg) Error {
    return self.errAt(self.token_idx, error_kind);
}

fn errAtPrev(self: *Self, error_kind: ParserMsg) Error {
    return self.errAt(self.token_idx - 1, error_kind);
}

/// If error already encountered and in the same statement parsing,
/// we exit, let synchronize and resume. It is likely that if we
/// are already in panic mode, the following errors are just
/// consequencies of actual bad statement
fn errAt(self: *Self, token: TokenIndex, error_kind: ParserMsg) Error {
    if (self.panic_mode) return error.err;

    self.panic_mode = true;

    const report = ParserReport.err(error_kind, self.token_spans[token]);
    try self.errs.append(report);

    return error.err;
}

/// If error already encountered and in the same statement parsing,
/// we exit, let synchronize and resume. It is likely that if we
/// are already in panic mode, the following errors are just
/// consequencies of actual bad statement
fn errAtSpan(self: *Self, span: Span, error_kind: ParserMsg) Error {
    if (self.panic_mode) return error.err;

    self.panic_mode = true;

    const report = ParserReport.err(error_kind, span);
    try self.errs.append(report);

    return error.err;
}

fn synchronize(self: *Self) void {
    self.panic_mode = false;

    while (!self.check(.eof)) {
        switch (self.token_tags[self.token_idx]) {
            .@"fn", .@"for", .@"if", .left_brace, .print, .@"return", .@"struct", .@"var", .@"while" => return,
            else => self.advance(),
        }
    }
}

fn declaration(self: *Self) Error!Node {
    // const idx = try if (self.match(.@"nf"))
    //     self.fnDecl()
    // else if (self.match(.@"var"))
    //     self.varDecl()
    // else if (self.match(.@"struct"))
    //     self.structDecl()
    // else if (self.match(.@"return"))
    //     self.returnExpr()
    // else if (self.match(.Underscore))
    //     self.discard()
    // else if (self.match(.use))
    //     self.use()
    // else
    // self.statement();

    return self.statement();

    // return self.finish_node(idx);
}

fn fnDecl(self: *Self) Error!Node {
    try self.expect(.identifier, .expect_fn_name);

    const idx = try self.add_node(.{
        .tag = .FnDecl,
        .main = self.token_idx - 1,
    });

    try self.expect(.left_paren, .expect_paren_after_fn_name);
    self.skipNewLines();

    var arity: usize = 0;

    while (!self.check(.right_paren)) {
        if (self.check(.eof)) {
            return self.errAtPrev(.expect_paren_after_fn_params);
        }

        if (arity == 255) return self.errAtCurrent(
            .{ .too_many_fn_args = .{ .what = "parameter" } },
        );

        if (self.match(.self)) {
            if (arity > 0) {
                return self.errAtPrev(.self_as_non_first_param);
            }

            if (self.match(.colon)) {
                return self.errAtCurrent(.typed_self);
            }

            _ = try self.add_node(.{ .tag = .self, .main = self.token_idx - 1 });
        } else {
            try self.expect(.identifier, .{ .ExpectName = .{ .kind = "parameter" } });
            _ = try self.add_node(.{ .tag = .Parameter, .main = self.token_idx - 1 });
            try self.expect(.colon, .missing_fn_param_type);
            _ = try self.parseType();
        }

        arity += 1;

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.right_paren, .expect_paren_after_fn_params);

    self.nodes.items(.data)[idx] = arity;

    _ = if (self.match(.small_arrow))
        try self.parseType()
    else if (self.check(.identifier) or self.check(.bool) or self.check(.int_kw) or self.check(.floatKw))
        return self.errAtCurrent(.expect_arrow_before_fn_type)
    else
        try self.add_node(.empty);

    self.skipNewLines();
    try self.expect(.left_brace, .expect_brace_before_fn_body);
    _ = try self.block();

    return idx;
}

fn structDecl(self: *Self) !Node {
    try self.expect(.identifier, .expect_struct_name);
    const name_idx = self.token_idx - 1;
    self.skipNewLines();
    try self.expectOrErrAtPrev(.left_brace, .expect_brace_before_struct_body);
    self.skipNewLines();

    const idx = try self.add_node(.{
        .tag = .StructDecl,
        .main = name_idx,
    });

    // Fields
    const fields = try self.add_node(.{ .tag = .count });
    var fields_count: usize = 0;

    // If at least one field
    while (!self.check(.@"fn") and !self.check(.right_brace) and !self.check(.eof)) {
        var valid = false;
        _ = try self.expect(.identifier, .expect_field_name);
        _ = try self.add_node(.{ .tag = .field, .main = self.token_idx - 1 });

        if (self.match(.colon)) {
            _ = try self.parseType();
            valid = true;
        } else _ = try self.add_node(.empty);

        if (self.match(.equal)) {
            valid = true;
            _ = try self.parsePrecedenceExpr(0);
        } else _ = try self.add_node(.empty);

        if (!valid) {
            return self.errAtCurrent(.expect_field_type_or_default);
        }

        fields_count += 1;

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    // If we are at an identifier, might be a missing comma between fields
    if (self.check(.identifier)) {
        return self.errAtPrev(.missing_comma_after_field);
    }

    self.nodes.items(.data)[fields] = fields_count;

    // Functions
    const func = try self.add_node(.{ .tag = .count });
    var fn_count: usize = 0;

    while (!self.check(.right_brace) and !self.check(.eof)) {
        _ = try self.expect(.@"fn", .expect_fn_in_struct_body);
        _ = try self.fnDecl();
        self.skipNewLines();
        fn_count += 1;
    }
    self.nodes.items(.data)[func] = fn_count;

    try self.expectOrErrAtPrev(.right_brace, .expect_brace_after_struct_body);

    return idx;
}

fn varDecl(self: *Self) !Node {
    try self.expect(.identifier, .{ .ExpectName = .{ .kind = "variable" } });

    if (self.check(.comma))
        return self.multiVarDecl();

    const idx = try self.add_node(.{
        .tag = .VarDecl,
        .main = self.token_idx - 1,
    });

    _ = try self.expectTypeOrEmpty();

    _ = if (self.match(.equal))
        try self.parsePrecedenceExpr(0)
    else
        try self.add_node(.empty);

    return idx;
}

fn multiVarDecl(self: *Self) !Node {
    const idx = try self.add_node(.{ .tag = .MultiVarDecl });

    _ = try self.add_node(.{ .tag = .VarDecl, .main = self.token_idx - 1 });

    var count: usize = 1;
    var variables = std.ArrayListUnmanaged(usize){};
    defer variables.deinit(self.allocator);

    while (self.match(.comma)) : (count += 1) {
        try self.expect(.identifier, .{ .ExpectName = .{ .kind = "variable" } });
        try variables.append(self.allocator, self.token_idx - 1);
    }
    self.nodes.items(.main)[idx] = count;

    const type_idx = try self.expectTypeOrEmpty();

    const first_value = if (self.match(.equal))
        try self.parsePrecedenceExpr(0)
    else
        try self.add_node(.empty);

    count = 1;

    // If only one value
    if (!self.check(.comma)) {
        for (variables.items) |var_idx| {
            _ = try self.add_node(.{ .tag = .VarDecl, .main = var_idx });
            _ = try self.add_node(self.nodes.get(type_idx));
            _ = try self.add_node(self.nodes.get(first_value));
        }
    } else while (self.match(.comma)) : (count += 1) {
        if (count > variables.items.len) {
            count -= 1;
            break;
        }

        _ = try self.add_node(.{ .tag = .VarDecl, .main = variables.items[count - 1] });
        _ = try self.add_node(self.nodes.get(type_idx));
        _ = try self.parsePrecedenceExpr(0);
    }

    if (count > 1 and count != variables.items.len + 1)
        return self.errAt(variables.items[count - 1], .{ .WrongValueCountVarDecl = .{
            .expect = variables.items.len + 1,
        } });

    self.nodes.items(.data)[idx] = count;

    return idx;
}

/// Expects a type after ':'. If no colon, declares an empty type
fn expectTypeOrEmpty(self: *Self) Error!Node {
    return if (self.match(.colon))
        try self.parseType()
    else if (self.check(.identifier))
        self.errAtCurrent(.expect_colon_before_type)
    else
        try self.add_node(.empty);
}

/// Parses a type. It assumes you know that a type is expected at this place
fn parseType(self: *Self) Error!Node {
    if (self.isIdentOrType()) {
        return self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
    } else if (self.match(.@"fn")) {
        const idx = try self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
        try self.expect(.left_paren, .expect_paren_after_fn_name);

        var arity: usize = 0;
        while (!self.check(.right_paren) and !self.check(.eof)) {
            // Parse parameters type
            _ = try self.parseType();
            arity += 1;

            if (!self.match(.comma)) break;
        }

        self.nodes.items(.data)[idx] = arity;
        try self.expect(.right_paren, .expect_paren_after_fn_params);

        _ = if (self.match(.small_arrow))
            try self.parseType()
        else if (self.isIdentOrType())
            return self.errAtCurrent(.expect_arrow_before_fn_type)
        else
            try self.add_node(.empty);

        return idx;
    } else {
        return self.errAtCurrent(.expect_type_name);
    }
}

fn isIdentOrType(self: *Self) bool {
    return if (self.match(.identifier) or
        self.match(.floatKw) or
        self.match(.int_kw) or
        self.match(.str_kw) or
        self.match(.bool))
        true
    else
        false;
}

fn discard(self: *Self) !Node {
    try self.expect(.equal, .invalid_discard);
    const idx = try self.add_node(.{
        .tag = .Discard,
        .main = self.token_idx - 1,
    });

    _ = try self.parsePrecedenceExpr(0);

    return idx;
}

fn use(self: *Self) Error!Node {
    const idx = try self.add_node(.{
        .tag = .use,
        .main = self.token_idx - 1,
    });

    if (!self.check(.identifier)) {
        return self.errAtCurrent(.{ .ExpectName = .{ .kind = "module" } });
    }

    var count: usize = 0;
    while (self.match(.identifier) and !self.check(.eof)) {
        _ = try self.add_node(.{
            .tag = .identifier,
            .main = self.token_idx - 1,
        });

        count += 1;

        if (self.match(.dot)) continue;
        break;
    }

    self.nodes.items(.data)[idx] = count;

    return idx;
}

fn statement(self: *Self) Error!Node {
    if (self.match(.print)) {
        return self.print();
    } else if (self.match(.@"while")) {
        unreachable;
        //     return self.whileStmt();
    } else {
        const assigne = try self.parsePrecedenceExpr(0);

        return if (self.match(.equal))
            self.assignment(assigne)
        else
            .{ .expr = assigne };
    }
}

fn assignment(self: *Self, assigne: *Expr) !Node {
    return .{ .assignment = .{
        .assigne = assigne,
        .value = try self.parsePrecedenceExpr(0),
    } };
}

fn print(self: *Self) Error!Node {
    return .{ .print = try self.parsePrecedenceExpr(0) };
}

fn whileStmt(self: *Self) !Node {
    const idx = try self.add_node(.{
        .tag = .@"while",
        .main = self.token_idx - 1,
    });
    _ = try self.parsePrecedenceExpr(0);

    _ = if (self.matchAndSkip(.left_brace))
        try self.block()
    else if (self.matchAndSkip(.do))
        try self.declaration()
    else
        return self.errAtCurrent(.{ .ExpectBraceOrDo = .{ .what = "while" } });

    return idx;
}

const Assoc = enum { left, none };
const Rule = struct { prec: i8, assoc: Assoc = .left };

const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = -1 }, 0, .{
    .@"and" = .{ .prec = 20 },
    .@"or" = .{ .prec = 20 },

    .equal_equal = .{ .prec = 30, .assoc = .none },
    .bang_equal = .{ .prec = 30, .assoc = .none },

    .greater = .{ .prec = 40, .assoc = .none },
    .greater_equal = .{ .prec = 40, .assoc = .none },
    .less = .{ .prec = 40, .assoc = .none },
    .less_equal = .{ .prec = 40, .assoc = .none },

    .minus = .{ .prec = 60 },
    .plus = .{ .prec = 60 },

    .slash = .{ .prec = 70 },
    .star = .{ .prec = 70 },
});

fn parsePrecedenceExpr(self: *Self, prec_min: i8) Error!*Expr {
    self.advance();
    var node = try self.parseExpr();

    var banned_prec: i8 = -1;

    while (true) {
        // We check the current before consuming it
        const next_rule = rules[@as(usize, @intFromEnum(self.token_tags[self.token_idx]))];

        if (next_rule.prec < prec_min) break;

        if (next_rule.prec == banned_prec) {
            return self.errAtCurrent(.chaining_cmp_op);
        }

        // Here, we can safely use it
        self.advance();
        const op = self.token_idx - 1;
        const rhs = try self.parsePrecedenceExpr(next_rule.prec + 1);

        const expr = try self.allocator.create(Expr);
        expr.* = .{ .binop = .{
            .lhs = node,
            .rhs = rhs,
            .op = op,
        } };

        node = expr;

        if (next_rule.assoc == .none) banned_prec = next_rule.prec;
    }

    return node;
}

/// Parses expressions (prefix + sufix)
fn parseExpr(self: *Self) Error!*Expr {
    // Prefix part
    const expr = try switch (self.prev(.tag)) {
        .left_brace => self.block(),
        .@"if" => self.ifExpr(),
        .minus, .not => self.unary(),
        .false => self.literal(.bool),
        .float => self.literal(.float),
        .identifier => self.literal(.identifier),
        .int => self.literal(.int),
        .left_paren => self.grouping(),
        .null => self.literal(.null),
        .@"return" => self.returnExpr(),
        .string => self.literal(.string),
        .true => self.literal(.bool),
        else => {
            const span = self.token_spans[self.token_idx - 1];
            return self.errAtPrev(.{ .ExpectExpr = .{ .found = span.text(self.source) } });
        },
    };

    // Apply postfix
    return self.postfix(expr);
}

fn block(self: *Self) Error!*Expr {
    const openning_brace = self.token_idx - 1;
    const expr = try self.allocator.create(Expr);
    var exprs: ArrayListUnmanaged(Node) = .{};

    self.skipNewLines();

    while (!self.check(.right_brace) and !self.check(.eof)) {
        try exprs.append(self.allocator, try self.declaration());
        self.skipNewLines();
    }

    try self.expectOrErrAtToken(.right_brace, .unclosed_brace, openning_brace);
    expr.* = .{ .block = .{
        .exprs = try exprs.toOwnedSlice(self.allocator),
        .span = .{ .start = openning_brace, .end = self.prev(.span).start },
    } };

    return expr;
}

fn ifExpr(self: *Self) Error!*Expr {
    const condition = try self.parsePrecedenceExpr(0);
    self.skipNewLines();

    // TODO: Warning for unnecessary 'do' if there is a block after
    const then: Node = if (self.matchAndSkip(.left_brace))
        .{ .expr = try self.block() }
    else if (self.matchAndSkip(.do))
        try self.declaration()
    else
        return self.errAtPrev(.{ .ExpectBraceOrDo = .{ .what = "if" } });

    self.skipNewLines();

    // If we dosen't match an else, we go back one token to be able
    // to match the rule "after each statement there is a new line"
    // tested in the main caller
    const else_body: ?Node = if (self.matchAndSkip(.@"else"))
        if (self.matchAndSkip(.left_brace))
            .{ .expr = try self.block() }
        else
            try self.declaration()
    else
        null;

    const expr = try self.allocator.create(Expr);
    expr.* = .{ .@"if" = .{
        .condition = condition,
        .then = then,
        .@"else" = else_body,
    } };

    return expr;
}

fn grouping(self: *Self) Error!*Expr {
    const start = self.token_idx - 1;
    const opening = self.prev(.span).start;
    self.skipNewLines();

    const expr = try self.allocator.create(Expr);
    const value = if (self.check(.right_paren))
        null
    else
        try self.parsePrecedenceExpr(0);

    try self.expectOrErrAtToken(.right_paren, .unclosed_paren, start);

    expr.* = .{ .grouping = .{
        .expr = value,
        .span = .{
            .start = opening,
            .end = self.current(.span).start,
        },
    } };

    return expr;
}

fn literal(self: *Self, tag: Ast.Literal.Tag) Error!*Expr {
    const expr = try self.allocator.create(Expr);
    expr.* = .{ .literal = .{ .tag = tag, .idx = self.token_idx - 1 } };

    return expr;
}

fn returnExpr(self: *Self) Error!*Expr {
    const expr = try self.allocator.create(Expr);
    expr.* = .{ .@"return" = .{
        .expr = if (self.check(.new_line) or self.check(.right_brace))
            null
        else
            try self.parsePrecedenceExpr(0),
    } };

    return expr;
}

fn unary(self: *Self) Error!*Expr {
    const expr = try self.allocator.create(Expr);
    self.advance();
    expr.* = .{ .unary = .{ .op = self.token_idx - 2, .expr = try self.parseExpr() } };

    return expr;
}

// Parses postfix expressions: calls, member access
fn postfix(self: *Self, prefixExpr: *Expr) Error!*Expr {
    var expr = prefixExpr;

    while (true) {
        if (self.match(.left_paren)) {
            expr = try self.finishCall(expr);
        } else if (self.match(.dot)) {
            if (self.match(.left_brace)) {
                expr = try self.structLiteral(expr);
            } else {
                expr = try self.field(expr);
            }
        } else break;
    }

    return expr;
}

// Takes the callee expression as input and output the full function call expression
fn finishCall(self: *Self, expr: *Expr) Error!*Expr {
    const call_expr = try self.allocator.create(Expr);

    // TODO: init with capacity of 255?
    var args: std.ArrayListUnmanaged(*Expr) = .{};

    // All the skip_lines cover the different syntaxes
    while (!self.check(.right_paren)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.expect_paren_after_fn_args);

        if (args.items.len == 255)
            return self.errAtCurrent(.{ .too_many_fn_args = .{ .what = "argument" } });

        try args.append(self.allocator, try self.parsePrecedenceExpr(0));

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.right_paren, .expect_paren_after_fn_args);

    call_expr.* = .{ .fn_call = .{
        .callee = expr,
        .args = try args.toOwnedSlice(self.allocator),
    } };

    return call_expr;
}

fn field(self: *Self, expr: *Expr) Error!*Expr {
    try self.expect(.identifier, .expect_name_after_dot);

    const field_access = try self.allocator.create(Expr);
    field_access.* = .{ .field = .{
        .structure = expr,
        .field = self.token_idx - 1,
    } };

    return field_access;
}

// TODO: check if need to destroy the expr
fn structLiteral(self: *Self, expr: *Expr) Error!*Expr {
    const struct_lit = try self.allocator.create(Expr);
    var fields_values: ArrayListUnmanaged(Ast.FieldAndValue) = .{};

    if (expr.* != .literal and expr.literal.tag != .identifier) {
        // TODO: Error
        @panic("ERROR: Structure literal must be name");
    }

    // All the skip_lines cover the different syntaxes
    while (!self.check(.right_brace)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.expect_brace_after_struct_lit);

        if (!self.match(.identifier)) {
            return self.errAtCurrent(.struct_lit_non_ident_field);
        }

        // Either: { x = 3 }  or { x }
        try fields_values.append(self.allocator, .{
            .name = self.token_idx - 1,
            .value = if (self.match(.equal)) try self.parsePrecedenceExpr(0) else null,
        });

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.right_brace, .expect_brace_after_struct_lit);
    struct_lit.* = .{ .struct_literal = .{
        .name = expr.literal.idx,
        .fields = try fields_values.toOwnedSlice(self.allocator),
    } };

    return struct_lit;
}
