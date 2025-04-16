const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;

const GenReport = @import("../reporter.zig").GenReport;
const Ast = @import("Ast.zig");
const Expr = Ast.Expr;
const Node2 = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const Span = @import("lexer.zig").Span;
const Token = @import("lexer.zig").Token;

source: []const u8,
errs: ArrayList(ParserReport),
arena: ArenaAllocator,
allocator: Allocator,
token_tags: []const Token.Tag,
token_spans: []const Span,
token_idx: usize,
nodes: ArrayListUnmanaged(Node2),
panic_mode: bool,

const Parser = @This();
pub const ParserReport = GenReport(ParserMsg);
const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;

pub const empty: Parser = .{
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

pub fn init(self: *Parser, allocator: Allocator) void {
    self.arena = .init(allocator);
    self.allocator = self.arena.allocator();
    self.errs = .init(self.allocator);
}

pub fn deinit(self: *Parser) void {
    self.arena.deinit();
}

/// Parses the token stream
pub fn parse(self: *Parser, source: [:0]const u8, tokens: *const MultiArrayList(Token)) !Ast {
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

        // If EOF, exit
        if (self.match(.eof)) break;

        // After each nodes we expect a new line
        if (!self.check(.newLine)) {
            // Could be that user wrote: Foo{} instead of Foo.{}, it's identifier + block
            // without a new line instead of structure literal
            if (self.token_tags[self.token_idx - 1] == .identifier and self.token_tags[self.token_idx] == .leftBrace) {
                std.debug.print("Could be trying to do structure literal? Missing '.'\n", .{});
                // TODO: Error
            }

            const start = self.token_spans[self.token_idx - 1].end;

            self.errAtSpan(
                .{ .start = start, .end = start + 1 },
                .ExpectNewLine,
            ) catch {};

            self.synchronize();
        }

        try self.nodes.append(self.allocator, stmt);

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

inline fn prev(self: *const Parser, comptime tkField: TokenField) TokenFieldType(tkField) {
    return self.getTkField(tkField, self.token_idx - 1);
}

inline fn current(self: *const Parser, comptime tkField: TokenField) TokenFieldType(tkField) {
    return self.getTkField(tkField, self.token_idx);
}

inline fn getTkField(self: *const Parser, comptime tkField: TokenField, idx: usize) TokenFieldType(tkField) {
    return switch (tkField) {
        .span => self.token_spans[idx],
        .tag => self.token_tags[idx],
    };
}

inline fn advance(self: *Parser) void {
    self.token_idx += 1;
}

/// Returns *true* if the current token is of the asked type and
/// advance the `current` field to next one. Otherwise, returns *false*
fn match(self: *Parser, kind: Token.Tag) bool {
    if (!self.check(kind)) {
        return false;
    }

    self.advance();
    return true;
}

fn matchAndSkip(self: *Parser, kind: Token.Tag) bool {
    const res = self.match(kind);
    self.skipNewLines();
    return res;
}

/// Checks if we currently are at a token
fn check(self: *const Parser, kind: Token.Tag) bool {
    return self.token_tags[self.token_idx] == kind;
}

fn skipNewLines(self: *Parser) void {
    while (self.check(.newLine)) {
        self.advance();
    }
}

/// Expect a specific type, otherwise it's an error and we enter
/// *panic* mode
fn expect(self: *Parser, kind: Token.Tag, error_kind: ParserMsg) !void {
    if (self.match(kind)) {
        return;
    }

    return self.errAtCurrent(error_kind);
}

/// Expect a specific type, otherwise it's an error and we enter
/// *panic* mode. Error will mark the previous token
fn expectOrErrAtPrev(self: *Parser, kind: Token.Tag, error_kind: ParserMsg) !void {
    return self.expectOrErrAtToken(kind, error_kind, self.token_idx - 1);
}

/// Expect a specific token tag, otherwise it's an error and we enter
/// *panic* mode. Allow to pass a token to be marked as initial error
/// (usefull for example when unclosed parenthesis, we give the first)
fn expectOrErrAtToken(self: *Parser, kind: Token.Tag, error_kind: ParserMsg, tk: TokenIndex) !void {
    if (self.match(kind)) return;

    return self.errAt(tk, error_kind);
}

fn errAtCurrent(self: *Parser, error_kind: ParserMsg) Error {
    return self.errAt(self.token_idx, error_kind);
}

fn errAtPrev(self: *Parser, error_kind: ParserMsg) Error {
    return self.errAt(self.token_idx - 1, error_kind);
}

/// If error already encountered and in the same statement parsing,
/// we exit, let synchronize and resume. It is likely that if we
/// are already in panic mode, the following errors are just
/// consequencies of actual bad statement
fn errAt(self: *Parser, token: TokenIndex, error_kind: ParserMsg) Error {
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
fn errAtSpan(self: *Parser, span: Span, error_kind: ParserMsg) Error {
    if (self.panic_mode) return error.err;

    self.panic_mode = true;

    const report = ParserReport.err(error_kind, span);
    try self.errs.append(report);

    return error.err;
}

fn synchronize(self: *Parser) void {
    self.panic_mode = false;

    while (!self.check(.eof)) {
        switch (self.token_tags[self.token_idx]) {
            .@"fn", .@"for", .@"if", .leftBrace, .print, .@"return", .@"struct", .@"var", .@"while" => return,
            else => self.advance(),
        }
    }
}

fn declaration(self: *Parser) Error!Node2 {
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

fn fnDecl(self: *Parser) Error!Node2 {
    try self.expect(.identifier, .ExpectFnName);

    const idx = try self.add_node(.{
        .tag = .FnDecl,
        .main = self.token_idx - 1,
    });

    try self.expect(.leftParen, .ExpectParenAfterFnName);
    self.skipNewLines();

    var arity: usize = 0;

    while (!self.check(.rightParen)) {
        if (self.check(.eof)) {
            return self.errAtPrev(.ExpectParenAfterFnParams);
        }

        if (arity == 255) return self.errAtCurrent(
            .{ .TooManyFnArgs = .{ .what = "parameter" } },
        );

        if (self.match(.self)) {
            if (arity > 0) {
                return self.errAtPrev(.SelfAsNonFirstParam);
            }

            if (self.match(.colon)) {
                return self.errAtCurrent(.TypedSelf);
            }

            _ = try self.add_node(.{ .tag = .self, .main = self.token_idx - 1 });
        } else {
            try self.expect(.identifier, .{ .ExpectName = .{ .kind = "parameter" } });
            _ = try self.add_node(.{ .tag = .Parameter, .main = self.token_idx - 1 });
            try self.expect(.colon, .MissingFnParamType);
            _ = try self.parseType();
        }

        arity += 1;

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.rightParen, .ExpectParenAfterFnParams);

    self.nodes.items(.data)[idx] = arity;

    _ = if (self.match(.smallArrow))
        try self.parseType()
    else if (self.check(.identifier) or self.check(.bool) or self.check(.intKw) or self.check(.floatKw))
        return self.errAtCurrent(.ExpectArrowBeforeFnType)
    else
        try self.add_node(.empty);

    self.skipNewLines();
    try self.expect(.leftBrace, .ExpectBraceBeforeFnBody);
    _ = try self.block();

    return idx;
}

fn returnExpr(self: *Parser) Error!Node2 {
    const tk = self.token_idx - 1;
    const idx = try self.add_node(.{ .tag = .@"return", .main = tk });

    _ = if (self.check(.newLine) or self.check(.rightBrace))
        try self.add_node(.empty)
    else
        try self.parsePrecedenceExpr(0);

    return idx;
}

fn structDecl(self: *Parser) !Node2 {
    try self.expect(.identifier, .ExpectStructName);
    const name_idx = self.token_idx - 1;
    self.skipNewLines();
    try self.expectOrErrAtPrev(.leftBrace, .ExpectBraceBeforeStructBody);
    self.skipNewLines();

    const idx = try self.add_node(.{
        .tag = .StructDecl,
        .main = name_idx,
    });

    // Fields
    const fields = try self.add_node(.{ .tag = .count });
    var fields_count: usize = 0;

    // If at least one field
    while (!self.check(.@"fn") and !self.check(.rightBrace) and !self.check(.eof)) {
        var valid = false;
        _ = try self.expect(.identifier, .ExpectFieldName);
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
            return self.errAtCurrent(.ExpectFieldTypeOrDefault);
        }

        fields_count += 1;

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    // If we are at an identifier, might be a missing comma between fields
    if (self.check(.identifier)) {
        return self.errAtPrev(.MissingCommaAfterField);
    }

    self.nodes.items(.data)[fields] = fields_count;

    // Functions
    const func = try self.add_node(.{ .tag = .count });
    var fn_count: usize = 0;

    while (!self.check(.rightBrace) and !self.check(.eof)) {
        _ = try self.expect(.@"fn", .ExpectFnInStructBody);
        _ = try self.fnDecl();
        self.skipNewLines();
        fn_count += 1;
    }
    self.nodes.items(.data)[func] = fn_count;

    try self.expectOrErrAtPrev(.rightBrace, .ExpectBraceAfterStructBody);

    return idx;
}

fn varDecl(self: *Parser) !Node2 {
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

fn multiVarDecl(self: *Parser) !Node2 {
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
fn expectTypeOrEmpty(self: *Parser) Error!Node2 {
    return if (self.match(.colon))
        try self.parseType()
    else if (self.check(.identifier))
        self.errAtCurrent(.ExpectcolonBeforeType)
    else
        try self.add_node(.empty);
}

/// Parses a type. It assumes you know that a type is expected at this place
fn parseType(self: *Parser) Error!Node2 {
    if (self.isIdentOrType()) {
        return self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
    } else if (self.match(.@"fn")) {
        const idx = try self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
        try self.expect(.leftParen, .ExpectParenAfterFnName);

        var arity: usize = 0;
        while (!self.check(.rightParen) and !self.check(.eof)) {
            // Parse parameters type
            _ = try self.parseType();
            arity += 1;

            if (!self.match(.comma)) break;
        }

        self.nodes.items(.data)[idx] = arity;
        try self.expect(.rightParen, .ExpectParenAfterFnParams);

        _ = if (self.match(.smallArrow))
            try self.parseType()
        else if (self.isIdentOrType())
            return self.errAtCurrent(.ExpectArrowBeforeFnType)
        else
            try self.add_node(.empty);

        return idx;
    } else {
        return self.errAtCurrent(.ExpectTypeName);
    }
}

fn isIdentOrType(self: *Parser) bool {
    return if (self.match(.identifier) or
        self.match(.floatKw) or
        self.match(.intKw) or
        self.match(.strKw) or
        self.match(.bool))
        true
    else
        false;
}

fn discard(self: *Parser) !Node2 {
    try self.expect(.equal, .InvalidDiscard);
    const idx = try self.add_node(.{
        .tag = .Discard,
        .main = self.token_idx - 1,
    });

    _ = try self.parsePrecedenceExpr(0);

    return idx;
}

fn use(self: *Parser) Error!Node2 {
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

fn statement(self: *Parser) !Node2 {
    // if (self.match(.print)) {
    //     return self.print();
    // } else if (self.match(.@"while")) {
    //     return self.whileStmt();
    // } else {
    const assigne = try self.parsePrecedenceExpr(0);

    return if (self.match(.equal))
        self.assignment(assigne)
    else
        .{ .expr = assigne };

    //     if (self.match(.equal)) {
    //         return self.assignment(assigne);
    //     } else return assigne;
    // }
}

fn assignment(self: *Parser, assigne: *Expr) !Node2 {
    return .{ .assignment = .{
        .assigne = assigne,
        .value = try self.parsePrecedenceExpr(0),
    } };
}

fn print(self: *Parser) Error!Node2 {
    const idx = try self.add_node(.{
        .tag = .print,
        .main = self.token_idx - 1,
    });

    _ = try self.parsePrecedenceExpr(0);
    self.nodes.items(.data)[idx] = self.nodes.len;

    return idx;
}

fn whileStmt(self: *Parser) !Node2 {
    const idx = try self.add_node(.{
        .tag = .@"while",
        .main = self.token_idx - 1,
    });
    _ = try self.parsePrecedenceExpr(0);

    _ = if (self.matchAndSkip(.leftBrace))
        try self.block()
    else if (self.matchAndSkip(.do))
        try self.declaration()
    else
        return self.errAtCurrent(.{ .ExpectBraceOrDo = .{ .what = "while" } });

    return idx;
}

const Assoc = enum { Left, none };

const Rule = struct { prec: i8, assoc: Assoc = .Left };

const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = -1 }, 0, .{
    // .@"and" = .{ .prec = 20, .tag = .@"and" },
    // .@"or" = .{ .prec = 20, .tag = .@"or" },
    //
    // .EqualEqual = .{ .prec = 30, .assoc = .none, .tag = .Eq },
    // .bangEqual = .{ .prec = 30, .assoc = .none, .tag = .Ne },
    //
    // .Greater = .{ .prec = 40, .assoc = .none, .tag = .Gt },
    // .GreaterEqual = .{ .prec = 40, .assoc = .none, .tag = .Ge },
    // .less = .{ .prec = 40, .assoc = .none, .tag = .Lt },
    // .lessEqual = .{ .prec = 40, .assoc = .none, .tag = .Le },
    //
    // .minus = .{ .prec = 60, .tag = .Sub },
    // .plus = .{ .prec = 60, .tag = .Add },
    //
    // .slash = .{ .prec = 70, .tag = .Div },
    // .star = .{ .prec = 70, .tag = .Mul },
});

fn parsePrecedenceExpr(self: *Parser, prec_min: i8) Error!*Expr {
    self.advance();
    const expr = try self.parseExpr();

    var banned_prec: i8 = -1;

    while (true) {
        // We check the current before consuming it
        const next_rule = rules[@as(usize, @intFromEnum(self.token_tags[self.token_idx]))];

        if (next_rule.prec < prec_min) break;

        if (next_rule.prec == banned_prec) {
            return self.errAtCurrent(.ChainingCmpOp);
        }

        // If we are in a binop, we insert the expr before the operand
        // There is no data, we just use the two next nodes
        // try self.nodes.insert(self.allocator, start_node, .{
        //     .tag = next_rule.tag,
        //     .main = start_tk,
        // });

        // Here, we can safely use it
        self.advance();
        _ = try self.parsePrecedenceExpr(next_rule.prec + 1);

        if (next_rule.assoc == .none) banned_prec = next_rule.prec;
    }

    return expr;
}

/// Parses expressions (prefix + sufix)
fn parseExpr(self: *Parser) Error!*Expr {
    // Prefix part
    const expr = try switch (self.prev(.tag)) {
        // .leftBrace => self.block(),
        // .@"if" => self.if_expr(),
        .minus, .not => self.unary(),
        // .@"return" => self.returnExpr(),
        else => self.primaryExpr(),
    };

    // Apply postfix
    return self.postfix(expr);
}

fn block(self: *Parser) Error!*Expr {
    const openning_brace = self.token_idx - 1;

    self.skipNewLines();
    const idx = try self.add_node(.{ .tag = .Block, .main = openning_brace });
    var length: usize = 0;

    while (!self.check(.rightBrace) and !self.check(.eof)) {
        _ = try self.declaration();
        self.skipNewLines();
        length += 1;
    }

    try self.expectOrErrAtToken(.rightBrace, .UnclosedBrace, openning_brace);

    self.nodes.items(.data)[idx] = length;

    return idx;
}

fn ifExpr(self: *Parser) Error!Expr {
    const idx = try self.add_node(.{
        .tag = .@"if",
        .main = self.token_idx - 1,
    });

    _ = try self.parsePrecedenceExpr(0);
    self.skipNewLines();

    // TODO: Warning for unnecessary 'do' if there is a block after
    _ = if (self.matchAndSkip(.leftBrace))
        try self.block()
    else if (self.matchAndSkip(.do))
        try self.declaration()
    else
        return self.errAtPrev(.{ .ExpectBraceOrDo = .{ .what = "if" } });

    self.skipNewLines();

    // If we dosen't match an else, we go back one token to be able
    // to match the rule "after each statement there is a new line"
    // tested in the main caller
    if (self.matchAndSkip(.@"else")) {
        _ = if (self.matchAndSkip(.leftBrace))
            try self.block()
        else
            try self.declaration();
    } else {
        self.token_idx -= 1;
        _ = try self.add_node(.empty);
    }

    return idx;
}

fn unary(self: *Parser) Error!*Expr {
    const expr = try self.allocator.create(Expr);
    self.advance();
    expr.* = .{ .unary = .{ .op = self.token_idx - 2, .expr = try self.parseExpr() } };

    return expr;
}

fn primaryExpr(self: *Parser) Error!*Expr {
    return switch (self.prev(.tag)) {
        .false => self.literal(.bool),
        .float => self.literal(.float),
        .identifier => self.literal(.identifier),
        .int => self.literal(.int),
        .leftParen => self.grouping(),
        .null => self.literal(.null),
        .string => self.literal(.string),
        .true => self.literal(.bool),
        else => {
            const span = self.token_spans[self.token_idx - 1];
            return self.errAtPrev(.{ .ExpectExpr = .{ .found = span.text(self.source) } });
        },
    };
}

fn grouping(self: *Parser) Error!*Expr {
    const start = self.token_idx - 1;
    const opening = self.prev(.span).start;
    self.skipNewLines();

    const expr = try self.allocator.create(Expr);
    const value = if (self.check(.rightParen))
        null
    else
        try self.parsePrecedenceExpr(0);

    try self.expectOrErrAtToken(.rightParen, .UnclosedParen, start);

    expr.* = .{ .grouping = .{
        .expr = value,
        .span = .{
            .start = opening,
            .end = self.current(.span).start,
        },
    } };

    return expr;
}

fn literal(self: *Parser, tag: Ast.Literal.Tag) Error!*Expr {
    const expr = try self.allocator.create(Expr);
    expr.* = .{ .literal = .{ .tag = tag, .idx = self.token_idx - 1 } };

    return expr;
}

// Parses postfix expressions: calls, member access
fn postfix(self: *Parser, prefixExpr: *Expr) Error!*Expr {
    var expr = prefixExpr;

    while (true) {
        if (self.match(.leftParen)) {
            expr = try self.finishCall(expr);
        } else if (self.match(.dot)) {
            // if (self.match(.leftBrace)) {
            //     try self.structLiteral(expr);
            // } else {
            //     try self.field(expr);
            // }
        } else break;
    }

    return expr;
}

// Takes the callee expression as input and output the full function call expression
fn finishCall(self: *Parser, expr: *Expr) Error!*Expr {
    const call_expr = try self.allocator.create(Expr);

    // TODO: init with capacity of 255?
    var args: std.ArrayListUnmanaged(*Expr) = .{};

    // All the skip_lines cover the different syntaxes
    while (!self.check(.rightParen)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.ExpectParenAfterFnArgs);

        if (args.items.len == 255)
            return self.errAtCurrent(.{ .TooManyFnArgs = .{ .what = "argument" } });

        try args.append(self.allocator, try self.parsePrecedenceExpr(0));

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.rightParen, .ExpectParenAfterFnArgs);

    call_expr.* = .{ .fnCall = .{
        .callee = expr,
        .args = try args.toOwnedSlice(self.allocator),
    } };

    return call_expr;
}

fn field(self: *Parser, expr: *Expr) Error!void {
    try self.nodes.insert(self.allocator, expr, .{ .tag = .field });
    try self.expect(.identifier, .ExpectNameAfterDot);
    _ = try self.literal(.identifier);
}

fn structLiteral(self: *Parser, expr: *Expr) Error!void {
    try self.nodes.insert(self.allocator, expr, .{ .tag = .structLiteral });
    var arity: usize = 0;

    // All the skip_lines cover the different syntaxes
    while (!self.check(.rightBrace)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.ExpectBraceAfterStructLit);

        if (!self.match(.identifier)) {
            return self.errAtCurrent(.StructLitNonIdentField);
        }

        _ = try self.add_node(.{ .tag = .identifier, .main = self.token_idx - 1 });

        // Either: { x = 3 }  or { x }, if x is a local variable
        if (self.match(.equal)) {
            _ = try self.parsePrecedenceExpr(0);
        } else _ = try self.add_node(.empty);

        arity += 1;

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.rightBrace, .ExpectBraceAfterStructLit);

    self.nodes.items(.data)[expr] = arity;
}
