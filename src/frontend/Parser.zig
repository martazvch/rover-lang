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
const oom = @import("../utils.zig").oom;

source: []const u8,
errs: ArrayList(ParserReport),
arena: ArenaAllocator,
allocator: Allocator,
token_tags: []const Token.Tag,
token_spans: []const Span,
token_idx: usize = 0,
nodes: ArrayListUnmanaged(Node) = .{},
panic_mode: bool = false,
in_cond: bool = false,
in_group: bool = false,

const Self = @This();
pub const ParserReport = GenReport(ParserMsg);
const Error = error{Err};

pub const empty: Self = .{
    .source = undefined,
    .errs = undefined,
    .arena = undefined,
    .allocator = undefined,
    .token_tags = undefined,
    .token_spans = undefined,
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
pub fn parse(self: *Self, source: [:0]const u8, token_tags: []const Token.Tag, token_spans: []const Span) Ast {
    self.source = source;
    self.token_tags = token_tags;
    self.token_spans = token_spans;

    self.skipNewLines();

    while (!self.match(.eof)) {
        const stmt = self.declaration() catch |e| switch (e) {
            // If it's our own error, we continue on parsing
            Error.Err => {
                self.synchronize();
                continue;
            },
            // else => return e,
        };
        self.nodes.append(self.allocator, stmt) catch oom();

        // If EOF, exit
        if (self.match(.eof)) break;

        // After each nodes we expect a new line
        if (!self.check(.new_line)) {
            const start = self.prev(.span).end;
            self.errAtSpan(.{ .start = start, .end = start + 1 }, .expect_new_line) catch {};
            self.synchronize();
        }

        self.skipNewLines();
    }

    return .{
        .source = source,
        .token_tags = token_tags,
        .token_spans = token_spans,
        .nodes = self.nodes.toOwnedSlice(self.allocator) catch oom(),
    };
}

fn TokenFieldType(kind: anytype) type {
    return switch (kind) {
        .span => Span,
        .tag => Token.Tag,
        else => @compileError("Parser's 'next' function can accept only '.tag' and '.span' literal"),
    };
}

/// `kind` should be `.tag` or `.span`
inline fn prev(self: *const Self, kind: anytype) TokenFieldType(kind) {
    return self.getTkField(kind, self.token_idx - 1);
}

/// `kind` should be `.tag` or `.span`
inline fn current(self: *const Self, kind: anytype) TokenFieldType(kind) {
    return self.getTkField(kind, self.token_idx);
}

/// `kind` should be `.tag` or `.span`
inline fn getTkField(self: *const Self, kind: anytype, idx: usize) TokenFieldType(kind) {
    return switch (kind) {
        .span => self.token_spans[idx],
        .tag => self.token_tags[idx],
        else => @compileError("Parser's 'next' function can accept only '.tag' and '.span' literal"),
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
    if (self.panic_mode) return error.Err;

    self.panic_mode = true;

    const report = ParserReport.err(error_kind, self.token_spans[token]);
    self.errs.append(report) catch oom();

    return error.Err;
}

/// If error already encountered and in the same statement parsing,
/// we exit, let synchronize and resume. It is likely that if we
/// are already in panic mode, the following errors are just
/// consequencies of actual bad statement
fn errAtSpan(self: *Self, span: Span, error_kind: ParserMsg) Error {
    if (self.panic_mode) return error.Err;

    self.panic_mode = true;

    const report = ParserReport.err(error_kind, span);
    self.errs.append(report) catch oom();

    return error.Err;
}

fn synchronize(self: *Self) void {
    self.panic_mode = false;

    while (!self.check(.eof)) {
        switch (self.token_tags[self.token_idx]) {
            .@"fn", .@"for", .@"if", .left_brace, .print, .@"return", .@"struct", .use, .@"var", .@"while" => return,
            else => self.advance(),
        }
    }
}

fn declaration(self: *Self) Error!Node {
    return if (self.match(.@"var"))
        self.varDecl()
    else if (self.match(.@"fn"))
        self.fnDecl()
    else if (self.match(.@"struct"))
        self.structDecl()
    else if (self.match(.underscore))
        self.discard()
    else if (self.match(.use))
        self.use()
    else
        self.statement();
}

fn fnDecl(self: *Self) Error!Node {
    try self.expect(.identifier, .expect_fn_name);
    const name = self.token_idx - 1;
    try self.expect(.left_paren, .expect_paren_after_fn_name);
    self.skipNewLines();

    var params: ArrayListUnmanaged(Ast.Param) = .{};
    var param_names: ArrayListUnmanaged(TokenIndex) = .{};

    param_list: while (!self.check(.right_paren)) {
        defer param_names.clearRetainingCapacity();

        if (self.check(.eof)) {
            return self.errAtPrev(.expect_paren_after_fn_params);
        }

        if (params.items.len == 255) return self.errAtCurrent(
            .{ .too_many_fn_args = .{ .what = "parameter" } },
        );

        // self managment
        if (self.match(.self)) {
            const name_idx = self.token_idx - 1;

            if (params.items.len > 0) {
                return self.errAtPrev(.self_as_non_first_param);
            }

            if (self.match(.colon)) {
                return self.errAtCurrent(.typed_self);
            }

            // Potential other argument following self
            _ = self.match(.comma);

            // TODO: Allocate only one `Self` type for all
            const typ = self.allocator.create(Ast.Type) catch oom();
            typ.* = .{ .self = name_idx };
            params.append(self.allocator, .{ .name = name_idx, .typ = typ }) catch oom();
            continue :param_list;
        }

        while (self.match(.identifier)) {
            param_names.append(self.allocator, self.token_idx - 1) catch oom();
            if (self.match(.comma)) {
                // TODO: Error
                if (!self.check(.identifier)) @panic("can't have trailing commas params");
                continue;
            }
        }

        if (param_names.items.len == 0) {
            try self.expect(.identifier, .{ .expect_name = .{ .kind = "parameter" } });
        }

        const typ = if (self.match(.colon)) try self.parseType() else null;
        const value = if (self.match(.equal)) try self.parsePrecedenceExpr(0) else null;

        if (typ == null and value == null) {
            //  TODO: change error for ':' or '='
            try self.expect(.colon, .missing_fn_param_type);
        }

        for (param_names.items) |p| {
            params.append(self.allocator, .{ .name = p, .typ = typ, .value = value }) catch oom();
        }

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.right_paren, .expect_paren_after_fn_params);

    const return_type: ?*Ast.Type = if (self.match(.small_arrow))
        try self.parseType()
    else if (self.check(.identifier) or self.check(.bool) or self.check(.int_kw) or self.check(.float_kw))
        return self.errAtCurrent(.expect_arrow_before_fn_type)
    else
        null;

    self.skipNewLines();
    try self.expect(.left_brace, .expect_brace_before_fn_body);

    return .{ .fn_decl = .{
        .name = name,
        .params = params.toOwnedSlice(self.allocator) catch oom(),
        .body = (try self.block()).block,
        .return_type = return_type,
    } };
}

fn structDecl(self: *Self) !Node {
    try self.expect(.identifier, .expect_struct_name);
    const name = self.token_idx - 1;
    self.skipNewLines();
    try self.expectOrErrAtPrev(.left_brace, .expect_brace_before_struct_body);
    self.skipNewLines();

    var fields: ArrayListUnmanaged(Ast.VarDecl) = .{};
    var field_names: ArrayListUnmanaged(TokenIndex) = .{};
    defer field_names.deinit(self.allocator);

    // If at least one field
    while (!self.check(.@"fn") and !self.check(.right_brace) and !self.check(.eof)) {
        defer field_names.clearRetainingCapacity();

        if (!self.check(.identifier))
            return self.errAtCurrent(.expect_field_name);

        // Support sharing type across fields
        while (self.match(.identifier)) {
            field_names.append(self.allocator, self.token_idx - 1) catch oom();

            if (self.match(.comma)) continue;
        }

        const typ = if (self.match(.colon)) try self.parseType() else null;
        const value = if (self.match(.equal)) try self.parsePrecedenceExpr(0) else null;

        if (typ == null and value == null) {
            return self.errAtCurrent(.expect_field_type_or_default);
        }

        for (field_names.items) |field_name| {
            fields.append(self.allocator, .{ .name = field_name, .typ = typ, .value = value }) catch oom();
        }

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    // If we are at an identifier, might be a missing comma between fields
    if (self.check(.identifier)) {
        return self.errAtPrev(.missing_comma_after_field);
    }

    // Functions
    var functions: ArrayListUnmanaged(Ast.FnDecl) = .{};

    while (!self.check(.right_brace) and !self.check(.eof)) {
        try self.expect(.@"fn", .expect_fn_in_struct_body);
        functions.append(self.allocator, (try self.fnDecl()).fn_decl) catch oom();
        self.skipNewLines();
    }

    try self.expectOrErrAtPrev(.right_brace, .expect_brace_after_struct_body);

    return .{ .struct_decl = .{
        .name = name,
        .fields = fields.toOwnedSlice(self.allocator) catch oom(),
        .functions = functions.toOwnedSlice(self.allocator) catch oom(),
    } };
}

fn varDecl(self: *Self) Error!Node {
    const name = self.token_idx;
    try self.expect(.identifier, .{ .expect_name = .{ .kind = "variable" } });

    if (self.check(.comma))
        return self.multiVarDecl(name);

    const typ = try self.expectTypeOrEmpty();

    const value = if (self.match(.equal))
        try self.parsePrecedenceExpr(0)
    else
        null;

    if (typ == null and value == null) {
        return self.errAt(name, .expect_type_or_value_in_decl);
    }

    return .{ .var_decl = .{ .name = name, .typ = typ, .value = value } };
}

fn multiVarDecl(self: *Self, first_name: usize) Error!Node {
    var count: usize = 1;
    var decls: ArrayListUnmanaged(Ast.VarDecl) = .{};
    var variables = ArrayListUnmanaged(usize){};
    defer variables.deinit(self.allocator);

    while (self.match(.comma)) {
        try self.expect(.identifier, .{ .expect_name = .{ .kind = "variable" } });
        variables.append(self.allocator, self.token_idx - 1) catch oom();
    }

    decls.ensureTotalCapacity(self.allocator, variables.items.len) catch oom();
    const typ = try self.expectTypeOrEmpty();
    const first_value = if (self.match(.equal))
        try self.parsePrecedenceExpr(0)
    else
        null;

    // First declaration
    decls.appendAssumeCapacity(.{ .name = first_name, .typ = typ, .value = first_value });

    // If only one value
    if (!self.check(.comma)) {
        for (variables.items) |var_idx| {
            decls.appendAssumeCapacity(.{ .name = var_idx, .typ = typ, .value = first_value });
        }
    } else while (self.match(.comma)) : (count += 1) {
        if (count > variables.items.len) {
            count -= 1;
            break;
        }

        decls.appendAssumeCapacity(.{
            .name = variables.items[count - 1],
            .typ = typ,
            .value = try self.parsePrecedenceExpr(0),
        });
    }

    if (count > 1 and count != variables.items.len + 1)
        return self.errAt(variables.items[count - 1], .{ .wrong_value_count_var_decl = .{
            .expect = variables.items.len + 1,
        } });

    return .{ .multi_var_decl = .{ .decls = decls.toOwnedSlice(self.allocator) catch oom() } };
}

/// Expects a type after ':'. If no colon, declares an empty type
fn expectTypeOrEmpty(self: *Self) Error!?*Ast.Type {
    return if (self.match(.colon))
        try self.parseType()
    else if (self.check(.identifier))
        self.errAtCurrent(.expect_colon_before_type)
    else
        null;
}

/// Parses a type. It assumes you know that a type is expected at this place
fn parseType(self: *Self) Error!*Ast.Type {
    const typ = self.allocator.create(Ast.Type) catch oom();

    if (self.isIdentOrType()) {
        if (self.check(.dot)) {
            var tokens: ArrayListUnmanaged(TokenIndex) = .{};
            tokens.append(self.allocator, self.token_idx - 1) catch oom();

            while (self.match(.dot)) {
                try self.expect(.identifier, .non_ident_in_type);
                tokens.append(self.allocator, self.token_idx - 1) catch oom();
            }

            // If there is a number in the chain ike: foo.1.bar, it will be considered
            // as a float by the Lexer
            if (self.check(.float)) return self.errAtCurrent(.non_ident_in_type);

            typ.* = .{ .fields = tokens.toOwnedSlice(self.allocator) catch oom() };
        } else {
            typ.* = .{ .scalar = self.token_idx - 1 };
        }
    } else if (self.match(.@"fn")) {
        var span: Span = .{ .start = self.token_idx - 1, .end = undefined };

        var params: ArrayListUnmanaged(*Ast.Type) = .{};
        try self.expect(.left_paren, .expect_paren_after_fn_name);

        while (!self.check(.right_paren) and !self.check(.eof)) {
            params.append(self.allocator, try self.parseType()) catch oom();
            if (!self.match(.comma)) break;
        }

        try self.expect(.right_paren, .expect_paren_after_fn_params);

        const return_type = if (self.match(.small_arrow))
            try self.parseType()
        else if (self.isIdentOrType())
            return self.errAtCurrent(.expect_arrow_before_fn_type)
        else
            null;

        span.end = self.token_idx - 1;

        typ.* = .{ .function = .{
            .params = params.toOwnedSlice(self.allocator) catch oom(),
            .return_type = return_type,
            .span = span,
        } };
    } else {
        return self.errAtCurrent(.expect_type_name);
    }

    return typ;
}

fn isIdentOrType(self: *Self) bool {
    return self.match(.identifier) or
        self.match(.float_kw) or
        self.match(.int_kw) or
        self.match(.str_kw) or
        self.match(.bool);
}

fn discard(self: *Self) Error!Node {
    try self.expect(.equal, .invalid_discard);

    return .{ .discard = try self.parsePrecedenceExpr(0) };
}

fn use(self: *Self) Error!Node {
    var names: ArrayListUnmanaged(usize) = .{};

    if (!self.check(.identifier)) {
        return self.errAtCurrent(.{ .expect_name = .{ .kind = "module" } });
    }

    while (self.match(.identifier) and !self.check(.eof)) {
        names.append(self.allocator, self.token_idx - 1) catch oom();
        if (self.match(.dot)) continue;
        break;
    }

    const items = if (self.match(.left_brace)) b: {
        var items: ArrayListUnmanaged(Ast.Use.ItemAndAlias) = .{};

        while (self.match(.identifier)) {
            items.append(self.allocator, .{ .item = self.token_idx - 1, .alias = try self.getAlias() }) catch oom();

            if (self.match(.comma)) continue;
            // In case of trailing comma
            if (self.check(.identifier)) break;
        }
        try self.expect(.right_brace, .missing_brace_items_import);

        break :b items.toOwnedSlice(self.allocator) catch oom();
    } else null;

    const alias = try self.getAlias();

    if (alias != null and items != null) {
        return self.errAtPrev(.import_alias_with_items);
    }

    return .{ .use = .{
        .names = names.toOwnedSlice(self.allocator) catch oom(),
        .items = items,
        .alias = alias,
    } };
}

fn getAlias(self: *Self) Error!?TokenIndex {
    return if (self.match(.as)) b: {
        try self.expect(.identifier, .non_ident_alias);
        break :b self.token_idx - 1;
    } else null;
}

fn statement(self: *Self) Error!Node {
    if (self.match(.print)) {
        return self.print();
    } else if (self.match(.@"while")) {
        return self.whileStmt();
    } else {
        const assigne = try self.parsePrecedenceExpr(0);

        return if (self.match(.equal))
            self.assignment(assigne)
        else if (self.match(.plus_equal) or self.match(.minus_equal) or self.match(.star_equal) or self.match(.slash_equal))
            self.compoundAssignment(assigne)
        else
            .{ .expr = assigne };
    }
}

fn assignment(self: *Self, assigne: *Expr) Error!Node {
    return .{ .assignment = .{
        .assigne = assigne,
        .value = try self.parsePrecedenceExpr(0),
    } };
}

fn compoundAssignment(self: *Self, assigne: *Expr) Error!Node {
    const op = self.prev(.tag);
    const value = try self.parsePrecedenceExpr(0);
    const binop = self.allocator.create(Expr) catch oom();

    binop.* = .{ .binop = .{
        .lhs = assigne,
        .op = switch (op) {
            .plus_equal => .plus,
            .minus_equal => .minus,
            .star_equal => .star,
            .slash_equal => .slash,
            else => unreachable,
        },
        .rhs = value,
    } };

    return .{ .assignment = .{
        .assigne = assigne,
        .value = binop,
    } };
}

fn print(self: *Self) Error!Node {
    return .{ .print = try self.parsePrecedenceExpr(0) };
}

fn whileStmt(self: *Self) Error!Node {
    const save = self.in_cond;
    self.in_cond = true;
    errdefer self.in_cond = save;
    const cond = try self.parsePrecedenceExpr(0);
    self.in_cond = save;

    const body = if (self.matchAndSkip(.left_brace))
        (try self.block()).block
    else
        return self.errAtCurrent(.expect_brace_after_while_cond);

    return .{ .@"while" = .{ .condition = cond, .body = body } };
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
            // TODO: why not support it like Python for example?
            return self.errAtCurrent(.chaining_cmp_op);
        }

        // Here, we can safely use it
        self.advance();
        const op = self.prev(.tag);
        const rhs = try self.parsePrecedenceExpr(next_rule.prec + 1);

        const expr = self.allocator.create(Expr) catch oom();
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
        .self => self.literal(.self),
        .string => self.literal(.string),
        .true => self.literal(.bool),
        else => {
            const span = self.token_spans[self.token_idx - 1];
            return self.errAtPrev(.{ .expect_expr = .{ .found = span.text(self.source) } });
        },
    };

    return self.postfix(expr);
}

fn block(self: *Self) Error!*Expr {
    const openning_brace = self.token_idx - 1;
    const expr = self.allocator.create(Expr) catch oom();
    var exprs: ArrayListUnmanaged(Node) = .{};

    self.skipNewLines();

    while (!self.check(.right_brace) and !self.check(.eof)) {
        exprs.append(self.allocator, try self.declaration()) catch oom();
        self.skipNewLines();
    }

    try self.expectOrErrAtToken(.right_brace, .unclosed_brace, openning_brace);
    expr.* = .{ .block = .{
        .nodes = exprs.toOwnedSlice(self.allocator) catch oom(),
        .span = .{ .start = openning_brace, .end = self.prev(.span).start },
    } };

    return expr;
}

fn ifExpr(self: *Self) Error!*Expr {
    const save = self.in_cond;
    self.in_cond = true;
    errdefer self.in_cond = save;
    const condition = try self.parsePrecedenceExpr(0);
    self.skipNewLines();
    self.in_cond = save;

    // TODO: Warning for unnecessary 'do' if there is a block after
    const then: Node = if (self.matchAndSkip(.left_brace))
        .{ .expr = try self.block() }
    else if (self.matchAndSkip(.do))
        try self.declaration()
    else
        return self.errAtPrev(.{ .expect_brace_or_do = .{ .what = "if" } });

    self.skipNewLines();

    // If we dosen't match an else, we go back one token to be able
    // to match the rule "after each statement there is a new line"
    // tested in the main caller
    const else_body: ?Node = if (self.matchAndSkip(.@"else"))
        if (self.matchAndSkip(.left_brace))
            .{ .expr = try self.block() }
        else
            try self.declaration()
    else blk: {
        self.token_idx -= 1;
        break :blk null;
    };

    const expr = self.allocator.create(Expr) catch oom();
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

    const save = self.in_group;
    self.in_group = true;
    errdefer self.in_group = save;
    defer self.in_group = save;

    const expr = self.allocator.create(Expr) catch oom();
    const value = try self.parsePrecedenceExpr(0);

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
    const expr = self.allocator.create(Expr) catch oom();
    expr.* = .{ .literal = .{ .tag = tag, .idx = self.token_idx - 1 } };

    return expr;
}

fn returnExpr(self: *Self) Error!*Expr {
    const kw = self.token_idx - 1;
    const expr = self.allocator.create(Expr) catch oom();
    expr.* = .{ .@"return" = .{
        .expr = if (self.check(.new_line) or self.check(.right_brace))
            null
        else
            try self.parsePrecedenceExpr(0),
        .kw = kw,
    } };

    return expr;
}

fn unary(self: *Self) Error!*Expr {
    const expr = self.allocator.create(Expr) catch oom();
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
            expr = try self.field(expr);
        } else if (self.match(.left_brace)) {
            if (self.in_cond and !self.in_group) {
                self.token_idx -= 1;
                return expr;
            }
            expr = try self.structLiteral(expr);
        } else break;
    }

    return expr;
}

// Takes the callee expression as input and output the full function call expression
fn finishCall(self: *Self, expr: *Expr) Error!*Expr {
    const call_expr = self.allocator.create(Expr) catch oom();

    var args: ArrayListUnmanaged(*Expr) = .{};
    args.ensureTotalCapacity(self.allocator, 256) catch oom();

    // All the skip_lines cover the different syntaxes
    while (!self.check(.right_paren)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.expect_paren_after_fn_args);

        if (args.items.len == 255)
            return self.errAtCurrent(.{ .too_many_fn_args = .{ .what = "argument" } });

        const param_expr = if (self.check(.identifier)) b: {
            const param_expr = try self.parsePrecedenceExpr(0);

            if (self.check(.comma) or self.check(.right_paren) or self.check(.new_line)) {
                break :b param_expr;
            }

            try self.expect(.equal, .missing_equal_named_param);
            const tmp = self.allocator.create(Expr) catch oom();
            tmp.* = .{ .named_arg = .{ .name = self.token_idx - 1, .value = try self.parsePrecedenceExpr(0) } };
            break :b tmp;
        } else try self.parsePrecedenceExpr(0);

        args.appendAssumeCapacity(param_expr);

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    try self.expect(.right_paren, .expect_paren_after_fn_args);

    call_expr.* = .{ .fn_call = .{
        .callee = expr,
        .args = args.toOwnedSlice(self.allocator) catch oom(),
    } };

    return call_expr;
}

fn field(self: *Self, expr: *Expr) Error!*Expr {
    try self.expect(.identifier, .expect_name_after_dot);

    const field_access = self.allocator.create(Expr) catch oom();
    field_access.* = .{ .field = .{
        .structure = expr,
        .field = self.token_idx - 1,
    } };

    return field_access;
}

fn structLiteral(self: *Self, expr: *Expr) Error!*Expr {
    const struct_lit = self.allocator.create(Expr) catch oom();
    var fields_values: ArrayListUnmanaged(Ast.FieldAndValue) = .{};

    b: {
        if (expr.* == .literal) {
            if (expr.literal.tag == .identifier) break :b;
        } else if (expr.* == .field) break :b;

        return self.errAtPrev(.invalid_struct_literal);
    }

    // All the skip_lines cover the different syntaxes
    while (!self.check(.right_brace)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.expect_brace_after_struct_lit);

        if (!self.match(.identifier)) {
            return self.errAtCurrent(.struct_lit_non_ident_field);
        }

        // Either: { x = 3 }  or { x }
        fields_values.append(self.allocator, .{
            .name = self.token_idx - 1,
            .value = if (self.match(.equal)) try self.parsePrecedenceExpr(0) else null,
        }) catch oom();

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }
    try self.expect(.right_brace, .expect_brace_after_struct_lit);

    struct_lit.* = .{ .struct_literal = .{
        .structure = expr,
        .fields = fields_values.toOwnedSlice(self.allocator) catch oom(),
    } };

    return struct_lit;
}
