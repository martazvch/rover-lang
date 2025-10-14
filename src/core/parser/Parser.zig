const std = @import("std");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const FieldEnum = std.meta.FieldEnum;

const GenReport = @import("misc").reporter.GenReport;
const Ast = @import("Ast.zig");
const Expr = Ast.Expr;
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;
const oom = @import("misc").oom;

source: []const u8,
errs: ArrayList(ParserReport),
allocator: Allocator,
token_tags: []const Token.Tag,
token_spans: []const Span,
token_idx: usize,
nodes: ArrayList(Node),
ctx: Context,

const Self = @This();
pub const ParserReport = GenReport(ParserMsg);
const Context = struct {
    panic_mode: bool,
    in_cond: bool,
    in_group: bool,
    can_extract: bool,

    pub const empty: Context = .{
        .panic_mode = false,
        .in_cond = false,
        .in_group = false,
        .can_extract = false,
    };

    pub fn setAndGetPrevious(self: *Context, comptime f: FieldEnum(Context), value: @FieldType(Context, @tagName(f))) @TypeOf(value) {
        const val = @field(self, @tagName(f));
        @field(self, @tagName(f)) = value;

        return val;
    }
};
const Error = error{Err};

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .source = undefined,
        .errs = .empty,
        .nodes = .empty,
        .token_idx = 0,
        .token_spans = undefined,
        .token_tags = undefined,
        .ctx = .empty,
    };
}

/// Parses the token stream
pub fn parse(self: *Self, source: [:0]const u8, token_tags: []const Token.Tag, token_spans: []const Span) Ast {
    self.source = source;
    self.token_tags = token_tags;
    self.token_spans = token_spans;

    self.skipNewLines();

    while (!self.match(.eof)) {
        const stmt = self.declaration() catch {
            self.synchronize();
            continue;
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
fn prev(self: *const Self, kind: anytype) TokenFieldType(kind) {
    return self.getTkField(kind, self.token_idx - 1);
}

/// `kind` should be `.tag` or `.span`
fn current(self: *const Self, kind: anytype) TokenFieldType(kind) {
    return self.getTkField(kind, self.token_idx);
}

/// `kind` should be `.tag` or `.span`
fn getTkField(self: *const Self, kind: anytype, idx: usize) TokenFieldType(kind) {
    return switch (kind) {
        .span => self.token_spans[idx],
        .tag => self.token_tags[idx],
        else => @compileError("Parser's 'next' function can accept only '.tag' and '.span' literal"),
    };
}

fn advance(self: *Self) void {
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
    if (self.ctx.panic_mode) return error.Err;

    self.ctx.panic_mode = true;

    const span = self.token_spans[token];
    const report = ParserReport.err(error_kind, span.start, span.end);
    self.errs.append(self.allocator, report) catch oom();

    return error.Err;
}

/// If error already encountered and in the same statement parsing,
/// we exit, let synchronize and resume. It is likely that if we
/// are already in panic mode, the following errors are just
/// consequencies of actual bad statement
fn errAtSpan(self: *Self, span: Span, error_kind: ParserMsg) Error {
    if (self.ctx.panic_mode) return error.Err;

    self.ctx.panic_mode = true;

    const report = ParserReport.err(error_kind, span.start, span.end);
    self.errs.append(self.allocator, report) catch oom();

    return error.Err;
}

fn synchronize(self: *Self) void {
    self.ctx.panic_mode = false;

    while (!self.check(.eof)) {
        switch (self.token_tags[self.token_idx]) {
            .@"fn", .@"for", .@"if", .left_brace, .print, .@"return", .@"struct", .use, .@"var", .when, .@"while" => return,
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
    const params = try self.fnParams(false);
    try self.expect(.right_paren, .expect_paren_after_fn_params);

    const return_type = try self.fnReturnType();
    self.skipNewLines();
    try self.expect(.left_brace, .expect_brace_before_fn_body);

    const body, const has_callable = try self.block(null);

    return .{ .fn_decl = .{
        .name = name,
        .params = params,
        .body = body.block,
        .return_type = return_type,
        .has_callable = has_callable,
        .is_closure = false,
    } };
}

fn fnParams(self: *Self, is_closure: bool) Error![]Ast.VarDecl {
    const closing_token: Token.Tag = if (is_closure) .pipe else .right_paren;
    var params: ArrayList(Ast.VarDecl) = .empty;
    var param_names: ArrayList(TokenIndex) = .empty;
    var named_started = false;

    while (!self.check(closing_token)) {
        defer param_names.clearRetainingCapacity();

        if (self.check(.eof)) {
            return self.errAtPrev(.expect_paren_after_fn_params);
        }

        if (params.items.len == 255) return self.errAtCurrent(
            .{ .too_many_fn_args = .{ .what = "parameter" } },
        );

        // self managment
        if (!is_closure and self.match(.self)) {
            const name_idx = self.token_idx - 1;

            if (params.items.len > 0) {
                return self.errAtPrev(.self_as_non_first_param);
            }

            if (self.match(.colon)) {
                return self.errAtCurrent(.typed_self);
            }

            if (self.match(.equal)) {
                return self.errAtCurrent(.default_value_self);
            }

            // Potential other argument following self
            _ = self.match(.comma);

            const ty = self.allocator.create(Ast.Type) catch oom();
            ty.* = .{ .self = name_idx };
            params.append(self.allocator, .{ .name = name_idx, .typ = ty, .value = null }) catch oom();
            continue;
        }

        while (self.match(.identifier)) {
            param_names.append(self.allocator, self.token_idx - 1) catch oom();
            if (self.match(.comma)) {
                continue;
            }
        }

        if (param_names.items.len == 0) {
            try self.expect(.identifier, .{ .expect_name = .{ .kind = "parameter" } });
        }

        const typ = if (self.match(.colon)) try self.parseType() else null;
        const value = if (self.match(.equal)) b: {
            named_started = true;
            break :b try self.parsePrecedenceExpr(0);
        } else if (named_started) return self.errAtCurrent(.positional_after_default_param) else null;

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

    return params.toOwnedSlice(self.allocator) catch oom();
}

fn fnReturnType(self: *Self) Error!?*Ast.Type {
    return if (self.match(.arrow_small))
        self.parseType()
    else if (self.check(.identifier))
        self.errAtCurrent(.expect_arrow_before_fn_type)
    else
        null;
}

fn structDecl(self: *Self) !Node {
    try self.expect(.identifier, .expect_struct_name);
    const name = self.token_idx - 1;
    self.skipNewLines();
    try self.expectOrErrAtPrev(.left_brace, .expect_brace_before_struct_body);
    self.skipNewLines();

    var fields: ArrayList(Ast.VarDecl) = .empty;
    var field_names: ArrayList(TokenIndex) = .empty;
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
    var functions: ArrayList(Ast.FnDecl) = .empty;

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
    var decls: ArrayList(Ast.VarDecl) = .empty;
    var variables: ArrayList(usize) = .empty;

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
    const ty = self.allocator.create(Ast.Type) catch oom();

    if (self.match(.identifier)) {
        // Namespaced type
        if (self.check(.dot)) {
            var tokens: ArrayList(TokenIndex) = .empty;
            tokens.append(self.allocator, self.token_idx - 1) catch oom();

            while (self.match(.dot)) {
                try self.expect(.identifier, .non_ident_in_type);
                tokens.append(self.allocator, self.token_idx - 1) catch oom();
            }

            // If there is a number in the chain ike: foo.1.bar, it will be considered
            // as a float by the Lexer
            if (self.check(.float)) return self.errAtCurrent(.non_ident_in_type);

            ty.* = .{ .fields = tokens.toOwnedSlice(self.allocator) catch oom() };
        }
        // Regular type
        else {
            ty.* = .{ .scalar = self.token_idx - 1 };
        }
    }
    // Array
    else if (self.match(.left_bracket)) {
        const openning = self.token_idx - 1;
        try self.expect(.right_bracket, .missing_bracket_array_type);
        ty.* = .{ .array = .{ .openning = openning, .child = try self.parseType() } };
    }
    // Function
    else if (self.match(.@"fn")) {
        var span: Span = .{ .start = self.token_idx - 1, .end = undefined };

        var params: ArrayList(*Ast.Type) = .empty;
        try self.expect(.left_paren, .expect_paren_after_fn_name);

        while (!self.check(.right_paren) and !self.check(.eof)) {
            params.append(self.allocator, try self.parseType()) catch oom();
            if (!self.match(.comma)) break;
        }

        try self.expect(.right_paren, .expect_paren_after_fn_params);

        const return_type = if (self.match(.arrow_small))
            try self.parseType()
        else if (self.match(.identifier))
            return self.errAtCurrent(.expect_arrow_before_fn_type)
        else
            null;

        span.end = self.token_idx - 1;

        ty.* = .{ .function = .{
            .params = params.toOwnedSlice(self.allocator) catch oom(),
            .return_type = return_type,
            .span = span,
        } };
    }
    // Optional
    else if (self.match(.question_mark)) {
        ty.* = .{ .optional = .{ .token = self.token_idx - 1, .child = try self.parseType() } };
    }
    // Unknown
    else {
        return self.errAtCurrent(.expect_type_name);
    }

    // Inline union
    if (self.check(.pipe)) {
        var types: ArrayList(*Ast.Type) = .empty;
        types.append(self.allocator, ty) catch oom();

        while (self.match(.pipe)) {
            types.append(self.allocator, try self.parseType()) catch oom();
        }

        const u = self.allocator.create(Ast.Type) catch oom();
        u.* = .{ .@"union" = types.toOwnedSlice(self.allocator) catch oom() };
        return u;
    }

    return ty;
}

fn discard(self: *Self) Error!Node {
    try self.expect(.equal, .invalid_discard);

    return .{ .discard = try self.parsePrecedenceExpr(0) };
}

fn use(self: *Self) Error!Node {
    var names: ArrayList(usize) = .empty;

    if (self.match(.dot)) {
        names.append(self.allocator, self.token_idx - 1) catch oom();
    }

    if (!self.check(.identifier)) {
        return self.errAtCurrent(.{ .expect_name = .{ .kind = "module" } });
    }

    while (self.match(.identifier) and !self.check(.eof)) {
        names.append(self.allocator, self.token_idx - 1) catch oom();
        if (self.match(.dot)) continue;
        break;
    }

    const items = if (self.match(.left_brace)) b: {
        var items: ArrayList(Ast.Use.ItemAndAlias) = .empty;

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
    if (self.match(.as)) {
        try self.expect(.identifier, .non_ident_alias);
        return self.token_idx - 1;
    }

    return null;
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

    return .{ .assignment = .{ .assigne = assigne, .value = binop } };
}

fn print(self: *Self) Error!Node {
    return .{ .print = try self.parsePrecedenceExpr(0) };
}

fn whileStmt(self: *Self) Error!Node {
    const cond = cond: {
        const save_in_cond = self.ctx.setAndGetPrevious(.in_cond, true);
        const save_extract = self.ctx.setAndGetPrevious(.can_extract, true);
        defer {
            self.ctx.in_cond = save_in_cond;
            self.ctx.can_extract = save_extract;
        }
        break :cond try self.parsePrecedenceExpr(0);
    };

    const body = if (self.isAtBlock())
        try self.blockExpr()
    else
        return self.errAtCurrent(.expect_brace_after_while_cond);

    return .{ .@"while" = .{ .condition = cond, .body = body.block } };
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
        .@"break" => self.breakExpr(),
        .colon => self.labelledBlock(),
        .false => self.literal(.bool),
        .float => self.literal(.float),
        .@"if" => self.ifExpr(),
        .identifier => self.literal(.identifier),
        .int => self.literal(.int),
        .left_brace => (try self.block(null)).@"0",
        .left_bracket => self.array(),
        .left_paren => self.leftParenExprStart(),
        .minus, .not => self.unary(),
        .null => self.literal(.null),
        .pipe => self.closure(),
        .@"return" => self.returnExpr(),
        .self => self.literal(.self),
        .string => self.literal(.string),
        .true => self.literal(.bool),
        .when => self.when(),
        else => {
            const span = self.token_spans[self.token_idx - 1];
            return self.errAtPrev(.{ .expect_expr = .{ .found = span.text(self.source) } });
        },
    };

    return self.postfix(expr);
}

fn array(self: *Self) Error!*Expr {
    const start = self.token_idx - 1;
    const expr = self.allocator.create(Expr) catch oom();
    var values: ArrayList(*Expr) = .empty;

    self.skipNewLines();
    while (!self.check(.eof) and !self.match(.right_bracket)) {
        const value_token = self.token_idx;
        values.append(self.allocator, try self.parsePrecedenceExpr(0)) catch oom();
        self.skipNewLines();
        if (self.matchAndSkip(.comma)) continue;
        if (self.match(.right_bracket)) break;

        return if (self.check(.eof))
            self.errAt(value_token, .missing_array_close_bracket)
        else
            self.errAtCurrent(.expect_comma_array_values);
    }

    const end = self.token_idx - 1;
    expr.* = .{ .array = .{
        .values = values.toOwnedSlice(self.allocator) catch oom(),
        .span = .{ .start = start, .end = end },
    } };

    return expr;
}

/// Checks wethre we are at a left brace or a colon for labelled and unlabelled blocks
/// It advances token_idx if matched
fn isAtBlock(self: *Self) bool {
    return self.match(.left_brace) or self.match(.colon);
}

/// Parses either a labelled or not block
fn blockExpr(self: *Self) Error!*Expr {
    if (self.prev(.tag) == .colon) return self.labelledBlock();

    const body, _ = try self.block(null);

    return body;
}

/// Parses a labelled block assuming we're passed ':'
fn labelledBlock(self: *Self) Error!*Expr {
    if (!self.match(.identifier)) return self.errAtCurrent(.non_ident_label);

    const label = self.token_idx - 1;
    if (!self.match(.left_brace)) return self.errAtCurrent(.missing_brace_after_label);

    return (try self.block(label)).@"0";
}

/// Returns the block expression and a flag to indicate if a function or closure was
/// defined in it
fn block(self: *Self, label: ?TokenIndex) Error!struct { *Expr, bool } {
    const openning_brace = self.token_idx - 1;
    const expr = self.allocator.create(Expr) catch oom();
    var exprs: ArrayList(Node) = .empty;
    var has_callable = false;

    self.skipNewLines();

    while (!self.check(.right_brace) and !self.check(.eof)) {
        const node = try self.declaration();

        switch (node) {
            .fn_decl => has_callable = true,
            .var_decl => |v| if (v.value) |val| {
                if (val.* == .closure) has_callable = true;
            },
            else => {},
        }

        exprs.append(self.allocator, node) catch oom();
        self.skipNewLines();
    }

    try self.expectOrErrAtToken(.right_brace, .unclosed_brace, openning_brace);
    expr.* = .{ .block = .{
        .label = label,
        .nodes = exprs.toOwnedSlice(self.allocator) catch oom(),
        .span = .{ .start = self.token_spans[openning_brace].start, .end = self.prev(.span).start },
    } };

    return .{ expr, has_callable };
}

fn breakExpr(self: *Self) Error!*Expr {
    const kw = self.token_idx - 1;
    const label = try self.parseLabel();
    const expr = self.allocator.create(Expr) catch oom();

    expr.* = .{ .@"break" = .{
        .kw = kw,
        .label = label,
        .expr = if (self.check(.new_line) or self.check(.right_brace) or self.check(.@"else"))
            null
        else
            try self.parsePrecedenceExpr(0),
    } };

    return expr;
}

/// Parses a label if possible, otherwise returns `null`
fn parseLabel(self: *Self) Error!?TokenIndex {
    if (self.match(.colon)) {
        if (!self.match(.identifier)) return self.errAtCurrent(.non_ident_label);
        return self.token_idx - 1;
    }

    return null;
}

fn closure(self: *Self) Error!*Expr {
    const opening = self.token_idx - 1;
    const expr = self.allocator.create(Expr) catch oom();
    const args = try self.fnParams(true);
    try self.expect(.pipe, .expect_closing_pipe);

    const return_type = try self.fnReturnType();
    self.skipNewLines();
    try self.expect(.left_brace, .expect_brace_before_fn_body);

    const body, _ = try self.block(null);

    expr.* = .{ .closure = .{
        .name = opening,
        .params = args,
        .body = body.block,
        .return_type = return_type,
        .has_callable = false,
        .is_closure = true,
    } };

    return expr;
}

fn ifExpr(self: *Self) Error!*Expr {
    const tk = self.token_idx - 1;

    const condition = condition: {
        const save_cond = self.ctx.setAndGetPrevious(.in_cond, true);
        const save_extract = self.ctx.setAndGetPrevious(.can_extract, true);
        defer {
            self.ctx.in_cond = save_cond;
            self.ctx.can_extract = save_extract;
        }

        break :condition try self.parsePrecedenceExpr(0);
    };

    self.skipNewLines();

    // TODO: Warning for unnecessary 'do' if there is a block after
    const then: Node = if (self.isAtBlock())
        .{ .expr = try self.blockExpr() }
    else if (self.matchAndSkip(.do))
        try self.declaration()
    else
        return self.errAtPrev(.{ .expect_block_or_do = .{ .what = "if" } });

    self.skipNewLines();

    // If we dosen't match an else, we go back one token to be able
    // to match the rule "after each statement there is a new line"
    // tested in the main caller
    const else_body: ?Node = if (self.matchAndSkip(.@"else"))
        if (self.matchAndSkip(.left_brace))
            .{ .expr = try self.blockExpr() }
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
        .if_token = tk,
    } };

    return expr;
}

fn leftParenExprStart(self: *Self) Error!*Expr {
    const start = self.token_idx - 1;
    const opening = self.prev(.span).start;
    self.skipNewLines();

    const save = self.ctx.in_group;
    defer self.ctx.in_group = save;
    self.ctx.in_group = true;

    const expr = self.allocator.create(Expr) catch oom();
    const value = try self.parsePrecedenceExpr(0);

    if (self.match(.right_paren)) {
        expr.* = .{ .grouping = .{
            .expr = value,
            .span = .{
                .start = opening,
                .end = self.current(.span).start,
            },
        } };
    }
    // Tuple
    else if (self.match(.comma)) {
        unreachable;
    } else return self.errAt(start, .unclosed_paren);

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
    // The check on 'else' allow syntax like: var a = if true do return else 4
    expr.* = .{ .@"return" = .{
        .expr = if (self.check(.new_line) or self.check(.right_brace) or self.check(.@"else"))
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

fn when(self: *Self) Error!*Expr {
    const kw = self.token_idx - 1;
    const expr = self.allocator.create(Expr) catch oom();

    const value = value: {
        const save_cond = self.ctx.setAndGetPrevious(.in_cond, true);
        defer self.ctx.in_cond = save_cond;
        break :value try self.parsePrecedenceExpr(0);
    };

    const alias = try self.getAlias();

    self.skipNewLines();
    try self.expectOrErrAtPrev(.left_brace, .expect_brace_before_when_body);
    const opening_brace = self.token_idx - 1;
    self.skipNewLines();

    var arms: ArrayList(Ast.When.Arm) = .empty;

    while (!self.check(.eof) and !self.check(.right_brace)) {
        arms.append(self.allocator, try self.whenArm()) catch oom();
        try self.expect(.new_line, .expect_new_line_pm_arm);
        self.skipNewLines();
    }

    try self.expectOrErrAtToken(.right_brace, .unclosed_brace, opening_brace);

    expr.* = .{ .when = .{
        .kw = kw,
        .expr = value,
        .alias = alias,
        .arms = arms.toOwnedSlice(self.allocator) catch oom(),
    } };

    return expr;
}

fn whenArm(self: *Self) Error!Ast.When.Arm {
    const ty = try self.parseType();
    const alias = try self.getAlias();
    try self.expect(.arrow_big, .expect_arrow_before_pm_arm_body);

    return .{
        .type = ty,
        .alias = alias,
        .body = try self.statement(),
    };
}

// Parses postfix expressions: calls, member access
fn postfix(self: *Self, prefixExpr: *Expr) Error!*Expr {
    var expr = prefixExpr;

    while (true) {
        // Field
        if (self.match(.dot)) {
            expr = try self.field(expr);
        }
        // Call
        else if (self.match(.left_paren)) {
            expr = try self.finishCall(expr);
        }
        // Array access
        else if (self.match(.left_bracket)) {
            expr = try self.arrayAccess(expr);
        }
        // Structure literal
        else if (self.check(.left_brace)) {
            if (self.ctx.in_cond and !self.ctx.in_group) {
                return expr;
            }
            self.token_idx += 1;

            // Can't chain them, break the loop
            return self.structLiteral(expr);
        }
        // Extractor
        else if (self.check(.extractor)) {
            if (!self.ctx.can_extract) return self.errAtCurrent(.invalid_extract_ctx);
            self.token_idx += 1;

            // Can't chain them, break the loop
            return self.extractor(expr);
        } else break;
    }

    return expr;
}

fn extractor(self: *Self, expr: *Expr) Error!*Expr {
    const res = self.allocator.create(Expr) catch oom();
    try self.expect(.identifier, .expect_name_after_extract);
    res.* = .{ .extractor = .{ .expr = expr, .alias = self.token_idx - 1 } };
    return res;
}

fn arrayAccess(self: *Self, expr: *Expr) Error!*Expr {
    const array_access = self.allocator.create(Expr) catch oom();
    const index = try self.parsePrecedenceExpr(0);
    try self.expect(.right_bracket, .missing_array_access_close_bracket);

    array_access.* = .{ .array_access = .{
        .array = expr,
        .index = index,
        .end = self.token_idx - 1,
    } };

    return array_access;
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

// Takes the callee expression as input and output the full function call expression
fn finishCall(self: *Self, expr: *Expr) Error!*Expr {
    const call_expr = self.allocator.create(Expr) catch oom();

    var args: ArrayList(Ast.FnCall.Arg) = .empty;
    args.ensureTotalCapacity(self.allocator, 256) catch oom();
    var named_started = false;

    // All the skip_lines cover the different syntaxes
    while (!self.check(.right_paren)) {
        self.skipNewLines();
        if (self.check(.eof)) return self.errAtPrev(.expect_paren_after_fn_args);

        if (args.items.len == 255)
            return self.errAtCurrent(.{ .too_many_fn_args = .{ .what = "argument" } });

        const param_name, const param_expr = if (self.check(.identifier) and self.token_tags[self.token_idx + 1] == .equal) b: {
            named_started = true;
            self.token_idx += 2;
            break :b .{ self.token_idx - 2, try self.parsePrecedenceExpr(0) };
        } else b: {
            if (named_started) return self.errAtCurrent(.positional_after_default_param);
            break :b .{ null, try self.parsePrecedenceExpr(0) };
        };

        args.appendAssumeCapacity(.{ .name = param_name, .value = param_expr });

        self.skipNewLines();
        if (!self.match(.comma)) break;
        self.skipNewLines();
    }

    call_expr.* = .{ .fn_call = .{
        .callee = expr,
        .args = args.toOwnedSlice(self.allocator) catch oom(),
    } };

    try self.expect(.right_paren, .expect_paren_after_fn_args);

    return call_expr;
}

fn structLiteral(self: *Self, expr: *Expr) Error!*Expr {
    self.skipNewLines();
    const struct_lit = self.allocator.create(Expr) catch oom();
    var fields_values: ArrayList(Ast.FieldAndValue) = .empty;

    check: {
        if (expr.* == .literal and expr.literal.tag == .identifier) break :check;
        if (expr.* == .field) break :check;

        return self.errAtPrev(.invalid_struct_literal);
    }

    // All the skip_lines cover the different syntaxes
    while (!self.check(.eof) and !self.check(.right_brace)) {
        if (!self.match(.identifier)) {
            return self.errAtCurrent(.struct_lit_non_ident_field);
        }
        const ident = self.token_idx - 1;
        self.skipNewLines();

        if (!self.check(.equal) and !self.check(.comma) and !self.check(.right_brace)) {
            return self.errAt(ident, .expect_equal_struct_lit);
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
