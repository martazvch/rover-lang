const std = @import("std");
const assert = std.debug.assert;
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const MultiArrayList = std.MultiArrayList;

const GenReport = @import("../reporter.zig").GenReport;
const Ast = @import("ast.zig");
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const Span = @import("lexer.zig").Span;
const Token = @import("lexer.zig").Token;

pub const Parser = struct {
    source: []const u8,
    errs: ArrayList(ParserReport),
    arena: ArenaAllocator,
    allocator: Allocator,
    token_tags: []const Token.Tag,
    token_spans: []const Span,
    token_idx: usize,
    nodes: MultiArrayList(Node),
    panic_mode: bool,

    const Self = @This();
    const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;

    pub const ParserReport = GenReport(ParserMsg);
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
    pub fn parse(
        self: *Self,
        source: [:0]const u8,
        token_tags: []const Token.Tag,
        token_spans: []const Span,
    ) !void {
        self.source = source;
        self.token_tags = token_tags;
        self.token_spans = token_spans;

        self.skip_new_lines();

        while (!self.match(.Eof)) {
            _ = self.declaration() catch |e| switch (e) {
                // If it's our own error, we continue on parsing
                Error.err => {
                    self.synchronize();
                    continue;
                },
                else => return e,
            };

            // If EOF, exit
            if (self.match(.Eof)) break;

            // After each statements we expect a new line
            if (!self.check(.newLine)) {
                // Could be that user wrote: Foo{} instead of Foo.{}, it's identifier + block
                // without a new line instead of structure literal
                if (self.token_tags[self.token_idx - 1] == .Identifier and self.token_tags[self.token_idx] == .LeftBrace) {
                    std.debug.print("Could be trying to do structure literal? Missing '.'\n", .{});
                    // TODO: Error
                }

                const start = self.token_spans[self.token_idx - 1].end;

                self.error_at_span(
                    .{ .start = start, .end = start + 1 },
                    .ExpectNewLine,
                ) catch {};

                self.synchronize();
            }

            self.skip_new_lines();
        }
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

    fn match_and_skip(self: *Self, kind: Token.Tag) bool {
        const res = self.match(kind);
        self.skip_new_lines();
        return res;
    }

    /// Checks if we currently are at a token
    fn check(self: *const Self, kind: Token.Tag) bool {
        return self.token_tags[self.token_idx] == kind;
    }

    /// Adds a new node and returns its index
    fn add_node(self: *Self, node: Node) !Node.Index {
        try self.nodes.append(self.allocator, node);
        return self.nodes.len - 1;
    }

    fn finish_node(self: *Self, index: Node.Index) Node.Index {
        self.nodes.items(.end)[index] = self.nodes.len;
        return index;
    }

    fn skip_new_lines(self: *Self) void {
        while (self.check(.newLine)) {
            self.advance();
        }
    }

    /// Expect a specific type, otherwise it's an error and we enter
    /// *panic* mode
    fn expect(self: *Self, kind: Token.Tag, error_kind: ParserMsg) !void {
        if (self.match(kind)) {
            return;
        }

        return self.error_at_current(error_kind);
    }

    /// Expect a specific type, otherwise it's an error and we enter
    /// *panic* mode. Error will mark the previous token
    fn expect_or_err_at_prev(self: *Self, kind: Token.Tag, error_kind: ParserMsg) !void {
        return self.expect_or_err_at_tk(kind, error_kind, self.token_idx - 1);
    }

    /// Expect a specific token tag, otherwise it's an error and we enter
    /// *panic* mode. Allow to pass a token to be marked as initial error
    /// (usefull for example when unclosed parenthesis, we give the first)
    fn expect_or_err_at_tk(
        self: *Self,
        kind: Token.Tag,
        error_kind: ParserMsg,
        tk: TokenIndex,
    ) !void {
        if (self.match(kind)) return;

        return self.error_at(tk, error_kind);
    }

    fn error_at_current(self: *Self, error_kind: ParserMsg) Error {
        return self.error_at(self.token_idx, error_kind);
    }

    fn error_at_prev(self: *Self, error_kind: ParserMsg) Error {
        return self.error_at(self.token_idx - 1, error_kind);
    }

    /// If error already encountered and in the same statement parsing,
    /// we exit, let synchronize and resume. It is likely that if we
    /// are already in panic mode, the following errors are just
    /// consequencies of actual bad statement
    fn error_at(self: *Self, token: TokenIndex, error_kind: ParserMsg) Error {
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
    fn error_at_span(self: *Self, span: Span, error_kind: ParserMsg) Error {
        if (self.panic_mode) return error.err;

        self.panic_mode = true;

        const report = ParserReport.err(error_kind, span);
        try self.errs.append(report);

        return error.err;
    }

    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (!self.check(.Eof)) {
            switch (self.token_tags[self.token_idx]) {
                .@"fn", .@"for", .@"if", .LeftBrace, .print, .@"return", .@"struct", .@"var", .@"while" => return,
                else => self.advance(),
            }
        }
    }

    fn declaration(self: *Self) Error!Node.Index {
        const idx = try if (self.match(.@"fn"))
            self.fn_declaration()
        else if (self.match(.@"var"))
            self.var_declaration()
        else if (self.match(.@"struct"))
            self.struct_declaration()
        else if (self.match(.@"return"))
            self.return_expr()
        else if (self.match(.Underscore))
            self.discard()
        else if (self.match(.use))
            self.use()
        else
            self.statement();

        return self.finish_node(idx);
    }

    fn fn_declaration(self: *Self) Error!Node.Index {
        try self.expect(.Identifier, .ExpectFnName);

        const idx = try self.add_node(.{
            .tag = .FnDecl,
            .main = self.token_idx - 1,
        });

        try self.expect(.leftParen, .ExpectParenAfterFnName);
        self.skip_new_lines();

        var arity: usize = 0;

        while (!self.check(.RightParen)) {
            if (self.check(.Eof)) {
                return self.error_at_prev(.ExpectParenAfterFnParams);
            }

            if (arity == 255) return self.error_at_current(
                .{ .TooManyFnArgs = .{ .what = "parameter" } },
            );

            if (self.match(.self)) {
                if (arity > 0) {
                    return self.error_at_prev(.SelfAsNonFirstParam);
                }

                if (self.match(.colon)) {
                    return self.error_at_current(.TypedSelf);
                }

                _ = try self.add_node(.{ .tag = .self, .main = self.token_idx - 1 });
            } else {
                try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "parameter" } });
                _ = try self.add_node(.{ .tag = .Parameter, .main = self.token_idx - 1 });
                try self.expect(.colon, .MissingFnParamType);
                _ = try self.parse_type();
            }

            arity += 1;

            self.skip_new_lines();
            if (!self.match(.comma)) break;
            self.skip_new_lines();
        }

        try self.expect(.RightParen, .ExpectParenAfterFnParams);

        self.nodes.items(.data)[idx] = arity;

        _ = if (self.match(.smallArrow))
            try self.parse_type()
        else if (self.check(.Identifier) or self.check(.Bool) or self.check(.intKw) or self.check(.floatKw))
            return self.error_at_current(.ExpectArrowBeforeFnType)
        else
            try self.add_node(.empty);

        self.skip_new_lines();
        try self.expect(.LeftBrace, .ExpectBraceBeforeFnBody);
        _ = try self.block_expr();

        return idx;
    }

    fn return_expr(self: *Self) Error!Node.Index {
        const tk = self.token_idx - 1;
        const idx = try self.add_node(.{ .tag = .@"return", .main = tk });

        _ = if (self.check(.newLine) or self.check(.rightBrace))
            try self.add_node(.empty)
        else
            try self.parse_precedence_expr(0);

        return idx;
    }

    fn struct_declaration(self: *Self) !Node.Index {
        try self.expect(.Identifier, .ExpectStructName);
        const name_idx = self.token_idx - 1;
        self.skip_new_lines();
        try self.expect_or_err_at_prev(.LeftBrace, .ExpectBraceBeforeStructBody);
        self.skip_new_lines();

        const idx = try self.add_node(.{
            .tag = .StructDecl,
            .main = name_idx,
        });

        // Fields
        const fields = try self.add_node(.{ .tag = .count });
        var fields_count: usize = 0;

        // If at least one field
        while (!self.check(.@"fn") and !self.check(.rightBrace) and !self.check(.Eof)) {
            var valid = false;
            _ = try self.expect(.Identifier, .ExpectFieldName);
            _ = try self.add_node(.{ .tag = .field, .main = self.token_idx - 1 });

            if (self.match(.colon)) {
                _ = try self.parse_type();
                valid = true;
            } else _ = try self.add_node(.empty);

            if (self.match(.Equal)) {
                valid = true;
                _ = try self.parse_precedence_expr(0);
            } else _ = try self.add_node(.empty);

            if (!valid) {
                return self.error_at_current(.ExpectFieldTypeOrDefault);
            }

            fields_count += 1;

            self.skip_new_lines();
            if (!self.match(.comma)) break;
            self.skip_new_lines();
        }

        // If we are at an identifier, might be a missing comma between fields
        if (self.check(.Identifier)) {
            return self.error_at_prev(.MissingCommaAfterField);
        }

        self.nodes.items(.data)[fields] = fields_count;

        // Functions
        const func = try self.add_node(.{ .tag = .count });
        var fn_count: usize = 0;

        while (!self.check(.rightBrace) and !self.check(.Eof)) {
            _ = try self.expect(.@"fn", .ExpectFnInStructBody);
            _ = try self.fn_declaration();
            self.skip_new_lines();
            fn_count += 1;
        }
        self.nodes.items(.data)[func] = fn_count;

        try self.expect_or_err_at_prev(.rightBrace, .ExpectBraceAfterStructBody);

        return idx;
    }

    fn var_declaration(self: *Self) !Node.Index {
        try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "variable" } });

        if (self.check(.comma))
            return self.multi_var_decl();

        const idx = try self.add_node(.{
            .tag = .VarDecl,
            .main = self.token_idx - 1,
        });

        _ = try self.expect_type_or_empty();

        _ = if (self.match(.Equal))
            try self.parse_precedence_expr(0)
        else
            try self.add_node(.empty);

        return idx;
    }

    fn multi_var_decl(self: *Self) !Node.Index {
        const idx = try self.add_node(.{ .tag = .MultiVarDecl });

        _ = try self.add_node(.{ .tag = .VarDecl, .main = self.token_idx - 1 });

        var count: usize = 1;
        var variables = std.ArrayListUnmanaged(usize){};
        defer variables.deinit(self.allocator);

        while (self.match(.comma)) : (count += 1) {
            try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "variable" } });
            try variables.append(self.allocator, self.token_idx - 1);
        }
        self.nodes.items(.main)[idx] = count;

        const type_idx = try self.expect_type_or_empty();

        const first_value = if (self.match(.Equal))
            try self.parse_precedence_expr(0)
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
            _ = try self.parse_precedence_expr(0);
        }

        if (count > 1 and count != variables.items.len + 1)
            return self.error_at(variables.items[count - 1], .{ .WrongValueCountVarDecl = .{
                .expect = variables.items.len + 1,
            } });

        self.nodes.items(.data)[idx] = count;

        return idx;
    }

    /// Expects a type after ':'. If no colon, declares an empty type
    fn expect_type_or_empty(self: *Self) Error!Node.Index {
        return if (self.match(.colon))
            try self.parse_type()
        else if (self.check(.Identifier))
            self.error_at_current(.ExpectcolonBeforeType)
        else
            try self.add_node(.empty);
    }

    /// Parses a type. It assumes you know that a type is expected at this place
    fn parse_type(self: *Self) Error!Node.Index {
        if (self.is_ident_or_type()) {
            return self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
        } else if (self.match(.@"fn")) {
            const idx = try self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
            try self.expect(.leftParen, .ExpectParenAfterFnName);

            var arity: usize = 0;
            while (!self.check(.RightParen) and !self.check(.Eof)) {
                // Parse parameters type
                _ = try self.parse_type();
                arity += 1;

                if (!self.match(.comma)) break;
            }

            self.nodes.items(.data)[idx] = arity;
            try self.expect(.RightParen, .ExpectParenAfterFnParams);

            _ = if (self.match(.smallArrow))
                try self.parse_type()
            else if (self.is_ident_or_type())
                return self.error_at_current(.ExpectArrowBeforeFnType)
            else
                try self.add_node(.empty);

            return idx;
        } else {
            return self.error_at_current(.ExpectTypeName);
        }
    }

    fn is_ident_or_type(self: *Self) bool {
        return if (self.match(.Identifier) or
            self.match(.floatKw) or
            self.match(.intKw) or
            self.match(.strKw) or
            self.match(.Bool))
            true
        else
            false;
    }

    fn discard(self: *Self) !Node.Index {
        try self.expect(.Equal, .InvalidDiscard);
        const idx = try self.add_node(.{
            .tag = .Discard,
            .main = self.token_idx - 1,
        });

        _ = try self.parse_precedence_expr(0);

        return idx;
    }

    fn use(self: *Self) Error!Node.Index {
        const idx = try self.add_node(.{
            .tag = .use,
            .main = self.token_idx - 1,
        });

        if (!self.check(.Identifier)) {
            return self.error_at_current(.{ .ExpectName = .{ .kind = "module" } });
        }

        var count: usize = 0;
        while (self.match(.Identifier) and !self.check(.Eof)) {
            _ = try self.add_node(.{
                .tag = .Identifier,
                .main = self.token_idx - 1,
            });

            count += 1;

            if (self.match(.Dot)) continue;
            break;
        }

        self.nodes.items(.data)[idx] = count;

        return idx;
    }

    fn statement(self: *Self) !Node.Index {
        if (self.match(.print)) {
            return self.print_stmt();
        } else if (self.match(.@"while")) {
            return self.while_stmt();
        } else {
            const assigne = try self.parse_precedence_expr(0);

            if (self.match(.Equal)) {
                return self.assignment(assigne);
            } else return assigne;
        }
    }

    fn assignment(self: *Self, assigne: Node.Index) !Node.Index {
        // Converts the previous expression to an assignment
        try self.nodes.insert(self.allocator, assigne, .{ .tag = .Assignment });

        _ = try self.parse_precedence_expr(0);

        return assigne;
    }

    fn print_stmt(self: *Self) Error!Node.Index {
        const idx = try self.add_node(.{
            .tag = .print,
            .main = self.token_idx - 1,
        });

        _ = try self.parse_precedence_expr(0);
        self.nodes.items(.data)[idx] = self.nodes.len;

        return idx;
    }

    fn while_stmt(self: *Self) !Node.Index {
        const idx = try self.add_node(.{
            .tag = .@"while",
            .main = self.token_idx - 1,
        });
        _ = try self.parse_precedence_expr(0);

        _ = if (self.match_and_skip(.LeftBrace))
            try self.block_expr()
        else if (self.match_and_skip(.do))
            try self.declaration()
        else
            return self.error_at_current(.{ .ExpectBraceOrDo = .{ .what = "while" } });

        return idx;
    }

    const Assoc = enum { Left, none };

    const Rule = struct { prec: i8, assoc: Assoc = .Left, tag: Node.Tag = .Empty };

    const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = -1 }, 0, .{
        .@"and" = .{ .prec = 20, .tag = .@"and" },
        .@"or" = .{ .prec = 20, .tag = .@"or" },

        .EqualEqual = .{ .prec = 30, .assoc = .none, .tag = .Eq },
        .bangEqual = .{ .prec = 30, .assoc = .none, .tag = .Ne },

        .Greater = .{ .prec = 40, .assoc = .none, .tag = .Gt },
        .GreaterEqual = .{ .prec = 40, .assoc = .none, .tag = .Ge },
        .less = .{ .prec = 40, .assoc = .none, .tag = .Lt },
        .lessEqual = .{ .prec = 40, .assoc = .none, .tag = .Le },

        .minus = .{ .prec = 60, .tag = .Sub },
        .plus = .{ .prec = 60, .tag = .Add },

        .slash = .{ .prec = 70, .tag = .Div },
        .star = .{ .prec = 70, .tag = .Mul },
    });

    fn parse_precedence_expr(self: *Self, prec_min: i8) Error!Node.Index {
        const start_tk = self.token_idx;
        const start_node = self.nodes.len;

        self.advance();
        const node = try self.parse_expr();

        var banned_prec: i8 = -1;

        while (true) {
            // We check the current before consuming it
            const next_rule = rules[@as(usize, @intFromEnum(self.token_tags[self.token_idx]))];

            if (next_rule.prec < prec_min) break;

            if (next_rule.prec == banned_prec) {
                return self.error_at_current(.ChainingCmpOp);
            }

            // If we are in a binop, we insert the node before the operand
            // There is no data, we just use the two next nodes
            try self.nodes.insert(self.allocator, start_node, .{
                .tag = next_rule.tag,
                .main = start_tk,
            });

            // Here, we can safely use it
            self.advance();
            _ = try self.parse_precedence_expr(next_rule.prec + 1);

            if (next_rule.assoc == .none) banned_prec = next_rule.prec;
        }

        return node;
    }

    /// Parses expressions (prefix + sufix)
    fn parse_expr(self: *Self) Error!Node.Index {
        const start = self.nodes.len;

        // Prefix part
        _ = try switch (self.token_tags[self.token_idx - 1]) {
            .LeftBrace => self.block_expr(),
            .@"if" => self.if_expr(),
            .minus, .not => self.unary_expr(),
            else => self.parse_primary_expr(),
        };

        // Apply postfix on the prefix expression
        return self.parse_postfix_expr(start);
    }

    fn block_expr(self: *Self) Error!Node.Index {
        const openning_brace = self.token_idx - 1;

        self.skip_new_lines();
        const idx = try self.add_node(.{ .tag = .Block, .main = openning_brace });
        var length: usize = 0;

        while (!self.check(.rightBrace) and !self.check(.Eof)) {
            _ = try self.declaration();
            self.skip_new_lines();
            length += 1;
        }

        try self.expect_or_err_at_tk(.rightBrace, .UnclosedBrace, openning_brace);

        self.nodes.items(.data)[idx] = length;

        return idx;
    }

    fn if_expr(self: *Self) Error!Node.Index {
        const idx = try self.add_node(.{
            .tag = .@"if",
            .main = self.token_idx - 1,
        });

        _ = try self.parse_precedence_expr(0);
        self.skip_new_lines();

        // TODO: Warning for unnecessary 'do' if there is a block after
        _ = if (self.match_and_skip(.LeftBrace))
            try self.block_expr()
        else if (self.match_and_skip(.do))
            try self.declaration()
        else
            return self.error_at_prev(.{ .ExpectBraceOrDo = .{ .what = "if" } });

        self.skip_new_lines();

        // If we dosen't match an else, we go back one token to be able
        // to match the rule "after each statement there is a new line"
        // tested in the main caller
        if (self.match_and_skip(.@"else")) {
            _ = if (self.match_and_skip(.LeftBrace))
                try self.block_expr()
            else
                try self.declaration();
        } else {
            self.token_idx -= 1;
            _ = try self.add_node(.empty);
        }

        return idx;
    }

    fn unary_expr(self: *Self) Error!Node.Index {
        const op = self.token_idx - 1;
        const idx = self.add_node(.{ .tag = .Unary, .main = op });
        self.advance();
        _ = try self.parse_expr();

        return idx;
    }

    fn parse_primary_expr(self: *Self) Error!Node.Index {
        return switch (self.token_tags[self.token_idx - 1]) {
            .false => self.bool_(),
            .Float => self.literal(.Float),
            .Identifier => self.literal(.Identifier),
            .int => self.literal(.int),
            .leftParen => self.grouping(),
            .null => self.literal(.null),
            .string => self.literal(.string),
            .true => self.bool_(),
            else => {
                const span = self.token_spans[self.token_idx - 1];
                return self.error_at_prev(.{ .ExpectExpr = .{ .found = span.text(self.source) } });
            },
        };
    }

    fn grouping(self: *Self) Error!Node.Index {
        const opening = self.token_idx - 1;
        self.skip_new_lines();

        const idx = try self.add_node(.{ .tag = .Grouping, .main = opening });

        if (!self.check(.RightParen)) {
            _ = try self.parse_precedence_expr(0);
            self.skip_new_lines();
        } else _ = try self.add_node(.{ .tag = .Empty });

        try self.expect_or_err_at_tk(.RightParen, .UnclosedParen, opening);

        // Closing parenthesis for error reporting
        self.nodes.items(.data)[idx] = self.token_idx - 1;

        return idx;
    }

    fn bool_(self: *Self) Error!Node.Index {
        return self.add_node(.{
            .tag = .Bool,
            .main = self.token_idx - 1,
        });
    }

    fn literal(self: *Self, tag: Node.Tag) Error!Node.Index {
        return self.add_node(.{
            .tag = tag,
            .main = self.token_idx - 1,
        });
    }

    // Parses postfix expressions: calls, member access
    fn parse_postfix_expr(self: *Self, node: Node.Index) Error!Node.Index {
        while (true) {
            if (self.match(.leftParen)) {
                try self.finish_call(node);
            } else if (self.match(.Dot)) {
                if (self.match(.LeftBrace)) {
                    try self.struct_literal(node);
                } else {
                    try self.field(node);
                }
            } else break;
        }

        return node;
    }

    // Takes the callee expression as input and output the full function call expression
    fn finish_call(self: *Self, node: Node.Index) Error!void {
        try self.nodes.insert(self.allocator, node, .{ .tag = .call });
        var arity: usize = 0;

        // All the skip_lines cover the different syntaxes
        while (!self.check(.RightParen)) {
            self.skip_new_lines();
            if (self.check(.Eof)) return self.error_at_prev(.ExpectParenAfterFnArgs);

            if (arity == 255) return self.error_at_current(.{ .TooManyFnArgs = .{ .what = "argument" } });

            _ = try self.parse_precedence_expr(0);
            arity += 1;

            self.skip_new_lines();
            if (!self.match(.comma)) break;
            self.skip_new_lines();
        }

        try self.expect(.RightParen, .ExpectParenAfterFnArgs);

        self.nodes.items(.data)[node] = arity;
    }

    fn field(self: *Self, node: Node.Index) Error!void {
        try self.nodes.insert(self.allocator, node, .{ .tag = .field });
        try self.expect(.Identifier, .ExpectNameAfterDot);
        _ = try self.literal(.Identifier);
    }

    fn struct_literal(self: *Self, node: Node.Index) Error!void {
        try self.nodes.insert(self.allocator, node, .{ .tag = .struct_literal });
        var arity: usize = 0;

        // All the skip_lines cover the different syntaxes
        while (!self.check(.rightBrace)) {
            self.skip_new_lines();
            if (self.check(.Eof)) return self.error_at_prev(.ExpectBraceAfterStructLit);

            if (!self.match(.Identifier)) {
                return self.error_at_current(.StructLitNonIdentField);
            }

            _ = try self.add_node(.{ .tag = .Identifier, .main = self.token_idx - 1 });

            // Either: { x = 3 }  or { x }, if x is a local variable
            if (self.match(.Equal)) {
                _ = try self.parse_precedence_expr(0);
            } else _ = try self.add_node(.empty);

            arity += 1;

            self.skip_new_lines();
            if (!self.match(.comma)) break;
            self.skip_new_lines();
        }

        try self.expect(.rightBrace, .ExpectBraceAfterStructLit);

        self.nodes.items(.data)[node] = arity;
    }
};
