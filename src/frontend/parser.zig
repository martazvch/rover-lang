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
            if (!self.check(.NewLine)) {
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
        while (self.check(.NewLine)) {
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

    /// Expecy a specific type, otherwise it's an error and we enter
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
                .Fn, .For, .If, .LeftBrace, .Print, .Return, .Struct, .Var, .While => return,
                else => self.advance(),
            }
        }
    }

    fn declaration(self: *Self) Error!Node.Index {
        const idx = try if (self.match(.Fn))
            self.fn_declaration()
        else if (self.match(.Var))
            self.var_declaration()
        else if (self.match(.Struct))
            self.struct_declaration()
        else if (self.match(.Return))
            self.return_expr()
        else if (self.match(.Underscore))
            self.discard()
        else if (self.match(.Use))
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

        try self.expect(.LeftParen, .ExpectParenAfterFnName);
        self.skip_new_lines();

        var arity: usize = 0;

        while (!self.check(.RightParen)) {
            if (self.check(.Eof)) {
                return self.error_at_prev(.ExpectParenAfterFnParams);
            }

            if (arity == 255) return self.error_at_current(
                .{ .TooManyFnArgs = .{ .what = "parameter" } },
            );

            try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "parameter" } });

            _ = try self.add_node(.{
                .tag = .Parameter,
                .main = self.token_idx - 1,
            });

            try self.expect(.Colon, .MissingFnParamType);
            _ = try self.parse_type();

            arity += 1;

            self.skip_new_lines();
            if (!self.match(.Comma)) break;
            self.skip_new_lines();
        }

        try self.expect(.RightParen, .ExpectParenAfterFnParams);

        self.nodes.items(.data)[idx] = arity;

        _ = if (self.match(.SmallArrow))
            try self.parse_type()
        else if (self.check(.Identifier) or self.check(.Bool) or self.check(.IntKw) or self.check(.FloatKw))
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
        const idx = try self.add_node(.{ .tag = .Return, .main = tk });

        _ = if (self.check(.NewLine) or self.check(.RightBrace))
            try self.add_node(.empty)
        else
            try self.parse_precedence_expr(0);

        return idx;
    }

    fn struct_declaration(self: *Self) !Node.Index {
        try self.expect(.Identifier, .ExpectStructName);
        try self.expect(.LeftBrace, .ExpectBraceBeforeStructBody);

        const idx = try self.add_node(.{
            .tag = .StructDecl,
            .main = self.token_idx - 2,
        });
        return idx;
    }

    fn var_declaration(self: *Self) !Node.Index {
        try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "variable" } });

        if (self.check(.Comma))
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

        while (self.match(.Comma)) : (count += 1) {
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
        if (!self.check(.Comma)) {
            for (variables.items) |var_idx| {
                _ = try self.add_node(.{ .tag = .VarDecl, .main = var_idx });
                _ = try self.add_node(self.nodes.get(type_idx));
                _ = try self.add_node(self.nodes.get(first_value));
            }
        } else while (self.match(.Comma)) : (count += 1) {
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
        return if (self.match(.Colon))
            try self.parse_type()
        else if (self.check(.Identifier))
            self.error_at_current(.ExpectColonBeforeType)
        else
            try self.add_node(.empty);
    }

    /// Parses a type. It assumes you know that a type is expected at this place
    fn parse_type(self: *Self) Error!Node.Index {
        if (self.is_ident_or_type()) {
            return self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
        } else if (self.match(.Fn)) {
            const idx = try self.add_node(.{ .tag = .Type, .main = self.token_idx - 1 });
            try self.expect(.LeftParen, .ExpectParenAfterFnName);

            var arity: usize = 0;
            while (!self.check(.RightParen) and !self.check(.Eof)) {
                // Parse parameters type
                _ = try self.parse_type();
                arity += 1;

                if (!self.match(.Comma)) break;
            }

            self.nodes.items(.data)[idx] = arity;
            try self.expect(.RightParen, .ExpectParenAfterFnParams);

            _ = if (self.match(.SmallArrow))
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
            self.match(.FloatKw) or
            self.match(.IntKw) or
            self.match(.StrKw) or
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
            .tag = .Use,
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
        if (self.match(.Print)) {
            return self.print_stmt();
        } else if (self.match(.While)) {
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
            .tag = .Print,
            .main = self.token_idx - 1,
        });

        _ = try self.parse_precedence_expr(0);
        self.nodes.items(.data)[idx] = self.nodes.len;

        return idx;
    }

    fn while_stmt(self: *Self) !Node.Index {
        const idx = try self.add_node(.{
            .tag = .While,
            .main = self.token_idx - 1,
        });
        _ = try self.parse_precedence_expr(0);

        _ = if (self.match_and_skip(.LeftBrace))
            try self.block_expr()
        else if (self.match_and_skip(.Do))
            try self.declaration()
        else
            return self.error_at_current(.{ .ExpectBraceOrDo = .{ .what = "while" } });

        return idx;
    }

    const Assoc = enum { Left, None };

    const Rule = struct { prec: i8, assoc: Assoc = .Left, tag: Node.Tag = .Empty };

    const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = -1 }, 0, .{
        .And = .{ .prec = 20, .tag = .And },
        .Or = .{ .prec = 20, .tag = .Or },

        .EqualEqual = .{ .prec = 30, .assoc = .None, .tag = .Eq },
        .BangEqual = .{ .prec = 30, .assoc = .None, .tag = .Ne },

        .Greater = .{ .prec = 40, .assoc = .None, .tag = .Gt },
        .GreaterEqual = .{ .prec = 40, .assoc = .None, .tag = .Ge },
        .Less = .{ .prec = 40, .assoc = .None, .tag = .Lt },
        .LessEqual = .{ .prec = 40, .assoc = .None, .tag = .Le },

        .Minus = .{ .prec = 60, .tag = .Sub },
        .Plus = .{ .prec = 60, .tag = .Add },

        .Slash = .{ .prec = 70, .tag = .Div },
        .Star = .{ .prec = 70, .tag = .Mul },
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

            if (next_rule.assoc == .None) banned_prec = next_rule.prec;
        }

        return node;
    }

    /// Parses expressions (prefix + sufix)
    fn parse_expr(self: *Self) Error!Node.Index {
        const start = self.nodes.len;

        // Prefix part
        _ = try switch (self.token_tags[self.token_idx - 1]) {
            .LeftBrace => self.block_expr(),
            .If => self.if_expr(),
            .Minus, .Not => self.unary_expr(),
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

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            _ = try self.declaration();
            self.skip_new_lines();
            length += 1;
        }

        try self.expect_or_err_at_tk(.RightBrace, .UnclosedBrace, openning_brace);

        self.nodes.items(.data)[idx] = length;

        return idx;
    }

    fn if_expr(self: *Self) Error!Node.Index {
        const idx = try self.add_node(.{
            .tag = .If,
            .main = self.token_idx - 1,
        });

        _ = try self.parse_precedence_expr(0);

        self.skip_new_lines();

        // TODO: Warning for unnecessary 'do' if there is a block after
        _ = if (self.match_and_skip(.LeftBrace))
            try self.block_expr()
        else if (self.match_and_skip(.Do))
            try self.declaration()
        else
            return self.error_at_prev(.{ .ExpectBraceOrDo = .{ .what = "if" } });

        self.skip_new_lines();

        // If we dosen't match an else, we go back one token to be able
        // to match the rule "after each statement there is a new line"
        // tested in the main caller
        if (self.match_and_skip(.Else)) {
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
            .False => self.bool_(),
            .Float => self.literal(.Float),
            .Identifier => self.literal(.Identifier),
            .Int => self.literal(.Int),
            .LeftParen => self.grouping(),
            .Null => self.literal(.Null),
            .String => self.literal(.String),
            .True => self.bool_(),
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
            if (self.match(.LeftParen)) {
                try self.finish_call(node);
            } else if (self.match(.Dot)) {
                // Member access
                unreachable;
            } else break;
        }

        return node;
    }

    // Takes the callee expression as input and output the full function call expression
    fn finish_call(self: *Self, node: Node.Index) Error!void {
        try self.nodes.insert(self.allocator, node, .{ .tag = .FnCall });

        var arity: usize = 0;

        // All the skip_lines cover the different syntaxes
        while (!self.check(.RightParen)) {
            self.skip_new_lines();
            if (self.check(.Eof)) return self.error_at_prev(.ExpectParenAfterFnArgs);

            if (arity == 255) return self.error_at_current(.{ .TooManyFnArgs = .{ .what = "argument" } });

            _ = try self.parse_precedence_expr(0);
            arity += 1;

            self.skip_new_lines();
            if (!self.match(.Comma)) break;
            self.skip_new_lines();
        }

        try self.expect(.RightParen, .ExpectParenAfterFnArgs);

        self.nodes.items(.data)[node] = arity;
    }
};
