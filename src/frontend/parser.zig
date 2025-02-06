const std = @import("std");
const tracy = @import("tracy");
const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;
const Ast = @import("ast.zig");
const Node = Ast.Node;
const NullNode = Ast.NullNode;
const Type = Ast.Type;
const TokenIndex = Ast.TokenIndex;
const GenReport = @import("../reporter.zig").GenReport;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const Token = @import("lexer.zig").Token;
const Span = @import("lexer.zig").Span;
const SourceSlice = Ast.SourceSlice;

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

pub const Parser = struct {
    source: []const u8,
    errs: ArrayList(ParserReport),
    arena: ArenaAllocator,
    allocator: Allocator,
    token_tags: []const Token.Tag,
    token_spans: []const Span,
    token_idx: usize,
    nodes: MultiArrayList(Node),
    flat_nodes: ArrayList(Node),
    // data: ArrayList(Node.Index),
    main_nodes: ArrayList(usize),
    panic_mode: bool,

    const Self = @This();
    const Error = error{err} || Allocator.Error || std.fmt.ParseIntError;

    pub const ParserReport = GenReport(ParserMsg);

    /// Initialize an instance of Parser. Use it as:
    /// ```
    /// var parser: Parser = undefined;
    /// parser.init(allocator);
    /// ```
    pub fn init(self: *Self, allocator: Allocator) void {
        self.arena = ArenaAllocator.init(allocator);
        self.allocator = self.arena.allocator();
        self.nodes = MultiArrayList(Node){};
        // self.data = ArrayList(Node.Index).init(self.allocator);
        self.main_nodes = ArrayList(usize).init(self.allocator);
        self.errs = ArrayList(ParserReport).init(self.allocator);
        self.flat_nodes = ArrayList(Node).init(self.allocator);
        self.token_idx = 0;
        self.panic_mode = false;
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
        // const zone = tracy.initZone(@src(), .{ .name = "Parsing" });
        // defer zone.deinit();

        self.source = source;
        self.token_tags = token_tags;
        self.token_spans = token_spans;

        // Saves slot 0 for null node
        _ = try self.add_node(undefined);

        self.skip_new_lines();

        while (!self.match(.Eof)) {
            // const stmt = self.declaration() catch |e| switch (e) {
            const node = self.declaration() catch |e| switch (e) {
                // If it's our own error, we continue on parsing
                Error.err => {
                    // If last error was Eof, exit the parser
                    if (self.errs.getLast().report == .UnexpectedEof) {
                        return;
                    }

                    self.synchronize();
                    continue;
                },
                else => return e,
            };

            // try self.stmts.append(stmt);

            // If NullNode is returned, it means it has already been added to main nodes
            if (node != NullNode) try self.main_nodes.append(node);

            // If EOF, exit
            if (self.match(.Eof)) break;

            // After each statements we expect a new line
            self.expect_or_err_at_tk(
                .NewLine,
                .ExpectNewLine,
                self.token_idx - 1,
            ) catch |e| switch (e) {
                // If it's our own error, we just synchronize before resuming
                error.err => self.synchronize(),
                else => return e,
            };

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

    /// Reserves a slot in the main nodes
    fn reserve_main(self: *Self) !Node.Index {
        try self.main_nodes.append(undefined);
        return self.main_nodes.items.len - 1;
    }

    /// Sets a main nodes at the given index. Returns the index back
    fn set_main(self: *Self, index: usize, node: Node.Index) void {
        self.main_nodes.items[index] = node;
    }

    fn reserve_node(self: *Self) !Node.Index {
        try self.flat_nodes.append(undefined);
        return self.flat_nodes.items.len - 1;
    }

    fn set_node(self: *Self, index: usize, node: Node) void {
        self.flat_nodes.items[index] = node;
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

    fn expect_or_err_at_span(
        self: *Self,
        kind: Token.Tag,
        error_kind: ParserMsg,
        span: Span,
    ) !void {
        if (self.match(kind)) return;

        return self.error_at_span(error_kind, span);
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

    fn error_at_span(self: *Self, error_kind: ParserMsg, span: Span) !void {
        if (self.panic_mode) return;

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
        if (self.match(.Fn)) {
            return self.fn_declaration();
        } else if (self.match(.Var)) {
            return self.var_declaration();
        } else if (self.match(.Underscore)) {
            return self.discard();
        } else if (self.match(.Use)) {
            return self.use();
        } else {
            return self.statement();
        }
    }

    fn fn_declaration(self: *Self) Error!Node.Index {
        try self.expect(.Identifier, .ExpectFnName);

        const idx = try self.add_node(.{
            .tag = .FnDecl,
            .main = self.token_idx - 1,
            .data = undefined,
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
                .data = undefined,
            });

            try self.expect(.Colon, .MissingFnParamType);
            _ = try self.extract_type();

            arity += 1;

            self.skip_new_lines();
            if (!self.match(.Comma)) break;
            self.skip_new_lines();
        }

        try self.expect(.RightParen, .ExpectParenAfterFnParams);

        self.nodes.items(.data)[idx].lhs = arity;

        _ = if (self.match(.SmallArrow))
            try self.extract_type()
        else if (self.check(.Identifier))
            return self.error_at_current(.ExpectArrowBeforeFnType)
        else
            try self.add_node(Node.Empty);

        self.skip_new_lines();
        try self.expect(.LeftBrace, .ExpectBraceBeforeFnBody);
        _ = try self.block_expr();

        // self.set_main(idx, try self.add_node(.{
        //     .tag = .FnDecl,
        //     .main = name,
        //     .data = .{ .lhs = arity, .rhs = undefined },
        // }));

        return idx;

        // return .{ .FnDecl = .{
        //     .name = SourceSlice.from_token(ident, self.source),
        //     .params = params,
        //     .arity = @intCast(arity),
        //     .body = body,
        //     .return_type = return_type,
        // } };
    }

    fn var_declaration(self: *Self) !Node.Index {
        try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "variable" } });
        // const ident = self.token_idx - 1;

        const idx = try self.add_node(.{
            .tag = .VarDecl,
            .main = self.token_idx - 1,
            .data = undefined,
        });
        // const var_name = try self.var_name_and_type("variable");
        try self.parse_type();

        // self.nodes.items(.main)[idx] = var_name;

        _ = if (self.match(.Equal))
            try self.parse_precedence_expr(0)
        else
            try self.add_node(Node.Empty);

        return idx;
        // return self.add_node(.{
        //     .tag = .VarDecl,
        //     .main = var_name,
        //     .data = .{ .lhs = var_infos.type_, .rhs = value },
        // });
        //
        // return .{
        //     .VarDecl = .{
        //         .name = var_infos.name,
        //         .is_const = is_const,
        //         .type_ = var_infos.type_,
        //         .value = value,
        //     },
        // };
    }

    /// Expects and declare a type. If none, declare an empty one
    fn parse_type(self: *Self) Error!void {
        _ = if (self.match(.Colon))
            try self.extract_type()
        else if (self.check(.Identifier))
            return self.error_at_current(.ExpectColonBeforeType)
        else
            try self.add_node(Node.Empty);
    }

    fn extract_type(self: *Self) Error!Node.Index {
        if (self.match(.FloatKw) or
            self.match(.IntKw) or
            self.match(.StrKw) or
            self.match(.Bool) or
            self.match(.Identifier))
        {
            return self.add_node(
                .{ .tag = .Type, .main = self.token_idx - 1, .data = undefined },
            );
            // } else if (self.match(.Fn)) {
            //     const start = self.prev().span.start;
            //     try self.expect(.LeftParen, .ExpectParenAfterFnName);
            //
            //     var param_types = ArrayList(SourceSlice).init(self.allocator);
            //
            //     while (!self.check(.RightParen)) {
            //         if (self.check(.Eof)) {
            //             return self.error_at_prev(.ExpectParenAfterFnParams);
            //         }
            //         // Parse parameters type
            //         try self.expect(.Identifier, .{ .ExpectName = .{ .kind = "parameter" } });
            //         // try param_types.append(try self.parse_type());
            //
            //         if (!self.match(.Comma)) break;
            //     }
            //
            //     try self.expect(.RightParen, .ExpectParenAfterFnParams);
            //
            //     // const return_type: ?Type = if (self.match(.SmallArrow))
            //     //     try self.parse_type()
            //     // else if (self.check(.Identifier))
            //     //     return self.error_at_current(.ExpectArrowBeforeFnType)
            //     // else
            //     //     null;
            //
            //     return .{
            //         .Function = .{
            //             .params = try param_types.toOwnedSlice(),
            //             // .return_type = return_type,
            //             .return_type = null,
            //             .start = start,
            //             // .span = .{ .start = start, .end = self.prev().span.end },
            //         },
            //     };
        } else {
            return self.error_at_current(.ExpectTypeName);
        }
    }

    fn discard(self: *Self) !Node.Index {
        try self.expect(.Equal, .InvalidDiscard);
        const idx = self.add_node(.{
            .tag = .Discard,
            .main = self.token_idx - 1,
            .data = undefined,
        });
        _ = try self.parse_precedence_expr(0);

        return idx;

        // return self.add_node(.{
        //     .tag = .Discard,
        //     .main = self.token_idx - 1,
        //     .data = .{
        //         .lhs = try self.parse_precedence_expr(0),
        //         .rhs = NullNode,
        //     },
        // });

        // return .{
        //     .Discard = .{
        //         .expr = try self.parse_precedence_expr(0),
        //     },
        // };
    }

    fn use(self: *Self) Error!Node.Index {
        const idx = try self.add_node(.{
            .tag = .Use,
            .main = self.token_idx - 1,
            .data = undefined,
        });

        if (!self.check(.Identifier)) {
            return self.error_at_current(.{ .ExpectName = .{ .kind = "module" } });
        }

        var count: usize = 0;
        while (self.match(.Identifier) and !self.check(.Eof)) {
            _ = try self.add_node(.{
                .tag = .Identifier,
                .main = self.token_idx - 1,
                .data = undefined,
            });

            count += 1;

            if (self.match(.Dot)) continue;
            break;
        }

        self.nodes.items(.data)[idx].lhs = count;

        return idx;

        // return self.add_node(.{
        //     .tag = .Use,
        //     .main = use_kw,
        //     .data = .{ .lhs = first, .rhs = self.nodes.len },
        // });
        // span.end = self.prev().span.end;

        // return .{ .Use = .{ .module = try list.toOwnedSlice(), .span = span } };
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

    fn print_stmt(self: *Self) Error!Node.Index {
        const idx = try self.add_node(.{
            .tag = .Print,
            .main = self.token_idx - 1,
            .data = undefined,
        });

        _ = try self.parse_precedence_expr(0);
        return idx;

        // return self.add_node(.{
        //     .tag = .Print,
        //     .main = self.token_idx - 1,
        //     .data = .{
        //         .lhs = try self.parse_precedence_expr(0),
        //         .rhs = undefined,
        //     },
        // });
        // return .{ .Print = .{ .expr = try self.parse_precedence_expr(0) } };
    }

    fn while_stmt(self: *Self) !Node.Index {
        const while_kw = self.token_idx - 1;
        const condition = try self.parse_precedence_expr(0);

        const body = if (self.match_and_skip(.LeftBrace))
            try self.block_expr()
        else if (self.match_and_skip(.Do))
            try self.declaration()
        else
            return self.error_at_current(.{ .ExpectBraceOrDo = .{ .what = "while" } });

        return self.add_node(.{
            .tag = .While,
            .main = while_kw,
            .data = .{ .lhs = condition, .rhs = body },
        });

        // const body = try self.allocator.create(Ast.Stmt);
        // body.* = if (self.match_and_skip(.LeftBrace))
        //     .{ .Expr = try self.block_expr() }
        // else if (self.match_and_skip(.Do))
        //     try self.declaration()
        // else
        //     return self.error_at_current(.{ .ExpectBraceOrDo = .{ .what = "while" } });

        // return .{ .While = .{
        //     .condition = condition,
        //     .body = body,
        // } };
    }

    fn assignment(self: *Self, assigne: Node.Index) !Node.Index {
        return self.add_node(.{
            .tag = .Assignment,
            .main = self.token_idx - 1,
            .data = .{
                .lhs = assigne,
                .rhs = try self.parse_precedence_expr(0),
            },
        });
        // return .{ .Assignment = .{
        //     .assigne = assigne,
        //     .value = try self.parse_precedence_expr(0),
        // } };
    }

    const Assoc = enum { Left, None };

    const Rule = struct { prec: i8, assoc: Assoc = .Left };

    const rules = std.enums.directEnumArrayDefault(Token.Tag, Rule, .{ .prec = -1 }, 0, .{
        .And = .{ .prec = 20 },
        .Or = .{ .prec = 20 },

        .EqualEqual = .{ .prec = 30, .assoc = .None },
        .BangEqual = .{ .prec = 30, .assoc = .None },

        .Greater = .{ .prec = 40, .assoc = .None },
        .GreaterEqual = .{ .prec = 40, .assoc = .None },
        .Less = .{ .prec = 40, .assoc = .None },
        .LessEqual = .{ .prec = 40, .assoc = .None },

        .Minus = .{ .prec = 60 },
        .Plus = .{ .prec = 60 },

        .Slash = .{ .prec = 70 },
        .Star = .{ .prec = 70 },
    });

    fn get_tag(token_tag: Token.Tag) Node.Tag {
        return switch (token_tag) {
            .Plus => .Add,
            .Minus => .Sub,
            .Star => .Mul,
            .Slash => .Div,
            else => unreachable,
        };
    }

    fn parse_precedence_expr(self: *Self, prec_min: i8) Error!Node.Index {
        const start = self.token_idx;
        // const idx = try self.reserve_main();
        // const idx = try self.reserve_node();

        self.advance();
        var node = try self.parse_expr();

        var banned_prec: i8 = -1;

        while (true) {
            // We check the current before consuming it
            const next_rule = rules[@as(usize, @intFromEnum(self.token_tags[self.token_idx]))];

            if (next_rule.prec < prec_min) break;

            if (next_rule.prec == banned_prec) {
                return self.error_at_current(.ChainingCmpOp);
            }

            // try self.add_node(self.nodes.get(self.nodes.len - 1));

            const op = self.token_idx;

            // If we are in a binop, we insert the node before the operand
            // There is no data, we just use the two next nodes
            try self.nodes.insert(self.allocator, self.nodes.len - 1, .{
                .tag = get_tag(self.token_tags[op]),
                .main = start,
                .data = undefined,
            });

            // Index of binop node
            node = self.nodes.len - 2;

            // Here, we can safely use it
            self.advance();
            // const rhs = try self.parse_precedence_expr(next_rule.prec + 1);
            _ = try self.parse_precedence_expr(next_rule.prec + 1);

            // node = try self.add_node(.{
            //     .tag = get_tag(self.token_tags[op]),
            //     .main = start,
            //     .data = .{ .lhs = node, .rhs = rhs },
            // });

            // self.set_node(idx, .{
            //     .tag = get_tag(self.token_tags[op]),
            //     .main = start,
            //     .data = .{ .lhs = node, .rhs = rhs },
            // });

            // const expr = try self.allocator.create(Expr);
            // expr.* = .{ .BinOp = .{
            //     .lhs = node,
            //     .rhs = rhs,
            //     .op = op,
            //     .span = .{ .start = node.span().start, .end = rhs.span().end },
            // } };
            //
            // node = expr;

            if (next_rule.assoc == .None) banned_prec = next_rule.prec;
        }

        return node;
        // self.set_main(idx, node);
        // self.set_node(idx, node);
        // return idx;
    }

    /// Parses expressions (prefix + sufix)
    fn parse_expr(self: *Self) Error!Node.Index {
        // Prefix part
        const expr = try switch (self.token_tags[self.token_idx - 1]) {
            .LeftBrace => self.block_expr(),
            // .If => self.if_expr(),
            .Minus, .Not => self.unary_expr(),
            .Return => self.return_expr(),
            else => self.parse_primary_expr(),
        };
        return expr;

        // Apply postfix on the prefix expression
        // return self.parse_postfix_expr(expr);
    }

    fn block_expr(self: *Self) Error!Node.Index {
        // const expr = try self.allocator.create(Expr);
        // expr.* = .{ .Block = try self.block() };
        // return expr;
        const openning_brace = self.token_idx - 1;

        self.skip_new_lines();
        // var span: Span = .{ .start = openning_brace.span.start, .end = 0 };

        // var stmts = ArrayList(Stmt).init(self.allocator);

        // const idx = try self.reserve_main();
        const idx = try self.add_node(
            .{ .tag = .Block, .main = openning_brace, .data = undefined },
        );

        // const start = self.main_nodes.items.len;
        var length: usize = 0;

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            // try stmts.append(try self.declaration());
            // try self.main_nodes.append(try self.declaration());
            _ = try self.declaration();
            self.skip_new_lines();
            length += 1;
        }

        try self.expect_or_err_at_tk(.RightBrace, .UnclosedBrace, openning_brace);

        // self.nodes.items(.data)[idx].lhs = self.nodes.len - start;
        self.nodes.items(.data)[idx].lhs = length;

        // self.set_main(idx, try self.add_node(.{
        //     .tag = .Block,
        //     .main = openning_brace,
        //     .data = .{ .lhs = self.main_nodes.items.len - start, .rhs = undefined },
        // }));

        return idx;
        // return NullNode;

        // span.end = self.prev().span.end;
        //
        // return .{
        //     .stmts = try stmts.toOwnedSlice(),
        //     .span = span,
        // };

    }

    // fn block(self: *Self) Error!Ast.Block {
    //     const openning_brace = self.prev();
    //
    //     self.skip_new_lines();
    //     var span: Span = .{ .start = openning_brace.span.start, .end = 0 };
    //
    //     var stmts = ArrayList(Stmt).init(self.allocator);
    //
    //     while (!self.check(.RightBrace) and !self.check(.Eof)) {
    //         try stmts.append(try self.declaration());
    //         self.skip_new_lines();
    //     }
    //
    //     try self.expect_or_err_at_tk(.RightBrace, .UnclosedBrace, &openning_brace);
    //
    //     span.end = self.prev().span.end;
    //
    //     return .{
    //         .stmts = try stmts.toOwnedSlice(),
    //         .span = span,
    //     };
    // }

    // fn if_expr(self: *Self) Error!*Expr {
    //     var span = self.prev().span;
    //     const condition = try self.parse_precedence_expr(0);
    //
    //     self.skip_new_lines();
    //
    //     // TODO: Warning for unnecessary 'do' if there is a block after
    //     const then_body: Ast.Stmt = if (self.match_and_skip(.LeftBrace))
    //         .{ .Expr = try self.block_expr() }
    //     else if (self.match_and_skip(.Do))
    //         try self.declaration()
    //     else
    //         return self.error_at_current(.{ .ExpectBraceOrDo = .{ .what = "if" } });
    //
    //     span.end = self.prev().span.end;
    //     self.skip_new_lines();
    //     var else_body: ?Stmt = null;
    //
    //     // If we dosen't match an else, we go back one token to be able
    //     // to match the rule "after each statement there is a new line"
    //     // tested in the main caller
    //     if (self.match_and_skip(.Else)) {
    //         if (self.match_and_skip(.LeftBrace)) {
    //             else_body = .{ .Expr = try self.block_expr() };
    //         } else else_body = try self.declaration();
    //
    //         span.end = self.prev().span.end;
    //     } else self.token_idx -= 1;
    //
    //     const expr = try self.allocator.create(Expr);
    //
    //     expr.* = .{ .If = .{
    //         .condition = condition,
    //         .then_body = then_body,
    //         .else_body = else_body,
    //         .span = span,
    //     } };
    //
    //     return expr;
    // }

    fn unary_expr(self: *Self) Error!Node.Index {
        const op = self.token_idx - 1;
        const idx = self.add_node(.{ .tag = .Unary, .main = op, .data = undefined });
        _ = try self.parse_precedence_expr(0);

        return idx;

        // return self.add_node(.{
        //     .tag = .Unary,
        //     .main = op,
        //     .data = undefined,
        //     // .data = .{ .lhs = try self.parse_precedence_expr(0), .rhs = undefined },
        // });

        // const expr = try self.allocator.create(Expr);
        //
        // const op = self.prev();
        // self.advance();
        //
        // // Recursion appens here
        // expr.* = .{ .Unary = .{
        //     .op = op.kind,
        //     .rhs = try self.parse_expr(),
        //     .span = .{
        //         .start = op.span.start,
        //         .end = self.current().span.start,
        //     },
        // } };
        //
        // return expr;
    }

    fn return_expr(self: *Self) Error!Node.Index {
        const tk = self.token_idx - 1;
        const idx = self.add_node(.{ .tag = .Return, .main = tk, .data = undefined });

        _ = if (self.check(.NewLine) or self.check(.RightBrace))
            try self.add_node(Node.Empty)
        else
            try self.parse_precedence_expr(0);

        return idx;

        // return self.add_node(.{
        //     .tag = .Return,
        //     .main = tk,
        //     .data = .{
        //         .lhs = if (self.check(.NewLine) or self.check(.RightBrace))
        //             NullNode
        //         else
        //             try self.parse_precedence_expr(0),
        //         .rhs = NullNode,
        //     },
        // });

        // const expr = try self.allocator.create(Expr);
        // const op = self.prev();
        //
        // // Checks cases like: fn add() { return }
        // expr.* = .{ .Return = .{
        //     .expr = if (self.check(.NewLine) or self.check(.RightBrace))
        //         null
        //     else
        //         try self.parse_precedence_expr(0),
        //     .span = .{ .start = op.span.start, .end = self.prev().span.end },
        // } };
        //
        // return expr;
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
            else => |k| {
                if (k == .Eof) {
                    return self.error_at_prev(.UnexpectedEof);
                } else {
                    const span = self.token_spans[self.token_idx - 1];
                    return self.error_at_prev(.{ .ExpectExpr = .{ .found = span.text(self.source) } });
                }
                unreachable;
            },
        };
    }

    fn grouping(self: *Self) Error!Node.Index {
        const opening = self.token_idx - 1;
        self.skip_new_lines();

        const idx = try self.add_node(
            .{ .tag = .Grouping, .main = opening, .data = undefined },
        );

        // const expr = try self.parse_precedence_expr(0);
        _ = try self.parse_precedence_expr(0);
        self.skip_new_lines();
        try self.expect_or_err_at_tk(.RightParen, .UnclosedParen, opening);

        return idx;
        //     return self.add_node(.{
        //         .tag = .Grouping,
        //         .main = opening,
        //         .data = .{ .lhs = expr, .rhs = undefined },
        //     });
    }

    fn bool_(self: *Self) Error!Node.Index {
        return self.add_node(.{
            .tag = .Bool,
            .main = self.token_idx - 1,
            .data = undefined,
        });
    }

    fn literal(self: *Self, tag: Node.Tag) Error!Node.Index {
        return self.add_node(.{
            .tag = tag,
            .main = self.token_idx - 1,
            .data = undefined,
        });
    }

    // / Parses postfix expressions: calls, member access
    // fn parse_postfix_expr(self: *Self, prefix_expr: *Expr) Error!*Expr {
    //     var expr: *Expr = prefix_expr;
    //
    //     while (true) {
    //         if (self.match(.LeftParen)) {
    //             expr = try self.finish_call(expr);
    //         } else if (self.match(.Dot)) {
    //             // Member access
    //             unreachable;
    //         } else break;
    //     }
    //
    //     return expr;
    // }

    // / Takes the callee expression as input and output the full function call expression
    // fn finish_call(self: *Self, expr: *Expr) Error!*Expr {
    //     const call_expr = try self.allocator.create(Expr);
    //
    //     var arity: usize = 0;
    //     var args: [256]*const Expr = undefined;
    //
    //     // All the skip_lines cover the different syntaxes
    //     while (!self.check(.RightParen)) {
    //         self.skip_new_lines();
    //         if (self.check(.Eof)) return self.error_at_prev(.ExpectParenAfterFnArgs);
    //
    //         if (arity == 255) return self.error_at_current(.{ .TooManyFnArgs = .{ .what = "argument" } });
    //
    //         args[arity] = try self.parse_precedence_expr(0);
    //         arity += 1;
    //
    //         self.skip_new_lines();
    //         if (!self.match(.Comma)) break;
    //         self.skip_new_lines();
    //     }
    //
    //     try self.expect(.RightParen, .ExpectParenAfterFnArgs);
    //
    //     call_expr.* = .{ .FnCall = .{
    //         .callee = expr,
    //         .args = args,
    //         .arity = arity,
    //         .span = .{ .start = expr.span().start, .end = self.prev().span.end },
    //     } };
    //
    //     return call_expr;
    // }
};

// Tests
test Parser {
    const GenericTester = @import("../tester.zig").GenericTester;
    const get_test_data = @import("test_parser.zig").get_test_data;

    const Tester = GenericTester("parser", ParserMsg, get_test_data);
    try Tester.run();
}
