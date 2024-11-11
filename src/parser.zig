const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Stmt = @import("ast.zig").Stmt;
const Report = @import("reporter.zig").Report;
const Token = @import("lexer.zig").Token;
const TokenKind = @import("lexer.zig").TokenKind;

pub const Parser = struct {
    stmts: ArrayList(Stmt),
    errs: []const Report,
    allocator: Allocator,
    current: usize,
    tokens: []const Token,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .stmts = ArrayList(Stmt).init(allocator),
            .errs = undefined,
            .allocator = allocator,
            .current = 0,
            .tokens = undefined,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stmts.deinit();
    }

    pub fn parse(self: *Self, tokens: []const Token) !void {
        self.tokens = tokens;
    }

    /// Returns *true* if the current token is of the asked type and
    /// advance the `current` field to next one. Otherwise, returns *false*
    fn match(self: *Self, kind: TokenKind) bool {
        if (self.tokens[self.current].kind == kind) {
            self.current += 1;
            return true;
        }
        return false;
    }

    /// Expect a specific type, otherwise it's an error and we enter
    /// *panic* mode
    fn expect(self: *Self, kind: TokenKind, msg: []const u8) void {
        if (self.match(kind)) {} else {
            _ = msg;
        }
    }

    /// Checks if we currently are at a token
    fn check(self: *const Self, kind: TokenKind) bool {
        return self.tokens[self.current].kind == kind;
    }

    // fn error_at(self: *Self, loc: usize, msg: []const u8) void {
    //     const report = Report.err();
    // }
};
