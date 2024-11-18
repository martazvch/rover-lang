const bufPrint = @import("std").fmt.bufPrint;
const Token = @import("frontend/lexer.zig").Token;
const IterList = @import("iter_list.zig").IterList;

pub const ErrorInfo = struct {
    msg: []const u8,
    hint: []const u8,
    help: ?[]const u8 = null,
};

pub const ErrKind = enum {
    // Lexer
    UnterminatedStr,
    UnexpectedChar,

    // Parser
    ChainingCmpOp,
    ExpectExpr,
    UnclosedParen,
    UnexpectedEof,

    // Compiler
    TooManyConst,

    pub fn get_infos(self: ErrKind) ErrorInfo {
        return switch (self) {
            // Lexer
            .UnterminatedStr => .{
                .msg = "unterminated string",
                .hint = "here",
                .help = "close the opening quote",
            },
            .UnexpectedChar => .{
                .msg = "unexpected character",
                .hint = "here",
            },

            // Parser
            .ChainingCmpOp => .{
                .msg = "chaining comparison operators",
                .hint = "this one is not allowed",
                .help = "split your comparison with 'and' and 'or' operators",
            },
            .ExpectExpr => .{
                .msg = "expected expression, found ",
                .hint = "here",
            },
            .UnclosedParen => .{
                .msg = "unclosed parenthesis",
                .hint = "here",
                .help = "close the opening parenthesis",
            },
            .UnexpectedEof => .{
                .msg = "unexpected end of file",
                .hint = "",
            },

            // Compiler
            .TooManyConst => .{
                .msg = "too many constant in this chunk (max 256)",
                .hint = "this one",
                .help = "try to split your code into smaller chunks",
            },
        };
    }

    /// Takes a buffer to print to. If the token kind requires extra informations,
    /// it will call next on the *IterList*
    /// Returns the number of bytes written
    pub fn extra(self: ErrKind, buf: []u8, items: *IterList([]const u8)) !?[]u8 {
        return switch (self) {
            .ExpectExpr => try bufPrint(buf, "'{s}'", .{items.next()}),
            else => null,
        };
    }
};
