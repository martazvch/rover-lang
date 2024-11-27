const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const expect = testing.expect;
const allocator = testing.allocator;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const AstPrinter = @import("ast_print.zig").AstPrinter;

pub fn run_test(source: [:0]const u8, expects: []const u8, errors: []const u8) !void {
    var lexer = Lexer.init(allocator);
    defer lexer.deinit();
    try lexer.lex(source);

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();
    try parser.parse(source, lexer.tokens.items);

    if (expects.len > 0) {
        var ast_printer = AstPrinter.init(allocator);
        defer ast_printer.deinit();
        try ast_printer.parse_ast(source, parser.stmts.items);

        expect(std.mem.eql(u8, ast_printer.tree.items, expects)) catch |e| {
            std.debug.print("expect:\n{s}\n", .{expects});
            std.debug.print("got:\n{s}\n", .{ast_printer.tree.items});
            return e;
        };
    } else if (errors.len > 0) {
        var errs = std.mem.splitScalar(u8, errors, '\n');
        const err = errs.next().?;

        const got_name = @tagName(parser.errs.items[0].report);
        expect(std.mem.eql(u8, err, got_name)) catch |e| {
            print("expect error: {s}, got {s}\n", .{ err, got_name });
            return e;
        };
    } else {
        print("Error, no expect and no erros in test\n", .{});
    }
}
