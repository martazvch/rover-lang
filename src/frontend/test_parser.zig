const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ParserMsg = @import("parser_msg.zig").ParserMsg;
const AstPrinter = @import("ast_print.zig").AstPrinter;
const Tester = @import("../tester.zig");
const GenTestData = Tester.GenTestData;
const Config = Tester.Config;

pub fn get_test_data(
    source: [:0]const u8,
    allocator: Allocator,
    _: ?Config,
) !GenTestData(ParserMsg) {
    var lexer = Lexer.init(allocator);
    defer lexer.deinit();
    try lexer.lex(source);

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();

    var errs = ArrayList(ParserMsg).init(allocator);

    try parser.parse(
        source,
        lexer.tokens.items(.tag),
        lexer.tokens.items(.span),
    );

    var ast_printer = AstPrinter.init(
        allocator,
        source,
        lexer.tokens.items(.tag),
        lexer.tokens.items(.span),
        parser.nodes.items(.tag),
        parser.nodes.items(.main),
        parser.nodes.items(.data),
    );
    defer ast_printer.deinit();

    if (parser.errs.items.len > 0) {
        for (parser.errs.items) |err| {
            try errs.append(err.report);
        }
    } else try ast_printer.parse_ast();

    return .{ .expect = try ast_printer.tree.toOwnedSlice(), .reports = try errs.toOwnedSlice() };
}
