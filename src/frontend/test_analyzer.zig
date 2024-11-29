const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Analyzer = @import("analyzer.zig").Analyzer;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const GenTestData = @import("../tester.zig").GenTestData;

pub fn get_test_data(source: [:0]const u8, allocator: Allocator) !GenTestData(AnalyzerMsg) {
    var lexer = Lexer.init(allocator);
    defer lexer.deinit();
    try lexer.lex(source);

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();
    try parser.parse(source, lexer.tokens.items);

    var analyzer = Analyzer.init(allocator);
    defer analyzer.deinit();

    try analyzer.analyze(parser.stmts.items);

    var msgs = ArrayList(AnalyzerMsg).init(allocator);

    for (analyzer.errs.items) |err| {
        try msgs.append(err.report);
    }

    for (analyzer.warns.items) |err| {
        try msgs.append(err.report);
    }

    return .{ .expect = "", .reports = try msgs.toOwnedSlice() };
}
