const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Analyzer = @import("analyzer.zig").Analyzer;
const AnalyzerMsg = @import("analyzer_msg.zig").AnalyzerMsg;
const Tester = @import("../tester.zig");
const GenTestData = Tester.GenTestData;
const Config = Tester.Config;
const RirRenderer = @import("rir_renderer.zig").RirRenderer;

pub fn get_test_data(
    source: [:0]const u8,
    allocator: Allocator,
    config: ?Config,
) !GenTestData(AnalyzerMsg) {
    var lexer = Lexer.init(allocator);
    defer lexer.deinit();
    try lexer.lex(source);

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();

    try parser.parse(
        source,
        lexer.tokens.items(.tag),
        lexer.tokens.items(.span),
    );

    var analyzer: Analyzer = undefined;
    try analyzer.init(allocator, false);
    defer analyzer.deinit();

    try analyzer.analyze(source, &lexer.tokens, &parser.nodes);
    var msgs = ArrayList(AnalyzerMsg).init(allocator);

    for (analyzer.errs.items) |err| {
        try msgs.append(err.report);
    }

    for (analyzer.warns.items) |err| {
        if (config) |conf| {
            for (conf.ignores.items) |ignore| {
                if (!std.mem.eql(u8, ignore, @tagName(err.report))) {
                    try msgs.append(err.report);
                }
            }
        } else {
            try msgs.append(err.report);
        }
    }

    var rir_renderer = RirRenderer.init(
        allocator,
        source,
        analyzer.instructions.items(.tag),
        analyzer.instructions.items(.data),
        &analyzer.interner,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parse_ir();

    return .{ .expect = try rir_renderer.tree.toOwnedSlice(), .reports = try msgs.toOwnedSlice() };
}
