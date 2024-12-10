const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Lexer = @import("../frontend/lexer.zig").Lexer;
const Parser = @import("../frontend/parser.zig").Parser;
const Analyzer = @import("../frontend/analyzer.zig").Analyzer;
const Compiler = @import("compiler.zig").Compiler;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const Disassembler = @import("disassembler.zig").Disassembler;
const Vm = @import("../runtime/vm.zig").Vm;
const GenTestData = @import("../tester.zig").GenTestData;

pub fn get_test_data(source: [:0]const u8, allocator: Allocator) !GenTestData(CompilerMsg) {
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

    var vm = Vm.new(allocator);
    defer vm.deinit();
    try vm.init();

    var compiler = Compiler.init(&vm, analyzer.analyzed_stmts.items);
    defer compiler.deinit();
    try compiler.compile(parser.stmts.items);

    var disassembler = Disassembler.init(&compiler.chunk, allocator, true);
    defer disassembler.deinit();
    try disassembler.dis_chunk("main");

    var msgs = ArrayList(CompilerMsg).init(allocator);

    for (compiler.errs.items) |err| {
        try msgs.append(err.report);
    }

    return .{ .expect = try disassembler.disassembled.toOwnedSlice(), .reports = try msgs.toOwnedSlice() };
}
