const std = @import("std");
const eql = std.mem.eql;
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Lexer = @import("../frontend/lexer.zig").Lexer;
const Parser = @import("../frontend/parser.zig").Parser;
const Analyzer = @import("../frontend/analyzer.zig").Analyzer;
const OpCode = @import("chunk.zig").OpCode;
const CompilationManager = @import("compiler.zig").CompilationManager;
const CompilerMsg = @import("compiler_msg.zig").CompilerMsg;
const Disassembler = @import("disassembler.zig").Disassembler;
const Vm = @import("../runtime/vm.zig").Vm;
const Tester = @import("../tester.zig");
const GenTestData = Tester.GenTestData;
const Config = Tester.Config;

pub fn get_test_data(source: [:0]const u8, allocator: Allocator, config: ?Config) !GenTestData(CompilerMsg) {
    var lexer = Lexer.init(allocator);
    defer lexer.deinit();
    try lexer.lex(source);

    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();
    try parser.parse(source, lexer.tokens.items);

    var analyzer = Analyzer.init(allocator, true);
    try analyzer.type_manager.init_builtins();
    defer analyzer.deinit();
    try analyzer.analyze(parser.stmts.items, source);

    var vm = Vm.new(allocator);
    defer vm.deinit();
    try vm.init();

    var compiler = CompilationManager.init(
        &vm,
        parser.stmts.items,
        analyzer.analyzed_stmts.items,
        false,
        analyzer.main,
    );
    defer compiler.deinit();
    const function = try compiler.compile();

    if (config) |conf| {
        for (conf.ignores.items) |ignore| {
            // Ignores implicit return and the void value associated
            if (eql(u8, ignore, "return")) {
                try expect(@as(OpCode, @enumFromInt(function.chunk.code.pop())) == .Return);
                try expect(@as(OpCode, @enumFromInt(function.chunk.code.pop())) == .Null);
            }
        }
    }

    var disassembler = Disassembler.init(&function.chunk, allocator, true);
    defer disassembler.deinit();
    try disassembler.dis_chunk("main");

    var msgs = ArrayList(CompilerMsg).init(allocator);

    for (compiler.errs.items) |err| {
        try msgs.append(err.report);
    }

    return .{ .expect = try disassembler.disassembled.toOwnedSlice(), .reports = try msgs.toOwnedSlice() };
}
