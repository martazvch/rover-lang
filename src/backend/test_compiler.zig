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
const ObjFunction = @import("../runtime/obj.zig").ObjFunction;

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
            // Ignores the call the main function, it corresponds to those last code
            if (eql(u8, ignore, "main-call")) {
                _ = function.chunk.code.pop();
                try expect(@as(OpCode, @enumFromInt(function.chunk.code.pop())) == .FnCall);
                _ = function.chunk.code.pop();
                try expect(@as(OpCode, @enumFromInt(function.chunk.code.pop())) == .GetGlobal);
                _ = function.chunk.code.pop();
                try expect(@as(OpCode, @enumFromInt(function.chunk.code.pop())) == .DefineGlobal);
                _ = function.chunk.code.pop();
                try expect(@as(OpCode, @enumFromInt(function.chunk.code.pop())) == .Constant);
            }
        }
    }

    var bytecode = ArrayList(u8).init(allocator);
    var writer = bytecode.writer();

    for (0..function.chunk.constant_count) |i| {
        const cte = &function.chunk.constants[i];

        if (cte.* == .Obj) {
            const obj = cte.Obj;

            if (obj.kind == .Fn) {
                const func = obj.as(ObjFunction);

                try writer.print("-- Function {s}\n", .{func.name.?.chars});
                var disassembler = Disassembler.init(&func.chunk, allocator, true);
                defer disassembler.deinit();
                try disassembler.dis_chunk(func.name.?.chars);
                try writer.print("{s}", .{disassembler.disassembled.items});
            }
        }
    }

    var disassembler = Disassembler.init(&function.chunk, allocator, true);
    defer disassembler.deinit();
    try disassembler.dis_chunk("Global scope");
    try writer.print("{s}", .{disassembler.disassembled.items});

    var msgs = ArrayList(CompilerMsg).init(allocator);

    for (compiler.errs.items) |err| {
        try msgs.append(err.report);
    }

    return .{ .expect = try bytecode.toOwnedSlice(), .reports = try msgs.toOwnedSlice() };
}
