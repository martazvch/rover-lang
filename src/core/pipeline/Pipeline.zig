const std = @import("std");
const Allocator = std.mem.Allocator;

const options = @import("options");

const State = @import("State.zig");
const Compiler = @import("../compiler/compiler.zig").Compiler;
const CompiledModule = @import("../compiler/compiler.zig").CompiledModule;
const CompilationManager = @import("../compiler/compiler.zig").CompilationManager;
const Analyzer = @import("../analyzer/Analyzer.zig");
const AnalyzerMsg = @import("../analyzer/analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("../parser/Ast.zig");
const AstRender = @import("../parser/AstRender.zig");
const Walker = @import("../parser/AstWalker.zig");
const Lexer = @import("../parser/Lexer.zig");
const LexerMsg = @import("../parser/lexer_msg.zig").LexerMsg;
const LexicalScope = @import("../analyzer/LexicalScope.zig");
const Parser = @import("../parser/Parser.zig");
const ParserMsg = @import("../parser/parser_msg.zig").ParserMsg;
const IrRenderer = @import("../analyzer/IrRenderer.zig");
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

const misc = @import("misc");
const reportAll = misc.reporter.reportAll;
const oom = misc.oom;

vm: *Vm,
allocator: Allocator,
state: *State,
instr_count: usize,
is_sub: bool,

const Self = @This();
const Error = error{ExitOnPrint};

pub fn init(allocator: Allocator, vm: *Vm, state: *State) Self {
    return .{
        .vm = vm,
        .allocator = allocator,
        .state = state,
        .instr_count = 0,
        .is_sub = false,
    };
}

/// Runs the pipeline
// TODO: could only need full path
pub fn run(self: *Self, file_name: []const u8, path: []const u8, source: [:0]const u8) !*Obj.Function {
    // Initiliaze the path builder
    self.state.path_builder.append(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch oom());

    const ast = try self.parse(file_name, source);

    var analyzer: Analyzer = .init(self.allocator, self);
    const analyzed_module = analyzer.analyze(&ast, file_name, !self.is_sub);

    // Analyzed Ast printer
    if (analyzer.warns.items.len > 0) {
        try reportAll(AnalyzerMsg, analyzer.warns.items, !options.test_mode, file_name, source);
    }
    if (analyzer.errs.items.len > 0) {
        try reportAll(AnalyzerMsg, analyzer.errs.items, !options.test_mode, file_name, source);
        self.instr_count = analyzer.irb.count();
        return error.ExitOnPrint;
    }
    if (self.state.config.print_ir) {
        try self.printIr(self.allocator, file_name, &analyzer, self.instr_count);
        if (options.test_mode and !self.is_sub) return error.ExitOnPrint;
    }

    // Compiler
    var compiler = CompilationManager.init(
        self.vm.allocator,
        file_name,
        self.vm,
        &self.state.interner,
        if (!self.state.config.print_bytecode) .none else if (options.test_mode) .@"test" else .normal,
        analyzer.scope.current.variables.count(),
        analyzer.scope.symbol_count,
    );

    const entry_point, const compiled_module = try compiler.compile(
        analyzer.irb.instructions.items(.data)[self.instr_count..],
        analyzer.irb.roots.items[self.instr_count..],
        analyzer.irb.computeLineFromOffsets(source)[self.instr_count..],
        analyzer.main,
        self.state.module_interner.compiled.count(),
    );

    self.instr_count = analyzer.irb.count();
    const path_interned = self.state.interner.intern(path);
    self.state.module_interner.add(path_interned, analyzed_module, compiled_module);

    return if (options.test_mode and self.state.config.print_bytecode and !self.is_sub)
        error.ExitOnPrint
    else
        entry_point;
}

pub fn parse(self: *Self, file_name: []const u8, source: [:0]const u8) !Ast {
    var lexer = Lexer.init(self.allocator);
    lexer.lex(source);
    defer lexer.deinit();

    if (lexer.errs.items.len > 0) {
        try reportAll(LexerMsg, lexer.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    }

    // Parser
    const token_slice = lexer.tokens.toOwnedSlice();
    var parser: Parser = .init(self.allocator);
    var ast = parser.parse(source, token_slice.items(.tag), token_slice.items(.span));

    var walker: Walker = .init(self.allocator, &self.state.interner, &ast);
    walker.walk();

    if (parser.errs.items.len > 0) {
        try reportAll(ParserMsg, parser.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    } else if (self.state.config.print_ast) {
        try printAst(self.allocator, &ast);
        if (options.test_mode) return error.ExitOnPrint;
    }

    return ast;
}

pub fn createSubPipeline(self: *Self) Self {
    var pipeline: Self = .init(self.allocator, self.vm, self.state);
    pipeline.is_sub = true;
    return pipeline;
}

fn printAst(allocator: Allocator, ast: *const Ast) !void {
    var buf: [2048]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch unreachable;

    var renderer: AstRender = .init(allocator, ast);
    try stdout.writeAll(try renderer.render());
}

fn printIr(self: *Self, allocator: Allocator, file_name: []const u8, analyzer: *const Analyzer, start: usize) !void {
    var buf: [2048]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch unreachable;

    var ir_renderer = IrRenderer.init(
        allocator,
        analyzer.irb.instructions.items(.data)[start..],
        &self.state.interner,
    );
    try stdout.writeAll(try ir_renderer.renderIr(file_name, analyzer.irb.roots.items[start..]));
}
