const std = @import("std");
const Allocator = std.mem.Allocator;

const options = @import("options");

const State = @import("State.zig");
const Compiler = @import("core/compiler/compiler.zig").Compiler;
const CompiledModule = @import("core/compiler/compiler.zig").CompiledModule;
const CompilationManager = @import("core/compiler/compiler.zig").CompilationManager;
const Analyzer = @import("core/analyzer/Analyzer.zig");
const AnalyzerMsg = @import("core/analyzer/analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("core/ast/Ast.zig");
const AstRender = @import("core/ast/AstRender.zig");
const Walker = @import("core/ast/AstWalker.zig");
const Lexer = @import("core/parser/Lexer.zig");
const LexerMsg = @import("core/parser/lexer_msg.zig").LexerMsg;
const LexicalScope = @import("core/analyzer/LexicalScope.zig");
const Parser = @import("core/parser/Parser.zig");
const ParserMsg = @import("core/parser/parser_msg.zig").ParserMsg;
const RirRenderer = @import("core/ir/RirRenderer.zig");
const reportAll = @import("misc").reporter.reportAll;
const oom = @import("misc").oom;
const Vm = @import("core/runtime/Vm.zig");

vm: *Vm,
allocator: Allocator,
state: *State,
analyzer: Analyzer,
instr_count: usize,
is_sub: bool,

const Self = @This();
const Error = error{ExitOnPrint};

pub fn init(allocator: Allocator, vm: *Vm, state: *State) Self {
    return .{
        .vm = vm,
        .allocator = allocator,
        .state = state,
        .analyzer = undefined,
        .instr_count = 0,
        .is_sub = false,
    };
}

/// Runs the pipeline
// TODO: could only need full path
pub fn run(self: *Self, file_name: []const u8, path: []const u8, source: [:0]const u8) !CompiledModule {
    const path_interned = self.state.interner.intern(path);
    self.analyzer = .init(self.allocator, self, path_interned);
    // Initiliaze the path builder
    self.state.path_builder.append(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch oom());

    // Lexer
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

    const analyzed_module = self.analyzer.analyze(&ast, file_name, !self.is_sub);

    // Analyzed Ast printer
    if (self.analyzer.warns.items.len > 0) {
        try reportAll(AnalyzerMsg, self.analyzer.warns.items, !options.test_mode, file_name, source);
    }
    if (self.analyzer.errs.items.len > 0) {
        try reportAll(AnalyzerMsg, self.analyzer.errs.items, !options.test_mode, file_name, source);
        self.instr_count = self.analyzer.irb.count();
        return error.ExitOnPrint;
    }
    if (self.state.config.print_ir) {
        try self.printIr(self.allocator, file_name, &self.analyzer, self.instr_count);
        if (options.test_mode and !self.is_sub) return error.ExitOnPrint;
    }

    // Compiler
    var compiler = CompilationManager.init(
        self.vm.allocator,
        file_name,
        self.vm,
        &self.state.interner,
        if (!self.state.config.print_bytecode) .none else if (options.test_mode) .@"test" else .normal,
        self.analyzer.scope.current.variables.count(),
        self.analyzer.scope.symbol_count,
    );

    const compiled_module = try compiler.compile(
        self.analyzer.irb.instructions.items(.data)[self.instr_count..],
        self.analyzer.irb.roots.items[self.instr_count..],
        self.analyzer.irb.computeLineFromOffsets(source)[self.instr_count..],
        self.analyzer.main,
        self.state.module_interner.compiled.count(),
    );

    self.instr_count = self.analyzer.irb.count();
    self.state.module_interner.add(path_interned, analyzed_module, compiled_module);

    return if (options.test_mode and self.state.config.print_bytecode and !self.is_sub)
        return error.ExitOnPrint
    else
        compiled_module;
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

    var rir_renderer = RirRenderer.init(
        allocator,
        analyzer.irb.instructions.items(.data)[start..],
        &self.state.interner,
    );
    try stdout.writeAll(try rir_renderer.renderIr(file_name, self.analyzer.irb.roots.items[start..]));
}
