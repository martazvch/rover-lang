const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const options = @import("options");

const Compiler = @import("backend/compiler.zig").Compiler;
const CompiledModule = @import("backend/compiler.zig").CompiledModule;
const CompilationManager = @import("backend/compiler.zig").CompilationManager;
const Disassembler = @import("backend/Disassembler.zig");
const Analyzer = @import("frontend/Analyzer.zig");
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("frontend/Ast.zig");
const AstRender = @import("frontend/AstRender.zig");
const Walker = @import("frontend/AstWalker.zig");
const Lexer = @import("frontend/Lexer.zig");
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const LexicalScope = @import("frontend/LexicalScope.zig");
const Parser = @import("frontend/Parser.zig");
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const Sb = @import("StringBuilder.zig");
const RirRenderer = @import("frontend/RirRenderer.zig");
const TypeInterner = @import("frontend/types.zig").TypeInterner;
const reportAll = @import("reporter.zig").reportAll;
const Interner = @import("Interner.zig");
const ModuleInterner = @import("ModuleInterner.zig");
const oom = @import("utils.zig").oom;
const Obj = @import("runtime/Obj.zig");
const Value = @import("runtime/values.zig").Value;
const Config = @import("runtime/Vm.zig").Config;
const Vm = @import("runtime/Vm.zig");

vm: *Vm,
allocator: Allocator,
ctx: *Context,
analyzer: Analyzer,
instr_count: usize,
code_count: usize,
is_sub: bool,

const Self = @This();
const Error = error{ExitOnPrint};

pub const Context = struct {
    config: Config,
    interner: Interner,
    type_interner: TypeInterner,
    path_builder: Sb,
    module_interner: ModuleInterner,

    pub fn new(allocator: Allocator, config: Config) Context {
        var ctx: Context = .{
            .config = config,
            .interner = .init(allocator),
            .type_interner = .init(allocator),
            .path_builder = .empty,
            .module_interner = .init(allocator),
        };
        ctx.type_interner.cacheFrequentTypes();

        return ctx;
    }
};

pub fn init(allocator: Allocator, vm: *Vm, ctx: *Context) Self {
    return .{
        .vm = vm,
        .allocator = allocator,
        .ctx = ctx,
        .analyzer = undefined,
        .instr_count = 0,
        .code_count = 0,
        .is_sub = false,
    };
}

/// Runs the pipeline
// TODO: could only need full path
pub fn run(self: *Self, file_name: []const u8, path: []const u8, source: [:0]const u8) !CompiledModule {
    const path_interned = self.ctx.interner.intern(path);
    self.analyzer = .init(self.allocator, self, path_interned);
    // Initiliaze the path builder
    self.ctx.path_builder.append(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch oom());

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

    var walker: Walker = .init(self.allocator, &self.ctx.interner, &ast);
    walker.walk();

    if (parser.errs.items.len > 0) {
        try reportAll(ParserMsg, parser.errs.items, !options.test_mode, file_name, source);
        return error.ExitOnPrint;
    } else if (self.ctx.config.print_ast) {
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
        self.instr_count = self.analyzer.ir_builder.count();
        return error.ExitOnPrint;
    }
    if (self.ctx.config.print_ir) {
        try self.printIr(self.allocator, file_name, &self.analyzer, self.instr_count);
        if (options.test_mode and !self.is_sub) return error.ExitOnPrint;
    }

    // Compiler
    var compiler = CompilationManager.init(
        self.vm.allocator,
        file_name,
        self.vm,
        &self.ctx.interner,
        if (!self.ctx.config.print_bytecode) .none else if (options.test_mode) .@"test" else .normal,
        self.analyzer.scope.current.variables.count(),
        self.analyzer.scope.symbol_count,
    );

    const compiled_module = try compiler.compile(
        self.instr_count,
        self.analyzer.ir_builder.instructions.items(.data),
        self.analyzer.ir_builder.computeLineFromOffsets(source),
        self.analyzer.main,
        self.ctx.module_interner.compiled.count(),
    );

    self.instr_count = self.analyzer.ir_builder.count();
    self.ctx.module_interner.add(path_interned, analyzed_module, compiled_module);

    return if (options.test_mode and self.ctx.config.print_bytecode and !self.is_sub)
        return error.ExitOnPrint
    else
        compiled_module;
}

pub fn createSubPipeline(self: *Self) Self {
    var pipeline: Self = .init(self.allocator, self.vm, self.ctx);
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
        analyzer.ir_builder.instructions.items(.data)[start..],
        &self.ctx.interner,
    );
    try stdout.writeAll(try rir_renderer.renderIr(file_name));
}
