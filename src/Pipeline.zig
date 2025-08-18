const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const options = @import("options");

const Compiler = @import("backend/compiler.zig").Compiler;
const CompilationManager = @import("backend/compiler.zig").CompilationManager;
const Disassembler = @import("backend/Disassembler.zig");
const Analyzer = @import("frontend/Analyzer.zig");
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("frontend/Ast.zig");
const AstRender = @import("frontend/AstRender.zig");
const Walker = @import("frontend/AstWalker.zig");
const Lexer = @import("frontend/Lexer.zig");
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const Parser = @import("frontend/Parser.zig");
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const PathBuilder = @import("frontend/PathBuilder.zig");
const RirRenderer = @import("frontend/RirRenderer.zig");
const GenReporter = @import("reporter.zig").GenReporter;
const Interner = @import("Interner.zig");
const oom = @import("utils.zig").oom;
const Obj = @import("runtime/Obj.zig");
const Value = @import("runtime/values.zig").Value;
const Config = @import("runtime/Vm.zig").Config;
const Vm = @import("runtime/Vm.zig");

vm: *Vm,
arena: std.heap.ArenaAllocator,
allocator: Allocator,
config: Config,
interner: Interner,
analyzer: Analyzer,
instr_count: usize,
code_count: usize,
is_sub: bool,
path_builder: PathBuilder,
modules: CompiledModules,

const Self = @This();
const Error = error{ExitOnPrint};
pub const CompiledModules = AutoArrayHashMapUnmanaged(Interner.Index, Module);

pub fn init(self: *Self, vm: *Vm, config: Config) void {
    self.vm = vm;
    self.arena = .init(vm.allocator);
    self.allocator = self.arena.allocator();
    self.config = config;
    self.interner = .init(self.allocator);
    self.modules = .{};
    self.analyzer = undefined;
    self.analyzer.init(self.allocator, &self.interner, &self.modules);
    self.instr_count = 0;
    self.code_count = 0;
    self.is_sub = false;
    self.path_builder = .init(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch unreachable);
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub const Module = struct {
    name: []const u8,
    imports: []Module,
    function: *Obj.Function,
    globals: []Value,
    symbols: []Value,

    pub fn deinit(self: *Module, allocator: Allocator) void {
        allocator.free(self.globals);
        allocator.free(self.symbols);
    }
};

/// Runs the pipeline
pub fn run(self: *Self, file_name: []const u8, source: [:0]const u8) !Module {
    // Lexer
    var lexer = Lexer.init(self.allocator);
    lexer.lex(source);
    defer lexer.deinit();

    if (lexer.errs.items.len > 0) {
        var reporter = GenReporter(LexerMsg).init(source);
        try reporter.reportAll(file_name, lexer.errs.items);
        return error.ExitOnPrint;
    }

    // Parser
    var parser: Parser = .empty;
    parser.init(self.allocator);
    defer parser.deinit();

    const token_slice = lexer.tokens.toOwnedSlice();
    var ast = parser.parse(source, token_slice.items(.tag), token_slice.items(.span));

    var walker: Walker = undefined;
    walker.init(self.allocator, &self.interner, &ast);
    defer walker.deinit();
    walker.walk();

    if (options.test_mode and self.config.print_ast) {
        try printAst(self.allocator, &ast, &parser);
        return error.ExitOnPrint;
    }

    if (parser.errs.items.len > 0) {
        var reporter = GenReporter(ParserMsg).init(source);
        try reporter.reportAll(file_name, parser.errs.items);
        return error.ExitOnPrint;
    } else if (self.config.print_ast) try printAst(self.allocator, &ast, &parser);

    self.analyzer.analyze(&ast, &self.path_builder);

    // Analyzed Ast printer
    if (options.test_mode and self.config.print_ir) {
        try self.renderIr(self.allocator, file_name, source, &self.analyzer, self.instr_count, self.config.static_analyzis);

        // If we are a sub-pipeline, we print and continue compile
        if (!self.is_sub) return error.ExitOnPrint;
    } else {
        if (self.analyzer.errs.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.reportAll(file_name, self.analyzer.errs.items);

            if (self.analyzer.warns.items.len > 0) {
                reporter = GenReporter(AnalyzerMsg).init(source);
                try reporter.reportAll(file_name, self.analyzer.warns.items);
            }

            self.instr_count = self.analyzer.ir_builder.count();
            return error.ExitOnPrint;
        } else if (self.config.print_ir) {
            try self.renderIr(self.allocator, file_name, source, &self.analyzer, self.instr_count, self.config.static_analyzis);
        }
    }

    // Analyzer warnings
    if (self.config.static_analyzis and self.analyzer.warns.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.reportAll(file_name, self.analyzer.warns.items);
        return error.ExitOnPrint;
    }

    // Compiler
    var compiler = CompilationManager.init(
        self.vm,
        &self.interner,
        // undefined,
        if (options.test_mode and self.config.print_bytecode) .@"test" else if (self.config.print_bytecode) .normal else .none,
        self.analyzer.scope.symbol_count,
        undefined,
    );
    defer compiler.deinit();
    errdefer compiler.globals.deinit(self.vm.allocator);

    const compiler_res = try compiler.compile(
        file_name,
        self.instr_count,
        self.analyzer.ir_builder.instructions.items(.data),
        self.analyzer.ir_builder.computeLineFromOffsets(source),
        if (self.config.embedded) 0 else self.analyzer.main.?,
        self.config.embedded,
    );
    self.instr_count = self.analyzer.ir_builder.count();

    return if (options.test_mode and self.config.print_bytecode and !self.is_sub) {
        return error.ExitOnPrint;
    } else .{
        .name = file_name,
        .imports = undefined,
        .function = compiler_res.function,
        .globals = compiler_res.globals,
        .symbols = compiler_res.symbols,
    };
}

pub fn createSubPipeline(self: *Self) Self {
    var pipeline: Self = undefined;
    var sub_config = self.config;
    // FIX: To make 'main' function not mandatory
    sub_config.embedded = true;

    pipeline.vm = self.vm;
    pipeline.arena = self.arena;
    pipeline.allocator = self.allocator;
    pipeline.config = sub_config;
    pipeline.analyzer.init(self.allocator, self, &self.interner, true);
    pipeline.is_sub = true;

    return pipeline;
}

fn printAst(allocator: Allocator, ast: *const Ast, parser: *const Parser) !void {
    var stdout = std.io.getStdOut().writer();

    if (parser.errs.items.len > 0) {
        for (parser.errs.items) |err| {
            try err.toStr(stdout);
            try stdout.writeAll("\n");
        }
    } else {
        var renderer: AstRender = .init(allocator, ast);
        try renderer.render();
        try stdout.writeAll(renderer.output.items);
    }
}

fn renderIr(
    self: *Self,
    allocator: Allocator,
    file_name: []const u8,
    source: [:0]const u8,
    analyzer: *const Analyzer,
    start: usize,
    static: bool,
) !void {
    var rir_renderer = RirRenderer.init(
        allocator,
        source,
        analyzer.ir_builder.instructions.items(.data)[start..],
        analyzer.errs.items,
        analyzer.warns.items,
        &self.interner,
        static,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parseIr(file_name);
    try rir_renderer.display();
}
