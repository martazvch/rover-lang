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
const GenReporter = @import("reporter.zig").GenReporter;
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
    walker.init(self.allocator, &self.ctx.interner, &ast);
    defer walker.deinit();
    walker.walk();

    if (options.test_mode and self.ctx.config.print_ast) {
        try printAst(self.allocator, &ast, &parser);
        return error.ExitOnPrint;
    }

    if (parser.errs.items.len > 0) {
        var reporter = GenReporter(ParserMsg).init(source);
        try reporter.reportAll(file_name, parser.errs.items);
        return error.ExitOnPrint;
    } else if (self.ctx.config.print_ast) try printAst(self.allocator, &ast, &parser);

    const analyzed_module = self.analyzer.analyze(&ast, file_name, !self.is_sub);

    // Analyzed Ast printer
    if (options.test_mode and self.ctx.config.print_ir) {
        try self.renderIr(self.allocator, file_name, source, &self.analyzer, self.instr_count, self.ctx.config.static_analyzis);

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
        } else if (self.ctx.config.print_ir) {
            try self.renderIr(self.allocator, file_name, source, &self.analyzer, self.instr_count, self.ctx.config.static_analyzis);
        }
    }

    // Analyzer warnings
    if (self.ctx.config.static_analyzis and self.analyzer.warns.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.reportAll(file_name, self.analyzer.warns.items);
        return error.ExitOnPrint;
    }

    // Compiler
    var compiler = CompilationManager.init(
        self.allocator,
        file_name,
        self.vm,
        &self.ctx.interner,
        if (options.test_mode and self.ctx.config.print_bytecode) .@"test" else if (self.ctx.config.print_bytecode) .normal else .none,
        self.analyzer.scope.current.variables.count(),
        self.analyzer.scope.symbol_count,
    );
    defer compiler.deinit();

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
        &self.ctx.interner,
        static,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parseIr(file_name);
    try rir_renderer.display();
}
