const std = @import("std");
const Allocator = std.mem.Allocator;

const options = @import("options");

const Compiler = @import("backend/compiler.zig").Compiler;
const CompilationManager = @import("backend/compiler.zig").CompilationManager;
const Disassembler = @import("backend/Disassembler.zig");
const Analyzer = @import("frontend/Analyzer.zig");
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const Ast = @import("frontend/Ast.zig");
const AstRender = @import("frontend/AstRender.zig");
const Lexer = @import("frontend/Lexer.zig");
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const Parser = @import("frontend/Parser.zig");
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const RirRenderer = @import("frontend/rir_renderer.zig").RirRenderer;
const GenReporter = @import("reporter.zig").GenReporter;
const ObjFunction = @import("runtime/Obj.zig").ObjFunction;
const Vm = @import("runtime/Vm.zig");

vm: *Vm,
arena: std.heap.ArenaAllocator,
allocator: Allocator,
config: Vm.Config,
analyzer: Analyzer,
instr_count: usize,
code_count: usize,

const Self = @This();
const Error = error{ExitOnPrint};

pub const empty: Self = .{
    .vm = undefined,
    .arena = undefined,
    .allocator = undefined,
    .config = undefined,
    .analyzer = undefined,
    .instr_count = 0,
    .code_count = 0,
};

pub fn init(self: *Self, vm: *Vm, config: Vm.Config) void {
    self.vm = vm;
    self.arena = .init(vm.allocator);
    self.allocator = self.arena.allocator();
    self.config = config;
    self.analyzer = undefined;
    self.analyzer.init(self.allocator, config.embedded);
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

/// Runs the pipeline
pub fn run(self: *Self, filename: []const u8, source: [:0]const u8) !*ObjFunction {
    // Lexer
    var lexer = Lexer.init(self.allocator);
    try lexer.lex(source);
    defer lexer.deinit();

    if (lexer.errs.items.len > 0) {
        var reporter = GenReporter(LexerMsg).init(source);
        try reporter.reportAll(filename, lexer.errs.items);
        return error.ExitOnPrint;
    }

    // Parser
    var parser: Parser = .empty;
    parser.init(self.allocator);
    defer parser.deinit();

    var ast = try parser.parse(source, &lexer.tokens);

    if (options.test_mode and self.config.print_ast) {
        try printAst(self.allocator, &ast, &parser);
        return error.ExitOnPrint;
    }

    if (parser.errs.items.len > 0) {
        var reporter = GenReporter(ParserMsg).init(source);
        try reporter.reportAll(filename, parser.errs.items);
        return error.ExitOnPrint;
    } else if (self.config.print_ast) try printAst(self.allocator, &ast, &parser);

    // Analyzer
    try self.analyzer.analyze(&ast);
    defer self.analyzer.reinit();

    // We don't keep errors/warnings from a prompt to another
    defer self.analyzer.errs.clearRetainingCapacity();
    defer self.analyzer.warns.clearRetainingCapacity();

    // Analyzed Ast printer
    if (options.test_mode and self.config.print_ir) {
        try renderIr(self.allocator, source, &self.analyzer, self.instr_count, self.config.static_analyzis);
        return error.ExitOnPrint;
    }

    if (self.analyzer.errs.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.reportAll(filename, self.analyzer.errs.items);

        if (self.analyzer.warns.items.len > 0) {
            reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.reportAll(filename, self.analyzer.warns.items);
        }

        self.instr_count = self.analyzer.instructions.len;
        return error.ExitOnPrint;
    } else if (self.config.print_ir)
        try renderIr(self.allocator, source, &self.analyzer, self.instr_count, self.config.static_analyzis);

    // Analyzer warnings
    if (self.config.static_analyzis and self.analyzer.warns.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.reportAll(filename, self.analyzer.warns.items);
        return error.ExitOnPrint;
    }

    // Compiler
    var compiler = CompilationManager.init(
        self.vm,
        self.analyzer.type_manager.natives.functions,
        &self.analyzer.interner,
        self.instr_count,
        &self.analyzer.instructions,
        if (options.test_mode and self.config.print_bytecode) .Test else if (self.config.print_bytecode) .Normal else .none,
        if (self.config.embedded) 0 else self.analyzer.main.?,
        self.config.embedded,
    );
    defer compiler.deinit();
    self.instr_count = self.analyzer.instructions.len;
    const function = try compiler.compile();

    return if (options.test_mode and self.config.print_bytecode)
        error.ExitOnPrint
    else
        function;
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
    allocator: Allocator,
    source: [:0]const u8,
    analyzer: *const Analyzer,
    start: usize,
    static: bool,
) !void {
    var rir_renderer = RirRenderer.init(
        allocator,
        source,
        start,
        analyzer.instructions,
        analyzer.errs.items,
        analyzer.warns.items,
        &analyzer.interner,
        static,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parse_ir();
    try rir_renderer.display();
}
