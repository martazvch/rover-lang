const std = @import("std");
const Allocator = std.mem.Allocator;

const options = @import("options");

const Compiler = @import("backend/compiler.zig").Compiler;
const CompilationManager = @import("backend/compiler.zig").CompilationManager;
const Disassembler = @import("backend/disassembler.zig").Disassembler;
const Analyzer = @import("frontend/analyzer.zig").Analyzer;
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const AstPrinter = @import("frontend/ast_print.zig").AstPrinter;
const Lexer = @import("frontend/lexer.zig").Lexer;
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const Parser = @import("frontend/parser.zig").Parser;
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const RirRenderer = @import("frontend/rir_renderer.zig").RirRenderer;
const GenReporter = @import("reporter.zig").GenReporter;
const ObjFunction = @import("runtime/obj.zig").ObjFunction;
const Vm = @import("runtime/vm.zig").Vm;

pub const Pipeline = struct {
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

    pub fn init(self: *Self, vm: *Vm, config: Vm.Config) !void {
        self.vm = vm;
        self.arena = .init(vm.allocator);
        self.allocator = self.arena.allocator();
        self.config = config;
        self.analyzer = undefined;
        try self.analyzer.init(self.allocator, config.embedded);
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
            try reporter.report_all(filename, lexer.errs.items);
            return error.ExitOnPrint;
        }

        // Parser
        // TODO: self.allocator is already an arena, modify Parser
        var parser: Parser = .empty;
        parser.init(self.allocator);
        defer parser.deinit();

        try parser.parse(
            source,
            lexer.tokens.items(.tag),
            lexer.tokens.items(.span),
        );

        // Printer
        if (options.test_mode and self.config.print_ast) {
            try print_ast(self.allocator, source, &lexer, &parser);
            return error.ExitOnPrint;
        }

        if (parser.errs.items.len > 0) {
            var reporter = GenReporter(ParserMsg).init(source);
            try reporter.report_all(filename, parser.errs.items);
            return error.ExitOnPrint;
        } else if (self.config.print_ast) try print_ast(self.allocator, source, &lexer, &parser);

        // Analyzer
        try self.analyzer.analyze(source, &lexer.tokens, &parser.nodes);
        defer self.analyzer.reinit();

        // We don't keep errors/warnings from a prompt to another
        defer self.analyzer.errs.clearRetainingCapacity();
        defer self.analyzer.warns.clearRetainingCapacity();

        // Analyzed Ast printer
        if (options.test_mode and self.config.print_ir) {
            try render_ir(self.allocator, source, &self.analyzer, self.instr_count, self.config.static_analyzis);
            return error.ExitOnPrint;
        }

        if (self.analyzer.errs.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.errs.items);

            if (self.analyzer.warns.items.len > 0) {
                reporter = GenReporter(AnalyzerMsg).init(source);
                try reporter.report_all(filename, self.analyzer.warns.items);
            }

            self.instr_count = self.analyzer.instructions.len;
            return error.ExitOnPrint;
        } else if (self.config.print_ir)
            try render_ir(self.allocator, source, &self.analyzer, self.instr_count, self.config.static_analyzis);

        // Analyzer warnings
        if (self.config.static_analyzis and self.analyzer.warns.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.warns.items);
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
};

fn print_ast(allocator: Allocator, source: [:0]const u8, lexer: *const Lexer, parser: *const Parser) !void {
    var ast_printer = AstPrinter.init(
        allocator,
        source,
        lexer.tokens.items(.tag),
        lexer.tokens.items(.span),
        parser.nodes.items(.tag),
        parser.nodes.items(.main),
        parser.nodes.items(.data),
        parser.errs.items,
    );
    defer ast_printer.deinit();

    try ast_printer.parse_ast();
    try ast_printer.display();
}

fn render_ir(
    allocator: Allocator,
    source: [:0]const u8,
    analyzer: *const Analyzer,
    start: usize,
    static: bool,
) !void {
    var rir_renderer = RirRenderer.init(
        allocator,
        source,
        analyzer.instructions.items(.tag)[start..],
        analyzer.instructions.items(.data)[start..],
        analyzer.errs.items,
        analyzer.warns.items,
        &analyzer.interner,
        static,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parse_ir();
    try rir_renderer.display();
}
