const std = @import("std");
const options = @import("config");
const Allocator = std.mem.Allocator;
const GenReporter = @import("reporter.zig").GenReporter;
const Lexer = @import("frontend/lexer.zig").Lexer;
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const Parser = @import("frontend/parser.zig").Parser;
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const AstPrinter = @import("frontend/ast_print.zig").AstPrinter;
const Analyzer = @import("frontend/analyzer.zig").Analyzer;
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const Compiler = @import("backend/compiler.zig").Compiler;
const CompilationManager = @import("backend/compiler.zig").CompilationManager;
const Vm = @import("runtime/vm.zig").Vm;
const Disassembler = @import("backend/disassembler.zig").Disassembler;
const RirRenderer = @import("frontend/rir_renderer.zig").RirRenderer;

pub const Config = struct {
    print_ast: bool,
    print_bytecode: bool,
    static_analyzis: bool,
    print_ir: bool,
};

pub const ReplPipeline = struct {
    allocator: Allocator,
    config: Config,
    analyzer: Analyzer,
    vm: Vm,
    stmts_count: usize,
    code_count: usize,

    const Self = @This();

    pub fn new(allocator: Allocator, config: Config) !Self {
        // Init of parser and analyzer in two times, maybe fix later when RLS is fixed in the lang
        return .{
            .allocator = allocator,
            .config = config,
            .analyzer = undefined,
            .vm = Vm.new(allocator),
            .stmts_count = 0,
            .code_count = 0,
        };
    }

    pub fn init(self: *Self) !void {
        try self.analyzer.init(self.allocator, true);
        try self.vm.init(true);
    }

    pub fn deinit(self: *Self) void {
        self.analyzer.deinit();
        self.vm.deinit();
    }

    /// Runs the pipeline
    pub fn run(self: *Self, filename: []const u8, source: [:0]const u8) !void {
        // Lexer
        var lexer = Lexer.init(self.allocator);
        try lexer.lex(source);
        defer lexer.deinit();

        if (lexer.errs.items.len > 0) {
            var reporter = GenReporter(LexerMsg).init(source);
            try reporter.report_all(filename, lexer.errs.items);
            return;
        }

        // Parser
        var parser: Parser = undefined;
        parser.init(self.allocator);
        defer parser.deinit();

        try parser.parse(
            source,
            lexer.tokens.items(.tag),
            lexer.tokens.items(.span),
        );

        if (parser.errs.items.len > 0) {
            var reporter = GenReporter(ParserMsg).init(source);
            try reporter.report_all(filename, parser.errs.items);
            return;
        } else if (self.config.print_ast) try print_ast(self.allocator, source, &lexer, &parser);

        // Analyzer
        try self.analyzer.analyze(source, &lexer.tokens, &parser.nodes, parser.mains.items);
        defer self.analyzer.reinit();
        // We don't keep errors/warnings from a prompt to another
        defer self.analyzer.errs.clearRetainingCapacity();
        defer self.analyzer.warns.clearRetainingCapacity();

        if (self.analyzer.errs.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.errs.items);

            if (self.analyzer.warns.items.len > 0) {
                reporter = GenReporter(AnalyzerMsg).init(source);
                try reporter.report_all(filename, self.analyzer.warns.items);
            }

            self.stmts_count = self.analyzer.instructions.len;
            return;
        } else if (self.config.print_ir)
            try render_ir(self.allocator, source, &self.analyzer, self.config.static_analyzis);

        // Analyzer warnings
        if (self.config.static_analyzis and self.analyzer.warns.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.warns.items);
            return;
        }

        // Compiler
        const slice = self.analyzer.instructions.slice();
        var compiler = CompilationManager.init(
            &self.vm,
            self.analyzer.type_manager.builtins.functions,
            &self.analyzer.interner,
            slice.items(.tag)[self.stmts_count..],
            slice.items(.data)[self.stmts_count..],
            slice.items(.start)[self.stmts_count..],
            if (self.config.print_bytecode) .Normal else .None,
            0,
            true,
        );
        defer compiler.deinit();
        const function = try compiler.compile();

        // Run the program
        try self.vm.run(function);

        self.stmts_count = self.analyzer.instructions.len;
    }
};

/// Runs the pipeline
pub fn run(allocator: Allocator, config: Config, filename: []const u8, source: [:0]const u8) !void {
    // Lexer
    var lexer = Lexer.init(allocator);
    try lexer.lex(source);
    defer lexer.deinit();

    if (lexer.errs.items.len > 0) {
        var reporter = GenReporter(LexerMsg).init(source);
        try reporter.report_all(filename, lexer.errs.items);
        return;
    }

    // Parser
    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();

    try parser.parse(
        source,
        lexer.tokens.items(.tag),
        lexer.tokens.items(.span),
    );

    // Printer
    if (options.test_mode and config.print_ast) {
        try print_ast(allocator, source, &lexer, &parser);
        return;
    }

    if (parser.errs.items.len > 0) {
        var reporter = GenReporter(ParserMsg).init(source);
        try reporter.report_all(filename, parser.errs.items);
        return;
    } else if (config.print_ast) try print_ast(allocator, source, &lexer, &parser);

    // Analyzer
    var analyzer: Analyzer = undefined;
    try analyzer.init(allocator, false);
    defer analyzer.deinit();

    try analyzer.analyze(source, &lexer.tokens, &parser.nodes, parser.mains.items);

    // Analyzed Ast printer
    if (options.test_mode and config.print_ir) {
        try render_ir(allocator, source, &analyzer, config.static_analyzis);
        return;
    }

    if (analyzer.errs.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.report_all(filename, analyzer.errs.items);

        if (analyzer.warns.items.len > 0) {
            reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, analyzer.warns.items);
        }

        return;
    } else if (config.print_ir) try render_ir(allocator, source, &analyzer, config.static_analyzis);

    // Analyzer warnings
    if (config.static_analyzis and analyzer.warns.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.report_all(filename, analyzer.warns.items);
        return;
    }

    // Start of Vm for compilation
    var vm: Vm = Vm.new(allocator);
    defer vm.deinit();
    try vm.init(false);

    // Compiler
    var compiler = CompilationManager.init(
        &vm,
        analyzer.type_manager.builtins.functions,
        &analyzer.interner,
        analyzer.instructions.items(.tag),
        analyzer.instructions.items(.data),
        analyzer.instructions.items(.start),
        if (options.test_mode and config.print_bytecode) .Test else if (config.print_bytecode) .Normal else .None,
        analyzer.main.?,
        false,
    );
    defer compiler.deinit();

    const function = try compiler.compile();
    if (options.test_mode and config.print_bytecode) return;

    // Run the program
    try vm.run(function);
}

fn print_ast(
    allocator: Allocator,
    source: [:0]const u8,
    lexer: *const Lexer,
    parser: *const Parser,
) !void {
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
    static: bool,
) !void {
    var rir_renderer = RirRenderer.init(
        allocator,
        source,
        analyzer.instructions.items(.tag),
        analyzer.instructions.items(.data),
        analyzer.errs.items,
        analyzer.warns.items,
        &analyzer.interner,
        static,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parse_ir();
    try rir_renderer.display();
}
