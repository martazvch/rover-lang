const std = @import("std");
const Allocator = std.mem.Allocator;
const GenReporter = @import("reporter.zig").GenReporter;
const GenReport = @import("reporter.zig").GenReport;
const Lexer = @import("frontend/lexer.zig").Lexer;
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const Token = @import("frontend/lexer.zig").Token;
const Parser = @import("frontend/parser.zig").Parser;
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const AstPrinter = @import("frontend/ast_print.zig").AstPrinter;
const Analyzer = @import("frontend/analyzer.zig").Analyzer;
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const Compiler = @import("backend/compiler.zig").Compiler;
const Chunk = @import("backend/chunk.zig").Chunk;
const Vm = @import("runtime/vm.zig").Vm;
const Disassembler = @import("backend/disassembler.zig").Disassembler;
const AnalyzedAstPrinter = @import("frontend/analyzed_ast_print.zig").AnalyzedAstPrinter;

pub const Config = struct {
    print_ast: bool,
    print_bytecode: bool,
    static_analysis: bool,
    print_analyzed_ast: bool,
};

/// Complete interpreter pipeline:
/// - Lexer
/// - Parser
/// - Analyzer
/// - Compiler
/// - Vm
pub const ReplPipeline = struct {
    allocator: Allocator,
    config: Config,
    analyzer: Analyzer,
    compiler: Compiler,
    vm: Vm,
    stmts_count: usize,
    code_count: usize,

    const Self = @This();

    pub fn new(allocator: Allocator, config: Config) !Self {
        var analyzer = Analyzer.init(allocator);
        try analyzer.type_manager.init_builtins();

        var vm = Vm.new(allocator);
        try vm.init();

        // Init of parser in two times, maybe fix later when RLS is fixed in the lang
        return .{
            .allocator = allocator,
            .config = config,
            .analyzer = analyzer,
            .compiler = undefined,
            .vm = vm,
            .stmts_count = 0,
            .code_count = 0,
        };
    }

    pub fn init(self: *Self) void {
        self.compiler = Compiler.init(&self.vm);
    }

    pub fn deinit(self: *Self) void {
        self.analyzer.deinit();
        self.compiler.deinit();
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
        try parser.parse(source, lexer.tokens.items);

        if (parser.errs.items.len > 0) {
            var reporter = GenReporter(ParserMsg).init(source);
            try reporter.report_all(filename, parser.errs.items);
            return;
        }

        // Printer
        if (self.config.print_ast) {
            var ast_printer = AstPrinter.init(self.allocator);
            defer ast_printer.deinit();

            try ast_printer.parse_ast(source, parser.stmts.items);
            ast_printer.display();
        }

        // Analyzer
        // TODO: init analyzer extra info with exact number of element per array list
        // for optimal memory allocation
        try self.analyzer.analyze(parser.stmts.items);
        // We don't keep errosr/warnings from a prompt to another
        defer self.analyzer.errs.clearRetainingCapacity();
        defer self.analyzer.warns.clearRetainingCapacity();

        if (self.analyzer.errs.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.errs.items);

            if (!self.config.static_analysis and self.analyzer.warns.items.len == 0) {
                return;
            }
        }

        if (self.config.static_analysis and self.analyzer.warns.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.warns.items);
            return;
        }

        // Analyzed Ast printer
        if (self.config.print_analyzed_ast) {
            var analyzed_ast_printer = AnalyzedAstPrinter.init(self.allocator, &self.analyzer.type_manager);
            defer analyzed_ast_printer.deinit();

            try analyzed_ast_printer.parse(source, self.analyzer.analyzed_stmts.items[self.stmts_count..]);
            analyzed_ast_printer.display();
        }

        // Compiler
        try self.compiler.compile(parser.stmts.items, self.analyzer.analyzed_stmts.items[self.stmts_count..]);
        // We isolate only new compiled code
        const code_len = self.compiler.chunk.code.items.len;

        // Disassembler
        if (self.config.print_bytecode) {
            var dis = Disassembler.init(&self.compiler.chunk, self.allocator, false);
            defer dis.deinit();
            try dis.dis_slice("main", self.code_count);
            std.debug.print("\n{s}", .{dis.disassembled.items});
        }

        // Vm run
        try self.vm.run_slice(&self.compiler.chunk, self.code_count);

        self.code_count = code_len;
        self.stmts_count = self.analyzer.analyzed_stmts.items.len;
    }
};

/// Runs the pipeline
pub fn run(allocator: Allocator, config: Config, filename: []const u8, source: [:0]const u8) !void {
    // Lexer
    var lexer = Lexer.init(allocator);
    try lexer.lex(source);
    defer lexer.deinit();

    // In case we have an error, we initialize the parser because here it's still
    // undefined and we are gonna call deinit on it

    if (lexer.errs.items.len > 0) {
        var reporter = GenReporter(LexerMsg).init(source);
        try reporter.report_all(filename, lexer.errs.items);
        return;
    }

    // Parser
    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();

    try parser.parse(source, lexer.tokens.items);

    if (parser.errs.items.len > 0) {
        var reporter = GenReporter(ParserMsg).init(source);
        try reporter.report_all(filename, parser.errs.items);
        return;
    }

    // Printer
    if (config.print_ast) {
        var ast_printer = AstPrinter.init(allocator);
        defer ast_printer.deinit();

        try ast_printer.parse_ast(source, parser.stmts.items);
        ast_printer.display();
    }

    // Analyzer
    // TODO: init analyzer extra info with exact number of element per array list
    // for optimal memory allocation
    var analyzer = Analyzer.init(allocator);
    try analyzer.type_manager.init_builtins();
    defer analyzer.deinit();

    try analyzer.analyze(parser.stmts.items);

    if (analyzer.errs.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.report_all(filename, analyzer.errs.items);

        if (!config.static_analysis and analyzer.warns.items.len == 0) {
            return;
        }
    }

    if (config.static_analysis and analyzer.warns.items.len > 0) {
        var reporter = GenReporter(AnalyzerMsg).init(source);
        try reporter.report_all(filename, analyzer.warns.items);
        return;
    }

    // Analyzed Ast printer
    if (config.print_analyzed_ast) {
        var analyzed_ast_printer = AnalyzedAstPrinter.init(allocator, &analyzer.type_manager);
        defer analyzed_ast_printer.deinit();

        try analyzed_ast_printer.parse(source, analyzer.analyzed_stmts.items);
        analyzed_ast_printer.display();
    }

    // Vm
    var vm: Vm = Vm.new(allocator);
    try vm.init();
    defer vm.deinit();

    // Compiler
    var compiler = Compiler.init(&vm);
    try compiler.compile(parser.stmts.items, analyzer.analyzed_stmts.items);
    defer compiler.deinit();

    // Disassembler
    if (config.print_bytecode) {
        var dis = Disassembler.init(&compiler.chunk, allocator, false);
        defer dis.deinit();
        try dis.dis_chunk("main");
        std.debug.print("\n{s}", .{dis.disassembled.items});
    }

    // Vm run
    try vm.run(&compiler.chunk);
}
