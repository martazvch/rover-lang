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
const CompilationManager = @import("backend/compiler.zig").CompilationManager;
const Chunk = @import("backend/chunk.zig").Chunk;
const Vm = @import("runtime/vm.zig").Vm;
const Disassembler = @import("backend/disassembler.zig").Disassembler;
const AnalyzedAstPrinter = @import("frontend/analyzed_ast_print.zig").AnalyzedAstPrinter;

pub const Config = struct {
    print_ast: bool,
    print_bytecode: bool,
    static_analyzis: bool,
    print_analyzed_ast: bool,
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
        try parser.parse(source, lexer.tokens.items);

        // Parser errors
        if (parser.errs.items.len > 0) {
            var reporter = GenReporter(ParserMsg).init(source);
            try reporter.report_all(filename, parser.errs.items);
            return;
        }

        // Ast printer
        if (self.config.print_ast) {
            var ast_printer = AstPrinter.init(self.allocator);
            defer ast_printer.deinit();

            try ast_printer.parse_ast(source, parser.stmts.items);
            ast_printer.display();
        }

        // Analyzer
        // TODO: init analyzer extra info with exact number of element per array list
        // for optimal memory allocation
        try self.analyzer.analyze(parser.stmts.items, source);
        // We don't keep errors/warnings from a prompt to another
        defer self.analyzer.errs.clearRetainingCapacity();
        defer self.analyzer.warns.clearRetainingCapacity();

        // Analyzer errors
        if (self.analyzer.errs.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.report_all(filename, self.analyzer.errs.items);

            if (self.analyzer.warns.items.len > 0) {
                reporter = GenReporter(AnalyzerMsg).init(source);
                try reporter.report_all(filename, self.analyzer.warns.items);
            }

            self.stmts_count = self.analyzer.analyzed_stmts.items.len;
            return;
        }

        // Analyzer warnings
        if (self.config.static_analyzis and self.analyzer.warns.items.len > 0) {
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

        // Vm run
        // try self.vm.run(parser.stmts.items, self.analyzer.analyzed_stmts.items[self.stmts_count..], self.config.print_bytecode);

        // Compiler
        var compiler = CompilationManager.init(
            &self.vm,
            self.analyzer.type_manager.builtins.functions,
            parser.stmts.items,
            self.analyzer.analyzed_stmts.items[self.stmts_count..],
            self.config.print_bytecode,
            undefined,
            true,
        );
        defer compiler.deinit();
        const function = try compiler.compile();

        // Run the program
        try self.vm.run(function);

        self.stmts_count = self.analyzer.analyzed_stmts.items.len;
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

    if (parser.errs.items.len > 0) {
        var reporter = GenReporter(ParserMsg).init(source);
        try reporter.report_all(filename, parser.errs.items);
        return;
    }
    // Printer
    if (config.print_ast) {
        var ast_printer = AstPrinter.init(
            allocator,
            source,
            lexer.tokens.items(.tag),
            lexer.tokens.items(.span),
            parser.nodes.items(.tag),
            parser.nodes.items(.span),
            parser.nodes.items(.data),
            parser.main_nodes.items,
        );
        defer ast_printer.deinit();

        try ast_printer.parse_ast();
        ast_printer.display();
    }

    // // Analyzer
    // // TODO: init analyzer extra info with exact number of element per array list
    // // for optimal memory allocation
    // var analyzer: Analyzer = undefined;
    // try analyzer.init(allocator, false);
    // defer analyzer.deinit();
    //
    // try analyzer.analyze(parser.stmts.items, source);
    //
    // // Analyzer errors
    // if (analyzer.errs.items.len > 0) {
    //     var reporter = GenReporter(AnalyzerMsg).init(source);
    //     try reporter.report_all(filename, analyzer.errs.items);
    //
    //     if (analyzer.warns.items.len > 0) {
    //         reporter = GenReporter(AnalyzerMsg).init(source);
    //         try reporter.report_all(filename, analyzer.warns.items);
    //     }
    //
    //     return;
    // }

    // Analyzer warnings
    // if (config.static_analyzis and analyzer.warns.items.len > 0) {
    //     var reporter = GenReporter(AnalyzerMsg).init(source);
    //     try reporter.report_all(filename, analyzer.warns.items);
    //     return;
    // }
    //
    // // Analyzed Ast printer
    // if (config.print_analyzed_ast) {
    //     var analyzed_ast_printer = AnalyzedAstPrinter.init(allocator, &analyzer.type_manager);
    //     defer analyzed_ast_printer.deinit();
    //
    //     try analyzed_ast_printer.parse(source, analyzer.analyzed_stmts.items);
    //     analyzed_ast_printer.display();
    // }
    //
    // // Start of Vm for compilation
    // var vm: Vm = Vm.new(allocator);
    // try vm.init(false);
    // defer vm.deinit();
    //
    // // Compiler
    // var compiler = CompilationManager.init(
    //     &vm,
    //     analyzer.type_manager.builtins.functions,
    //     parser.stmts.items,
    //     analyzer.analyzed_stmts.items,
    //     config.print_bytecode,
    //     analyzer.main.?,
    //     false,
    // );
    // defer compiler.deinit();
    // const function = try compiler.compile();
    //
    // // Run the program
    // try vm.run(function);
}
