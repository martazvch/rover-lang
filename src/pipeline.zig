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
const Vm = @import("runtime/vm.zig").Vm;
const Disassembler = @import("backend/disassembler.zig").Disassembler;

/// Complete interpreter pipeline:
/// - Lexer
/// - Parser
/// - Analyzer
/// - Compiler
/// - Vm
pub const Pipeline = struct {
    allocator: Allocator,
    config: Config,
    lexer: Lexer,
    parser: Parser,
    analyzer: Analyzer,

    const Self = @This();

    pub const Config = struct {
        print_ast: bool,
        print_bytecode: bool,
        static_analysis: bool,
    };

    pub fn init(allocator: Allocator, config: Config) Self {
        return .{
            .allocator = allocator,
            .config = config,
            .lexer = Lexer.init(allocator),
            .parser = undefined,
            .analyzer = Analyzer.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        defer self.lexer.deinit();
        self.parser.deinit();
        self.analyzer.deinit();
    }

    /// Reinitialize the frontend only: lexer and parser.
    /// Meant to be used in the *REPL*
    pub fn reinit_frontend(self: *Self) void {
        self.lexer.reinit();
        self.parser.reinit();
        self.analyzer.reinit();
    }

    /// Runs the pipeline
    pub fn run(self: *Self, filename: []const u8, source: [:0]const u8) !void {
        // Lexer
        try self.lexer.lex(source);

        if (self.lexer.errs.items.len > 0) {
            var reporter = GenReporter(LexerMsg).init(source);
            try reporter.report_all(filename, self.lexer.errs.items);
            return;
        }

        // Parser
        self.parser.init(self.allocator);
        try self.parser.parse(source, self.lexer.tokens.items);

        if (self.parser.errs.items.len > 0) {
            var reporter = GenReporter(ParserMsg).init(source);
            try reporter.report_all(filename, self.parser.errs.items);
            return;
        }

        // Printer
        if (self.config.print_ast) {
            var ast_printer = AstPrinter.init(self.allocator);
            defer ast_printer.deinit();

            try ast_printer.parse_ast(source, self.parser.stmts.items);
            ast_printer.display();
        }

        // Analyzer
        try self.analyzer.analyze(self.parser.stmts.items);

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

        // We start the Vm for the allocator and string interning

        // Vm
        var vm = Vm.new(self.allocator);
        try vm.init();
        defer vm.deinit();

        // Compiler
        var compiler = Compiler.init(&vm, self.analyzer.binop_casts.items);
        defer compiler.deinit();
        try compiler.compile(self.parser.stmts.items);

        // Disassembler
        if (self.config.print_bytecode) {
            var dis = Disassembler.init(&compiler.chunk, self.allocator, false);
            defer dis.deinit();
            try dis.dis_chunk("main");
            std.debug.print("{s}", .{dis.disassembled.items});
        }

        // Vm run
        try vm.run(&compiler.chunk);
    }
};
