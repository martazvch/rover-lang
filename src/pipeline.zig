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
    vm: Vm,

    const Self = @This();

    pub const Config = struct {
        print_ast: bool,
        print_bytecode: bool,
        static_analysis: bool,
    };

    pub fn init(allocator: Allocator, config: Config) !Self {
        var analyzer = Analyzer.init(allocator);
        try analyzer.type_manager.init_builtins();

        var vm = Vm.new(allocator);
        try vm.init();

        // Init of parser in two times, maybe fix later when RLS is fixed in the lang
        return .{
            .allocator = allocator,
            .config = config,
            .lexer = Lexer.init(allocator),
            .parser = undefined,
            .analyzer = analyzer,
            .vm = vm,
        };
    }

    pub fn deinit(self: *Self) void {
        self.parser.deinit();
        self.lexer.deinit();
        self.analyzer.deinit();
        self.vm.deinit();
    }

    /// Runs the pipeline
    pub fn run(self: *Self, filename: []const u8, source: [:0]const u8) !void {
        // Lexer
        try self.lexer.lex(source);
        defer self.lexer.reinit();

        // In case we have an error, we initialize the parser because here it's still
        // undefined and we are gonna call deinit on it
        self.parser.init(self.allocator);
        defer self.parser.reinit();

        if (self.lexer.errs.items.len > 0) {
            var reporter = GenReporter(LexerMsg).init(source);
            try reporter.report_all(filename, self.lexer.errs.items);
            return;
        }

        // Parser
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
        // TODO: init analyzer extra info with exact number of element per array list
        // for optimal memory allocation
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

        // Compiler
        var compiler = Compiler.init(&self.vm);
        try compiler.compile(self.parser.stmts.items, self.analyzer.analyzed_stmts.items);
        defer compiler.deinit();

        // Disassembler
        if (self.config.print_bytecode) {
            var dis = Disassembler.init(&compiler.chunk, self.allocator, false);
            defer dis.deinit();
            try dis.dis_chunk("main");
            std.debug.print("{s}", .{dis.disassembled.items});
        }

        // Vm run
        try self.vm.run(&compiler.chunk);
    }
};
