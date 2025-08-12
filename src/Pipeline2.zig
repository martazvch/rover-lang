const std = @import("std");
const Allocator = std.mem.Allocator;

const options = @import("options");

const Compiler = @import("backend/compiler.zig").Compiler;
const CompilationManager = @import("backend/compiler2.zig").CompilationManager;
const Disassembler = @import("backend/Disassembler.zig");
const Analyzer = @import("frontend/Analyzer2.zig");
const AnalyzerMsg = @import("frontend/analyzer_msg.zig").AnalyzerMsg;
const Lexer = @import("frontend/Lexer.zig");
const LexerMsg = @import("frontend/lexer_msg.zig").LexerMsg;
const Parser = @import("frontend/Parser.zig");
const ParserMsg = @import("frontend/parser_msg.zig").ParserMsg;
const Ast = @import("frontend/Ast.zig");
const AstRender = @import("frontend/AstRender.zig");
const Walker = @import("frontend/AstWalker.zig");
const RirRenderer = @import("frontend/RirRenderer2.zig");
const Symbols = @import("frontend/TypeManager.zig").Symbols;
const TypeManager = @import("frontend/TypeManager.zig");
const GenReporter = @import("reporter.zig").GenReporter;
const oom = @import("utils.zig").oom;
const Obj = @import("runtime/Obj2.zig");
const Value = @import("runtime/values2.zig").Value;
const Vm = @import("runtime/Vm2.zig");

const Config = @import("runtime/Vm.zig").Config;

vm: *Vm,
arena: std.heap.ArenaAllocator,
allocator: Allocator,
config: Config,
analyzer: Analyzer,
type_manager: TypeManager,
instr_count: usize,
code_count: usize,
is_sub: bool = false,
globals: std.ArrayListUnmanaged(Value) = .{},

const Self = @This();
const Error = error{ExitOnPrint};

pub const empty: Self = .{
    .vm = undefined,
    .arena = undefined,
    .allocator = undefined,
    .config = undefined,
    .analyzer = undefined,
    .type_manager = undefined,
    .instr_count = 0,
    .code_count = 0,
};

pub fn init(self: *Self, vm: *Vm, config: Config) void {
    self.vm = vm;
    self.arena = .init(vm.allocator);
    self.allocator = self.arena.allocator();
    self.config = config;
    self.analyzer = undefined;
    self.analyzer.init(self.allocator, &self.vm.interner);
    self.type_manager = .init(self.allocator);
    self.type_manager.init_builtins(&self.vm.interner);
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.globals.deinit(self.vm.allocator);
}

// // TODO: clean unused
pub const Module = struct {
    name: []const u8,
    imports: []Module,
    function: *Obj.Function,
    globals: []Value,
    symbols: []Value,
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
    walker.init(self.allocator, &self.vm.interner, &ast);
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

    // Analyzer
    // self.analyzer.analyze(&ast);
    // defer self.analyzer.reinit();
    //
    // // We don't keep errors/warnings from a prompt to another
    // defer self.analyzer.errs.clearRetainingCapacity();
    // defer self.analyzer.warns.clearRetainingCapacity();
    //

    // TMP
    self.analyzer.analyze(&ast) catch @panic("Error analyzer");

    // Analyzed Ast printer
    if (options.test_mode and self.config.print_ir) {
        try self.renderIr(self.allocator, file_name, source, &self.analyzer, self.instr_count, self.config.static_analyzis);

        // If we are a sub-pipeline, we print and contnue compile
        if (!self.is_sub) return error.ExitOnPrint;
    } else {
        if (self.analyzer.errs.items.len > 0) {
            var reporter = GenReporter(AnalyzerMsg).init(source);
            try reporter.reportAll(file_name, self.analyzer.errs.items);

            if (self.analyzer.warns.items.len > 0) {
                reporter = GenReporter(AnalyzerMsg).init(source);
                try reporter.reportAll(file_name, self.analyzer.warns.items);
            }

            self.instr_count = self.analyzer.ir_builder.instructions.len;
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
        file_name,
        self.vm,
        undefined,
        self.instr_count,
        &self.analyzer.ir_builder.instructions,
        undefined,
        if (options.test_mode and self.config.print_bytecode) .Test else if (self.config.print_bytecode) .Normal else .none,
        if (self.config.embedded) 0 else self.analyzer.main.?,
        self.config.embedded,
        &self.globals,
        self.analyzer.scope.symbol_count,
    );
    defer compiler.deinit();

    self.instr_count = self.analyzer.ir_builder.instructions.len;
    const function = try compiler.compile();
    errdefer compiler.globals.deinit(self.vm.allocator);

    return if (options.test_mode and self.config.print_bytecode and !self.is_sub) {
        return error.ExitOnPrint;
    } else .{
        .name = file_name,
        .imports = undefined,
        .function = function,
        .globals = self.globals.items,
        .symbols = compiler.symbols,
    };
}

pub fn createSubPipeline(self: *Self) Self {
    var pipeline: Self = .empty;
    var sub_config = self.config;
    // FIX: To make 'main' function not mandatory
    sub_config.embedded = true;

    pipeline.vm = self.vm;
    pipeline.arena = self.arena;
    pipeline.allocator = self.allocator;
    pipeline.config = sub_config;
    pipeline.analyzer.init(self.allocator, self, &self.vm.interner, true);
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
        &self.vm.interner,
        static,
    );
    defer rir_renderer.deinit();

    try rir_renderer.parse_ir(file_name);
    try rir_renderer.display();
}
