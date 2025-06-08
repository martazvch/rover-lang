const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const Interner = @import("../Interner.zig");
const oom = @import("../utils.zig").oom;
const AnalyzerReport = @import("Analyzer.zig").AnalyzerReport;
const Ast = @import("Ast.zig");
const Instruction = @import("rir.zig").Instruction;
const Node = @import("Ast.zig").Node;
const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;
const Type = @import("rir.zig").Type;

const Labels = struct { depth: usize, msg: []const u8 };

source: []const u8,
instr_data: []const Instruction.Data,
errs: []const AnalyzerReport,
warns: []const AnalyzerReport,
interner: *const Interner,
static_analyzis: bool,
indent_level: u8 = 0,
tree: ArrayList(u8),
writer: std.ArrayList(u8).Writer,
instr_idx: usize = 0,

const indent_size: u8 = 4;
const spaces: [1024]u8 = [_]u8{' '} ** 1024;

const Error = Allocator.Error || std.fmt.BufPrintError;
const Self = @This();

pub fn init(
    allocator: Allocator,
    source: []const u8,
    instr_data: []const Instruction.Data,
    errs: []const AnalyzerReport,
    warns: []const AnalyzerReport,
    interner: *const Interner,
    static_analyzis: bool,
) Self {
    return .{
        .source = source,
        .instr_data = instr_data,
        .errs = errs,
        .warns = warns,
        .interner = interner,
        .static_analyzis = static_analyzis,
        .tree = ArrayList(u8).init(allocator),
        .writer = undefined,
    };
}

pub fn deinit(self: *Self) void {
    self.tree.deinit();
}

pub fn display(self: *const Self) !void {
    var stdout = std.io.getStdOut().writer();
    try stdout.writeAll(self.tree.items);
}

fn indent(self: *Self) void {
    self.tree.appendSlice(Self.spaces[0 .. self.indent_level * Self.indent_size]) catch oom();
}

pub fn parse_ir(self: *Self, file_name: []const u8) !void {
    self.writer = self.tree.writer();
    // TODO: remove the comment
    try self.writer.print("//-- {s} --\n", .{file_name});

    if (self.errs.len > 0)
        try self.parseErrs()
    else if (self.static_analyzis and self.warns.len > 0)
        try self.parseErrs()
    else while (self.instr_idx < self.instr_data.len)
        try self.parseInstr();

    try self.writer.writeAll("\n");
}

fn parseErrs(self: *Self) !void {
    const stdout = std.io.getStdOut().writer();

    for (self.errs) |err| {
        try err.toStr(stdout);
        try stdout.writeAll("\n");
    }

    for (self.warns) |warn| {
        try warn.toStr(stdout);
        try stdout.writeAll("\n");
    }
}

fn at(self: *const Self) *const Instruction.Data {
    return &self.instr_data[self.instr_idx];
}

fn eof(self: *const Self) bool {
    return self.instr_idx == self.instr_data.len;
}

fn next(self: *Self) Instruction.Data {
    defer self.instr_idx += 1;

    return self.instr_data[self.instr_idx];
}

fn parseInstr(self: *Self) !void {
    try switch (self.next()) {
        .array => |*data| self.array(data),
        .array_access => self.arrayAccess(),
        .assignment => |*data| self.assignment(data),
        .binop => |*data| self.binop(data),
        .block => |*data| self.block(data),
        .bool => |data| self.boolInstr(data),
        .call => |*data| self.fnCall(data),
        .cast => |data| self.cast(data),
        .discard => self.discard(),
        .float => |data| self.floatInstr(data),
        .fn_decl => |*data| self.fnDeclaration(data),
        .identifier => |*data| self.identifier(data),
        .identifier_id => |data| self.identifier(&self.instr_data[data].var_decl.variable),
        .identifier_absolute => |data| self.identifierAbsolute(data),
        .@"if" => |*data| self.ifInstr(data),
        // TODO: delete later
        .imported => unreachable,
        .int => |data| self.intInstr(data),
        .item_import => |*data| self.itemImport(data),
        .member => |*data| self.getMember(data),
        .module_import => |*data| self.moduleImport(data),
        .multiple_var_decl => |data| self.multipleVarDecl(data),
        .name => unreachable,
        .null => unreachable,
        .print => {
            self.indent();
            try self.tree.appendSlice("[Print]\n");
            self.indent_level += 1;
            try self.parseInstr();
            self.indent_level -= 1;
        },
        .@"return" => |*data| self.returnInstr(data),
        .string => |data| self.stringInstr(data),
        .struct_decl => |*data| self.structDecl(data),
        .default_value => unreachable,
        .struct_literal => |data| self.structLiteral(data),
        .value => unreachable,
        .unary => |*data| self.unary(data),
        .use => |data| self.use(data),
        .var_decl => |*data| self.varDecl(data),
        .@"while" => self.whileInstr(),
    };
}

fn array(self: *Self, data: *const Instruction.Array) Error!void {
    self.indent();
    try self.writer.writeAll("[Array]\n");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    var cast_count: usize = 0;

    for (0..data.len) |i| {
        try self.parseInstr();

        if (data.cast_until > 0 and i < data.cast_until - 1) {
            self.indent();
            try self.writer.writeAll("[Cast to float]\n");
        }

        if (!self.eof() and self.at().* == .cast and cast_count < data.cast_count) {
            cast_count += 1;
            try self.parseInstr();
        }
    }
}

fn arrayAccess(self: *Self) Error!void {
    self.indent();
    try self.writer.writeAll("[Array access]\n");
    self.indent_level += 1;
    defer self.indent_level -= 1;

    self.indent();
    try self.writer.writeAll("- array\n");
    try self.parseInstr();

    self.indent();
    try self.writer.writeAll("- index\n");
    try self.parseInstr();
}

fn assignment(self: *Self, data: *const Instruction.Assignment) Error!void {
    // Value
    try self.parseInstr();

    if (data.cast) {
        self.indent();
        try self.writer.writeAll("[Cast to float]\n");
    }

    const variable_data = switch (self.next()) {
        .array_access => return self.arrayAssignment(),
        .identifier => |*variable| variable,
        .identifier_id => |idx| &self.instr_data[idx].var_decl.variable,
        .member => |*member| return self.fieldAssignment(member),
        else => unreachable,
    };
    self.indent();
    try self.writer.print("[Assignment index: {}, scope: {s}]\n", .{
        variable_data.index, @tagName(variable_data.scope),
    });
}

fn arrayAssignment(self: *Self) Error!void {
    self.indent();
    try self.writer.writeAll("[Array assignment]\n");

    self.indent_level += 1;
    defer self.indent_level -= 1;

    // Index
    self.indent();
    try self.writer.writeAll("- index\n");
    try self.parseInstr();
    // Array
    self.indent();
    try self.writer.writeAll("- variable\n");
    try self.parseInstr();
}

fn fieldAssignment(self: *Self, data: *const Instruction.Member) Error!void {
    self.indent();
    try self.writer.writeAll("[Field assignment]\n");

    self.indent_level += 1;
    try self.getMember(data);
    self.indent_level -= 1;
}

fn binop(self: *Self, data: *const Instruction.Binop) Error!void {
    self.indent();
    try self.writer.print(
        "[Binop type: {s}, cast: {s}]\n",
        .{ @tagName(data.op), @tagName(data.cast) },
    );

    self.indent_level += 1;
    try self.parseInstr();
    try self.parseInstr();
    self.indent_level -= 1;
}

fn block(self: *Self, data: *const Instruction.Block) Error!void {
    self.indent();
    try self.writer.print(
        "[Block pop count: {}, is_expr: {}]\n",
        .{ data.pop_count, data.is_expr },
    );

    self.indent_level += 1;
    for (0..data.length) |_| {
        try self.parseInstr();
    }
    self.indent_level -= 1;
}

fn boolInstr(self: *Self, value: bool) Error!void {
    self.indent();
    try self.writer.print("[Bool {}]\n", .{value});
}

fn cast(self: *Self, typ: Type) Error!void {
    self.indent();
    try self.writer.print("[Cast to {s}]\n", .{@tagName(typ)});
}

fn discard(self: *Self) Error!void {
    self.indent();
    try self.tree.appendSlice("[Discard]\n");
    self.indent_level += 1;
    try self.parseInstr();
    self.indent_level -= 1;
}

fn floatInstr(self: *Self, value: f64) Error!void {
    self.indent();
    try self.writer.print("[Float {d}]\n", .{value});
}

fn fnCall(self: *Self, data: *const Instruction.Call) Error!void {
    self.indent();
    try self.writer.print("[Fn call arity: {}, call_tag: {s}]\n", .{
        data.arity, @tagName(data.tag),
    });

    self.indent_level += 1;

    // Variable
    try self.parseInstr();

    if (data.arity > 0) {
        self.indent();
        try self.tree.appendSlice("- args:\n");

        var last: usize = 0;
        for (0..data.arity) |_| {
            switch (self.next()) {
                .value => |param_data| {
                    if (param_data.cast) {
                        self.indent();
                        self.tree.appendSlice("[Cast next value to float]\n") catch oom();
                    }
                    const save = self.instr_idx;
                    self.instr_idx = param_data.value_instr;
                    try self.parseInstr();
                    last = @max(last, self.instr_idx);

                    self.instr_idx = save;
                },
                .default_value => {},
                else => unreachable,
            }
        }

        if (last > self.instr_idx) self.instr_idx = last;
    }

    if (data.tag == .import) {
        self.indent();
        try self.tree.appendSlice("- load module:\n");
        try self.parseInstr();
    }

    self.indent_level -= 1;
}

fn getMember(self: *Self, data: *const Instruction.Member) Error!void {
    self.indent();
    try self.writer.print(
        "[{s} access {}]\n",
        .{ if (data.kind == .field) "Field" else "Method", data.index },
    );
    self.indent_level += 1;

    // Variable
    try self.parseInstr();
    self.indent_level -= 1;
}

fn moduleImport(self: *Self, data: *const Instruction.ModuleImport) Error!void {
    self.indent();
    try self.writer.print("[Import module {}, scope {s}]\n", .{ data.index, @tagName(data.scope) });
}

fn fnDeclaration(self: *Self, data: *const Instruction.FnDecl) Error!void {
    const fn_name = self.interner.getKey(self.next().name).?;
    const fn_var = self.next().var_decl.variable;

    self.indent();
    try self.writer.print(
        "[Fn declaration {s}, index: {}, scope: {s}, return kind: {s}]\n",
        .{ fn_name, fn_var.index, @tagName(fn_var.scope), @tagName(data.return_kind) },
    );

    self.indent_level += 1;
    if (data.default_params > 0) {
        self.indent();
        try self.tree.appendSlice("- default params\n");
        for (0..data.default_params) |_| {
            try self.parseInstr();
        }

        if (data.body_len > 0) {
            self.indent();
            try self.tree.appendSlice("- body\n");
        }
    }

    for (0..data.body_len) |_| {
        try self.parseInstr();
    }
    self.indent_level -= 1;
}

fn identifier(self: *Self, data: *const Instruction.Variable) Error!void {
    self.indent();
    try self.writer.print("[Variable index: {}, scope: {s}]\n", .{
        data.index, @tagName(data.scope),
    });
}

fn identifierAbsolute(self: *Self, data: usize) Error!void {
    self.indent();
    try self.writer.print("[Variable absolute index: {}]\n", .{data});
}

fn ifInstr(self: *Self, data: *const Instruction.If) Error!void {
    self.indent();
    try self.writer.print("[If cast: {s}, has else: {}]\n", .{
        @tagName(data.cast),
        data.has_else,
    });

    self.indent();
    try self.tree.appendSlice("- condition:\n");
    self.indent_level += 1;
    try self.parseInstr();
    self.indent_level -= 1;

    self.indent();
    try self.tree.appendSlice("- then:\n");
    self.indent_level += 1;
    try self.parseInstr();
    self.indent_level -= 1;

    if (data.has_else) {
        self.indent();
        try self.tree.appendSlice("- else:\n");
        self.indent_level += 1;
        try self.parseInstr();
        self.indent_level -= 1;
    }
}

fn intInstr(self: *Self, data: isize) Error!void {
    self.indent();
    try self.writer.print("[Int {}]\n", .{data});
}

fn itemImport(self: *Self, data: *const Instruction.ItemImport) Error!void {
    self.indent();
    try self.writer.print(
        "[Import field {} of module {} to scope {s}]\n",
        .{ data.field_index, data.module_index, @tagName(data.scope) },
    );
}

fn multipleVarDecl(self: *Self, count: usize) Error!void {
    for (0..count) |_| {
        try self.parseInstr();
    }
}

fn returnInstr(self: *Self, data: *const Instruction.Return) Error!void {
    self.indent();
    try self.writer.print("[Return expr: {}, cast: {}]\n", .{ data.value, data.cast });

    if (data.value) {
        self.indent_level += 1;
        try self.parseInstr();
        if (data.cast) try self.parseInstr();
        self.indent_level -= 1;
    }
}

fn stringInstr(self: *Self, index: usize) Error!void {
    self.indent();
    try self.writer.print("[String {s}]\n", .{self.interner.getKey(index).?});
}

fn structDecl(self: *Self, data: *const Instruction.StructDecl) Error!void {
    const name = self.next().name;
    const struct_var = self.next().var_decl.variable;

    self.indent();
    try self.writer.print("[Structure declaration {s}, index: {}, scope: {s}]\n", .{
        self.interner.getKey(name).?,
        struct_var.index,
        @tagName(struct_var.scope),
    });
    self.indent_level += 1;

    if (data.default_fields > 0) {
        self.indent();
        try self.tree.appendSlice("- default values\n");
        for (0..data.default_fields) |_| {
            try self.parseInstr();
        }
    }

    for (0..data.func_count) |_| {
        try self.parseInstr();
    }
    self.indent_level -= 1;
}

fn structLiteral(self: *Self, field_count: usize) Error!void {
    self.indent();
    try self.tree.appendSlice("[Structure literal]\n");
    self.indent_level += 1;
    // Variable containing type
    self.indent();
    try self.tree.appendSlice("- structure\n");
    try self.parseInstr();

    if (field_count > 0) {
        self.indent();
        try self.tree.appendSlice("- args\n");
        var last: usize = 0;

        for (0..field_count) |_| {
            switch (self.next()) {
                .value => |data| {
                    if (data.cast) {
                        self.indent();
                        self.tree.appendSlice("[Cast next value to float]\n") catch oom();
                    }
                    const save = self.instr_idx;
                    self.instr_idx = data.value_instr;
                    try self.parseInstr();
                    last = @max(last, self.instr_idx);

                    self.instr_idx = save;
                },
                .default_value => {},
                else => {},
            }
        }

        if (last > self.instr_idx) self.instr_idx = last;
        self.indent_level -= 1;
    }
}

fn unary(self: *Self, data: *const Instruction.Unary) Error!void {
    self.indent();
    try self.writer.print("[Unary {s}]\n", .{@tagName(data.op)});
    self.indent_level += 1;
    try self.parseInstr();
    self.indent_level -= 1;
}

fn use(self: *Self, count: u64) Error!void {
    // NOTE: For now, skips the first 'Null' placed by the analyzer
    // Needs a rework
    self.instr_idx += 1;

    self.indent();
    try self.writer.print("[Use count: {}]\n", .{count});
    self.instr_idx += 1;

    for (0..count) |_| {
        self.instr_idx += 1;
    }
}

fn varDecl(self: *Self, data: *const Instruction.VarDecl) Error!void {
    self.indent();
    try self.writer.print("[Variable declaration index: {}, scope: {s}]\n", .{
        data.variable.index,
        @tagName(data.variable.scope),
    });

    self.indent_level += 1;
    if (data.has_value)
        try self.parseInstr()
    else {
        self.indent();
        try self.tree.appendSlice("[Null]\n");
    }

    if (data.cast) try self.parseInstr();
    self.indent_level -= 1;
}

fn whileInstr(self: *Self) Error!void {
    self.indent();
    try self.tree.appendSlice("[While]\n");
    self.instr_idx += 1;
    self.indent();
    try self.tree.appendSlice("- condition:\n");
    self.indent_level += 1;
    try self.parseInstr();
    self.indent_level -= 1;
    self.indent();
    try self.tree.appendSlice("- body:\n");
    self.indent_level += 1;
    try self.parseInstr();
    self.indent_level -= 1;
}
