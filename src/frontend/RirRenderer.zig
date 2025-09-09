const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const Interner = @import("../Interner.zig");
const oom = @import("../utils.zig").oom;
const AnalyzerReport = @import("Analyzer.zig").AnalyzerReport;
const Ast = @import("Ast.zig");
const Node = @import("Ast.zig").Node;
const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const Type = rir.Type;
const Span = @import("Lexer.zig").Span;
const Token = @import("Lexer.zig").Token;

const Labels = struct { depth: usize, msg: []const u8 };

source: []const u8,
instrs: []const Instruction.Data,
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
    instrs: []const Instruction.Data,
    errs: []const AnalyzerReport,
    warns: []const AnalyzerReport,
    interner: *const Interner,
    static_analyzis: bool,
) Self {
    return .{
        .source = source,
        .instrs = instrs,
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

pub fn parseIr(self: *Self, file_name: []const u8) !void {
    self.writer = self.tree.writer();
    // TODO: remove the comment
    try self.writer.print("//-- {s} --\n", .{file_name});

    if (self.errs.len > 0)
        try self.parseErrs()
    else if (self.static_analyzis and self.warns.len > 0)
        try self.parseErrs()
    else while (self.instr_idx < self.instrs.len)
        self.parseInstr();

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
    return &self.instrs[self.instr_idx];
}

fn eof(self: *const Self) bool {
    return self.instr_idx == self.instrs.len;
}

fn next(self: *Self) Instruction.Data {
    defer self.instr_idx += 1;

    return self.instrs[self.instr_idx];
}

fn parseInstr(self: *Self) void {
    switch (self.next()) {
        .array => |*data| self.array(data),
        .array_access => self.arrayAccess(1, false, false, false),
        .array_access_chain => |*data| self.arrayAccess(data.depth, data.incr_rc, data.cow, false),
        .assignment => |*data| self.assignment(data),
        .binop => |*data| self.binop(data),
        .block => |*data| self.block(data),
        .bool => |data| self.boolInstr(data),
        .bound_method => |data| self.boundMethod(data),
        .call => |*data| self.fnCall(data),
        .capture => |*data| self.capture(data),
        .cast => |data| self.cast(data),
        .discard => self.discard(),
        .field => |*data| self.getField(data, false),
        .float => |data| self.floatInstr(data),
        .fn_decl => |*data| self.fnDeclaration(data),
        .identifier => |*data| self.identifier(data),
        .@"if" => |*data| self.ifInstr(data),
        .int => |data| self.intInstr(data),
        .load_symbol => |*data| self.loadSymbol(data),
        .multiple_var_decl => |data| self.multipleVarDecl(data),
        .name => unreachable,
        .null => unreachable,
        .print => {
            self.indentAndAppendSlice("[Print]");
            self.indent_level += 1;
            defer self.indent_level -= 1;
            self.parseInstr();
        },
        .@"return" => |*data| self.returnInstr(data),
        .string => |data| self.stringInstr(data),
        .struct_decl => |*data| self.structDecl(data),
        .default_value => unreachable,
        .struct_literal => |*data| self.structLiteral(data),
        .value => unreachable,
        .unary => |*data| self.unary(data),
        .var_decl => |*data| self.varDecl(data),
        .@"while" => self.whileInstr(),
    }
}

fn array(self: *Self, data: *const Instruction.Array) void {
    self.indentAndAppendSlice("[Array]");
    self.indent_level += 1;
    defer self.indent_level -= 1;

    for (data.elems) |elem| {
        self.parseInstr();

        if (elem.cast) {
            self.indentAndAppendSlice("[Cast to float]");
        } else if (elem.incr_rc) {
            self.indentAndAppendSlice("[Increment ref count]");
        }
    }
}

fn arrayAccess(self: *Self, depth: usize, incr_ref: bool, cow: bool, is_assign: bool) void {
    if (is_assign)
        self.indentAndAppendSlice(if (depth > 1) "[Array chain assignment]" else "[Array assignment]")
    else
        self.indentAndAppendSlice(if (depth > 1) "[Array chain access]" else "[Array access]");
    self.indent_level += 1;
    defer self.indent_level -= 1;

    if (incr_ref) self.indentAndAppendSlice("[Increment reference count]");
    if (cow) self.indentAndAppendSlice("[Cow]");

    if (depth > 1) {
        self.indentAndAppendSlice("- indicies");
        for (0..depth) |_| self.parseInstr();
        self.indentAndAppendSlice("- array");
        self.parseInstr();
    } else {
        self.indentAndAppendSlice("- array");
        self.parseInstr();
        self.indentAndAppendSlice("- index");
        self.parseInstr();
    }
}

fn assignment(self: *Self, data: *const Instruction.Assignment) void {
    // Value
    self.parseInstr();

    if (data.cast) {
        self.indentAndAppendSlice("[Cast to float]");
    } else if (data.incr_rc) {
        self.indentAndAppendSlice("[Incr ref count]");
    }

    const variable_data = switch (self.next()) {
        .array_access => return self.arrayAccess(1, false, data.cow, true),
        .array_access_chain => |*array_data| return self.arrayAccess(array_data.depth, false, data.cow, true),
        .identifier => |*variable| variable,
        .field => |*member| return self.fieldAssignment(member, data.cow),
        else => unreachable,
    };

    self.indentAndPrintSlice("[Assignment index: {}, scope: {s}{s}]", .{
        variable_data.index, @tagName(variable_data.scope), if (data.cow) ", cow" else "",
    });
}

fn arrayAssignment(self: *Self, depth: usize) void {
    self.indentAndAppendSlice(if (depth > 1) "[Array chain assignment]" else "[Array assignment]");
    self.indent_level += 1;
    defer self.indent_level -= 1;

    // Array
    self.indentAndAppendSlice("- variable");
    self.parseInstr();
    // Index
    self.indentAndAppendSlice(if (depth > 1) "- indicies" else "- index");

    for (0..depth) |_| self.parseInstr();
}

fn fieldAssignment(self: *Self, data: *const Instruction.Field, cow: bool) void {
    self.indentAndAppendSlice("[Field assignment]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.getField(data, cow);
}

fn binop(self: *Self, data: *const Instruction.Binop) void {
    self.indentAndPrintSlice("[Binop type: {s}, cast: {s}]", .{ @tagName(data.op), @tagName(data.cast) });
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr();
    self.parseInstr();
}

fn block(self: *Self, data: *const Instruction.Block) void {
    self.indentAndPrintSlice("[Block pop count: {}, is_expr: {}]", .{ data.pop_count, data.is_expr });

    self.indent_level += 1;
    defer self.indent_level -= 1;
    for (0..data.length) |_| {
        self.parseInstr();
    }
}

fn boolInstr(self: *Self, value: bool) void {
    self.indentAndPrintSlice("[Bool {}]", .{value});
}

fn boundMethod(self: *Self, field_index: usize) void {
    self.indentAndPrintSlice("[Bound method, method index: {}]", .{field_index});

    self.indent_level += 1;
    defer self.indent_level -= 1;

    // Variable
    self.parseInstr();
}

fn cast(self: *Self, typ: Type) void {
    self.indentAndPrintSlice("[Cast to {s}]", .{@tagName(typ)});
}

fn discard(self: *Self) void {
    self.indentAndAppendSlice("[Discard]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr();
}

fn floatInstr(self: *Self, value: f64) void {
    self.indentAndPrintSlice("[Float {d}]", .{value});
}

fn fnCall(self: *Self, data: *const Instruction.Call) void {
    self.indentAndPrintSlice("[Fn call arity: {}]", .{data.arity});

    self.indent_level += 1;
    defer self.indent_level -= 1;

    // Variable
    self.parseInstr();

    if (data.arity > 0) {
        self.indentAndAppendSlice("- args:");

        var last: usize = 0;
        for (0..data.arity) |_| {
            switch (self.next()) {
                .value => |param_data| {
                    if (param_data.cast) {
                        self.indentAndAppendSlice("[Cast next value to float]");
                    }
                    if (param_data.box) {
                        self.indentAndAppendSlice("[Box next value]");
                    }
                    const save = self.instr_idx;
                    self.instr_idx = param_data.value_instr;
                    self.parseInstr();
                    last = @max(last, self.instr_idx);

                    self.instr_idx = save;
                },
                .default_value => {},
                else => |eee| {
                    std.log.debug("Found: {any}", .{eee});
                    unreachable;
                },
            }
        }

        if (last > self.instr_idx) self.instr_idx = last;
    }
}

fn capture(self: *Self, data: *const Instruction.Capture) void {
    self.indentAndPrintSlice("[Capture index: {}, is_local: {}]", .{ data.index, data.is_local });
}

fn getField(self: *Self, data: *const Instruction.Field, cow: bool) void {
    self.indentAndPrintSlice(
        "[{s} access {}{s}]",
        .{ if (data.kind == .field) "Field" else "Method", data.index, if (cow) ", cow" else "" },
    );

    self.indent_level += 1;
    defer self.indent_level -= 1;

    // Variable
    self.parseInstr();
}

//TODO: captures count
fn fnDeclaration(self: *Self, data: *const Instruction.FnDecl) void {
    const fn_name = if (data.name) |idx| self.interner.getKey(idx).? else "";

    const fn_kind = switch (data.kind) {
        .symbol => "Function",
        .closure => "Closure",
    };

    self.indentAndPrintSlice(
        "[{s} declaration {s}, return kind: {s}]",
        .{ fn_kind, fn_name, @tagName(data.return_kind) },
    );

    self.indent_level += 1;
    defer self.indent_level -= 1;

    if (data.default_params > 0) {
        self.indentAndAppendSlice("- default params");
        for (0..data.default_params) |_| {
            self.parseInstr();
        }

        if (data.body_len > 0) {
            self.indentAndAppendSlice("- body");
        }
    }

    for (0..data.body_len) |_| {
        self.parseInstr();
    }

    if (data.captures_count > 0) {
        self.indentAndAppendSlice("- captures");
        for (0..data.captures_count) |_| {
            self.parseInstr();
        }
    }
}

fn identifier(self: *Self, data: *const Instruction.Variable) void {
    self.indentAndPrintSlice("[Variable index: {}, scope: {s}]", .{
        data.index, @tagName(data.scope),
    });
}

fn identifierId(self: *Self, data: *const Instruction.IdentifierId) void {
    const variable_data = self.instrs[data.index].var_decl.variable;
    self.indentAndPrintSlice("[Variable index: {}, scope: {s}]", .{
        variable_data.index, @tagName(variable_data.scope),
    });

    if (data.rc_action == .increment)
        self.indentAndAppendSlice("[Increment reference count]")
    else if (data.rc_action == .cow)
        self.indentAndAppendSlice("[Cow]");
}

fn ifInstr(self: *Self, data: *const Instruction.If) void {
    self.indentAndPrintSlice("[If cast: {s}, has else: {}]", .{
        @tagName(data.cast),
        data.has_else,
    });

    self.indentAndAppendSlice("- condition:");
    self.indent_level += 1;
    self.parseInstr();
    self.indent_level -= 1;

    self.indentAndAppendSlice("- then:");
    self.indent_level += 1;
    self.parseInstr();
    self.indent_level -= 1;

    if (data.has_else) {
        self.indentAndAppendSlice("- else:");
        self.indent_level += 1;
        self.parseInstr();
        self.indent_level -= 1;
    }
}

fn intInstr(self: *Self, data: isize) void {
    self.indentAndPrintSlice("[Int {}]", .{data});
}

fn loadSymbol(self: *Self, data: *const Instruction.LoadSymbol) void {
    if (data.module_index) |mod| {
        self.indentAndPrintSlice("[Load symbol {} of module {}]", .{ data.symbol_index, mod });
    } else {
        self.indentAndPrintSlice("[Load symbol {}]", .{data.symbol_index});
    }
}

fn multipleVarDecl(self: *Self, count: usize) void {
    for (0..count) |_| {
        self.parseInstr();
    }
}

fn returnInstr(self: *Self, data: *const Instruction.Return) void {
    self.indentAndPrintSlice("[Return expr: {}, cast: {}]", .{ data.value, data.cast });

    if (data.value) {
        self.indent_level += 1;
        self.parseInstr();
        if (data.cast) self.parseInstr();
        self.indent_level -= 1;
    }
}

fn stringInstr(self: *Self, index: usize) void {
    self.indentAndPrintSlice("[String {s}]", .{self.interner.getKey(index).?});
}

fn structDecl(self: *Self, data: *const Instruction.StructDecl) void {
    const name = self.next().name;
    self.indentAndPrintSlice("[Structure declaration {s}]", .{self.interner.getKey(name).?});
    self.indent_level += 1;
    defer self.indent_level -= 1;

    if (data.default_fields > 0) {
        self.indentAndAppendSlice("- default values");
        for (0..data.default_fields) |_| {
            self.parseInstr();
        }
    }

    for (0..data.func_count) |_| {
        self.parseInstr();
    }
}

fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) void {
    self.indentAndAppendSlice("[Structure literal]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    // Variable containing type
    self.indentAndAppendSlice("- structure");
    self.parseInstr();

    if (data.fields_count > 0) {
        self.indentAndAppendSlice("- args");
        var last: usize = 0;

        for (0..data.fields_count) |_| {
            switch (self.next()) {
                .value => |value_data| {
                    if (value_data.cast) {
                        self.indentAndAppendSlice("[Cast next value to float]");
                    }
                    const save = self.instr_idx;
                    self.instr_idx = value_data.value_instr;
                    self.parseInstr();
                    last = @max(last, self.instr_idx);

                    self.instr_idx = save;
                },
                .default_value => {},
                else => {},
            }
        }

        if (last > self.instr_idx) self.instr_idx = last;
    }
}

fn unary(self: *Self, data: *const Instruction.Unary) void {
    self.indentAndPrintSlice("[Unary {s}]", .{@tagName(data.op)});
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr();
}

fn varDecl(self: *Self, data: *const Instruction.VarDecl) void {
    self.indentAndPrintSlice("[Variable declaration index: {}, scope: {s}{s}{s}]", .{
        data.variable.index,
        @tagName(data.variable.scope),
        if (data.box) ", box" else "",
        if (data.incr_rc) ", incr_rc" else "",
    });

    self.indent_level += 1;
    defer self.indent_level -= 1;
    if (data.has_value) self.parseInstr() else self.indentAndAppendSlice("[Null]");

    if (data.cast) self.parseInstr();
}

fn whileInstr(self: *Self) void {
    self.indentAndAppendSlice("[While]");
    self.instr_idx += 1;
    self.indentAndAppendSlice("- condition:");
    self.indent_level += 1;
    self.parseInstr();
    self.indent_level -= 1;
    self.indentAndAppendSlice("- body:");
    self.indent_level += 1;
    self.parseInstr();
    self.indent_level -= 1;
}

fn indentAndAppendSlice(self: *Self, text: []const u8) void {
    self.indent();
    self.tree.appendSlice(text) catch oom();
    self.tree.appendSlice("\n") catch oom();
}

fn indentAndPrintSlice(self: *Self, comptime fmt: []const u8, args: anytype) void {
    self.indent();
    self.writer.print(fmt, args) catch oom();
    self.tree.appendSlice("\n") catch oom();
}
