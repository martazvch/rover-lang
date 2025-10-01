const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const Interner = @import("misc").Interner;
const oom = @import("misc").oom;
const AnalyzerReport = @import("../analyzer/Analyzer.zig").AnalyzerReport;
const Ast = @import("../ast/Ast.zig");
const Node = Ast.Node;
const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const Type = rir.Type;
const Span = @import("../parser/Lexer.zig").Span;
const Token = @import("../parser/Lexer.zig").Token;

const Labels = struct { depth: usize, msg: []const u8 };

allocator: Allocator,
interner: *const Interner,
instrs: []const Instruction.Data,
indent_level: u8,
tree: ArrayList(u8),
writer: std.ArrayList(u8).Writer,

const indent_size: u8 = 4;
const spaces: [1024]u8 = [_]u8{' '} ** 1024;

const Error = Allocator.Error || std.fmt.BufPrintError;
const Self = @This();

pub fn init(allocator: Allocator, instrs: []const Instruction.Data, interner: *const Interner) Self {
    return .{
        .allocator = allocator,
        .interner = interner,
        .instrs = instrs,
        .tree = .empty,
        .writer = undefined,
        .indent_level = 0,
    };
}

fn indent(self: *Self) void {
    self.tree.appendSlice(self.allocator, Self.spaces[0 .. self.indent_level * Self.indent_size]) catch oom();
}

pub fn renderIr(self: *Self, file_name: []const u8, roots: []const usize) Error![]const u8 {
    self.writer = self.tree.writer(self.allocator);
    // TODO: remove the comment
    try self.writer.print("//-- {s} --\n", .{file_name});

    for (roots) |root| {
        self.parseInstr(root);
    }

    try self.writer.writeAll("\n");

    return self.tree.items;
}

fn parseInstr(self: *Self, instr: rir.Index) void {
    switch (self.instrs[instr]) {
        .array => |*data| self.array(data),
        .array_access => |*data| self.arrayAccess(data, false, false),
        .assignment => |*data| self.assignment(data),
        .binop => |*data| self.binop(data),
        .block => |*data| self.block(data),
        .bool => |data| self.boolInstr(data),
        .box => |data| self.indexInstr("Box", data),
        .bound_method => |data| self.boundMethod(data),
        .@"break" => |data| self.breakInstr(data),
        .call => |*data| self.fnCall(data),
        .cast_to_float => |index| self.indexInstr("Cast to float", index),
        .discard => |index| self.indexInstr("Discard", index),
        .extractor => |index| self.indexInstr("Extractor", index),
        .field => |*data| self.getField(data, false),
        .float => |data| self.floatInstr(data),
        .fn_decl => |*data| self.fnDeclaration(data),
        .identifier => |*data| self.identifier(data),
        .@"if" => |*data| self.ifInstr(data),
        .int => |data| self.intInstr(data),
        .incr_rc => |index| self.indexInstr("Incr rc", index),
        .load_symbol => |*data| self.loadSymbol(data),
        .multiple_var_decl => |*data| self.multipleVarDecl(data),
        .null => self.indentAndAppendSlice("[Null]"),
        .pop => |index| self.indexInstr("Pop", index),
        .print => |index| self.indexInstr("Print", index),
        .@"return" => |*data| self.returnInstr(data),
        .string => |data| self.stringInstr(data),
        .struct_decl => |*data| self.structDecl(data),
        .struct_literal => |*data| self.structLiteral(data),
        .unary => |*data| self.unary(data),
        .unbox => |index| self.indexInstr("Unbox", index),
        .var_decl => |*data| self.varDecl(data),
        .@"while" => |data| self.whileInstr(data),

        .noop => {},
    }
}

fn indexInstr(self: *Self, name: []const u8, index: rir.Index) void {
    self.indentAndPrintSlice("[{s}]", .{name});
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(index);
}

fn array(self: *Self, data: *const Instruction.Array) void {
    self.indentAndAppendSlice("[Array]");
    self.indent_level += 1;
    defer self.indent_level -= 1;

    for (data.values) |value| {
        self.parseInstr(value);
    }
}

fn arrayAccess(self: *Self, data: *const Instruction.ArrayAccess, cow: bool, is_assign: bool) void {
    self.indentAndAppendSlice(if (is_assign) "[Array assignment]" else "[Array access]");
    self.indent_level += 1;
    defer self.indent_level -= 1;

    if (cow) self.indentAndAppendSlice("[Cow]");

    self.indentAndAppendSlice("- index");
    for (data.indicies) |index| self.parseInstr(index);
    self.indentAndAppendSlice("- array");
    self.parseInstr(data.array);
}

fn assignment(self: *Self, data: *const Instruction.Assignment) void {
    self.parseInstr(data.value);

    const variable_data, const unbox = switch (self.instrs[data.assigne]) {
        .array_access => |*arr_data| return self.arrayAccess(arr_data, data.cow, true),
        .identifier => |*variable| .{ variable, false },
        .field => |*member| return self.fieldAssignment(member, data.cow),
        .unbox => |index| .{ &self.instrs[index].identifier, true },
        else => |got| {
            std.log.debug("Got: {any}", .{got});
            unreachable;
        },
    };

    self.indentAndPrintSlice("[Assignment index: {}, scope: {s}{s}{s}]", .{
        variable_data.index,           @tagName(variable_data.scope),
        if (data.cow) ", cow" else "", if (unbox) ", unbox" else "",
    });
}

fn fieldAssignment(self: *Self, data: *const Instruction.Field, cow: bool) void {
    self.indentAndAppendSlice("[Field assignment]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.getField(data, cow);
}

fn binop(self: *Self, data: *const Instruction.Binop) void {
    self.indentAndPrintSlice("[Binop type: {t}]", .{data.op});
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(data.lhs);
    self.parseInstr(data.rhs);
}

fn block(self: *Self, data: *const Instruction.Block) void {
    // self.indentAndPrintSlice("[Block pop count: {}, is_expr: {}]", .{ data.pop_count, data.ret_slot != null });
    self.indentAndPrintSlice("[Block pop count: {}, is_expr: {}]", .{ data.pop_count, data.is_expr });

    self.indent_level += 1;
    defer self.indent_level -= 1;
    for (data.instrs) |instr| {
        self.parseInstr(instr);
    }
}

fn boolInstr(self: *Self, value: bool) void {
    self.indentAndPrintSlice("[Bool {}]", .{value});
}

fn boundMethod(self: *Self, data: Instruction.BoundMethod) void {
    self.indentAndPrintSlice("[Bound method, method index: {}]", .{data.index});
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(data.structure);
}

fn breakInstr(self: *Self, data: Instruction.Break) void {
    self.indentAndPrintSlice("[Break depth: {}]", .{data.depth});
    if (data.instr) |instr| {
        self.indent_level += 1;
        defer self.indent_level -= 1;
        self.parseInstr(instr);
    }
}

fn discard(self: *Self) void {
    self.indentAndAppendSlice("[Discard]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr();
}

fn extractor(self: *Self) void {
    self.indentAndAppendSlice("[Extractor]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr();
}

fn floatInstr(self: *Self, value: f64) void {
    self.indentAndPrintSlice("[Float {d}]", .{value});
}

fn fnCall(self: *Self, data: *const Instruction.Call) void {
    self.indentAndAppendSlice("[Fn call]");

    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(data.callee);
    self.argsList(data.args);
}

fn argsList(self: *Self, args: []const Instruction.Arg) void {
    if (args.len == 0) return;

    self.indentAndAppendSlice("- args");
    for (args) |arg| {
        switch (arg) {
            .default => |def| self.indentAndPrintSlice("[Default field {}]", .{def}),
            .instr => |i| self.parseInstr(i),
        }
    }
}

fn capture(self: *Self, data: *const Instruction.Capture) void {
    self.indentAndPrintSlice("[Capture index: {}, is_local: {}]", .{ data.index, data.local });
}

fn getField(self: *Self, data: *const Instruction.Field, cow: bool) void {
    self.indentAndPrintSlice(
        "[{s} access {}{s}]",
        .{ if (data.kind == .field) "Field" else "Method", data.index, if (cow) ", cow" else "" },
    );

    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(data.structure);
}

fn fnDeclaration(self: *Self, data: *const Instruction.FnDecl) void {
    const fn_name = if (data.name) |idx| self.interner.getKey(idx).? else "";

    const fn_kind = switch (data.kind) {
        .symbol => "Function",
        .closure => "Closure",
    };

    self.indentAndPrintSlice(
        "[{s} declaration {s}{s}]",
        .{ fn_kind, fn_name, if (data.returns) ", returns" else "" },
    );

    self.indent_level += 1;
    defer self.indent_level -= 1;

    if (data.defaults.len > 0) {
        self.indentAndAppendSlice("- default params");
        for (data.defaults) |def| {
            self.parseInstr(def);
        }

        // Add a separation between the two category, otherwise not needed
        if (data.body.len > 0) self.indentAndAppendSlice("- body");
    }

    for (data.body) |instr| {
        self.parseInstr(instr);
    }

    if (data.captures.len > 0) {
        self.indentAndAppendSlice("- captures");
        for (data.captures) |capt| {
            self.indentAndPrintSlice("[Capture index: {}, is_local: {}]", .{ capt.index, capt.local });
        }
    }
}

fn identifier(self: *Self, data: *const Instruction.Variable) void {
    self.indentAndPrintSlice("[Variable index: {}, scope: {t}]", .{
        data.index, data.scope,
    });
}

fn ifInstr(self: *Self, data: *const Instruction.If) void {
    self.indentAndAppendSlice("[If]");

    self.indentAndAppendSlice("- condition:");
    self.indent_level += 1;
    self.parseInstr(data.cond);
    self.indent_level -= 1;

    self.indentAndAppendSlice("- then:");
    self.indent_level += 1;
    self.parseInstr(data.then);
    self.indent_level -= 1;

    if (data.@"else") |instr| {
        self.indentAndAppendSlice("- else:");
        self.indent_level += 1;
        self.parseInstr(instr);
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

fn multipleVarDecl(self: *Self, data: *const Instruction.MultiVarDecl) void {
    for (data.decls) |decl| {
        self.parseInstr(decl);
    }
}

fn returnInstr(self: *Self, data: *const Instruction.Return) void {
    self.indentAndAppendSlice("[Return]");

    if (data.value) |val| {
        self.indent_level += 1;
        self.parseInstr(val);
        self.indent_level -= 1;
    }
}

fn stringInstr(self: *Self, index: usize) void {
    self.indentAndPrintSlice("[String {s}]", .{self.interner.getKey(index).?});
}

fn structDecl(self: *Self, data: *const Instruction.StructDecl) void {
    self.indentAndPrintSlice("[Structure declaration {s}]", .{self.interner.getKey(data.name).?});
    self.indent_level += 1;
    defer self.indent_level -= 1;

    if (data.default_fields.len > 0) {
        self.indentAndAppendSlice("- default values");
        for (data.default_fields) |def| {
            self.parseInstr(def);
        }
    }

    for (data.functions) |func| {
        self.parseInstr(func);
    }
}

fn structLiteral(self: *Self, data: *const Instruction.StructLiteral) void {
    self.indentAndAppendSlice("[Structure literal]");

    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.indentAndAppendSlice("- structure");
    self.parseInstr(data.structure);
    self.argsList(data.values);
}

fn unary(self: *Self, data: *const Instruction.Unary) void {
    self.indentAndPrintSlice("[Unary {s}]", .{@tagName(data.op)});
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(data.instr);
}

fn varDecl(self: *Self, data: *const Instruction.VarDecl) void {
    self.indentAndPrintSlice("[Variable declaration index: {}, scope: {t}]", .{
        data.variable.index,
        data.variable.scope,
    });

    self.indent_level += 1;
    defer self.indent_level -= 1;
    if (data.value) |index| self.parseInstr(index) else self.indentAndAppendSlice("[Null]");
}

fn whileInstr(self: *Self, data: Instruction.While) void {
    self.indentAndAppendSlice("[While]");
    self.indentAndAppendSlice("- condition:");
    self.indent_level += 1;
    self.parseInstr(data.cond);
    self.indent_level -= 1;
    self.indentAndAppendSlice("- body:");
    self.indent_level += 1;
    self.parseInstr(data.body);
    self.indent_level -= 1;
}

fn indentAndAppendSlice(self: *Self, text: []const u8) void {
    self.indent();
    self.tree.appendSlice(self.allocator, text) catch oom();
    self.tree.appendSlice(self.allocator, "\n") catch oom();
}

fn indentAndPrintSlice(self: *Self, comptime fmt: []const u8, args: anytype) void {
    self.indent();
    self.writer.print(fmt, args) catch oom();
    self.tree.appendSlice(self.allocator, "\n") catch oom();
}

fn checkInrcRc(self: *Self, incr_rc: bool) void {
    if (incr_rc) self.indentAndAppendSlice("[Increment ref count]");
}
