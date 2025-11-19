const std = @import("std");
const ArrayList = std.ArrayList;
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const misc = @import("misc");

const Interner = misc.Interner;
const oom = misc.oom;
const Set = misc.Set;
const AnalyzerReport = @import("../analyzer/Analyzer.zig").AnalyzerReport;
const Ast = @import("../parser/Ast.zig");
const Node = Ast.Node;
const ir = @import("ir.zig");
const Instruction = ir.Instruction;
const Type = ir.Type;
const Span = @import("../parser/Lexer.zig").Span;
const Token = @import("../parser/Lexer.zig").Token;

const Labels = struct { depth: usize, msg: []const u8 };

allocator: Allocator,
interner: *const Interner,
instrs: []const Instruction.Data,
indent_level: u8,
tree: ArrayList(u8),
writer: std.ArrayList(u8).Writer,
compiled_constants: Set(usize),

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
        .compiled_constants = .empty,
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

fn parseInstr(self: *Self, instr: ir.Index) void {
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
        .constant => |data| self.constant(data),
        .discard => |index| self.indexInstr("Discard", index),
        .enum_create => |data| self.enumCreate(data),
        .enum_decl => |*data| self.enumDecl(data),
        .field => |data| self.getField(data, false),
        .float => |data| self.floatInstr(data),
        .fn_decl => |*data| self.fnDeclaration(data),
        .identifier => |*data| self.identifier(data),
        .@"if" => |*data| self.ifInstr(data),
        .int => |data| self.intInstr(data),
        .incr_rc => |index| self.indexInstr("Incr rc", index),
        .load_symbol => |*data| self.loadSymbol(data),
        .load_builtin => |index| self.indentAndPrintSlice("[Builtin symbol: {}]", .{index}),
        .match => |*data| self.match(data),
        .multiple_var_decl => |*data| self.multipleVarDecl(data),
        .null => self.indentAndAppendSlice("[Null]"),
        .pat_nullable => |index| self.indexInstr("Nullable pattern", index),
        .pop => |index| self.indexInstr("Pop", index),
        .print => |index| self.indexInstr("Print", index),
        .@"return" => |*data| self.returnInstr(data),
        .string => |data| self.stringInstr(data),
        .struct_decl => |*data| self.structDecl(data),
        .struct_literal => |*data| self.structLiteral(data),
        .unary => |*data| self.unary(data),
        .unbox => |index| self.indexInstr("Unbox", index),
        .var_decl => |*data| self.varDecl(data),
        .when => |*data| self.when(data),
        .@"while" => |data| self.whileInstr(data),

        .noop => {},
    }
}

fn indexInstr(self: *Self, name: []const u8, index: ir.Index) void {
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
        .field => |member| return self.fieldAssignment(member, data.cow),
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

fn fieldAssignment(self: *Self, data: Instruction.Field, cow: bool) void {
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
    self.indentAndPrintSlice("[Bound method, symbol index: {}]", .{data.index});
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

fn floatInstr(self: *Self, value: f64) void {
    self.indentAndPrintSlice("[Float {d}]", .{value});
}

fn fnCall(self: *Self, data: *const Instruction.Call) void {
    callee: {
        switch (self.instrs[data.callee]) {
            .field => |f| {
                if (f.kind == .function) {
                    self.parseInstr(data.callee);

                    if (data.ext_mod) |mod| {
                        self.indentAndPrintSlice("[Invoke symbol {} module {}]", .{ f.index, mod });
                    } else {
                        self.indentAndPrintSlice("[Invoke symbol {}]", .{f.index});
                    }
                    break :callee;
                }
            },
            .load_symbol => |sym| {
                if (data.ext_mod) |mod| {
                    self.indentAndPrintSlice("[Call symbol {} module {}]", .{ sym.symbol_index, mod });
                } else {
                    self.indentAndPrintSlice("[Call symbol {}]", .{sym.symbol_index});
                }
                break :callee;
            },
            else => {},
        }

        self.indentAndAppendSlice(if (data.native) "[Native fn call]" else "[Fn call]");
        self.indent_level += 1;
        defer self.indent_level -= 1;
        self.parseInstr(data.callee);
    }

    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.argsList("param", data.args);
}

fn argsList(self: *Self, kind: []const u8, args: []const Instruction.Arg) void {
    if (args.len == 0) return;

    self.indentAndAppendSlice("- args");
    for (args) |arg| {
        switch (arg) {
            .default => |def| {
                if (def.mod) |mod| {
                    self.indentAndPrintSlice("[Default {s} constant index {}, module {}]", .{ kind, def.const_index, mod });
                } else {
                    self.indentAndPrintSlice("[Default {s} constant index {}]", .{ kind, def.const_index });
                }
            },
            .instr => |i| self.parseInstr(i),
        }
    }
}

fn capture(self: *Self, data: *const Instruction.Capture) void {
    self.indentAndPrintSlice("[Capture index: {}, is_local: {}]", .{ data.index, data.local });
}

fn constant(self: *Self, data: Instruction.Constant) void {
    self.indentAndPrintSlice("[Constant {}]", .{data.index});
    const gop = self.compiled_constants.getOrPut(self.allocator, data.index) catch oom();
    if (!gop.found_existing) {
        self.indent_level += 1;
        defer self.indent_level -= 1;
        self.parseInstr(data.instr);
    }
}

fn enumCreate(self: *Self, data: Instruction.EnumCreate) void {
    self.indentAndAppendSlice("[Enum create]");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.indentAndAppendSlice("- enum");
    self.loadSymbol(&data.sym);
    self.indentAndAppendSlice("- tag");
    self.indentAndPrintSlice("{}", .{data.tag_index});
}

fn enumDecl(self: *Self, data: *const Instruction.EnumDecl) void {
    self.indentAndPrintSlice("[Enum declaration {s}]", .{self.interner.getKey(data.name).?});
}

fn getField(self: *Self, data: Instruction.Field, cow: bool) void {
    // if (data.kind == .field) {
    self.indentAndPrintSlice("[Field access {}{s}]", .{ data.index, if (cow) ", cow" else "" });
    // } else {
    //     self.indentAndPrintSlice("[Invoke symbol {}{s}]", .{ data.index, if (cow) ", cow" else "" });
    // }

    self.indent_level += 1;
    defer self.indent_level -= 1;
    self.parseInstr(data.structure);
}

fn fnDeclaration(self: *Self, data: *const Instruction.FnDecl) void {
    const fn_name = if (data.name) |idx| self.interner.getKey(idx).? else "";
    self.indentAndPrintSlice("[Function declaration {s}{s}]", .{ fn_name, if (data.returns) ", returns" else "" });

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
        self.indentAndPrintSlice("[Load symbol {} module {}]", .{ data.symbol_index, mod });
    } else {
        self.indentAndPrintSlice("[Load symbol {}]", .{data.symbol_index});
    }
}

fn match(self: *Self, data: *const Instruction.Match) void {
    self.indentAndAppendSlice("[Match]");
    self.indentAndAppendSlice("- expression:");
    self.indent_level += 1;
    self.parseInstr(data.expr);
    self.indent_level -= 1;
    self.indentAndAppendSlice("- arms:");
    self.indent_level += 1;
    defer self.indent_level -= 1;
    for (data.arms) |arm| {
        self.indentAndAppendSlice("- expr:");
        self.indent_level += 1;
        self.parseInstr(arm.expr);
        self.indent_level -= 1;
        self.indentAndAppendSlice("- body:");
        self.indent_level += 1;
        self.parseInstr(arm.body);
        self.indent_level -= 1;
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
    self.argsList("field", data.values);
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

fn when(self: *Self, data: *const Instruction.When) void {
    self.indentAndAppendSlice("[When]");
    self.indentAndAppendSlice("- expression:");
    self.indent_level += 1;
    self.parseInstr(data.expr);
    self.indent_level -= 1;
    self.indentAndAppendSlice("- arms:");
    for (data.arms) |arm| {
        self.indentAndPrintSlice("[Types id: {}]", .{arm.type_id});
        self.indent_level += 1;
        self.parseInstr(arm.body);
        self.indent_level -= 1;
    }
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
