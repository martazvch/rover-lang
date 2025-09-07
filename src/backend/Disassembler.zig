const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Obj = @import("../runtime/Obj.zig");
const Module = @import("compiler.zig").CompiledModule;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;

chunk: *const Chunk,
module: *const Module,
disassembled: ArrayList(u8),
render_mode: RenderMode,

prev_line: usize = 0,

const Self = @This();
pub const RenderMode = enum { none, normal, @"test" };

pub fn init(allocator: Allocator, chunk: *const Chunk, module: *const Module, render_mode: RenderMode) Self {
    return .{
        .chunk = chunk,
        .module = module,
        .disassembled = ArrayList(u8).init(allocator),
        .render_mode = render_mode,
    };
}

pub fn deinit(self: *Self) void {
    self.disassembled.deinit();
}

pub fn disChunk(self: *Self, name: []const u8) !void {
    try self.disSlice(name, 0);
}

pub fn disSlice(self: *Self, name: []const u8, start: usize) !void {
    var writer = self.disassembled.writer();
    try writer.print("-- {s} --\n", .{name});

    var i: usize = start;
    while (i < self.chunk.code.items.len) {
        i = try self.disInstruction(i, writer);
    }
}

pub fn disInstruction(self: *Self, offset: usize, writer: anytype) (Allocator.Error || std.posix.WriteError)!usize {
    if (self.render_mode == .normal) {
        try writer.print(" {:0>4}  ", .{offset});

        const line = self.chunk.offsets.items[offset];
        if (line > self.prev_line) {
            try writer.print("{:>4}  ", .{line});
        } else {
            try writer.writeAll("   |  ");
        }
        self.prev_line = line;
    }

    const op: OpCode = @enumFromInt(self.chunk.code.items[offset]);
    return switch (op) {
        .add_float => self.simpleInstruction("add_float", offset, writer),
        .add_int => self.simpleInstruction("add_int", offset, writer),
        .array_new => self.indexInstruction("array_new", offset, writer),
        .array_get => self.simpleInstruction("array_get", offset, writer),
        .array_get_cow => self.simpleInstruction("array_get_cow", offset, writer),
        .array_get_chain => self.indexInstruction("array_get_chain", offset, writer),
        .array_set => self.simpleInstruction("array_set", offset, writer),
        .array_set_chain => self.indexInstruction("array_set_chain", offset, writer),
        .box => self.simpleInstruction("box", offset, writer),
        .call => self.indexInstruction("call", offset, writer),
        .call_native => self.indexInstruction("call_native", offset, writer),
        .cast_to_float => self.simpleInstruction("cast_to_float", offset, writer),
        .closure => self.indexInstruction("closure", offset, writer),
        .constant => self.constantInstruction("constant", offset, writer),
        .def_global => self.indexInstruction("def_global", offset, writer),
        .div_float => self.simpleInstruction("div_float", offset, writer),
        .div_int => self.simpleInstruction("div_int", offset, writer),
        .dup => self.simpleInstruction("dup", offset, writer),
        .eq_bool => self.simpleInstruction("eq_bool", offset, writer),
        .eq_float => self.simpleInstruction("eq_float", offset, writer),
        .eq_int => self.simpleInstruction("eq_int", offset, writer),
        .eq_str => self.simpleInstruction("eq_str", offset, writer),
        .exit_repl => self.simpleInstruction("exit_repl", offset, writer),
        .ge_float => self.simpleInstruction("ge_float", offset, writer),
        .ge_int => self.simpleInstruction("ge_int", offset, writer),
        .get_default => self.indexInstruction("get_default", offset, writer),
        .get_field => self.getMember("get_field", offset, writer),
        .get_field_cow => self.getMember("get_field_cow", offset, writer),
        .get_global => self.getGlobal(false, offset, writer),
        .get_global_cow => self.getGlobal(true, offset, writer),
        .get_local => self.indexInstruction("get_local", offset, writer),
        .get_local_cow => self.indexInstruction("get_local_cow", offset, writer),
        .get_method => self.indexInstruction("get_method", offset, writer),
        .get_static_method => self.getMember("get_static_method", offset, writer),
        .gt_float => self.simpleInstruction("gt_float", offset, writer),
        .gt_int => self.simpleInstruction("gt_int", offset, writer),
        .incr_ref => self.simpleInstruction("incr_ref", offset, writer),
        .jump => self.jumpInstruction("jump", 1, offset, writer),
        .jump_false => self.jumpInstruction("jump_false", 1, offset, writer),
        .jump_true => self.jumpInstruction("jump_true", 1, offset, writer),
        .le_float => self.simpleInstruction("le_float", offset, writer),
        .le_int => self.simpleInstruction("le_int", offset, writer),
        .lt_float => self.simpleInstruction("lt_float", offset, writer),
        .lt_int => self.simpleInstruction("lt_int", offset, writer),
        .load_extern_sym => self.indexExternInstruction("load_extern_sym", offset, writer),
        // .load_fn_default => self.simpeInstruction("load_fn_default", offset, writer),
        // .load_struct_def => self.simpleInstruction("load_struct_def", offset, writer),
        .load_sym => self.loadSymbol(offset, writer),
        .loop => self.jumpInstruction("loop", -1, offset, writer),
        .mul_float => self.simpleInstruction("mul_float", offset, writer),
        .mul_int => self.simpleInstruction("mul_int", offset, writer),
        .ne_bool => self.simpleInstruction("ne_bool", offset, writer),
        .ne_float => self.simpleInstruction("ne_float", offset, writer),
        .ne_int => self.simpleInstruction("ne_int", offset, writer),
        .ne_str => self.simpleInstruction("ne_str", offset, writer),
        .negate_float => self.simpleInstruction("negate_float", offset, writer),
        .negate_int => self.simpleInstruction("negate_int", offset, writer),
        .not => self.simpleInstruction("not", offset, writer),
        .pop => self.simpleInstruction("pop", offset, writer),
        .print => self.simpleInstruction("print", offset, writer),
        .push_false => self.simpleInstruction("push_false", offset, writer),
        .push_null => self.simpleInstruction("push_null", offset, writer),
        .push_true => self.simpleInstruction("push_true", offset, writer),
        .ret => self.simpleInstruction("ret", offset, writer),
        .ret_naked => self.simpleInstruction("ret_naked", offset, writer),
        .ret_scope => self.indexInstruction("ret_scope", offset, writer),
        .set_field => self.indexInstruction("set_field", offset, writer),
        .set_global => self.indexInstruction("set_global", offset, writer),
        .set_local => self.indexInstruction("set_local", offset, writer),
        .set_local_box => self.indexInstruction("set_local_box", offset, writer),
        .str_cat => self.simpleInstruction("str_cat", offset, writer),
        .str_mul_l => self.simpleInstruction("str_mul_l", offset, writer),
        .str_mul_r => self.simpleInstruction("str_mul_r", offset, writer),
        .struct_lit => self.indexInstruction("struct_lit", offset, writer),
        .sub_float => self.simpleInstruction("sub_float", offset, writer),
        .sub_int => self.simpleInstruction("sub_int", offset, writer),
        .swap => self.simpleInstruction("swap", offset, writer),
        .unbox => self.simpleInstruction("unbox", offset, writer),
    };
}

fn simpleInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    if (self.render_mode == .@"test") {
        try writer.print("{s}\n", .{name});
    } else {
        try writer.print("{s:<20}\n", .{name});
    }

    return offset + 1;
}

fn indexInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    const index = self.chunk.code.items[offset + 1];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}\n", .{ name, index });
    } else {
        try writer.print("{s:<20} index {:>4}\n", .{ name, index });
    }

    return offset + 2;
}

fn indexExternInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}\n", .{ name, module, index });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}\n", .{ name, module, index });
    }

    return offset + 3;
}

fn getGlobal(self: *const Self, cow: bool, offset: usize, writer: anytype) !usize {
    const index = self.chunk.code.items[offset + 1];
    const text = if (cow) "get_global_cow" else "get_global";

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}", .{ text, index });
    } else {
        try writer.print("{s:<20} index {:>4}", .{ text, index });
    }

    if (self.module.globals[index].asObj()) |obj| {
        try writer.writeAll(", ");
        try obj.print(writer);
    }
    try writer.writeAll("\n");

    return offset + 2;
}

fn constantInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    const constant = self.chunk.code.items[offset + 1];
    const value = self.chunk.constants[constant];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, value ", .{ name, constant });
    } else {
        try writer.print("{s:<20} index {:>4}, value ", .{ name, constant });
    }

    try value.print(writer);
    try writer.print("\n", .{});
    return offset + 2;
}

fn jumpInstruction(
    self: *const Self,
    name: []const u8,
    sign: isize,
    offset: usize,
    writer: anytype,
) !usize {
    var jump: u16 = @as(u16, self.chunk.code.items[offset + 1]) << 8;
    jump |= self.chunk.code.items[offset + 2];
    const target = @as(isize, jump) * sign + @as(isize, @intCast(offset)) + 3;

    if (self.render_mode == .@"test") {
        try writer.print("{s} {} -> {}\n", .{ name, offset, target });
    } else {
        try writer.print("{s:<20} {:>4} -> {}\n", .{ name, offset, target });
    }

    return offset + 3;
}

fn for_instruction(
    self: *const Self,
    name: []const u8,
    sign: isize,
    offset: usize,
    writer: anytype,
) !usize {
    var jump: u16 = @as(u16, self.chunk.code.items[offset + 1]) << 8;
    jump |= self.chunk.code.items[offset + 2];
    const target = @as(isize, jump) * sign + @as(isize, @intCast(offset)) + 4;
    const iter_index = self.chunk.code.items[offset + 3];

    if (self.render_mode == .@"test") {
        try writer.print("{s} iter index {}, {} -> {}\n", .{ name, iter_index, offset, target });
    } else {
        try writer.print("{s:<20} iter index {}, {:<4} -> {}\n", .{ name, iter_index, offset, target });
    }

    return offset + 4;
}

fn loadSymbol(self: *const Self, offset: usize, writer: anytype) !usize {
    const text = "load_sym";
    const idx = self.chunk.code.items[offset + 1];
    const sym = self.module.symbols[idx].obj;

    var buf: [512]u8 = undefined;
    var bw = std.io.fixedBufferStream(&buf);
    switch (sym.kind) {
        .function => try sym.as(Obj.Function).print(bw.writer()),
        .structure => try sym.as(Obj.Structure).print(bw.writer()),
        else => unreachable,
    }

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, {s}\n", .{ text, idx, bw.getWritten() });
    } else {
        try writer.print("{s:<20} index {:>4}, {s}\n", .{ text, idx, bw.getWritten() });
    }

    return offset + 2;
}
fn getMember(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    // Skips the structure op
    // TODO: What?
    var local_offset = offset + 1;
    const idx = self.chunk.code.items[local_offset];
    local_offset += 1;

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}\n", .{ name, idx });
    } else {
        try writer.print("{s:<20} index {:>4}\n", .{ name, idx });
    }

    return local_offset;
}

fn invokeInstruction(self: *const Self, text: []const u8, offset: usize, writer: anytype) !usize {
    const arity = self.chunk.code.items[offset + 1];
    const obj_idx = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} arity {}, method index {}\n", .{ text, arity, obj_idx });
    } else {
        try writer.print("{s:<20} arity {:>4}, method index {:>4}\n", .{ text, arity, obj_idx });
    }

    return offset + 3;
}
