const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const Obj = @import("../runtime/Obj.zig");
const oom = @import("../utils.zig").oom;
const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;
const Module = @import("compiler.zig").CompiledModule;

chunk: *const Chunk,
module: *const Module,
render_mode: RenderMode,

prev_line: usize = 0,

const Self = @This();
pub const RenderMode = enum { none, normal, @"test" };

pub fn init(chunk: *const Chunk, module: *const Module, render_mode: RenderMode) Self {
    return .{
        .chunk = chunk,
        .module = module,
        .render_mode = render_mode,
    };
}

pub fn disChunk(self: *Self, writer: *Writer, name: []const u8) void {
    self.disSlice(writer, name, 0);
}

pub fn disSlice(self: *Self, writer: *Writer, name: []const u8, start: usize) void {
    writer.print("-- {s} --\n", .{name}) catch oom();

    var i: usize = start;
    while (i < self.chunk.code.items.len) {
        i = self.disInstruction(writer, i);
    }
}

pub fn disInstruction(self: *Self, writer: *Writer, offset: usize) usize {
    if (self.render_mode == .normal) {
        writer.print(" {:0>4}  ", .{offset}) catch oom();

        const line = self.chunk.offsets.items[offset];
        if (line > self.prev_line) {
            writer.print("{:>4}  ", .{line}) catch oom();
            self.prev_line = line;
        } else {
            writer.writeAll("   |  ") catch oom();
        }
    }

    const op: OpCode = @enumFromInt(self.chunk.code.items[offset]);
    return switch (op) {
        .add_float => self.simpleInstruction(writer, "add_float", offset),
        .add_int => self.simpleInstruction(writer, "add_int", offset),
        .array_new => self.indexInstruction(writer, "array_new", offset),
        .array_get => self.simpleInstruction(writer, "array_get", offset),
        .array_get_chain => self.indexInstruction(writer, "array_get_chain", offset),
        .array_get_chain_cow => self.indexInstruction(writer, "array_get_chain_cow", offset),
        .array_set => self.simpleInstruction(writer, "array_set", offset),
        .array_set_chain => self.indexInstruction(writer, "array_set_chain", offset),
        .box => self.simpleInstruction(writer, "box", offset),
        .call => self.indexInstruction(writer, "call", offset),
        .call_native => self.indexInstruction(writer, "call_native", offset),
        .cast_to_float => self.simpleInstruction(writer, "cast_to_float", offset),
        .closure => self.indexInstruction(writer, "closure", offset),
        .constant => self.constantInstruction(writer, "constant", offset),
        .def_global => self.indexInstruction(writer, "def_global", offset),
        .div_float => self.simpleInstruction(writer, "div_float", offset),
        .div_int => self.simpleInstruction(writer, "div_int", offset),
        .eq_bool => self.simpleInstruction(writer, "eq_bool", offset),
        .eq_float => self.simpleInstruction(writer, "eq_float", offset),
        .eq_int => self.simpleInstruction(writer, "eq_int", offset),
        .eq_null => self.simpleInstruction(writer, "eq_null", offset),
        .eq_str => self.simpleInstruction(writer, "eq_str", offset),
        .exit_repl => self.simpleInstruction(writer, "exit_repl", offset),
        .ge_float => self.simpleInstruction(writer, "ge_float", offset),
        .ge_int => self.simpleInstruction(writer, "ge_int", offset),
        .get_capt_frame => self.indexInstruction(writer, "get_capt_frame", offset),
        .get_capt_local => self.indexInstruction(writer, "get_capt_local", offset),
        .get_default => self.indexInstruction(writer, "get_default", offset),
        .get_field => self.getMember(writer, "get_field", offset),
        .get_field_cow => self.getMember(writer, "get_field_cow", offset),
        .get_global => self.getGlobal(writer, false, offset),
        .get_global_cow => self.getGlobal(writer, true, offset),
        .get_local => self.indexInstruction(writer, "get_local", offset),
        .get_local_cow => self.indexInstruction(writer, "get_local_cow", offset),
        .get_method => self.indexInstruction(writer, "get_method", offset),
        .get_static_method => self.getMember(writer, "get_static_method", offset),
        .gt_float => self.simpleInstruction(writer, "gt_float", offset),
        .gt_int => self.simpleInstruction(writer, "gt_int", offset),
        .incr_ref => self.simpleInstruction(writer, "incr_ref", offset),
        .jump => self.jumpInstruction(writer, "jump", 1, offset),
        .jump_false => self.jumpInstruction(writer, "jump_false", 1, offset),
        .jump_true => self.jumpInstruction(writer, "jump_true", 1, offset),
        .le_float => self.simpleInstruction(writer, "le_float", offset),
        .le_int => self.simpleInstruction(writer, "le_int", offset),
        .lt_float => self.simpleInstruction(writer, "lt_float", offset),
        .lt_int => self.simpleInstruction(writer, "lt_int", offset),
        .load_blk_val => self.simpleInstruction(writer, "load_blk_val", offset),
        .load_extern_sym => self.indexExternInstruction(writer, "load_extern_sym", offset),
        .load_sym => self.loadSymbol(writer, offset),
        .loop => self.jumpInstruction(writer, "loop", -1, offset),
        .mul_float => self.simpleInstruction(writer, "mul_float", offset),
        .mul_int => self.simpleInstruction(writer, "mul_int", offset),
        .ne_bool => self.simpleInstruction(writer, "ne_bool", offset),
        .ne_float => self.simpleInstruction(writer, "ne_float", offset),
        .ne_int => self.simpleInstruction(writer, "ne_int", offset),
        .ne_null => self.simpleInstruction(writer, "ne_null", offset),
        .ne_null_push => self.simpleInstruction(writer, "ne_null_push", offset),
        .ne_str => self.simpleInstruction(writer, "ne_str", offset),
        .neg_float => self.simpleInstruction(writer, "neg_float", offset),
        .neg_int => self.simpleInstruction(writer, "neg_int", offset),
        .not => self.simpleInstruction(writer, "not", offset),
        .pop => self.simpleInstruction(writer, "pop", offset),
        .print => self.simpleInstruction(writer, "print", offset),
        .push_false => self.simpleInstruction(writer, "push_false", offset),
        .push_null => self.simpleInstruction(writer, "push_null", offset),
        .push_true => self.simpleInstruction(writer, "push_true", offset),
        .ret => self.simpleInstruction(writer, "ret", offset),
        .ret_naked => self.simpleInstruction(writer, "ret_naked", offset),
        .ret_scope => self.indexInstruction(writer, "ret_scope", offset),
        .set_field => self.indexInstruction(writer, "set_field", offset),
        .set_global => self.indexInstruction(writer, "set_global", offset),
        .set_local => self.indexInstruction(writer, "set_local", offset),
        .set_local_box => self.indexInstruction(writer, "set_local_box", offset),
        .store_blk_val => self.simpleInstruction(writer, "store_blk_val", offset),
        .str_cat => self.simpleInstruction(writer, "str_cat", offset),
        .str_mul => self.simpleInstruction(writer, "str_mul", offset),
        .struct_lit => self.indexInstruction(writer, "struct_lit", offset),
        .sub_float => self.simpleInstruction(writer, "sub_float", offset),
        .sub_int => self.simpleInstruction(writer, "sub_int", offset),
        .swap => self.simpleInstruction(writer, "swap", offset),
        .unbox => self.simpleInstruction(writer, "unbox", offset),
    } catch oom();
}

fn simpleInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    if (self.render_mode == .@"test") {
        try writer.print("{s}\n", .{name});
    } else {
        try writer.print("{s:<20}\n", .{name});
    }

    return offset + 1;
}

fn indexInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}\n", .{ name, index });
    } else {
        try writer.print("{s:<20} index {:>4}\n", .{ name, index });
    }

    return offset + 2;
}

fn indexExternInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const index = self.chunk.code.items[offset + 1];
    const module = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, module {}\n", .{ name, module, index });
    } else {
        try writer.print("{s:<20} index {:>4}, module {:>4}\n", .{ name, module, index });
    }

    return offset + 3;
}

fn getGlobal(self: *Self, writer: *Writer, cow: bool, offset: usize) Writer.Error!usize {
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

fn constantInstruction(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
    const constant = self.chunk.code.items[offset + 1];
    const value = self.chunk.constants[constant];

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, value ", .{ name, constant });
    } else {
        try writer.print("{s:<20} index {:>4}, value ", .{ name, constant });
    }

    value.print(writer);
    try writer.print("\n", .{});
    return offset + 2;
}

fn jumpInstruction(self: *Self, writer: *Writer, name: []const u8, sign: isize, offset: usize) Writer.Error!usize {
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

fn for_instruction(self: *Self, writer: *Writer, name: []const u8, sign: isize, offset: usize) Writer.Error!usize {
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

fn loadSymbol(self: *Self, writer: *Writer, offset: usize) Writer.Error!usize {
    const text = "load_sym";
    const idx = self.chunk.code.items[offset + 1];
    const sym = self.module.symbols[idx].obj;

    var buf: [512]u8 = undefined;
    var bw = std.Io.Writer.fixed(&buf);
    sym.print(&bw) catch oom();

    if (self.render_mode == .@"test") {
        try writer.print("{s} index {}, {s}\n", .{ text, idx, bw.buffered() });
    } else {
        try writer.print("{s:<20} index {:>4}, {s}\n", .{ text, idx, bw.buffered() });
    }

    return offset + 2;
}
fn getMember(self: *Self, writer: *Writer, name: []const u8, offset: usize) Writer.Error!usize {
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

fn invokeInstruction(self: *Self, writer: *Writer, text: []const u8, offset: usize) Writer.Error!usize {
    const arity = self.chunk.code.items[offset + 1];
    const obj_idx = self.chunk.code.items[offset + 2];

    if (self.render_mode == .@"test") {
        try writer.print("{s} arity {}, method index {}\n", .{ text, arity, obj_idx });
    } else {
        try writer.print("{s:<20} arity {:>4}, method index {:>4}\n", .{ text, arity, obj_idx });
    }

    return offset + 3;
}
