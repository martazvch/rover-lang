const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Value = @import("../runtime/values2.zig").Value;
const Chunk = @import("Chunk2.zig");
const OpCode = Chunk.OpCode;

chunk: *const Chunk,
globals: []const Value,
disassembled: ArrayList(u8),
render_mode: RenderMode,

const Self = @This();
pub const RenderMode = enum { none, Normal, Test };

pub fn init(allocator: Allocator, chunk: *const Chunk, globals: []const Value, render_mode: RenderMode) Self {
    return .{
        .chunk = chunk,
        .globals = globals,
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

pub fn disInstruction(self: *const Self, offset: usize, writer: anytype) (Allocator.Error || std.posix.WriteError)!usize {
    if (self.render_mode == .Normal) try writer.print("{:0>4} ", .{offset});

    const op: OpCode = @enumFromInt(self.chunk.code.items[offset]);
    return switch (op) {
        .array => self.indexInstruction("OP_ARRAY", offset, writer),
        .array_access => self.simpleInstruction("OP_ARRAY_ACCESS", offset, writer),
        // .array_access_reg => self.simpleInstruction("OP_ARRAY_ACCESS_REG", offset, writer),
        // .array_access_reg_cow => self.simpleInstruction("OP_ARRAY_ACCESS_REG_COW", offset, writer),
        .array_access_chain => self.indexInstruction("OP_ARRAY_ACCESS_CHAIN", offset, writer),
        // .array_access_chain_reg => self.indexInstruction("OP_ARRAY_ACCESS_CHAIN_REG", offset, writer),
        .array_assign => self.simpleInstruction("OP_ARRAY_ASSIGN", offset, writer),
        .array_assign_chain => self.indexInstruction("OP_ARRAY_ASSIGN_CHAIN", offset, writer),
        .add_float => self.simpleInstruction("OP_ADD_FLOAT", offset, writer),
        .add_int => self.simpleInstruction("OP_ADD_INT", offset, writer),
        .bound_import => self.getMember("OP_BOUND_IMPORT", offset, writer),
        .box => self.simpleInstruction("BOX", offset, writer),
        // .bound_method => self.getMember("OP_BOUND_METHOD", offset, writer),
        .call => self.indexInstruction("OP_CALL", offset, writer),
        .call_native => self.indexInstruction("OP_CALL_NATIVE", offset, writer),
        .cast_to_float => self.simpleInstruction("OP_CAST_TO_FLOAT", offset, writer),
        .closure => self.indexInstruction("CLOSURE", offset, writer),
        .constant => self.constantInstruction("OP_CONSTANT", offset, writer),
        // .define_heap_var => self.indexInstruction("OP_DEFINE_HEAP_VAR", offset, writer),
        .define_global => self.indexInstruction("OP_DEFINE_GLOBAL", offset, writer),
        .div_float => self.simpleInstruction("OP_DIVIDE_FLOAT", offset, writer),
        .div_int => self.simpleInstruction("OP_DIVIDE_INT", offset, writer),
        .dup => self.simpleInstruction("DUP", offset, writer),
        .eq_bool => self.simpleInstruction("OP_EQUAL_BOOL", offset, writer),
        .eq_float => self.simpleInstruction("OP_EQUAL_FLOAT", offset, writer),
        .eq_int => self.simpleInstruction("OP_EQUAL_INT", offset, writer),
        .eq_str => self.simpleInstruction("OP_EQUAL_STRING", offset, writer),
        .exit_repl => self.simpleInstruction("OP_EXIT_REPL", offset, writer),
        .false => self.simpleInstruction("OP_FALSE", offset, writer),
        .ge_float => self.simpleInstruction("OP_GREATER_EQUAL_FLOAT", offset, writer),
        .ge_int => self.simpleInstruction("OP_GREATER_EQUAL_INT", offset, writer),
        // .get_capture => self.indexInstruction("GET_CAPTURE", offset, writer),
        .get_default => self.indexInstruction("OP_GET_DEFAULT", offset, writer),
        .get_field => self.getMember("OP_GET_FIELD", offset, writer),
        // .get_field_reg => self.getMember("OP_GET_FIELD_REG", offset, writer),
        // .get_field_reg_cow => self.getMember("OP_GET_FIELD_REG_COW", offset, writer),
        .get_global => self.getGlobal(false, offset, writer),
        // .get_global_reg => self.getGlobal(true, offset, writer),
        .get_heap => self.indexInstruction("OP_GET_HEAP", offset, writer),
        .get_local => self.indexInstruction("OP_GET_LOCAL", offset, writer),
        // .get_local_reg => self.indexInstruction("OP_GET_LOCAL_REG", offset, writer),
        // .get_local_reg_cow => self.indexInstruction("OP_GET_LOCAL_REG_COW", offset, writer),
        .get_local_absolute => self.indexInstruction("OP_GET_LOCAL_ABSOLUTE", offset, writer),
        .get_method => self.indexInstruction("OP_GET_METHOD", offset, writer),
        .get_static_method => self.getMember("OP_GET_STATIC_METHOD", offset, writer),
        .get_symbol => self.indexInstruction("OP_GET_SYMBOL", offset, writer),
        // .get_symbol_reg => self.indexInstruction("OP_GET_SYMBOL_REG", offset, writer),
        .gt_float => self.simpleInstruction("OP_GREATER_FLOAT", offset, writer),
        .gt_int => self.simpleInstruction("OP_GREATER_INT", offset, writer),
        .incr_ref_count => self.simpleInstruction("OP_INCR_REF_COUNT", offset, writer),
        // .invoke => self.invokeInstruction("OP_INVOKE", offset, writer),
        .jump => self.jumpInstruction("OP_JUMP", 1, offset, writer),
        .jump_if_false => self.jumpInstruction("OP_JUMP_IF_FALSE", 1, offset, writer),
        .jump_if_true => self.jumpInstruction("OP_JUMP_IF_TRUE", 1, offset, writer),
        .le_float => self.simpleInstruction("OP_LESS_EQUAL_FLOAT", offset, writer),
        .le_int => self.simpleInstruction("OP_LESS_EQUAL_INT", offset, writer),
        .load_fn_default => self.simpleInstruction("OP_LOAD_FN_DEF", offset, writer),
        .load_invoke_default => self.indexInstruction("OP_LOAD_INVOKE_DEF", offset, writer),
        .load_struct_def => self.simpleInstruction("OP_LOAD_STRUCT_DEF", offset, writer),
        .loop => self.jumpInstruction("OP_LOOP", -1, offset, writer),
        .lt_float => self.simpleInstruction("OP_LESS_FLOAT", offset, writer),
        .lt_int => self.simpleInstruction("OP_LESS_INT", offset, writer),
        .mul_float => self.simpleInstruction("OP_MULTIPLY_FLOAT", offset, writer),
        .mul_int => self.simpleInstruction("OP_MULTIPLY_INT", offset, writer),
        .naked_return => self.simpleInstruction("OP_NAKED_RETURN", offset, writer),
        .ne_bool => self.simpleInstruction("OP_DIFFERENT_BOOL", offset, writer),
        .ne_float => self.simpleInstruction("OP_DIFFERENT_FLOAT", offset, writer),
        .ne_int => self.simpleInstruction("OP_DIFFERENT_INT", offset, writer),
        .ne_str => self.simpleInstruction("OP_DIFFERENT_STR", offset, writer),
        .negate_float => self.simpleInstruction("OP_NEGATE_FLOAT", offset, writer),
        .negate_int => self.simpleInstruction("OP_NEGATE_INT", offset, writer),
        .not => self.simpleInstruction("OP_NOT", offset, writer),
        .null => self.simpleInstruction("OP_NULL", offset, writer),
        .pop => self.simpleInstruction("OP_POP", offset, writer),
        .print => self.simpleInstruction("OP_PRINT", offset, writer),
        .push_module => self.indexInstruction("OP_PUSH_MODULE", offset, writer),
        // .reg_assign => self.simpleInstruction("OP_REG_ASSIGN", offset, writer),
        // .reg_assign_cow => self.simpleInstruction("OP_REG_ASSIGN_COW", offset, writer),
        // .reg_push => self.simpleInstruction("OP_REG_PUSH", offset, writer),
        .@"return" => self.simpleInstruction("OP_RETURN", offset, writer),
        .scope_return => self.indexInstruction("OP_SCOPE_RETURN", offset, writer),
        .set_capture => self.indexInstruction("SET_CAPTURE", offset, writer),
        .set_field => self.indexInstruction("SET_FIELD", offset, writer),
        .set_global => self.indexInstruction("OP_SET_GLOBAL", offset, writer),
        .set_heap => self.indexInstruction("OP_SET_HEAP", offset, writer),
        .set_local => self.indexInstruction("OP_SET_LOCAL", offset, writer),
        .str_cat => self.simpleInstruction("OP_STRING_CONCAT", offset, writer),
        .str_mul_l => self.simpleInstruction("OP_STRING_MUL_L", offset, writer),
        .str_mul_r => self.simpleInstruction("OP_STRING_MUL_R", offset, writer),
        .struct_literal => self.indexInstruction("OP_STRUCT_LIT", offset, writer),
        .sub_float => self.simpleInstruction("OP_SUBTRACT_FLOAT", offset, writer),
        .sub_int => self.simpleInstruction("OP_SUBTRACT_INT", offset, writer),
        .swap => self.simpleInstruction("SWAP", offset, writer),
        .true => self.simpleInstruction("OP_TRUE", offset, writer),
        .unbox => self.simpleInstruction("UNBOX", offset, writer),
    };
}

fn simpleInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    if (self.render_mode == .Test) {
        try writer.print("{s}\n", .{name});
    } else {
        try writer.print("{s:<24}\n", .{name});
    }

    return offset + 1;
}

fn indexInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    const index = self.chunk.code.items[offset + 1];

    if (self.render_mode == .Test) {
        try writer.print("{s} index {}\n", .{ name, index });
    } else {
        try writer.print("{s:<24} index {:>4}\n", .{ name, index });
    }

    return offset + 2;
}

fn getGlobal(self: *const Self, reg: bool, offset: usize, writer: anytype) !usize {
    const index = self.chunk.code.items[offset + 1];
    const text = if (reg) "OP_GET_GLOBAL_REG" else "OP_GET_GLOBAL";

    if (self.render_mode == .Test) {
        try writer.print("{s} index {}", .{ text, index });
    } else {
        try writer.print("{s:<24} index {:>4}", .{ text, index });
    }

    if (self.globals[index].asObj()) |obj| {
        try writer.writeAll(", ");
        try obj.print(writer);
    }
    try writer.writeAll("\n");

    return offset + 2;
}

fn constantInstruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    const constant = self.chunk.code.items[offset + 1];
    const value = self.chunk.constants[constant];

    if (self.render_mode == .Test) {
        try writer.print("{s} index {}, value ", .{ name, constant });
    } else {
        try writer.print("{s:<24} index {:>4}, value ", .{ name, constant });
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

    if (self.render_mode == .Test) {
        try writer.print("{s} {} -> {}\n", .{ name, offset, target });
    } else {
        try writer.print("{s:<24} {:>4} -> {}\n", .{ name, offset, target });
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

    if (self.render_mode == .Test) {
        try writer.print("{s} iter index {}, {} -> {}\n", .{ name, iter_index, offset, target });
    } else {
        try writer.print("{s:<24} iter index {}, {:<4} -> {}\n", .{ name, iter_index, offset, target });
    }

    return offset + 4;
}

fn getMember(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
    // Skips the structure op
    var local_offset = offset + 1;
    const idx = self.chunk.code.items[local_offset];
    local_offset += 1;

    if (self.render_mode == .Test) {
        try writer.print("{s} index {}\n", .{ name, idx });
    } else {
        try writer.print("{s:<24} index {:>4}\n", .{ name, idx });
    }

    return local_offset;
}

fn invokeInstruction(self: *const Self, text: []const u8, offset: usize, writer: anytype) !usize {
    const arity = self.chunk.code.items[offset + 1];
    const obj_idx = self.chunk.code.items[offset + 2];

    if (self.render_mode == .Test) {
        try writer.print("{s} arity {}, method index {}\n", .{ text, arity, obj_idx });
    } else {
        try writer.print("{s:<24} arity {:>4}, method index {:>4}\n", .{ text, arity, obj_idx });
    }

    return offset + 3;
}

fn importItem(self: *const Self, offset: usize, writer: anytype) !usize {
    const module = self.chunk.code.items[offset + 1];
    const field = self.chunk.code.items[offset + 2];
    const text = "OP_IMPORT_ITEM";

    if (self.render_mode == .Test) {
        try writer.print("{s} module index: {}, field index: {}\n", .{ text, module, field });
    } else {
        try writer.print("{s:<24} module index {:>4}: field index: {:>4}\n", .{ text, module, field });
    }

    return offset + 3;
}
