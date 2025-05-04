const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Chunk = @import("Chunk.zig");
const OpCode = Chunk.OpCode;

chunk: *const Chunk,
disassembled: ArrayList(u8),
render_mode: RenderMode,

const Self = @This();
pub const RenderMode = enum { none, Normal, Test };

pub fn init(chunk: *const Chunk, allocator: Allocator, render_mode: RenderMode) Self {
    return .{
        .chunk = chunk,
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

    // if (offset > 0 and self.chunk.lines.items[offset] == self.chunk.lines.items[offset - 1]) {
    //     print("   | ", .{});
    // } else {
    //     print("{:>4} ", .{self.chunk.lines.items[offset]});
    // }

    const op: OpCode = @enumFromInt(self.chunk.code.items[offset]);
    return switch (op) {
        .AddFloat => self.simpleInstruction("OP_ADD_FLOAT", offset, writer),
        .AddInt => self.simpleInstruction("OP_ADD_INT", offset, writer),
        // .CloseUpValue => simpleInstruction("OP_CLOSE_UPVALUE", offset, writer),
        // .Closure => {
        //     var local_offset = offset + 1;
        //     const constant = self.chunk.code.items[local_offset];
        //     local_offset += 1;
        //
        //     print("{s:<16} index: {:<4}", .{ "OP_CLOSURE", constant });
        //
        //     const obj = self.chunk.constants.items[constant].as_obj().?;
        //     obj.print(writer);
        //     print("\n", .{});
        //
        //     const obj_fn = obj.as(ObjFunction);
        //
        //     for (0..obj_fn.upvalue_count) |_| {
        //         const is_local = if (self.chunk.code.items[local_offset] == 1) "local" else "upvalue";
        //         local_offset += 1;
        //         const index = self.chunk.code.items[local_offset];
        //         local_offset += 1;
        //
        //         print("{:>4}      |                     {s} {}\n", .{ local_offset - 2, is_local, index });
        //     }
        //
        //     return local_offset;
        // },
        .CastToFloat => self.simpleInstruction("OP_CAST_TO_FLOAT", offset, writer),
        .Constant => self.constantInstruction("OP_CONSTANT", offset, writer),
        // .CreateIter => self.simpleInstruction("OP_CREATE_ITER", offset, writer),
        .DefineHeapVar => self.indexInstruction("OP_DEFINE_HEAP_VAR", offset, writer),
        .DefineGlobal => self.indexInstruction("OP_DEFINE_GLOBAL", offset, writer),
        .DivFloat => self.simpleInstruction("OP_DIVIDE_FLOAT", offset, writer),
        .DivInt => self.simpleInstruction("OP_DIVIDE_INT", offset, writer),
        .EqBool => self.simpleInstruction("OP_EQUAL_BOOL", offset, writer),
        .EqFloat => self.simpleInstruction("OP_EQUAL_FLOAT", offset, writer),
        .EqInt => self.simpleInstruction("OP_EQUAL_INT", offset, writer),
        .EqStr => self.simpleInstruction("OP_EQUAL_STRING", offset, writer),
        .ExitRepl => self.simpleInstruction("OP_EXIT_REPL", offset, writer),
        .false => self.simpleInstruction("OP_FALSE", offset, writer),
        .call => self.indexInstruction("OP_CALL", offset, writer),
        // .ForIter => self.for_instruction("OP_FOR_ITER", 1, offset),
        .field_assign => self.fieldAssign(offset, writer),
        .get_field => self.getMember(offset, writer, "OP_GET_FIELD"),
        .GetGlobal => self.indexInstruction("OP_GET_GLOBAL", offset, writer),
        .GetHeap => self.indexInstruction("OP_GET_HEAP", offset, writer),
        .GetLocal => self.indexInstruction("OP_GET_LOCAL", offset, writer),
        .get_method => self.getMember(offset, writer, "OP_GET_METHOD"),
        // .GetProperty => self.constantInstruction("OP_GET_PROPERTY", offset),
        // .GetUpvalue => self.byte_instruction("OP_GET_UPVALUE", offset),
        .GtFloat => self.simpleInstruction("OP_GREATER_FLOAT", offset, writer),
        .GtInt => self.simpleInstruction("OP_GREATER_INT", offset, writer),
        .GeFloat => self.simpleInstruction("OP_GREATER_EQUAL_FLOAT", offset, writer),
        .GeInt => self.simpleInstruction("OP_GREATER_EQUAL_INT", offset, writer),
        // .Invoke => self.invokeInstruction("OP_INVOKE", offset),
        .Jump => self.jumpInstruction("OP_JUMP", 1, offset, writer),
        .JumpIfFalse => self.jumpInstruction("OP_JUMP_IF_FALSE", 1, offset, writer),
        .JumpIfTrue => self.jumpInstruction("OP_JUMP_IF_TRUE", 1, offset, writer),
        .LtFloat => self.simpleInstruction("OP_LESS_FLOAT", offset, writer),
        .LtInt => self.simpleInstruction("OP_LESS_INT", offset, writer),
        .LeFloat => self.simpleInstruction("OP_LESS_EQUAL_FLOAT", offset, writer),
        .LeInt => self.simpleInstruction("OP_LESS_EQUAL_INT", offset, writer),
        .Loop => self.jumpInstruction("OP_LOOP", -1, offset, writer),
        // .Method => self.constantInstruction("OP_METHOD", offset),
        .MulFloat => self.simpleInstruction("OP_MULTIPLY_FLOAT", offset, writer),
        .MulInt => self.simpleInstruction("OP_MULTIPLY_INT", offset, writer),
        .NakedReturn => self.simpleInstruction("OP_NAKED_RETURN", offset, writer),
        .NativeFnCall => self.indexInstruction("OP_NATIVE_CALL", offset, writer),
        .NeBool => self.simpleInstruction("OP_DIFFERENT_BOOL", offset, writer),
        .NeFloat => self.simpleInstruction("OP_DIFFERENT_FLOAT", offset, writer),
        .NeInt => self.simpleInstruction("OP_DIFFERENT_INT", offset, writer),
        .NeStr => self.simpleInstruction("OP_DIFFERENT_STR", offset, writer),
        .NegateFloat => self.simpleInstruction("OP_NEGATE_FLOAT", offset, writer),
        .NegateInt => self.simpleInstruction("OP_NEGATE_INT", offset, writer),
        .not => self.simpleInstruction("OP_NOT", offset, writer),
        .null => self.simpleInstruction("OP_NULL", offset, writer),
        .Pop => self.simpleInstruction("OP_POP", offset, writer),
        .print => self.simpleInstruction("OP_PRINT", offset, writer),
        .@"return" => self.simpleInstruction("OP_RETURN", offset, writer),
        .ScopeReturn => self.indexInstruction("OP_SCOPE_RETURN", offset, writer),
        .SetGlobal => self.indexInstruction("OP_SET_GLOBAL", offset, writer),
        .SetHeap => self.indexInstruction("OP_SET_HEAP", offset, writer),
        .SetLocal => self.indexInstruction("OP_SET_LOCAL", offset, writer),
        // .SetProperty => self.constantInstruction("OP_SET_PROPERTY", offset),
        // .SetUpvalue => self.byte_instruction("OP_SET_UPVALUE", offset),
        // .@"struct" => self.constantInstruction("OP_STRUCT", offset),
        .StrCat => self.simpleInstruction("OP_STRING_CONCAT", offset, writer),
        .StrMulL => self.simpleInstruction("OP_STRING_MUL_L", offset, writer),
        .StrMulR => self.simpleInstruction("OP_STRING_MUL_R", offset, writer),
        // .struct_literal => self.struct_literal(offset, writer),
        .struct_literal => self.indexInstruction("OP_STRUCT_LIT", offset, writer),
        .SubFloat => self.simpleInstruction("OP_SUBTRACT_FLOAT", offset, writer),
        .SubInt => self.simpleInstruction("OP_SUBTRACT_INT", offset, writer),
        .true => self.simpleInstruction("OP_TRUE", offset, writer),
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

fn constantInstruction(
    self: *const Self,
    name: []const u8,
    offset: usize,
    writer: anytype,
) !usize {
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

fn fieldAssign(self: *const Self, offset: usize, writer: anytype) !usize {
    if (self.render_mode == .Test) {
        try writer.print("{s}\n", .{"OP_FIELD_ASSIGN"});
    } else {
        try writer.print("{s:<24}\n", .{"OP_FIELD_ASSIGN"});
    }

    return offset + 1;
}

fn getMember(self: *const Self, offset: usize, writer: anytype, name: []const u8) !usize {
    // Skips the struct_literal op
    var local_offset = offset;
    local_offset += 1;
    const idx = self.chunk.code.items[local_offset];
    local_offset += 1;

    if (self.render_mode == .Test) {
        try writer.print("{s} index {} of next variable\n", .{ name, idx });
    } else {
        try writer.print("{s:<24} index {:>4} of next variable\n", .{ name, idx });
    }

    local_offset = try self.disInstruction(local_offset, writer);

    return local_offset;
}

fn invokeInstruction(
    self: *const Self,
    name: []const u8,
    offset: usize,
    writer: anytype,
) !usize {
    const name_id = self.chunk.code.items[offset + 1];
    const method_name = self.chunk.constants.items[name_id];
    const arg_count = self.chunk.code.items[offset + 2];

    try writer.print("{s:<24} index {}, value ", .{ name, name_id });
    try method_name.print(writer);
    try writer.print(", {} args\n", .{arg_count});

    return offset - 3;
}
