const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub const Disassembler = struct {
    chunk: *const Chunk,
    disassembled: ArrayList(u8),
    render_mode: RenderMode,

    const Self = @This();
    pub const RenderMode = enum { None, Normal, Test };

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

    pub fn dis_chunk(self: *Self, name: []const u8) !void {
        try self.dis_slice(name, 0);
    }

    pub fn dis_slice(self: *Self, name: []const u8, start: usize) !void {
        var writer = self.disassembled.writer();
        try writer.print("-- {s} --\n", .{name});

        var i: usize = start;
        while (i < self.chunk.code.items.len) {
            i = try self.dis_instruction(i, writer);
        }
    }

    pub fn dis_instruction(self: *const Self, offset: usize, writer: anytype) !usize {
        if (self.render_mode == .Normal) try writer.print("{:0>4} ", .{offset});

        // if (offset > 0 and self.chunk.lines.items[offset] == self.chunk.lines.items[offset - 1]) {
        //     print("   | ", .{});
        // } else {
        //     print("{:>4} ", .{self.chunk.lines.items[offset]});
        // }

        const op: OpCode = @enumFromInt(self.chunk.code.items[offset]);
        return switch (op) {
            .AddFloat => self.simple_instruction("OP_ADD_FLOAT", offset, writer),
            .AddInt => self.simple_instruction("OP_ADD_INT", offset, writer),
            // .CloseUpValue => simple_instruction("OP_CLOSE_UPVALUE", offset, writer),
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
            .CastToFloat => self.simple_instruction("OP_CAST_TO_FLOAT", offset, writer),
            .Constant => self.constant_instruction("OP_CONSTANT", offset, writer),
            // .CreateIter => self.simple_instruction("OP_CREATE_ITER", offset, writer),
            .DefineHeapVar => self.index_instruction("OP_DEFINE_HEAP_VAR", offset, writer),
            .DefineGlobal => self.index_instruction("OP_DEFINE_GLOBAL", offset, writer),
            .DivFloat => self.simple_instruction("OP_DIVIDE_FLOAT", offset, writer),
            .DivInt => self.simple_instruction("OP_DIVIDE_INT", offset, writer),
            .EqBool => self.simple_instruction("OP_EQUAL_BOOL", offset, writer),
            .EqFloat => self.simple_instruction("OP_EQUAL_FLOAT", offset, writer),
            .EqInt => self.simple_instruction("OP_EQUAL_INT", offset, writer),
            .EqStr => self.simple_instruction("OP_EQUAL_STRING", offset, writer),
            .ExitRepl => self.simple_instruction("OP_EXIT_REPL", offset, writer),
            .False => self.simple_instruction("OP_FALSE", offset, writer),
            .FnCall => self.index_instruction("OP_CALL", offset, writer),
            // .ForIter => self.for_instruction("OP_FOR_ITER", 1, offset),
            .GetGlobal => self.index_instruction("OP_GET_GLOBAL", offset, writer),
            .GetHeap => self.index_instruction("OP_GET_HEAP", offset, writer),
            .GetLocal => self.index_instruction("OP_GET_LOCAL", offset, writer),
            // .GetProperty => self.constant_instruction("OP_GET_PROPERTY", offset),
            // .GetUpvalue => self.byte_instruction("OP_GET_UPVALUE", offset),
            .GtFloat => self.simple_instruction("OP_GREATER_FLOAT", offset, writer),
            .GtInt => self.simple_instruction("OP_GREATER_INT", offset, writer),
            .GeFloat => self.simple_instruction("OP_GREATER_EQUAL_FLOAT", offset, writer),
            .GeInt => self.simple_instruction("OP_GREATER_EQUAL_INT", offset, writer),
            // .Invoke => self.invoke_instruction("OP_INVOKE", offset),
            .Jump => self.jump_instruction("OP_JUMP", 1, offset, writer),
            .JumpIfFalse => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset, writer),
            .JumpIfTrue => self.jump_instruction("OP_JUMP_IF_TRUE", 1, offset, writer),
            .LtFloat => self.simple_instruction("OP_LESS_FLOAT", offset, writer),
            .LtInt => self.simple_instruction("OP_LESS_INT", offset, writer),
            .LeFloat => self.simple_instruction("OP_LESS_EQUAL_FLOAT", offset, writer),
            .LeInt => self.simple_instruction("OP_LESS_EQUAL_INT", offset, writer),
            .Loop => self.jump_instruction("OP_LOOP", -1, offset, writer),
            // .Method => self.constant_instruction("OP_METHOD", offset),
            .MulFloat => self.simple_instruction("OP_MULTIPLY_FLOAT", offset, writer),
            .MulInt => self.simple_instruction("OP_MULTIPLY_INT", offset, writer),
            .NakedReturn => self.simple_instruction("OP_NAKED_RETURN", offset, writer),
            .NativeFnCall => self.index_instruction("OP_NATIVE_CALL", offset, writer),
            .NeBool => self.simple_instruction("OP_DIFFERENT_BOOL", offset, writer),
            .NeFloat => self.simple_instruction("OP_DIFFERENT_FLOAT", offset, writer),
            .NeInt => self.simple_instruction("OP_DIFFERENT_INT", offset, writer),
            .NeStr => self.simple_instruction("OP_DIFFERENT_STR", offset, writer),
            .NegateFloat => self.simple_instruction("OP_NEGATE_FLOAT", offset, writer),
            .NegateInt => self.simple_instruction("OP_NEGATE_INT", offset, writer),
            .Not => self.simple_instruction("OP_NOT", offset, writer),
            .Null => self.simple_instruction("OP_NULL", offset, writer),
            .Pop => self.simple_instruction("OP_POP", offset, writer),
            .Print => self.simple_instruction("OP_PRINT", offset, writer),
            .Return => self.simple_instruction("OP_RETURN", offset, writer),
            .ScopeReturn => self.index_instruction("OP_SCOPE_RETURN", offset, writer),
            .SetGlobal => self.index_instruction("OP_SET_GLOBAL", offset, writer),
            .SetHeap => self.index_instruction("OP_SET_HEAP", offset, writer),
            .SetLocal => self.index_instruction("OP_SET_LOCAL", offset, writer),
            // .SetProperty => self.constant_instruction("OP_SET_PROPERTY", offset),
            // .SetUpvalue => self.byte_instruction("OP_SET_UPVALUE", offset),
            // .Struct => self.constant_instruction("OP_STRUCT", offset),
            .StrCat => self.simple_instruction("OP_STRING_CONCAT", offset, writer),
            .StrMulL => self.simple_instruction("OP_STRING_MUL_L", offset, writer),
            .StrMulR => self.simple_instruction("OP_STRING_MUL_R", offset, writer),
            .SubFloat => self.simple_instruction("OP_SUBTRACT_FLOAT", offset, writer),
            .SubInt => self.simple_instruction("OP_SUBTRACT_INT", offset, writer),
            .True => self.simple_instruction("OP_TRUE", offset, writer),
        };
    }

    fn simple_instruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
        if (self.render_mode == .Test) {
            try writer.print("{s}\n", .{name});
        } else {
            try writer.print("{s:<24}\n", .{name});
        }

        return offset + 1;
    }

    fn index_instruction(self: *const Self, name: []const u8, offset: usize, writer: anytype) !usize {
        const index = self.chunk.code.items[offset + 1];

        if (self.render_mode == .Test) {
            try writer.print("{s} index {}\n", .{ name, index });
        } else {
            try writer.print("{s:<24} index {:>4}\n", .{ name, index });
        }

        return offset + 2;
    }

    fn constant_instruction(
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

    fn jump_instruction(
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

    fn invoke_instruction(
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
};
