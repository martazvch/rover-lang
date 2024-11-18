const std = @import("std");
const print = std.debug.print;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const BinOpType = @import("chunk.zig").BinOpType;

pub const Disassembler = struct {
    chunk: *const Chunk,

    const Self = @This();

    pub fn init(chunk: *const Chunk) Self {
        return .{
            .chunk = chunk,
        };
    }

    pub fn dis_chunk(self: *const Self, name: []const u8) !void {
        print("== {s} ==\n", .{name});

        var i: usize = 0;
        while (i < self.chunk.code.items.len) {
            i = self.dis_instruction(i);
        }
    }

    pub fn dis_instruction(self: *const Self, offset: usize) usize {
        // print("{:0>4} ", .{offset});
        print("{:0>4} ", .{offset});

        // if (offset > 0 and self.chunk.lines.items[offset] == self.chunk.lines.items[offset - 1]) {
        //     print("   | ", .{});
        // } else {
        //     print("{:>4} ", .{self.chunk.lines.items[offset]});
        // }

        const op: OpCode = @enumFromInt(self.chunk.code.items[offset]);
        return switch (op) {
            .Add => self.binop_instruction("OP_ADD", offset),
            // .Call => self.byte_instruction("OP_CALL", offset),
            // .CloseUpValue => simple_instruction("OP_CLOSE_UPVALUE", offset),
            // .Closure => {
            //     var local_offset = offset + 1;
            //     const constant = self.chunk.code.items[local_offset];
            //     local_offset += 1;
            //
            //     print("{s:<16} index: {:<4}", .{ "OP_CLOSURE", constant });
            //
            //     const obj = self.chunk.constants.items[constant].as_obj().?;
            //     obj.log();
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
            .Constant => self.constant_instruction("OP_CONSTANT", offset),
            // .CreateIter => simple_instruction("OP_CREATE_ITER", offset),
            // .DefineGlobal => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            .Divide => self.binop_instruction("OP_DIVIDE", offset),
            // .Equal => simple_instruction("OP_EQUAL", offset),
            .False => simple_instruction("OP_FALSE", offset),
            // .ForIter => self.for_instruction("OP_FOR_ITER", 1, offset),
            // .GetGlobal => self.constant_instruction("OP_GET_GLOBAL", offset),
            // .GetLocal => self.byte_instruction("OP_GET_LOCAL", offset),
            // .GetProperty => self.constant_instruction("OP_GET_PROPERTY", offset),
            // .GetUpvalue => self.byte_instruction("OP_GET_UPVALUE", offset),
            // .Greater => simple_instruction("OP_GREATER", offset),
            // .Invoke => self.invoke_instruction("OP_INVOKE", offset),
            // .Jump => self.jump_instruction("OP_JUMP", 1, offset),
            // .JumpIfFalse => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            // .Less => simple_instruction("OP_LESS", offset),
            // .Loop => self.jump_instruction("OP_LOOP", -1, offset),
            // .Method => self.constant_instruction("OP_METHOD", offset),
            .Multiply => self.binop_instruction("OP_MULTIPLY", offset),
            .Negate => simple_instruction("OP_NEGATE", offset),
            .Not => simple_instruction("OP_NOT", offset),
            .Null => simple_instruction("OP_NULL", offset),
            // .Pop => simple_instruction("OP_POP", offset),
            .Print => simple_instruction("OP_PRINT", offset),
            .Return => simple_instruction("OP_RETURN", offset),
            // .SetGlobal => self.constant_instruction("OP_SET_GLOBAL", offset),
            // .SetLocal => self.byte_instruction("OP_SET_LOCAL", offset),
            // .SetProperty => self.constant_instruction("OP_SET_PROPERTY", offset),
            // .SetUpvalue => self.byte_instruction("OP_SET_UPVALUE", offset),
            // .Struct => self.constant_instruction("OP_STRUCT", offset),
            .Subtract => self.binop_instruction("OP_SUBTRACT", offset),
            .True => simple_instruction("OP_TRUE", offset),
        };
    }

    fn simple_instruction(name: []const u8, offset: usize) usize {
        print("{s:<16}\n", .{name});
        return offset + 1;
    }

    fn binop_instruction(self: *const Self, name: []const u8, offset: usize) usize {
        const binop_type = self.chunk.code.items[offset + 1];
        print("{s:<16} type: {s}\n", .{ name, @tagName(@as(BinOpType, @enumFromInt(binop_type))) });
        return offset + 2;
    }

    fn constant_instruction(self: *const Self, name: []const u8, offset: usize) usize {
        const constant = self.chunk.code.items[offset + 1];
        const value = self.chunk.constants[constant];
        print("{s:<16} index: {:<4} value: ", .{ name, constant });
        value.log();
        print("\n", .{});
        return offset + 2;
    }

    // NOTE: for locals, we don't store their name into the chunk (great for performance
    // bit bad for introspection). Maybe get a compile time way to do so?
    fn byte_instruction(self: *const Self, name: []const u8, offset: usize) usize {
        const index = self.chunk.code.items[offset + 1];
        print("{s:<16} {:<4} \n", .{ name, index });

        return offset + 2;
    }

    fn jump_instruction(self: *const Self, name: []const u8, sign: isize, offset: usize) usize {
        var jump: u16 = @as(u16, self.chunk.code.items[offset + 1]) << 8;
        jump |= self.chunk.code.items[offset + 2];
        const target = @as(isize, jump) * sign + @as(isize, @intCast(offset)) + 3;

        print("{s:<16} {:<4} -> {}\n", .{ name, offset, target });

        return offset + 3;
    }

    fn for_instruction(self: *const Self, name: []const u8, sign: isize, offset: usize) usize {
        var jump: u16 = @as(u16, self.chunk.code.items[offset + 1]) << 8;
        jump |= self.chunk.code.items[offset + 2];
        const target = @as(isize, jump) * sign + @as(isize, @intCast(offset)) + 4;
        const iter_index = self.chunk.code.items[offset + 3];

        print("{s:<16} iter index: {}, {:<4} -> {}\n", .{ name, iter_index, offset, target });

        return offset + 4;
    }

    fn invoke_instruction(self: *const Self, name: []const u8, offset: usize) usize {
        const name_id = self.chunk.code.items[offset + 1];
        const method_name = self.chunk.constants.items[name_id];
        const arg_count = self.chunk.code.items[offset + 2];

        print("{s:<16} index: {}, value: ", .{ name, name_id });
        method_name.log();
        print(", {} args\n", .{arg_count});

        return offset - 3;
    }
};
