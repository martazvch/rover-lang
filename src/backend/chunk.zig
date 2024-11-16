const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Value = @import("../runtime/values.zig").Value;

pub const OpCode = enum(u8) {
    Add,
    Constant,
    Divide,
    Multiply,
    Negate,
    Subtract,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: std.BoundedArray(Value, CONST_MAX + 1),

    const Self = @This();
    const CONST_MAX = std.math.maxInt(u8);
    pub const Error = error{Overflow} || Allocator.Error;

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = ArrayList(u8).init(allocator),
            .constants = std.BoundedArray(Value, 256).init(0) catch unreachable,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
    }

    pub fn write_op(self: *Self, op: OpCode) Error!void {
        try self.code.append(@intFromEnum(op));
    }

    pub fn write_byte(self: *Self, byte: u8) Error!void {
        try self.code.append(byte);
    }

    pub fn write_constant(self: *Self, value: Value) Error!u8 {
        try self.constants.append(value);
        return @truncate(self.constants.len - 1);
    }
};
