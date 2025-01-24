const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Value = @import("../runtime/values.zig").Value;

pub const OpCode = enum(u8) {
    AddInt,
    AddFloat,
    CastToFloat,
    Constant,
    DifferentInt,
    DifferentFloat,
    DefineGlobal,
    DivideFloat,
    DivideInt,
    EqualInt,
    EqualFloat,
    EqualStr,
    ExitRepl,
    False,
    FnCall,
    GetGlobal,
    GetLocal,
    GreaterInt,
    GreaterFloat,
    GreaterEqualInt,
    GreaterEqualFloat,
    Jump,
    JumpIfFalse,
    JumpIfTrue,
    LessInt,
    LessFloat,
    LessEqualInt,
    LessEqualFloat,
    Loop,
    MultiplyFloat,
    MultiplyInt,
    NegateFloat,
    NegateInt,
    Not,
    Null,
    Pop,
    Print,
    Return,
    ScopeReturn,
    SetGlobal,
    SetLocal,
    StrCat,
    StrMulL,
    StrMulR,
    SubtractFloat,
    SubtractInt,
    True,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    offsets: ArrayList(usize),
    constants: [CONST_MAX]Value,
    constant_count: u8,

    const Self = @This();
    const CONST_MAX = std.math.maxInt(u8) + 1;
    pub const Error = error{TooManyConst} || Allocator.Error;

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = ArrayList(u8).init(allocator),
            .offsets = ArrayList(usize).init(allocator),
            .constants = undefined,
            .constant_count = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.offsets.deinit();
    }

    pub fn write_op(self: *Self, op: OpCode, offset: usize) Error!void {
        try self.code.append(@intFromEnum(op));
        try self.offsets.append(offset);
    }

    pub fn write_byte(self: *Self, byte: u8, offset: usize) Error!void {
        try self.code.append(byte);
        try self.offsets.append(offset);
    }

    pub fn write_constant(self: *Self, value: Value) Error!u8 {
        if (self.constant_count == CONST_MAX) {
            return error.TooManyConst;
        }

        self.constants[self.constant_count] = value;
        self.constant_count += 1;
        return self.constant_count - 1;
    }
};
