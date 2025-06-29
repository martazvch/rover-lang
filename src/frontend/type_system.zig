const std = @import("std");
const Allocator = std.mem.Allocator;

const oom = @import("../utils.zig").oom;

//   0000    0     0     0    0   0000  0000000000 0000000000
//   |--|    |     |     |    |   |--|  |-------------------|
// CallConv  Save  Save  Ref  Nul  Kind          Value

// Types are 32 bits long
const TypeSize = u32;
pub const Type = enum(TypeSize) {
    void,
    null,
    int,
    float,
    bool,
    str,
    self,
    _,

    const Self = @This();
    const CALLCONV_MASK: TypeSize = 0xf0000000;
    const REF_MASK: TypeSize = 0x02000000;
    const NUL_MASK: TypeSize = 0x01000000;
    const KIND_MASK: TypeSize = 0x00f00000;
    const VAL_MASK: TypeSize = 0x000fffff;

    pub inline fn toIdx(self: Self) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) Self {
        return @enumFromInt(index);
    }

    /// Creates a type from kind and value information
    pub inline fn create(kind: Kind, value: Value, extra: CallConv) Self {
        return @enumFromInt(extra.toIdx() << 28 | 0 << 24 | kind.toIdx() << 20 | value);
    }

    /// Get a type kind, discarding calling convention and value information bits
    pub inline fn getKind(self: Self) Kind {
        return @enumFromInt((self.toIdx() & KIND_MASK) >> 20);
    }

    /// Get a type kind, discarding calling convention and value information bits
    pub inline fn setKind(self: *Self, kind: Kind) void {
        const erased = self.toIdx() & ~KIND_MASK;
        self.* = @enumFromInt(erased | (kind.toIdx() << 20));
    }

    /// Get calling convention information bits about a type
    pub inline fn getCallConv(self: Self) CallConv {
        return @enumFromInt(self.toIdx() >> 28);
    }

    /// Set calling convention on a type
    pub inline fn setCallConv(self: *Self, extra: CallConv) void {
        const erased = self.toIdx() & ~CALLCONV_MASK;
        self.* = @enumFromInt(erased | extra.toIdx() << 28);
    }

    pub inline fn copyCallConv(self: *Self, other: Type) void {
        self.setCallConv(other.getCallConv());
    }

    /// Extract the value bits associated to a type
    pub inline fn getValue(self: Self) Value {
        return @as(Value, @intCast(self.toIdx() & VAL_MASK));
    }

    /// Checks if a type is of a certain kind
    pub inline fn is(self: Self, kind: Kind) bool {
        return getKind(self) == kind;
    }

    /// Checks if the type is a heap allocated object
    pub inline fn isHeap(self: Self) bool {
        const kind = self.getKind();
        return kind == .@"struct" or kind == .array;
    }
};

// 4 first bits (16 values) are for:
pub const Kind = enum(u4) {
    none,
    function,
    method,
    array,
    tuple,
    @"enum",
    map,
    @"error",
    @"struct",
    module,
    _,

    pub inline fn toIdx(self: Kind) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) Kind {
        return @enumFromInt(index);
    }
};

// 20 other allow 1048575 different types
pub const Value = u20;

// 1 bit for reference or not
// 1 bit for nullable or not
// 1 bit of reserve
// 1 bit of reserve

// 4 next bits are for calling convention info
pub const CallConv = enum(u4) {
    none,
    builtin,
    bound_method,
    imported,
    _,

    pub inline fn toIdx(self: CallConv) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) CallConv {
        return @enumFromInt(index);
    }
};

test "types" {
    const expect = @import("std").testing.expect;
    const var1: Type = .int;
    const var2: Type = .str;

    try expect(var1.is(.variable));
    try expect(!var2.is(.func));

    const str: Type = .create(.variable, 0, .str);
    try expect(str == .str);
    try expect(str.is(.variable));
    try expect(str.getValue() == .str);

    const func: Type = .create(.func, .builtin, 16777214);
    try expect(func.is(.func));
    try expect(func.isBuiltin());
    try expect(func.getValue() == 16777214);
}
