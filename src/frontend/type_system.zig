const std = @import("std");
const Allocator = std.mem.Allocator;

const oom = @import("../utils.zig").oom;

// 0000 0000  0000  0000000000 0000000000
// |-------|  |--|  |-------------------|
//    Save    Kind          Value

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
    const KIND_MASK: TypeSize = 0x00f00000;
    const VAL_MASK: TypeSize = 0x000fffff;

    pub inline fn toIdx(self: Self) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) Self {
        return @enumFromInt(index);
    }

    /// Creates a type from kind and value information
    pub inline fn create(kind: Kind, value: Value, imported: bool) Self {
        return @enumFromInt(@as(u32, @intFromBool(imported)) << 25 | 0 << 24 | kind.toIdx() << 20 | value);
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

// 1 bit for nullable or not
// 1 bit of reserve
// 1 bit of reserve
// 1 bit of reserve

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
