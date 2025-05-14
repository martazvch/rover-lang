const std = @import("std");
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

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

    pub fn toIdx(self: Self) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn fromIdx(index: usize) Self {
        return @enumFromInt(index);
    }

    /// Creates a type from kind and value information
    pub fn create(kind: Kind, extra: Extra, value: Value) Self {
        const tmp: u32 = @intCast(kind.toIdx());
        const tmp2: u32 = @intCast(extra.toIdx());
        return @enumFromInt(tmp << 28 | tmp2 << 24 | value);
    }

    /// Get a type kind, discarding extra and value information bits
    pub fn getKind(self: Self) Kind {
        return @enumFromInt(self.toIdx() >> 28);
    }

    /// Get a type kind, discarding extra and value information bits
    pub fn setKind(self: *Self, kind: Kind) Self {
        // Looking for the 28 last bits = 7 hexa numbers
        const erased = self.toIdx() & 0xfffffff;
        self.* = @enumFromInt((@as(TypeSize, kind.toIdx()) << 28) | erased);
    }

    // We shift to get the last 8bits. After, we want the first 4bits
    //  value: x x x x  x x x x
    //  mask:  0 0 0 0  1 1 1 1  -> 15 -> 0xf
    /// Get extra information bits about a type
    pub fn getExtra(self: Self) Extra {
        return @enumFromInt(@as(u8, @intCast(self.toIdx() >> 24)) & 0xf);
    }

    /// Get a type kind, discarding extra and value information bits
    pub fn setExtra(self: *Self, extra: Extra) void {
        // Looking for the 5-6-7-8 bits from left
        const erased = self.toIdx() & 0xf0ffffff;
        self.* = @enumFromInt(erased | extra.toIdx() << 24);
    }

    // Looking for the 24 first bits. 24 bits = 6 hexa numbers. We set
    // all to one and mask it
    /// Extract the value bits associated to a type
    pub fn getValue(self: Self) Value {
        return @as(Value, @intCast(self.toIdx() & 0xffffff));
    }

    /// Checks if a type is of a certain kind
    pub fn is(self: Self, kind: Kind) bool {
        return getKind(self) == kind;
    }

    /// Checks if a type is a builtin one, regardless of the kind
    pub fn isBuiltin(self: Self) bool {
        const extra = getExtra(self);
        return extra == .builtin;
    }
};

// 4 first bits (16 values) are for:
pub const Kind = enum(u4) {
    variable,
    func,
    array,
    tuple,
    @"enum",
    map,
    nullable,
    ptr,
    @"error",
    param,
    @"struct",
    module,
    _,

    pub fn toIdx(self: Kind) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn fromIdx(index: usize) Kind {
        return @enumFromInt(index);
    }

    /// Renders Kind as a string
    pub fn toStr(kind: Kind) []const u8 {
        return switch (kind) {
            .func => "function",
            else => |k| @tagName(k),
        };
    }
};

// 4 next bits are for extra infos:
pub const Extra = enum(u4) {
    none,
    builtin,
    bound_method,
    imported,
    _,

    pub fn toIdx(self: Extra) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn fromIdx(index: usize) Extra {
        return @enumFromInt(index);
    }
};

// 24 other allow 16777215 different types
pub const Value = u24;

// Custom types
pub const TypeInfo = union(enum) {
    func: FnInfo,
    module: Symbols,
    @"struct": StructInfo,
};

// TODO: slice instead of fixes size array
pub const FnInfo = struct {
    params: []const Type,
    return_type: Type,
    tag: Tag = .function,

    pub const Tag = enum { builtin, function };
};

pub const StructInfo = struct {
    functions: AutoHashMapUnmanaged(usize, MemberInfo),
    fields: AutoHashMapUnmanaged(usize, MemberInfo),
    default_value_fields: usize,

    pub fn proto(self: *const StructInfo, allocator: std.mem.Allocator) std.AutoHashMapUnmanaged(usize, bool) {
        var res = std.AutoHashMapUnmanaged(usize, bool){};
        res.ensureTotalCapacity(allocator, self.fields.capacity()) catch @panic("oom");

        var kv = self.fields.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.default) {
                res.putAssumeCapacity(entry.key_ptr.*, false);
            }
        }

        return res;
    }
};

pub const MemberInfo = struct {
    /// Order of declaration
    index: usize,
    /// Field's type
    type: Type,
    /// Has a default value
    default: bool = false,
};

pub const Symbols = AutoArrayHashMapUnmanaged(usize, SymbolInfo);

pub const SymbolInfo = struct {
    /// Order of declaration
    index: usize,
    /// Field's type
    type: Type,
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
