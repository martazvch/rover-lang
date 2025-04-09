const std = @import("std");

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

    pub fn to_idx(self: Type) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn from_idx(index: usize) Type {
        return @enumFromInt(index);
    }
};

// 4 first bits (16 values) are for:
const KindSize = u4;
pub const Kind = enum(KindSize) {
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
    _,

    pub fn to_idx(self: Kind) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn from_idx(index: usize) Kind {
        return @enumFromInt(index);
    }
};

// 4 next bits are for extra infos:
const ExtraSize = u4;
pub const Extra = enum(u4) {
    none,
    builtin,
    _,

    pub fn to_idx(self: Extra) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn from_idx(index: usize) Extra {
        return @enumFromInt(index);
    }
};

// 24 other allow 16777215 different types
pub const Value = u24;

/// Creates a type from kind and value information
pub fn create(kind: Kind, extra: Extra, value: Value) Type {
    const tmp: u32 = @intCast(kind.to_idx());
    const tmp2: u32 = @intCast(extra.to_idx());
    return @enumFromInt(tmp << 28 | tmp2 << 24 | value);
}

/// Get a type kind, discarding extra and value information bits
pub fn get_kind(typ: Type) Kind {
    return @enumFromInt(typ.to_idx() >> 28);
}

/// Get a type kind, discarding extra and value information bits
pub fn set_kind(typ: Type, kind: Kind) Type {
    // Looking for the 28 last bits = 7 hexa numbers
    const erased = typ.to_idx() & 0xfffffff;
    return @enumFromInt((@as(TypeSize, kind.to_idx()) << 28) | erased);
}

// We shift to get the last 8bits. After, we want the first 4bits
//  value: x x x x  x x x x
//  mask:  0 0 0 0  1 1 1 1  -> 15 -> 0xf
/// Get extra information bits about a type
pub fn get_extra(typ: Type) Extra {
    return @enumFromInt(@as(u8, @intCast(typ.to_idx() >> 24)) & 0xf);
}

// Looking for the 24 first bits. 24 bits = 6 hexa numbers. We set
// all to one and mask it
/// Extract the value bits associated to a type
pub fn get_value(typ: Type) Value {
    return @as(Value, @intCast(typ.to_idx() & 0xffffff));
}

/// Checks if a type is of a certain kind
pub fn is(typ: Type, kind: Kind) bool {
    return get_kind(typ) == kind;
}

/// Checks if a type is a builtin one, regardless of the kind
pub fn is_builtin(typ: Type) bool {
    const extra = get_extra(typ);
    return extra == .builtin;
}

// Custom types
pub const TypeInfo = union(enum) {
    func: FnInfo,
    @"struct": StructInfo,
};

// TODO: slice instead of fixes size array
pub const FnInfo = struct {
    arity: usize,
    params: [256]Type,
    return_type: Type,
    builtin: bool = false,
};

pub const StructInfo = struct {
    init: ?usize = null,
    functions: std.AutoHashMapUnmanaged(usize, usize),
    fields: std.AutoHashMapUnmanaged(usize, FieldInfo),
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

pub const FieldInfo = struct {
    idx: usize,
    type: Type,
    default: bool,
};

pub fn str_kind(kind: Kind) []const u8 {
    return switch (kind) {
        .func => "function",
        else => |k| @tagName(k),
    };
}

test "types" {
    const expect = @import("std").testing.expect;
    const var1: Type = .int;
    const var2: Type = .str;

    try expect(is(var1, .variable));
    try expect(!is(var2, .func));

    const str = create(.variable, 0, .str);
    try expect(str == .str);
    try expect(is(str, .variable));
    try expect(get_value(str) == .str);

    const func = create(.func, .builtin, 16777214);
    try expect(is(func, .func));
    try expect(is_builtin(func));
    try expect(get_value(func) == 16777214);
}
