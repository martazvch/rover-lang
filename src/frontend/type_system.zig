// Types are 32 bits long
pub const Type = u32;

// 4 first bits (16 values) are for:
pub const Kind = u4;
pub const Var: Kind = 0;
pub const Fn: Kind = 1;
pub const Array: Kind = 2;
pub const Tuple: Kind = 3;
pub const Enum: Kind = 4;
pub const HashMap: Kind = 5;
pub const Nullable: Kind = 6;
pub const Ptr: Kind = 7;
pub const Error: Kind = 8;
pub const Param: Kind = 9;

// 4 next bits are for extra infos:
pub const Extra = u4;
pub const Builtin = 0;

// 24 other allow 16777215 different types
pub const Value = u24;

// Constants
pub const Void: Type = 0;
pub const Null: Type = 1;
pub const Int: Type = 2;
pub const Float: Type = 3;
pub const Bool: Type = 4;
pub const Str: Type = 5;

/// Creates a type from kind and value information
pub fn create(kind: Kind, extra: Extra, value: Value) Type {
    return @as(Type, kind) << 28 | @as(Type, extra) << 24 | value;
}

/// Get a type kind, discarding extra and value information bits
pub fn get_kind(typ: Type) Kind {
    return @as(Kind, @intCast(typ >> 28));
}

/// Get a type kind, discarding extra and value information bits
pub fn set_kind(typ: Type, kind: Kind) Type {
    // Looking for the 28 last bits = 7 hexa numbers
    const erased = typ & 0xfffffff;
    return (@as(Type, kind) << 28) | erased;
}

// We shift to get the last 8bits. After, we want the first 4bits
//  value: x x x x  x x x x
//  mask:  0 0 0 0  1 1 1 1  -> 15 -> 0xf
/// Get extra information bits about a type
pub fn get_extra(typ: Type) Extra {
    return @as(Extra, @intCast(@as(u8, @intCast(typ >> 24)) & 0xf));
}

// Looking for the 24 first bits. 24 bits = 6 hexa numbers. We set
// all to one and mask it
/// Extract the value bits associated to a type
pub fn get_value(typ: Type) Value {
    return @as(Value, @intCast(typ & 0xffffff));
}

/// Checks if a type is of a certain kind
pub fn is(typ: Type, kind: Kind) bool {
    return get_kind(typ) == kind;
}

/// Checks if a type is a builtin one, regardless of the kind
pub fn is_builtin(typ: Type) bool {
    const extra = get_extra(typ);
    return extra == Builtin;
}

// Custom types
pub const TypeInfo = union(enum) {
    Fn: FnInfo,
};

pub const FnInfo = struct {
    arity: usize,
    params: [256]Type,
    return_type: Type,
    builtin: bool = false,
};

pub fn str_kind(kind: Kind) []const u8 {
    return switch (kind) {
        0 => "variable",
        1 => "function",
        2 => "array",
        3 => "tuple",
        4 => "enum",
        5 => "hashmap",
        6 => "nullable",
        7 => "pointer",
        8 => "error",
        else => unreachable,
    };
}

test "types" {
    const expect = @import("std").testing.expect;
    const var1 = Int;
    const var2 = Str;

    try expect(is(var1, Var));
    try expect(!is(var2, Fn));

    const str = create(Var, 0, Str);
    try expect(str == Str);
    try expect(is(str, Var));
    try expect(get_value(str) == Str);

    const func = create(Fn, Builtin, 16777214);
    try expect(is(func, Fn));
    try expect(is_builtin(func));
    try expect(get_value(func) == 16777214);
}
