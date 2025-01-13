// Types are 32 bits long
pub const Type = u32;

// 4 first bytes (16 values) are for:
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

// 28 other allow 268435455 different types
pub const Value = u28;
pub const Void: Value = 0;
pub const Null: Value = 1;
pub const Int: Value = 2;
pub const Float: Value = 3;
pub const Bool: Value = 4;
pub const Str: Value = 5;

/// Checks if a type is of a certain kind
pub inline fn is(type_: Type, kind: Kind) bool {
    return get_kind(type_) == kind;
}

/// Creates a type from kind and value information
pub inline fn create(kind: Kind, value: Value) Type {
    return @as(Type, kind) << 28 | value;
}

// 0xff -> 31
// 0x0f -> 15
// 0x7f -> 27 for the 28th first bits
/// Extract the value associated to a type, whatever kind it is
pub inline fn get_value(type_: Type) Value {
    return @as(Value, @intCast(type_ & 0x7f));
}

pub inline fn get_kind(type_: Type) Kind {
    return @as(Kind, @intCast(type_ >> 28));
}

// Custom types
pub const TypeInfo = union(enum) {
    Fn: FnInfo,
};

pub const FnInfo = struct {
    arity: usize,
    params: [256]Type,
    return_type: Type,
};

test "types" {
    const expect = @import("std").testing.expect;
    const var1 = Int;
    const var2 = Str;

    try expect(is(var1, Var));
    try expect(!is(var2, Fn));

    const str = create(Var, Str);
    try expect(str == Str);
    try expect(is(str, Var));
    try expect(get_value(str) == Str);
}
