const std = @import("std");
const expect = std.testing.expect;

// 4 first bytes are:
// - variable
// - function
// - array
// - tuple
// - enum
// - hashmap
// - nullable
// - pointer
// - error
//
// 28 other allow 268435455 different types

const Mask = u4;
const Variable: Mask = 0;
const Function: Mask = 1;
const Array: Mask = 2;
const Tuple: Mask = 3;
const Enum: Mask = 4;
const HashMap: Mask = 5;
const Nullable: Mask = 6;
const Pointer: Mask = 7;
const Error: Mask = 8;

const TypeInfo = u28;
const Void: TypeInfo = 0;
const Int: TypeInfo = 1;
const Bool: TypeInfo = 2;
const Str: TypeInfo = 3;

const Type = u32;

inline fn is(type_: Type, kind: Mask) bool {
    return @as(Mask, @intCast(type_ >> 28)) == kind;
}

inline fn create(kind: Mask, value: TypeInfo) Type {
    return @as(Type, kind) << 28 | value;
}

// 0xff -> 31
// 0x0f -> 15
// 0x7f -> 27? for the 28th first bits
inline fn extract_type_value(type_: Type) TypeInfo {
    return @as(TypeInfo, @intCast(type_ & 0x7f));
}

pub fn main() !void {
    const var1 = Int;
    const var2 = Str;

    try expect(is(var1, Variable));
    try expect(!is(var2, Function));

    const str = create(Variable, Str);
    try expect(str == Str);
    try expect(is(str, Variable));
    try expect(extract_type_value(str) == Str);
}
