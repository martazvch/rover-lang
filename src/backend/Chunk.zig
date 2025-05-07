const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("../runtime/values.zig").Value;
const oom = @import("../utils.zig").oom;

code: ArrayList(u8),
offsets: ArrayList(usize),
constants: [CONST_MAX]Value,
// constants: ArrayList(Value),
constant_count: u8,

const Self = @This();
const CONST_MAX = std.math.maxInt(u8) + 1;
pub const Error = error{TooManyConst};

pub fn init(allocator: Allocator) Self {
    return .{
        .code = ArrayList(u8).init(allocator),
        .offsets = ArrayList(usize).init(allocator),
        // .constants = ArrayList(Value).init(allocator),
        .constants = undefined,
        .constant_count = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
    self.offsets.deinit();
}

pub fn writeOp(self: *Self, op: OpCode, offset: usize) void {
    self.code.append(@intFromEnum(op)) catch oom();
    self.offsets.append(offset) catch oom();
}

pub fn writeByte(self: *Self, byte: u8, offset: usize) void {
    self.code.append(byte) catch oom();
    self.offsets.append(offset) catch oom();
}

pub fn writeConstant(self: *Self, value: Value) Error!u8 {
    if (self.constant_count == CONST_MAX) {
        // if (self.constants.items.len == CONST_MAX) {
        return error.TooManyConst;
    }

    // try self.constants.append(value);
    // return @intCast(self.constants.items.len - 1);

    self.constants[self.constant_count] = value;
    self.constant_count += 1;
    return self.constant_count - 1;
}

pub const OpCode = enum(u8) {
    add_int,
    add_float,
    bound_method,
    bound_method_call,
    cast_to_float,
    constant,
    define_heap_var,
    define_global,
    div_float,
    div_int,
    eq_bool,
    eq_float,
    eq_int,
    eq_str,
    exit_repl,
    false,
    call,
    field_assign,
    get_field,
    get_global,
    get_heap,
    get_local,
    gt_float,
    gt_int,
    ge_float,
    ge_int,
    invoke,
    jump,
    jump_if_false,
    jump_if_true,
    lt_int,
    lt_float,
    le_int,
    le_float,
    loop,
    mul_float,
    mul_int,
    naked_return,
    native_fn_call,
    ne_bool,
    ne_float,
    ne_int,
    ne_str,
    negate_float,
    negate_int,
    not,
    null,
    pop,
    print,
    @"return",
    scope_return,
    set_global,
    set_heap,
    set_local,
    str_cat,
    str_mul_l,
    str_mul_r,
    struct_literal,
    sub_float,
    sub_int,
    true,
};
