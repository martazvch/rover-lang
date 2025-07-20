const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const Value = @import("../runtime/values2.zig").Value;
const oom = @import("../utils.zig").oom;

allocator: Allocator,
code: ArrayList(u8),
offsets: ArrayList(usize),
constants: [CONST_MAX]Value,
constant_count: u8,

const Self = @This();
const CONST_MAX = std.math.maxInt(u8) + 1;
pub const Error = error{TooManyConst};

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
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
        return error.TooManyConst;
    }

    self.constants[self.constant_count] = value;
    self.constant_count += 1;
    return self.constant_count - 1;
}

pub const OpCode = enum(u8) {
    array,
    array_access,
    array_access_reg,
    array_access_reg_cow,
    array_access_chain,
    array_access_chain_reg,
    array_assign,
    array_assign_chain,
    add_int,
    add_float,
    bound_import,
    bound_method,
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
    call_native,
    ge_float,
    ge_int,
    get_default,
    get_field,
    get_field_reg,
    get_field_reg_cow,
    get_global,
    get_global_reg,
    get_heap,
    get_local,
    get_local_reg,
    get_local_reg_cow,
    get_local_absolute,
    get_static_method,
    get_symbol,
    get_symbol_reg,
    gt_float,
    gt_int,
    incr_ref_count,
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
    push_module,
    reg_assign,
    reg_assign_cow,
    load_fn_default,
    load_invoke_default,
    load_struct_def,
    reg_push,
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
