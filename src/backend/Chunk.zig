const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("../runtime/values.zig").Value;
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
        .code = .empty,
        .offsets = .empty,
        .constants = undefined,
        .constant_count = 0,
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.code.deinit(allocator);
    self.offsets.deinit(allocator);
}

pub fn writeOp(self: *Self, op: OpCode, offset: usize) void {
    self.code.append(self.allocator, @intFromEnum(op)) catch oom();
    self.offsets.append(self.allocator, offset) catch oom();
}

pub fn writeByte(self: *Self, byte: u8, offset: usize) void {
    self.code.append(self.allocator, byte) catch oom();
    self.offsets.append(self.allocator, offset) catch oom();
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
    add_int,
    add_float,
    array_new,
    array_get,
    array_get_chain,
    array_get_chain_cow,
    array_set,
    array_set_chain,
    box,
    call,
    call_native,
    cast_to_float,
    closure,
    constant,
    def_global,
    div_float,
    div_int,
    eq_bool,
    eq_float,
    eq_int,
    eq_null,
    eq_str,
    exit_repl,
    ge_float,
    ge_int,
    get_capt_frame,
    get_capt_local,
    get_default,
    get_field,
    get_field_cow,
    get_global,
    get_global_cow,
    get_local,
    get_local_cow,
    get_method,
    get_static_method,
    gt_float,
    gt_int,
    incr_ref,
    jump,
    jump_false,
    jump_true,
    le_int,
    le_float,
    lt_int,
    lt_float,
    load_blk_val,
    load_extern_sym,
    load_sym,
    loop,
    mul_float,
    mul_int,
    ne_bool,
    ne_float,
    ne_int,
    ne_null,
    ne_null_push,
    ne_str,
    neg_float,
    neg_int,
    not,
    pop,
    print,
    push_false,
    push_null,
    push_true,
    ret,
    ret_naked,
    set_field,
    set_global,
    set_local,
    set_local_box,
    store_blk_val,
    str_cat,
    str_mul,
    struct_lit,
    sub_float,
    sub_int,
    unbox,
};
