const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Value = @import("../runtime/values.zig").Value;
const oom = @import("misc").oom;

code: ArrayList(u8),
offsets: ArrayList(usize),

const Self = @This();
pub const empty: Self = .{ .code = .empty, .offsets = .empty };

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.code.deinit(allocator);
    self.offsets.deinit(allocator);
}

pub fn writeOp(self: *Self, allocator: Allocator, op: OpCode, offset: usize) void {
    errdefer oom();
    try self.code.append(allocator, @intFromEnum(op));
    try self.offsets.append(allocator, offset);
}

pub fn writeByte(self: *Self, allocator: Allocator, byte: u8, offset: usize) void {
    errdefer oom();
    try self.code.append(allocator, byte);
    try self.offsets.append(allocator, offset);
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
    closure,
    def_global,
    div_float,
    div_int,
    dup,
    enum_create,
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
    get_field,
    get_field_cow,
    get_global,
    get_global_cow,
    get_local,
    get_local_cow,
    invoke,
    get_fn,
    get_tag,
    gt_float,
    gt_int,
    incr_ref,
    is_bool,
    is_float,
    is_int,
    is_str,
    is_type,
    jump,
    jump_false,
    jump_true,
    le_int,
    le_float,
    lt_int,
    lt_float,
    load_blk_val,
    load_constant,
    load_ext_constant,
    load_sym,
    load_ext_sym,
    load_builtin,
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
    swap_pop,
    unbox,
};
