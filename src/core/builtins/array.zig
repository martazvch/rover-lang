const std = @import("std");
const ArrayList = std.array_list.Managed(Value);
const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;

array: ArrayList,

const Self = @This();

const SelfFfi: ffi.Type = .{ .structure = .{
    .fields = &.{},
    .functions = &.{len},
} };

pub const len: ffi.FnMeta = .{
    .params = &.{},
    .return_type = .int,
    .function = ffi.genNative(_len),
};

fn _len(self: *const Self) usize {
    return self.array.items.len;
}
