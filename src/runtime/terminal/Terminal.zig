const std = @import("std");

ptr: *anyopaque,
vtable: *const VTable,

pub const Self = @This();
pub const Error = error{ InitFail, RawModeFail, ReadInputError, NotAnEvent };

pub const Key = struct {
    value: union(enum) {
        up,
        down,
        left,
        right,
        enter,
        chars: []u8,
    },
    ctrl: bool,
};

pub const VTable = struct {
    enableRawMode: *const fn (*anyopaque) Error!void,
    disableRawMode: *const fn (*anyopaque) void,
    getKey: *const fn (*anyopaque) Error!Key,
};

pub fn enableRawMode(self: *@This()) Error!void {
    try self.vtable.enableRawMode(self.ptr);
}

pub fn disableRawMode(self: *@This()) void {
    self.vtable.disableRawMode(self.ptr);
}

pub fn getKey(self: *@This()) Error!Key {
    return self.vtable.getKey(self.ptr);
}
