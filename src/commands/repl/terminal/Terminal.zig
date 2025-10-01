const std = @import("std");

pub const Self = @This();
pub const Error = error{ InitFail, RawModeFail, ReadInputError, NotAnEvent, NonAsciiChar };

pub const Key = struct {
    value: union(enum) {
        up,
        down,
        left,
        right,
        enter,
        back,
        delete,
        tab,
        char: u8,
    },
    ctrl: bool,
};

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    enableRawMode: *const fn (*anyopaque) Error!void,
    disableRawMode: *const fn (*anyopaque) void,
    getKey: *const fn (*anyopaque) Error!Key,
};

pub fn enableRawMode(self: *Self) Error!void {
    try self.vtable.enableRawMode(self.ptr);
}

pub fn disableRawMode(self: *Self) void {
    self.vtable.disableRawMode(self.ptr);
}

pub fn getKey(self: *Self) Error!Key {
    return self.vtable.getKey(self.ptr);
}
