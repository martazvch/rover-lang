const std = @import("std");
const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Vm = @import("../runtime/Vm.zig");

fd: std.fs.File,

const Self = @This();

pub const module: ffi.ZigModule = .{
    .is_module = false,
    .functions = &.{
        .init("open", "", &.{}, open),
    },
    // .structures = &.{.{
    //     .name = "File",
    //     .functions = &.{
    //         // .init("open", "", &.{}, open),
    //     },
    // }},
};

pub const rover_self: ffi.ZigStruct = .{
    .name = "File",
    .type = Self,
    .functions = &.{
        .init("readAll", "", &.{}, readAll),
    },
};

fn open(vm: *Vm, path: []const u8) *Self {
    const self = vm.gc_alloc.create(Self) catch unreachable;

    self.* = .{
        .fd = std.fs.cwd().openFile(path, .{}) catch unreachable,
    };

    return self;
}

fn readAll(vm: *Vm, self: *Self) []const u8 {
    _ = self; // autofix
    // fn readAll(vm: *Vm, v: i64) []const u8 {
    _ = vm; // autofix
    return "";
}
