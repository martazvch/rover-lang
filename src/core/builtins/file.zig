const std = @import("std");
const ffi = @import("ffi.zig");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

pub const module: ffi.ZigModule = .{
    .is_module = false,
    .functions = &.{
        .init("open", open, "", &.{.{ .name = "path" }}),
    },
    .structures = &.{
        File,
    },
};

const File = struct {
    fd: std.fs.File,

    const Self = @This();

    pub const zig_struct: ffi.ZigStructMeta = .{
        .name = "File",
        .functions = &.{
            .init("readAll", readAll, "", &.{}),
            .init("deinit", deinit, "", &.{}),
        },
    };

    fn readAll(self: *Self, vm: *Vm) []const u8 {
        _ = self; // autofix
        _ = vm; // autofix
        return "abcd";
    }

    fn deinit(self: *Self, vm: *Vm) void {
        vm.allocator.destroy(self);
    }
};

fn open(vm: *Vm, path: []const u8) *File {
    const self = vm.gc_alloc.create(File) catch unreachable;
    self.* = .{
        .fd = std.fs.cwd().openFile(path, .{}) catch unreachable,
    };

    const obj = Obj.NativeObj.create(vm, "File", self, File.zig_struct.getFunctions());

    return @ptrCast(@alignCast(&obj.child));
}
