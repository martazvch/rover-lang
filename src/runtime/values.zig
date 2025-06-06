const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const Obj = @import("Obj.zig");
const Module = @import("../Pipeline.zig").Module;
const Vm = @import("Vm.zig");

pub const Value = union(enum) {
    bool: bool,
    float: f64,
    int: i64,
    null,
    obj: *Obj,
    module: *Module,

    const Self = @This();
    pub const true_: Self = .{ .bool = true };
    pub const false_: Self = .{ .bool = false };
    pub const null_: Self = .{ .null = undefined };

    pub fn makeBool(value: bool) Self {
        return .{ .bool = value };
    }

    pub fn makeFloat(value: f64) Self {
        return .{ .float = value };
    }

    pub fn makeInt(value: i64) Self {
        return .{ .int = value };
    }

    pub fn makeModule(module: *Module) Self {
        return .{ .module = module };
    }

    pub fn makeObj(object: *Obj) Self {
        return .{ .obj = object };
    }

    // Safety garenteed by the analyzer
    pub fn not(self: *Self) void {
        self.bool = !self.bool;
    }

    pub fn asObj(self: *const Self) ?*Obj {
        return switch (self.*) {
            .obj => |v| v,
            else => null,
        };
    }

    pub fn deepCopy(self: Self, vm: *Vm) Self {
        return switch (self) {
            .bool, .float, .int, .null => self,
            // TODO: see if protected in Analyzer
            .module => unreachable,
            .obj => |obj| Self.makeObj(obj.deepCopy(vm)),
        };
    }

    pub fn print(self: *const Self, writer: anytype) Obj.PrintError!void {
        try switch (self.*) {
            .bool => |v| writer.print("{}", .{v}),
            .float => |v| writer.print("{d}", .{v}),
            .int => |v| writer.print("{}", .{v}),
            .null => writer.print("null", .{}),
            .obj => |v| v.print(writer),
            .module => |v| writer.print("<module {s}>", .{v.name}),
        };
    }
};
