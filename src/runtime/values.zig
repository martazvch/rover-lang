const std = @import("std");
const print = std.debug.print;

const Obj = @import("Obj.zig");

pub const Value = union(enum) {
    bool: bool,
    float: f64,
    int: i64,
    null,
    obj: *Obj,

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

    pub fn makeObj(object: *Obj) Value {
        return .{ .obj = object };
    }

    // Safety garenteed by the analyzer
    pub fn not(self: *Self) void {
        self.bool = !self.bool;
    }

    pub fn asObj(self: *const Value) ?*Obj {
        return switch (self.*) {
            .obj => |v| v,
            else => null,
        };
    }

    pub fn print(self: *const Value, writer: anytype) !void {
        try switch (self.*) {
            .bool => |v| writer.print("{}", .{v}),
            .float => |v| writer.print("{d}", .{v}),
            .int => |v| writer.print("{}", .{v}),
            .null => writer.print("null", .{}),
            .obj => |v| v.print(writer),
        };
    }
};
