const std = @import("std");
const print = std.debug.print;
const Obj = @import("obj.zig").Obj;

pub const Value = union(enum) {
    Bool: bool,
    Float: f64,
    Int: i64,
    Null,
    Obj: *Obj,

    const Self = @This();

    pub fn bool_(value: bool) Self {
        return .{ .Bool = value };
    }

    pub fn float(value: f64) Self {
        return .{ .Float = value };
    }

    pub fn int(value: i64) Self {
        return .{ .Int = value };
    }

    pub fn null_() Self {
        return .{ .Null = undefined };
    }

    pub fn obj(object: *Obj) Value {
        return .{ .Obj = object };
    }

    // Safety garenteed by the analyzer
    pub fn not(self: *Self) void {
        self.Bool = !self.Bool;
    }

    pub fn as_obj(self: *const Value) ?*Obj {
        return switch (self.*) {
            .Obj => |v| v,
            else => null,
        };
    }

    pub fn print(self: *const Value, writer: anytype) !void {
        try switch (self.*) {
            .Bool => |v| writer.print("{}", .{v}),
            .Float => |v| writer.print("{d}", .{v}),
            .Int => |v| writer.print("{}", .{v}),
            .Null => writer.print("null", .{}),
            .Obj => |v| v.print(writer),
        };
    }
};
