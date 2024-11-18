const std = @import("std");
const print = std.debug.print;

pub const Value = union(enum) {
    Bool: bool,
    Float: f64,
    Int: i64,
    Uint: u64,
    Null,

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

    pub fn uint(value: u64) Self {
        return .{ .Uint = value };
    }

    pub fn null_() Self {
        return .{ .Null = undefined };
    }

    pub fn log(self: *const Value) void {
        switch (self.*) {
            .Bool => |v| print("{}", .{v}),
            .Float => |v| print("{d}", .{v}),
            .Int => |v| print("{}", .{v}),
            .Null => print("null", .{}),
            .Uint => |v| print("{}", .{v}),
            // .Obj => |v| v.log(),
        }
    }
};
