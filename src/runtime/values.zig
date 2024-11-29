const std = @import("std");
const print = std.debug.print;

pub const Type = union(enum) {
    Bool,
    Float,
    Int,
    Null,

    const Self = @This();

    pub fn str(self: Self) []const u8 {
        return switch (self) {
            .Bool => "bool",
            .Float => "float",
            .Int => "int",
            .Null => "null",
        };
    }
};

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

    // Safety garenteed by the analyzer
    pub fn not(self: *Self) void {
        self.Bool = !self.Bool;
    }

    pub fn log(self: *const Value, writer: anytype) !void {
        try switch (self.*) {
            .Bool => |v| writer.print("{}", .{v}),
            .Float => |v| writer.print("{d}", .{v}),
            .Int => |v| writer.print("{}", .{v}),
            .Null => writer.print("null", .{}),
            .Uint => |v| writer.print("{}", .{v}),
            // .Obj => |v| v.log(),
        };
    }
};
