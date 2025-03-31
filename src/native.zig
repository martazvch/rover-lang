const std = @import("std");
const Value = @import("runtime/values.zig").Value;

// Common function wrapper interface
pub const FnWrapper = struct {
    ptr: *const anyopaque,
    call_fn: *const fn (*const anyopaque, []const Value) Value,

    pub fn call(self: *const FnWrapper, stack: []const Value) Value {
        return self.call_fn(self.ptr, stack);
    }
};

pub fn NativeWrap(func: anytype) type {
    const ArgsType = std.meta.ArgsTuple(@TypeOf(func));

    return struct {
        comptime func: @TypeOf(func) = func,

        pub fn call(ctx: *const anyopaque, stack: []const Value) Value {
            const self = @as(*const @This(), @alignCast(@ptrCast(ctx)));
            var args: ArgsType = undefined;
            const fields = @typeInfo(ArgsType).@"struct".fields;

            inline for (fields, 0..) |f, i| {
                args[i] = from_value(f.type, stack[i]);
            }

            const res = @call(.auto, self.func, args);
            return to_value(res);
        }

        fn from_value(T: type, value: Value) T {
            return switch (@typeInfo(T)) {
                .int => value.Int,
                .bool => value.bool,
                else => @panic("unsupported type"),
            };
        }

        fn to_value(value: anytype) Value {
            return switch (@typeInfo(@TypeOf(value))) {
                .int => .{ .Int = value },
                .bool => .{ .Bool = value },
                else => @panic("unsupported type"),
            };
        }

        pub fn to_wrap(self: *const @This()) FnWrapper {
            return .{
                .ptr = self,
                .call_fn = call,
            };
        }
    };
}
