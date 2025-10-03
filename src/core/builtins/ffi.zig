const std = @import("std");
const Value = @import("../runtime/values.zig").Value;

pub const Type = union(enum) {
    int,
    float,
    @"union": []const Type,
};

pub const FnMeta = struct {
    params: []const Param = &.{},
    return_type: Type,
    function: NativeFn,

    pub const Param = struct {
        type: Type,
        desc: ?[]const u8 = null,
        default: ?Value = null,
    };
};

pub const NativeFn = struct {
    call_fn: *const fn ([]const Value) ?Value,

    pub fn call(self: *const NativeFn, stack: []const Value) ?Value {
        return self.call_fn(stack);
    }
};

pub fn makeNative(func: anytype) NativeFn {
    return GenNative(func).ffi();
}

pub fn GenNative(func: anytype) type {
    return struct {
        const ArgsType = ArgsTuple(@TypeOf(func));
        comptime func: @TypeOf(func) = func,

        pub fn call(stack: []const Value) ?Value {
            var args: ArgsType = undefined;
            const fields = @typeInfo(ArgsType).@"struct".fields;

            inline for (fields, 0..) |f, i| {
                args[i] = fromValue(f.type, stack[i]);
            }

            if (@typeInfo(@TypeOf(func)).@"fn".return_type != null) {
                const res = @call(.auto, func, args);
                return toValue(res);
            } else {
                @call(.auto, func, args);
                return null;
            }
        }

        fn fromValue(T: type, value: Value) T {
            if (T == Value) return value;

            return switch (@typeInfo(T)) {
                .float => value.float,
                .int => value.int,
                .bool => value.bool,
                else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(T)),
            };
        }

        fn toValue(value: anytype) Value {
            return switch (@typeInfo(@TypeOf(value))) {
                .float => .{ .float = value },
                .int => .{ .int = value },
                .bool => .{ .bool = value },
                else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(@TypeOf(value))),
            };
        }

        pub fn ffi() NativeFn {
            return .{ .call_fn = call };
        }
    };
}

pub fn ArgsTuple(comptime FnType: type) type {
    const infos = @typeInfo(FnType);
    if (infos != .@"fn") {
        @compileError("FFI: Can't generate native function for a non-function");
    }

    const fn_infos = infos.@"fn";
    if (fn_infos.is_var_args) {
        @compileError("FFI: Can't generate native function for variadic function");
    }

    var args_type: [fn_infos.params.len]type = undefined;
    inline for (fn_infos.params, 0..) |arg, i| {
        args_type[i] = arg.type orelse Value;
    }

    var fields: [fn_infos.params.len]std.builtin.Type.StructField = undefined;
    for (args_type, 0..) |T, i| {
        fields[i] = .{
            .name = std.fmt.comptimePrint("{}", .{i}),
            .type = T,
            .default_value_ptr = null,
            .is_comptime = false,
            .alignment = @alignOf(T),
        };
    }

    return @Type(.{ .@"struct" = .{
        .is_tuple = true,
        .decls = &.{},
        .fields = &fields,
        .layout = .auto,
    } });
}
