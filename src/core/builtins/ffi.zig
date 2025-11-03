const std = @import("std");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

pub const ZigModule = struct {
    name: ?[]const u8 = null,
    is_module: bool = true,
    functions: []const ZigFnMeta = &.{},
    structures: []const ZigStruct = &.{},
};

pub const ZigFnMeta = struct {
    name: []const u8,
    params: []const []const u8,
    desc: []const u8,
    info: std.builtin.Type.Fn,
    function: ZigFn,

    pub fn init(name: []const u8, desc: []const u8, params: []const []const u8, func: anytype) ZigFnMeta {
        if (name.len == 0) {
            @compileError("Function's name can't be empty");
        }

        const info = @typeInfo(@TypeOf(func));
        if (info != .@"fn") {
            @compileError("Trying to declare a non-function, found: " ++ @typeName(func));
        }

        const fn_info = info.@"fn";
        if (params.len > fn_info.params.len) {
            @compileError("Too many parameters descriptions");
        }

        return .{
            .name = name,
            .desc = desc,
            .params = params,
            .info = fn_info,
            .function = makeNative(func),
        };
    }
};

pub const ZigFn = *const fn (*Vm, []const Value) ?Value;

pub const ZigStruct = struct {
    name: []const u8,
    type: type,
    fields: []const Field = &.{},
    functions: []const ZigFnMeta = &.{},

    pub const Field = struct {
        name: []const u8,
        desc: []const u8,
        offset: usize,
    };
};

pub fn makeNative(func: anytype) ZigFn {
    return struct {
        comptime func: @TypeOf(func) = func,

        pub fn call(vm: *Vm, stack: []const Value) ?Value {
            const ArgsType = ArgsTuple(@TypeOf(func));
            var args: ArgsType = undefined;

            if (@TypeOf(args[0]) != *Vm) {
                @compileError("First argument of functions must be of type *Vm");
            }

            const fields = @typeInfo(ArgsType).@"struct".fields;
            args[0] = vm;

            inline for (fields[1..], 0..) |f, i| {
                args[i + 1] = fromValue(f.type, stack[i]);
            }

            if (@typeInfo(@TypeOf(func)).@"fn".return_type != null) {
                const res = @call(.auto, func, args);
                return toValue(vm, res);
            } else {
                @call(.auto, func, args);
                return null;
            }
        }

        fn fromValue(T: type, value: Value) T {
            return switch (@typeInfo(T)) {
                .float => value.float,
                .int => value.int,
                .bool => value.bool,
                // TODO: make this generic
                .@"union" => switch (value) {
                    .int => .{ .int = value.int },
                    .float => .{ .float = value.float },
                    else => unreachable,
                },
                .pointer => |ptr| {
                    return switch (ptr.child) {
                        u8 => value.obj.as(Obj.String).chars,
                        else => {
                            // const obj = value.asObj() orelse @compileError("Unsupported pointer child type: " ++ @typeName(T));
                            const obj = value.asObj() orelse unreachable;
                            const native = obj.as(Obj.NativeObj);
                            return @ptrCast(@alignCast(native.child));
                        },
                    };
                },
                else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(T)),
            };
        }

        fn toValue(vm: *Vm, value: anytype) Value {
            return switch (@typeInfo(@TypeOf(value))) {
                .float => .{ .float = value },
                .int => .{ .int = value },
                .bool => .{ .bool = value },
                .pointer => |ptr| {
                    if (ptr.child == u8) {
                        return .makeObj(Obj.String.take(vm, value).asObj());
                    }

                    return .makeObj(Obj.NativeObj.create(vm, "file", value).asObj());
                },
                else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(@TypeOf(value))),
            };
        }
    }.call;
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
        args_type[i] = arg.type.?;
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
