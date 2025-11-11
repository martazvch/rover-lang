const std = @import("std");
const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

pub const ZigModule = struct {
    name: ?[]const u8 = null,
    is_module: bool = true,
    functions: []const ZigFnMeta = &.{},
    structures: []const type = &.{},
};

pub const ZigFnMeta = struct {
    name: []const u8,
    params: []const Param,
    desc: []const u8,
    info: std.builtin.Type.Fn,
    function: ZigFn,

    // NOTE: it is not possible in Zig 0.15.2 to reflect on parameters name, we have to provide them
    pub const Param = struct {
        name: [:0]const u8,
        desc: []const u8 = "",
    };

    pub fn init(name: []const u8, func: anytype, desc: []const u8, params: []const Param) ZigFnMeta {
        if (name.len == 0) {
            @compileError("Function's name can't be empty");
        }

        const info = @typeInfo(@TypeOf(func));
        if (info != .@"fn") {
            @compileError("Trying to declare a non-function, found: " ++ @typeName(func));
        }

        return .{
            .name = name,
            .desc = desc,
            .params = params,
            .info = info.@"fn",
            .function = makeNative(func),
        };
    }
};

pub const ZigFn = *const fn (*Vm, []const Value) ?Value;

pub const ZigStructMeta = struct {
    name: []const u8,
    fields: []const Field = &.{},
    functions: []const ZigFnMeta = &.{},

    pub const Field = struct {
        name: []const u8,
        desc: []const u8,
        offset: usize,
    };

    pub fn getFunctions(self: *const ZigStructMeta) []const ZigFn {
        comptime var funcs: [self.functions.len]ZigFn = undefined;

        inline for (self.functions, 0..) |func, i| {
            funcs[i] = func.function;
        }

        const copy = funcs;
        return &copy;
    }
};

pub const ZigStruct = struct {
    child: *anyopaque,
    funcs: []const ZigFn,
};

pub fn makeNative(func: anytype) ZigFn {
    return struct {
        pub fn call(vm: *Vm, stack: []const Value) ?Value {
            const ArgsType, const vm_index = ArgsTuple(@TypeOf(func));
            var args: ArgsType = undefined;

            const fields = @typeInfo(ArgsType).@"struct".fields;

            comptime var offset: usize = 0;

            inline for (fields, 0..) |f, i| {
                if (i == vm_index) {
                    args[vm_index] = vm;
                    offset = 1;
                } else {
                    args[i] = fromValue(f.type, stack[i - offset]);
                }
            }

            if (@typeInfo(@TypeOf(func)).@"fn".return_type.? != void) {
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
                        anyopaque => @compileError("Can't use *anyopaque in functions"),
                        else => {
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
                .pointer => |ptr| return switch (ptr.child) {
                    // TODO: check memory managment
                    u8 => .makeObj(Obj.String.take(vm, value).asObj()),
                    else => {
                        const native = @as(
                            *Obj.NativeObj,
                            @alignCast(@fieldParentPtr(
                                "child",
                                @as(*align(@alignOf(@FieldType(Obj.NativeObj, "child"))) *anyopaque, @ptrCast(@alignCast(value))),
                            )),
                        );

                        return .makeObj(native.asObj());
                    },
                },
                else => @compileError("FFI: Unsupported type in auto conversion: " ++ @typeName(@TypeOf(value))),
            };
        }
    }.call;
}

pub fn ArgsTuple(comptime FnType: type) struct { type, usize } {
    const infos = @typeInfo(FnType);
    if (infos != .@"fn") {
        @compileError("FFI: Can't generate native function for a non-function");
    }

    const fn_infos = infos.@"fn";
    if (fn_infos.is_var_args) {
        @compileError("FFI: Can't generate native function for variadic function");
    }

    const vm_index = if (fn_infos.params[0].type.? == *Vm)
        0
    else if (fn_infos.params[1].type.? == *Vm)
        1
    else
        @compileError("Either first or second argument of functions must be of type *Vm");

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

    return .{
        @Type(.{ .@"struct" = .{
            .is_tuple = true,
            .decls = &.{},
            .fields = &fields,
            .layout = .auto,
        } }),
        vm_index,
    };
}
