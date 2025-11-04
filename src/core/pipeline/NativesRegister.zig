const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ffi = @import("../builtins/ffi.zig");
const MapNameType = @import("../analyzer/types.zig").MapNameType;
const Type = @import("../analyzer/types.zig").Type;
const TypeInterner = @import("../analyzer/types.zig").TypeInterner;

const Value = @import("../runtime/values.zig").Value;
const Obj = @import("../runtime/Obj.zig");
const Vm = @import("../runtime/Vm.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const oom = misc.oom;

/// Native functions used at runtime
// funcs: ArrayList(struct { name: []const u8, func: ffi.ZigFn }),
funcs: ArrayList(Value),
/// Native functions translated to Rover's type system for compilation
funcs_meta: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),

/// Native structures used at runtime
structs: ArrayList(struct { name: []const u8, func: Value }),
/// Native structures translated to Rover's type system for compilation
structs_meta: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),
/// Native structures translated to Rover's type system used here for self references
scratch_structs: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),

const Self = @This();

pub const empty: Self = .{
    .funcs_meta = .empty,
    .funcs = .empty,
    .structs = .empty,
    .structs_meta = .empty,
    .scratch_structs = .empty,
};

pub fn register(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, Module: type) void {
    if (!@hasDecl(Module, "module")) {
        @compileError("Native Zig files must declare a module");
    }

    const mod = @field(Module, "module");
    if (@TypeOf(mod) != ffi.ZigModule) {
        @compileError("Native Zig module's 'module' variable must be of type " ++ @typeName(ffi.ZigModule));
    }

    inline for (mod.structures) |s| {
        self.registerStruct(allocator, s, interner, ti);
    }

    inline for (mod.functions) |func| {
        self.registerFn(allocator, &func, interner, ti);
    }
}

fn registerStruct(self: *Self, allocator: Allocator, S: type, interner: *Interner, ti: *TypeInterner) void {
    if (!@hasDecl(S, "zig_struct")) {
        @compileError("Native structures must declare a public 'zig_struct' constant");
    }

    const zig_struct = @field(S, "zig_struct");
    if (@TypeOf(zig_struct) != ffi.ZigStructMeta) {
        @compileError("zig_struct constant must be of type: " ++ @typeName(ffi.ZigStructMeta));
    }

    var s: Type.Structure = .{
        .loc = null,
        .fields = .empty,
        .functions = .empty,
    };
    s.fields.ensureTotalCapacity(allocator, zig_struct.fields.len) catch oom();
    s.functions.ensureTotalCapacity(allocator, zig_struct.functions.len) catch oom();

    const ty = ti.intern(.{ .structure = s });
    self.scratch_structs.put(allocator, interner.intern(@typeName(S)), ty) catch oom();

    inline for (zig_struct.functions) |*func| {
        const f = self.fnZigToRover(allocator, func, interner, ti);
        ty.structure.functions.putAssumeCapacity(interner.intern(func.name), f);
    }

    // TODO: use assume capacity (check all the 'put')
    self.structs_meta.put(allocator, interner.intern(zig_struct.name), ty) catch oom();
}

// We can use pointers here because we refer to comptime declarations in Module
fn registerFn(self: *Self, allocator: Allocator, func: *const ffi.ZigFnMeta, interner: *Interner, ti: *TypeInterner) void {
    self.funcs_meta.put(allocator, interner.intern(func.name), self.fnZigToRover(allocator, func, interner, ti)) catch oom();
    const value = Value.makeObj(Obj.NativeFunction.create(allocator, func.name, func.function).asObj());

    // self.funcs.append(allocator, .{ .name = func.name, .func = func.function }) catch oom();
    // self.funcs.append(allocator, .{ .name = func.name, .func = value }) catch oom();
    self.funcs.append(allocator, value) catch oom();
}

fn fnZigToRover(self: *Self, allocator: Allocator, func: *const ffi.ZigFnMeta, interner: *Interner, ti: *TypeInterner) *const Type {
    var params: Type.Function.ParamsMap = .empty;

    // We don't take into account param *Vm and if it's in second place, it means 'self' is in first and we skip it too
    const offset: usize = if (func.info.params.len > 1 and func.info.params[1].type.? == *Vm) 2 else 1;

    params.ensureTotalCapacity(allocator, func.info.params.len - offset) catch oom();

    inline for (func.info.params[offset..], 0..) |*p, i| {
        const param_ty = self.zigToRover(allocator, p.type.?, interner, ti);

        const param_index = if (i == 0) i else i - 1;
        params.putAssumeCapacity(
            interner.intern(func.params[param_index].name),
            .{ .type = param_ty, .default = false, .captured = false },
        );
    }

    const ty: Type.Function = .{
        .kind = if (offset == 2) .native_method else .native,
        .loc = null,
        .return_type = self.zigToRover(allocator, func.info.return_type.?, interner, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

fn zigToRover(self: *Self, allocator: Allocator, ty: type, interner: *Interner, ti: *TypeInterner) *const Type {
    return switch (ty) {
        i64 => ti.getCached(.int),
        f64 => ti.getCached(.float),
        []const u8 => ti.getCached(.str),
        else => switch (@typeInfo(ty)) {
            .@"union" => |u| {
                var childs = ArrayList(*const Type).initCapacity(allocator, u.fields.len) catch oom();
                inline for (u.fields) |f| {
                    childs.appendAssumeCapacity(self.zigToRover(allocator, f.type, interner, ti));
                }
                return ti.intern(.{ .@"union" = .{ .types = childs.toOwnedSlice(allocator) catch oom() } });
            },
            .pointer => |ptr| switch (ptr.child) {
                Obj => unreachable,
                else => |C| self.zigToRover(allocator, C, interner, ti),
            },
            .@"struct" => {
                if (self.scratch_structs.get(interner.intern(@typeName(ty)))) |t| {
                    return t;
                }

                // TODO: error, occurs when everything isn't correctly registered inside the file
                // like a static function returning a type not declared in the structures of the module
                unreachable;
            },
            else => @compileError("Zig to Rover type conversion not supported for type: " ++ @typeName(ty)),
        },
    };
}
