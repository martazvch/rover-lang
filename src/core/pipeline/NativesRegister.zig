const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ffi = @import("../builtins/ffi.zig");
const MapNameType = @import("../analyzer/types.zig").MapNameType;
const Type = @import("../analyzer/types.zig").Type;
const TypeInterner = @import("../analyzer/types.zig").TypeInterner;

const misc = @import("misc");
const Interner = misc.Interner;
const oom = misc.oom;

meta: std.AutoArrayHashMapUnmanaged(Interner.Index, *const Type),
funcs: ArrayList(struct { name: []const u8, func: ffi.ZigFn }),

const Self = @This();

pub const empty: Self = .{ .meta = .empty, .funcs = .empty };

pub fn register(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, Module: type) void {
    const SelfType, const self_interned = s: {
        if (!@hasDecl(Module, "rover_self")) {
            break :s .{ null, null };
        }

        const field = @field(Module, "rover_self");
        if (@TypeOf(field) != ffi.ZigStruct) {
            @compileError("Native Zig module's 'rover_self' variable must be of type " ++ @typeName(ffi.ZigStruct));
        }

        break :s .{ field.type, registerStruct(allocator, &field, interner, ti) };
    };

    if (@hasDecl(Module, "module")) {
        const mod = @field(Module, "module");

        if (@TypeOf(mod) != ffi.ZigModule) {
            @compileError("Native Zig module's 'module' variable must be of type " ++ @typeName(ffi.ZigModule));
        }

        inline for (mod.functions) |func| {
            self.registerFn(allocator, &func, SelfType, self_interned, interner, ti);
        }
    } else {
        @compileError("Native Zig files must declare a module");
    }
}

fn registerStruct(allocator: Allocator, structure: *const ffi.ZigStruct, interner: *Interner, ti: *TypeInterner) *const Type {
    var s: Type.Structure = .{
        .loc = null,
        .fields = .empty,
        .functions = .empty,
    };
    s.fields.ensureTotalCapacity(allocator, structure.fields.len) catch oom();
    s.functions.ensureTotalCapacity(allocator, structure.functions.len) catch oom();

    const ty = ti.intern(.{ .structure = s });

    inline for (structure.functions) |*func| {
        const f = fnZigToRover(allocator, func, structure.type, ty, interner, ti);
        s.functions.putAssumeCapacity(interner.intern(func.name), f);
    }

    return ty;
}

// We can use pointers here because we refer to comptime declarations in Module
fn registerFn(
    self: *Self,
    allocator: Allocator,
    func: *const ffi.ZigFnMeta,
    SelfType: ?type,
    self_interned: ?*const Type,
    interner: *Interner,
    ti: *TypeInterner,
) void {
    self.meta.put(allocator, interner.intern(func.name), fnZigToRover(allocator, func, SelfType, self_interned, interner, ti)) catch oom();
    self.funcs.append(allocator, .{ .name = func.name, .func = func.function }) catch oom();
}

fn fnZigToRover(
    allocator: Allocator,
    func: *const ffi.ZigFnMeta,
    SelfType: ?type,
    self_interned: ?*const Type,
    interner: *Interner,
    ti: *TypeInterner,
) *const Type {
    var params: Type.Function.ParamsMap = .empty;
    // -1 for the 'Vm' parameter
    params.ensureTotalCapacity(allocator, func.info.params.len - 1) catch oom();

    inline for (func.info.params[1..], 0..) |*p, i| {
        if (i == 0) {
            params.putAssumeCapacity(
                i,
                .{ .type = maybeSelf(allocator, SelfType, self_interned, p.type.?, interner, ti), .default = false, .captured = false },
            );
        } else {
            params.putAssumeCapacity(i, .{ .type = zigToRover(allocator, p.type.?, interner, ti), .default = false, .captured = false });
        }
    }

    const ty: Type.Function = .{
        .kind = .native,
        .loc = null,
        .return_type = maybeSelf(allocator, SelfType, self_interned, func.info.return_type.?, interner, ti),
        .params = params,
    };

    return ti.intern(.{ .function = ty });
}

fn maybeSelf(allocator: Allocator, SelfType: ?type, self_interned: ?*const Type, ty: type, interner: *Interner, ti: *TypeInterner) *const Type {
    if (SelfType) |S| {
        if (ty == *S) return self_interned.?;
    }
    return zigToRover(allocator, ty, interner, ti);
}

fn zigToRover(allocator: Allocator, ty: type, interner: *Interner, ti: *TypeInterner) *const Type {
    return switch (ty) {
        i64 => ti.getCached(.int),
        f64 => ti.getCached(.float),
        []const u8 => ti.getCached(.str),
        else => switch (@typeInfo(ty)) {
            .@"union" => |u| {
                var childs = ArrayList(*const Type).initCapacity(allocator, u.fields.len) catch oom();
                inline for (u.fields) |f| {
                    childs.appendAssumeCapacity(zigToRover(allocator, f.type, interner, ti));
                }
                return ti.intern(.{ .@"union" = .{ .types = childs.toOwnedSlice(allocator) catch oom() } });
            },
            .pointer => |ptr| switch (ptr.child) {
                []const u8 => @panic("TODO"),
                else => |C| zigToRover(allocator, C, interner, ti),
            },
            // .@"struct" => {
            //     // It is considered that it must refer to a 'self' value, can't return a pointer from
            //     // a builtin if it's not the object itself
            //     const obj = self_data orelse {
            //         @compileError("Expect to find 'self' type when returning pointer from native function");
            //     };
            //
            //     var fields: Type.Structure.FieldsMap = .empty;
            //     fields.ensureTotalCapacity(allocator, obj.fields.len) catch oom();
            //
            //     inline for (obj.fields) |f| {
            //         fields.putAssumeCapacity(interner.intern(f.name), .{ .type = undefined, .default = false });
            //     }
            //
            //     var functions: MapNameType = .empty;
            //     functions.ensureTotalCapacity(allocator, obj.functions.len) catch oom();
            //
            //     inline for (obj.functions) |func| {
            //         functions.putAssumeCapacity(interner.intern(func.name), fnZigToRover(allocator, &func, obj, interner, ti));
            //     }
            //
            //     return ti.intern(.{ .structure = .{
            //         .loc = null,
            //         .fields = fields,
            //         .functions = functions,
            //     } });
            // },
            else => @compileError("Zig to Rover type conversion not supported for type: " ++ @typeName(ty)),
        },
    };
}
