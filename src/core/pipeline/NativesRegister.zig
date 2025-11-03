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

meta: std.AutoArrayHashMapUnmanaged(Interner.Index, Type.Function),
funcs: ArrayList(struct { name: []const u8, func: ffi.ZigFn }),

const Self = @This();

pub const empty: Self = .{ .meta = .empty, .funcs = .empty };

pub fn register(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, Module: type) void {
    if (!@hasDecl(Module, "module")) {
        @compileError("Native Zig module must declare a 'module'");
    }

    const mod = @field(Module, "module");

    if (@TypeOf(mod) != ffi.ZigModule) {
        @compileError("Native Zig module's 'module' variable must be of type " ++ @typeName(ffi.ZigModule));
    }

    inline for (mod.functions) |func| {
        self.registerFn(allocator, interner, ti, &func);
    }
}

// We can use pointers here because we refer to comptime declarations in Module
fn registerFn(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, func: *const ffi.ZigFnMeta) void {
    var params: Type.Function.ParamsMap = .empty;
    params.ensureTotalCapacity(allocator, func.info.params.len) catch oom();

    inline for (func.info.params, 0..) |*p, i| {
        params.putAssumeCapacity(i, .{ .type = zigToRover(allocator, p.type.?, ti), .default = false, .captured = false });
    }

    const ty: Type.Function = .{
        .kind = .native,
        .loc = null,
        .return_type = zigToRover(allocator, func.info.return_type.?, ti),
        .params = params,
    };

    self.meta.put(allocator, interner.intern(func.name), ty) catch oom();
    self.funcs.append(allocator, .{ .name = func.name, .func = func.function }) catch oom();
}

fn zigToRover(allocator: Allocator, ty: type, ti: *TypeInterner) *const Type {
    return switch (ty) {
        i64 => ti.getCached(.int),
        f64 => ti.getCached(.float),
        else => switch (@typeInfo(ty)) {
            .@"union" => |u| {
                var childs = ArrayList(*const Type).initCapacity(allocator, u.fields.len) catch oom();
                inline for (u.fields) |f| {
                    childs.appendAssumeCapacity(zigToRover(allocator, f.type, ti));
                }
                return ti.intern(.{ .@"union" = .{ .types = childs.toOwnedSlice(allocator) catch oom() } });
            },
            else => unreachable,
        },
    };
}
