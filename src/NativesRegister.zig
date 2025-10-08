const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ffi = @import("core/builtins/ffi.zig");
const Type = @import("core/analyzer/types.zig").Type;
const TypeInterner = @import("core/analyzer/types.zig").TypeInterner;

const misc = @import("misc");
const Interner = misc.Interner;
const oom = misc.oom;

meta: std.AutoArrayHashMapUnmanaged(Interner.Index, Type.Function),
funcs: ArrayList(struct { name: []const u8, func: ffi.NativeFn }),

const Self = @This();

pub const empty: Self = .{ .meta = .empty, .funcs = .empty };

pub fn register(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, Module: type) void {
    const infos = @typeInfo(Module);
    if (infos != .@"struct") {
        @compileError("Native Reg: can't register natives outside of a file-container");
    }

    const struct_infos = infos.@"struct";

    inline for (struct_infos.decls) |decl| {
        const fn_meta = @field(Module, decl.name);
        if (@TypeOf(fn_meta) != ffi.FnMeta) {
            @compileError("Native Reg: can't register anything else than ffi.FnMeta");
        }

        self.registerFn(allocator, interner, ti, decl.name, &fn_meta);
    }
}

// We can use pointers here because we refer to comptime declarations in Module
fn registerFn(self: *Self, allocator: Allocator, interner: *Interner, ti: *TypeInterner, name: []const u8, fn_meta: *const ffi.FnMeta) void {
    var params: Type.Function.ParamsMap = .empty;
    params.ensureTotalCapacity(allocator, fn_meta.params.len) catch oom();

    for (fn_meta.params, 0..) |*p, i| {
        params.putAssumeCapacity(i, .{ .type = ffiTypesToRover(allocator, ti, p.type), .default = p.default != null, .captured = false });
    }

    const ty: Type.Function = .{
        .kind = .native,
        .loc = null,
        .return_type = ffiTypesToRover(allocator, ti, fn_meta.return_type),
        .params = params,
    };

    self.meta.put(allocator, interner.intern(name), ty) catch oom();
    self.funcs.append(allocator, .{ .name = name, .func = fn_meta.function }) catch oom();
}

fn ffiTypesToRover(allocator: Allocator, ti: *TypeInterner, ffi_ty: ffi.Type) *const Type {
    return switch (ffi_ty) {
        .int => ti.getCached(.int),
        .float => ti.getCached(.float),
        .@"union" => |u| {
            var childs = ArrayList(*const Type).initCapacity(allocator, u.len) catch oom();
            for (u) |t| {
                childs.appendAssumeCapacity(ffiTypesToRover(allocator, ti, t));
            }
            return ti.intern(.{ .@"union" = .{ .types = childs.toOwnedSlice(allocator) catch oom() } });
        },
    };
}
