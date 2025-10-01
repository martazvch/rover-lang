const std = @import("std");
const Type = @import("type_system.zig").Type;
const Meta = @import("../std/meta.zig");
const NativeFn = Meta.NativeFn;
const NativeFnMeta = Meta.NativeFnMeta;
const ModuleMeta = Meta.ModuleMeta;

// Modules are identified by name and map to a HashMap fn_name -> fn_definition
const ModuleKV = struct { []const u8, std.StaticStringMap(FnDeclaration) };

// Function key-value type
const FnKV = struct { []const u8, FnDeclaration };
pub const FnDeclaration = struct {
    arity: usize,
    params: [256]Type,
    return_type: Type,
    index: usize,
};

const AnalyzedModule = struct {
    name: []const u8,
    declarations: []const FnKV = &.{},
};

pub const BuiltinAnalyzer = struct {
    declarations: std.StaticStringMap(std.StaticStringMap(FnDeclaration)),
    functions: []const NativeFn,
};

pub fn init() BuiltinAnalyzer {
    comptime {
        var all_fn: []const NativeFn = &.{};
        const modules = analyze_all(
            &.{
                // Testing purpose only
                @import("../std/_test/_test1/test1.zig"),
                @import("../std/_test/_test2/test2.zig"),

                @import("../std/time/time.zig"),
                @import("../std/testing/testing.zig"),
            },
            &all_fn,
        );

        return .{
            .declarations = std.StaticStringMap(std.StaticStringMap(FnDeclaration)).initComptime(modules),
            .functions = all_fn,
        };
    }
}

fn analyze_all(Modules: []const type, current_fns: *[]const NativeFn) []const ModuleKV {
    comptime {
        var modules: []const ModuleKV = &.{};

        for (Modules) |Module| {
            const analyzed = analyze_module(Module, current_fns);

            modules = modules ++ .{.{
                analyzed.name,
                std.StaticStringMap(FnDeclaration).initComptime(analyzed.declarations),
            }};
        }

        return modules;
    }
}

fn analyze_module(Module: type, current_fns: *[]const NativeFn) AnalyzedModule {
    comptime {
        var all_kv: []const FnKV = &.{};
        var name: []const u8 = undefined;

        for (@typeInfo(Module).@"struct".decls) |decl| {
            const field = @field(Module, decl.name);

            if (@TypeOf(field) == NativeFnMeta) {
                const infos = @typeInfo(@TypeOf(field));

                var value: FnDeclaration = undefined;
                value.index = current_fns.len;

                for (infos.@"struct".fields) |f| {
                    if (std.mem.eql(u8, "params", f.name)) {
                        const params = @field(field, f.name);

                        value.arity = params.len;

                        for (0..params.len) |i| {
                            value.params[i] = params[i];
                        }
                    } else if (std.mem.eql(u8, "return_type", f.name)) {
                        value.return_type = @field(field, f.name);
                    } else if (std.mem.eql(u8, "function", f.name)) {
                        current_fns.* = current_fns.* ++ .{@field(field, f.name)};
                    }
                }

                all_kv = all_kv ++ .{FnKV{ decl.name, value }};
                // Module meta data
            } else if (@TypeOf(field) == ModuleMeta) {
                const infos = @typeInfo(@TypeOf(field));

                for (infos.@"struct".fields) |f| {
                    if (std.mem.eql(u8, "name", f.name)) {
                        name = @field(field, f.name);
                    }
                }
            }
        }

        return .{ .name = name, .declarations = all_kv };
    }
}
