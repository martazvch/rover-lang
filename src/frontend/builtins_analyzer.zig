const std = @import("std");
const Type = @import("type_system.zig").Type;
const Meta = @import("../std/meta.zig");
const NativeFnMeta = Meta.NativeFnMeta;
const NativeFn = Meta.NativeFn;

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
    declarations: []const FnKV = &.{},
    // functions: []const NativeFn,
};

pub const BuiltinAnalyzer = struct {
    declarations: std.StaticStringMap(std.StaticStringMap(FnDeclaration)),
    functions: []const NativeFn,
};

pub fn init() BuiltinAnalyzer {
    comptime {
        var all_fn: []const NativeFn = &.{};
        const analyzed = analyze_module(@import("../std/time/time.zig"), &all_fn);

        const module: []const ModuleKV = &.{
            .{
                "time",
                std.StaticStringMap(FnDeclaration).initComptime(analyzed.declarations),
            },
        };

        return .{
            .declarations = std.StaticStringMap(std.StaticStringMap(FnDeclaration)).initComptime(module),
            // .functions = analyzed.functions,
            .functions = all_fn,
        };
    }
}

fn analyze_all(Modules: []const type) AnalyzedModule {
    comptime {
        var declarations: []const FnKV = &.{};
        var functions: []const NativeFn = &.{};

        for (Modules) |Module| {
            const analyzed = analyze_module(Module);

            declarations = declarations ++ analyzed.declarations;
            functions = functions ++ analyzed.functions;
        }

        return .{ .declarations = declarations, .functions = functions };
    }
}

// fn analyze_module(Module: type, current_fns: []const NativeFn) AnalyzedModule {
fn analyze_module(Module: type, current_fns: *[]const NativeFn) AnalyzedModule {
    comptime {
        var all_kv: []const FnKV = &.{};
        // var all_fn: []const NativeFn = current_fns;

        for (@typeInfo(Module).@"struct".decls) |decl| {
            const field = @field(Module, decl.name);

            if (@TypeOf(field) == NativeFnMeta) {
                const infos = @typeInfo(@TypeOf(field));

                var value: FnDeclaration = undefined;
                // value.index = all_fn.len - 1;
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
                        // current_fns = current_fns ++ @field(field, f.name);
                        current_fns.* = current_fns.* ++ .{@field(field, f.name)};
                    }
                }

                all_kv = all_kv ++ .{FnKV{ decl.name, value }};
            }
        }

        // return .{ .declarations = all_kv, .functions = all_fn };
        return .{ .declarations = all_kv };
    }
}
