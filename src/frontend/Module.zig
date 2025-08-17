const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const Ast = @import("Ast.zig");
const AnalyzerReport = @import("Analyzer2.zig").AnalyzerReport;
const Interner = @import("../Interner.zig");
const oom = @import("../utils.zig").oom;

const Self = @This();
const Path = ArrayListUnmanaged([]const u8);
pub const Result = union(enum) {
    ok: struct { name: []const u8, content: [:0]const u8 },
    err: AnalyzerReport,
};

/// Import rules and order
/// - If path starts with a '.', consider it as a relative path and fails if not found
/// - If path starts with an identifier, consider it as an absolute path from where the process was invoked
///     If fails, consider it as an imported package
/// - If considered as a package, look for it in package place TODO:
///
/// Naming rules
/// - Last identifier is the file to import
pub fn fetchImportedFile(
    allocator: Allocator,
    ast: *const Ast,
    path_parts: []const Ast.TokenIndex,
    path: *Path,
    interner: *Interner,
) Result {
    if (ast.token_tags[path_parts[0]] == .dot) {
        return fetchRelative(allocator, ast, path_parts[1..], path, interner);
    }

    unreachable;
}

fn fetchRelative(
    allocator: Allocator,
    ast: *const Ast,
    path_parts: []const Ast.TokenIndex,
    path: *Path,
    interner: *Interner,
) Result {
    _ = interner; // autofix
    var cwd = std.fs.openDirAbsolute(path.items[0], .{}) catch unreachable;

    for (path_parts, 0..) |part, i| {
        const name = ast.toSource(part);

        if (i == path_parts.len - 1) {
            // TODO: manage this better. We should hash the whole path and later manage it even better
            // because same module could be imported from different files with different path. We should
            // create a path from the root folder for every import so that all path can be compared regardless
            // of where files are
            // const interned = interner.intern(ast.toSource(part));
            // if (self.modules.get(interned)) |mod| return .{ mod, true };

            const file_name = allocator.alloc(u8, name.len + 3) catch oom();
            defer allocator.free(file_name);

            @memcpy(file_name[0..name.len], name);
            @memcpy(file_name[name.len..], ".rv");

            const file = cwd.openFile(file_name, .{}) catch {
                const owned_name = allocator.dupe(u8, file_name) catch oom();
                return .{ .err = .err(
                    if (path_parts.len > 1)
                        .{ .missing_file_in_module = .{
                            .file = owned_name,
                            .module = ast.toSource(path_parts[path_parts.len - 2]),
                        } }
                    else
                        .{ .missing_file_in_cwd = .{ .file = owned_name } },
                    ast.getSpan(part),
                ) };
            };
            defer file.close();

            // The file has a new line inserted by default
            const size = file.getEndPos() catch @panic("Rover internal error: wrong import file end position");
            const buf = allocator.alloc(u8, size + 1) catch oom();

            _ = file.readAll(buf) catch @panic("Rover internal error: error while reading imported file");
            buf[size] = 0;

            // var pipeline = self.pipeline.createSubPipeline();
            // // Exit for now, just showing the error of the sub-pipeline
            // const module = pipeline.run(name, buf[0..size :0]) catch {
            //     std.process.exit(0);
            // };

            // return .{ module, false };
        } else {
            cwd = cwd.openDir(name, .{}) catch return .{ .err = .err(.{ .unknown_module = .{ .name = name } }, ast.getSpan(part)) };
        }
    }

    unreachable;
}
