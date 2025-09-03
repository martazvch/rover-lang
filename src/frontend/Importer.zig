const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const Ast = @import("Ast.zig");
const AnalyzerReport = @import("Analyzer.zig").AnalyzerReport;
const Sb = @import("../StringBuilder.zig");
const oom = @import("../utils.zig").oom;

const Self = @This();
pub const Result = union(enum) {
    ok: struct { name: []const u8, path: []const u8, content: [:0]const u8 },
    // TODO: would be nice to remove this dependence
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
///
/// **Caller owns memory of result**
pub fn fetchImportedFile(allocator: Allocator, ast: *const Ast, path_chunks: []const Ast.TokenIndex, sb: *Sb) Result {
    if (ast.token_tags[path_chunks[0]] == .dot) {
        return fetchRelative(allocator, ast, path_chunks[1..], sb);
    }

    @panic("Absolute imports not yet implemented");
}

fn fetchRelative(allocator: Allocator, ast: *const Ast, path_chunks: []const Ast.TokenIndex, sb: *Sb) Result {
    var buf_path: [std.fs.max_path_bytes]u8 = undefined;
    const buf_written = sb.render(&buf_path);
    var cwd = std.fs.openDirAbsolute(buf_written, .{}) catch unreachable;

    for (path_chunks, 0..) |part, i| {
        const name = ast.toSource(part);

        if (i == path_chunks.len - 1) {
            const file_name = allocator.alloc(u8, name.len + 3) catch oom();
            @memcpy(file_name[0..name.len], name);
            @memcpy(file_name[name.len..], ".rv");

            const file = cwd.openFile(file_name, .{}) catch {
                const owned_name = allocator.dupe(u8, file_name) catch oom();
                return .{ .err = .err(
                    .{ .missing_file_in_module = .{ .file = owned_name } },
                    ast.getSpan(part),
                ) };
            };
            defer file.close();

            // The file has a new line inserted by default
            const size = file.getEndPos() catch @panic("Rover internal error: wrong import file end position");
            const buf = allocator.allocSentinel(u8, size, 0) catch oom();
            _ = file.readAll(buf) catch @panic("Rover internal error: error while reading imported file");

            sb.append(allocator, ".");
            sb.append(allocator, file_name);
            defer sb.popMany(2);

            return .{ .ok = .{ .name = file_name, .path = sb.renderAlloc(allocator), .content = buf } };
        } else {
            cwd = cwd.openDir(name, .{}) catch return .{ .err = .err(.{ .unknown_module = .{ .name = name } }, ast.getSpan(part)) };
            sb.append(allocator, std.fs.path.sep_str);
            sb.append(allocator, name);
        }
    }

    unreachable;
}
