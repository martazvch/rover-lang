const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const expect = testing.expect;
const allocator = testing.allocator;
const Parser = @import("../../parser.zig").Parser;
const AstPrinter = @import("../../ast_print.zig").AstPrinter;

pub fn test_all() !void {
    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "src", "tests", "parser",
    });
    defer allocator.free(path);

    var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer cwd.close();

    var walker = try cwd.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |*entry| {
        if (std.mem.endsWith(u8, entry.basename, ".test")) {
            try test_file(&cwd, entry.path);
        }
    }
}

fn test_file(dir: *std.fs.Dir, file_path: []const u8) !void {
    const file = try dir.openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 100000000);
    defer allocator.free(content);
    var lines = std.mem.splitScalar(u8, content, '\n');

    var code = std.ArrayList(u8).init(allocator);
    defer code.deinit();
    var answer = std.ArrayList(u8).init(allocator);
    defer answer.deinit();

    var code_section = false;
    var answer_section = false;
    var test_count: usize = 0;

    while (lines.next()) |line| {
        if (line.len == 0 or std.mem.eql(u8, line, "\r")) continue;
        // If after removing the \r there is only spaces, we skip it
        if (std.mem.trimRight(u8, std.mem.trimRight(u8, line, "\r"), " ").len == 0) continue;

        if (std.mem.startsWith(u8, line, "code")) {
            code_section = true;
            answer_section = false;
            continue;
        } else if (std.mem.startsWith(u8, line, "expect")) {
            code_section = false;
            answer_section = true;
            continue;
        } else if (std.mem.startsWith(u8, line, "==")) {
            run_test(code.items, answer.items) catch |e| {
                print("Error in test {} in file {s}\n\n", .{ test_count, file_path });
                return e;
            };

            code.clearRetainingCapacity();
            answer.clearRetainingCapacity();
            code_section = false;
            answer_section = false;
            test_count += 1;
            continue;
        }

        if (code_section) {
            try code.appendSlice(std.mem.trimRight(u8, line, "\r"));
            try code.append('\n');
        } else if (answer_section) {
            try answer.appendSlice(std.mem.trimRight(u8, line, "\r"));
            try answer.append('\n');
        }
    }
}

fn run_test(source: []const u8, answer: []const u8) !void {
    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();
    try parser.parse(source);

    var ast_printer = AstPrinter.init(allocator);
    defer ast_printer.deinit();
    try ast_printer.parse_ast(parser.stmts.items);

    expect(std.mem.eql(u8, ast_printer.tree.items, answer)) catch |e| {
        std.debug.print("answer:\n{s}\n", .{answer});
        std.debug.print("got:\n{s}\n", .{ast_printer.tree.items});
        return e;
    };
}
