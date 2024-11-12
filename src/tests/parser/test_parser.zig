const std = @import("std");
const testing = std.testing;
const allocator = testing.allocator;
const Parser = @import("../../parser.zig").Parser;
const AstPrinter = @import("../../ast_print.zig").AstPrinter;

const TestParser = struct {};

pub fn test_file(file_path: []const u8) !void {
    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "src", "tests", "parser", file_path,
    });
    defer allocator.free(path);
    const file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 100000000);
    defer allocator.free(content);
    var lines = std.mem.splitScalar(u8, content, '\n');

    var code = std.ArrayList(u8).init(allocator);
    defer code.deinit();
    var expect = std.ArrayList(u8).init(allocator);
    defer expect.deinit();

    var code_section = false;
    var expect_section = false;

    while (lines.next()) |line| {
        std.debug.print("line: {s}\n", .{line});

        if (code_section) {
            try code.appendSlice(line);
        } else if (expect_section) {
            try expect.appendSlice(line);
        }

        if (std.mem.startsWith(u8, line, "code")) {
            code_section = true;
            expect_section = false;
        } else if (std.mem.startsWith(u8, line, "expect")) {
            code_section = false;
            expect_section = true;
        } else if (std.mem.startsWith(u8, line, "==")) {
            // Test
            code.clearRetainingCapacity();
            expect.clearRetainingCapacity();
            code_section = false;
            expect_section = false;
        }
    }
}

fn run_test(source: []const u8, answer: []const u8) !void {
    var parser = Parser.init(allocator);
    try parser.parse(source);

    var ast_printer = AstPrinter.init(allocator);
    try ast_printer.parse_ast(parser.stmts.items);

    _ = answer;
}
