const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const expect = testing.expect;
const allocator = testing.allocator;
const Parser = @import("parser.zig").Parser;
const AstPrinter = @import("ast_print.zig").AstPrinter;
const IterList = @import("../iter_list.zig").IterList;

const Err = struct { kind: []const u8, extra: ?[]const u8 };

const Section = enum { Code, Expect, Err, None };

pub fn test_all() !void {
    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "tests", "parser",
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
    var expects = std.ArrayList(u8).init(allocator);
    defer expects.deinit();
    var errors = std.ArrayList(u8).init(allocator);
    defer errors.deinit();

    var section: Section = .None;
    var test_count: usize = 0;

    while (lines.next()) |line| {
        if (line.len == 0 or std.mem.eql(u8, line, "\r")) continue;
        // If after removing the \r there is only spaces, we skip it
        if (std.mem.trimRight(u8, std.mem.trimRight(u8, line, "\r"), " ").len == 0) continue;

        if (std.mem.startsWith(u8, line, "code")) {
            section = .Code;
            continue;
        } else if (std.mem.startsWith(u8, line, "expect")) {
            section = .Expect;
            continue;
        } else if (std.mem.startsWith(u8, line, "error")) {
            section = .Err;
            continue;
        } else if (std.mem.startsWith(u8, line, "==")) {
            run_test(code.items, expects.items, errors.items) catch |e| {
                print("Error in test {} in file {s}\n\n", .{ test_count, file_path });
                return e;
            };

            code.clearRetainingCapacity();
            expects.clearRetainingCapacity();
            section = .None;
            test_count += 1;
            continue;
        }

        var writer = switch (section) {
            .Code => code.writer(),
            .Expect => expects.writer(),
            .Err => errors.writer(),
            .None => continue,
        };

        _ = try writer.write(std.mem.trimRight(u8, line, "\r"));
        _ = try writer.write("\n");
    }
}

fn run_test(source: []const u8, expects: []const u8, errors: []const u8) !void {
    var parser: Parser = undefined;
    parser.init(allocator);
    defer parser.deinit();
    try parser.parse(source);

    if (expects.len > 0) {
        var ast_printer = AstPrinter.init(allocator);
        defer ast_printer.deinit();
        try ast_printer.parse_ast(parser.stmts.items);

        expect(std.mem.eql(u8, ast_printer.tree.items, expects)) catch |e| {
            std.debug.print("expect:\n{s}\n", .{expects});
            std.debug.print("got:\n{s}\n", .{ast_printer.tree.items});
            return e;
        };
    } else if (errors.len > 0) {
        var iter_list = IterList([]const u8).init(parser.errs_extra.items);
        var all = std.mem.splitScalar(u8, errors, '\n');

        var i: usize = 0;
        while (all.next()) |err| : (i += 1) {
            if (err.len == 0) continue;

            var extra = std.mem.splitScalar(u8, err, ',');
            const err_name = extra.next().?;

            const got_name = @tagName(parser.errs.items[i].kind);
            expect(std.mem.eql(u8, err_name, got_name)) catch |e| {
                print("expect error: {s}, got {s}\n", .{ err_name, got_name });
                return e;
            };

            if (extra.next()) |ex| {
                const extra_arg = iter_list.next();
                expect(std.mem.eql(u8, std.mem.trimLeft(u8, ex, " "), extra_arg)) catch |e| {
                    print("expect error extra arg: {s}, got {s}\n", .{ ex, extra_arg });
                    return e;
                };
            }
        }
    } else {
        print("Error, no expect and no erros in test\n", .{});
    }
}
