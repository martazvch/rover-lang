const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");
const expect = testing.expect;
const print = std.debug.print;
const allocator = std.testing.allocator;
const test_config = @import("test_config");
const eql = std.mem.eql;

const RED = "\x1b[31m";
const NORMAL = "\x1b[0m";

fn colorize_dif(str1: []const u8, str2: []const u8) ![]const u8 {
    var res = std.ArrayList(u8).init(allocator);

    const State = enum {
        normal,
        until_nl1,
        until_nl2,
    };

    var i: usize = 0;
    var j: usize = 0;
    dif: switch (State.normal) {
        .normal => {
            if (i >= str1.len) break :dif;
            if (j >= str2.len) break :dif;

            if (str1[i] != str2[j]) {
                try res.appendSlice(RED);
                try res.append(str2[j]);
                i += 1;
                j += 1;
                continue :dif .until_nl1;
            }

            try res.append(str2[j]);
            i += 1;
            j += 1;
            continue :dif .normal;
        },
        .until_nl1 => {
            if (i >= str1.len) break :dif;

            if (str1[i] == '\n') {
                i += 1;
                continue :dif .until_nl2;
            }

            i += 1;
            continue :dif .until_nl1;
        },
        .until_nl2 => {
            if (j >= str2.len) break :dif;

            if (str2[j] == '\n') {
                try res.appendSlice(NORMAL);
                try res.append(str2[j]);
                j += 1;
                continue :dif .normal;
            }

            try res.append(str2[j]);
            j += 1;
            continue :dif .until_nl2;
        },
    }

    return res.toOwnedSlice();
}

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // defer {
    //     const status = gpa.deinit();
    //     std.debug.assert(status == .ok);
    // }
    // const allocator = gpa.allocator();
    //
    // const params = comptime clap.parseParamsComptime(
    //     \\-h, --help             Display this help and exit
    //     \\-s, --stage <Stage>    Stage of the pipeline to test: lexer, parser, analyzer, compiler, vm, all
    //     \\-f, --file <File>      Tests only the specified file
    // );
    //
    // const parsers = comptime .{
    //     .Stage = clap.parsers.enumeration(Stage),
    //     .File = clap.parsers.string,
    // };
    //
    // // Initialize our diagnostics, which can be used for reporting useful errors.
    // // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // // care about the extra information `Diagnostics` provides.
    // var diag = clap.Diagnostic{};
    // var res = clap.parse(clap.Help, &params, parsers, .{
    //     .diagnostic = &diag,
    //     .allocator = allocator,
    // }) catch |err| {
    //     // Report useful error and exit
    //     diag.report(std.io.getStdErr().writer(), err) catch {};
    //     std.process.exit(0);
    // };
    // defer res.deinit();
    //
    // if (res.args.help != 0) return clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});
    //
    // const stage = if (res.args.stage) |s| s else .all;
    // const file = res.args.file orelse null;
    //
    // _ = stage;
    // _ = file;
    // try expect(false);
}

fn run_vm_tests(exe: []const u8) !void {
    const cwd = std.fs.cwd();

    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "tests", "vm",
    });
    defer allocator.free(path);

    var test_dir = try cwd.openDir(path, .{ .iterate = true });
    defer test_dir.close();

    var base_walker = try test_dir.walk(allocator);
    defer base_walker.deinit();

    while (try base_walker.next()) |*item| {
        if (std.mem.endsWith(u8, item.path, ".rv")) {
            const path_to_exe = try std.fs.path.join(allocator, &[_][]const u8{
                "zig-out", "bin", exe,
            });
            defer allocator.free(path_to_exe);

            const file_path = try std.fs.path.join(allocator, &[_][]const u8{
                "tests", "vm", item.path,
            });
            defer allocator.free(file_path);

            const res = try std.process.Child.run(.{
                .allocator = allocator,
                .cwd_dir = cwd,
                .argv = &[_][]const u8{
                    path_to_exe,
                    file_path,
                },
            });
            defer allocator.free(res.stdout);
            defer allocator.free(res.stderr);

            var got_expects = std.mem.splitScalar(u8, res.stdout, '\n');

            // Read file
            const file = try test_dir.openFile(item.path, .{ .mode = .read_only });
            defer file.close();

            const size = try file.getEndPos();
            const content = try allocator.alloc(u8, size);
            defer allocator.free(content);

            _ = try file.readAll(content);
            var lines = std.mem.splitScalar(u8, content, '\n');

            var expects = std.ArrayList(u8).init(allocator);
            defer expects.deinit();

            var i: usize = 0;

            while (lines.next()) |line| : (i += 1) {
                const trimmed = std.mem.trimRight(u8, line, "\r");

                if (std.mem.containsAtLeast(u8, trimmed, 1, "expect:")) {
                    var split = std.mem.splitSequence(u8, trimmed, "expect: ");
                    _ = split.next();
                    const exp = split.next().?;

                    const got = got_expects.next().?;
                    expect(std.mem.eql(u8, got, exp)) catch |e| {
                        print("Error in file: {s} line: {}\n", .{ item.path, i });
                        print("expect:\n{s}\n", .{exp});
                        print("got:\n{s}\n", .{got});
                        print("\nStderr: {s}\n", .{res.stderr});
                        return e;
                    };
                }
            }
        }
    }
}

fn run_stage_tests(exe: []const u8, stage: []const u8) !void {
    _ = exe;

    const path = try std.fs.path.join(allocator, &[_][]const u8{
        "tests", stage,
    });
    defer allocator.free(path);

    var cwd = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer cwd.close();

    var walker = try cwd.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |*entry| {
        if (std.mem.endsWith(u8, entry.basename, ".rvt")) {
            try test_file(&cwd, entry.path);
        }
    }
}

const Section = enum { Code, Config, Expect, Err, None };

fn test_file(dir: *std.fs.Dir, file_path: []const u8) !void {
    const file = try dir.openFile(file_path, .{ .mode = .read_only });
    defer file.close();

    const size = try file.getEndPos();
    const content = try allocator.alloc(u8, size + 1);
    defer allocator.free(content);

    _ = try file.readAll(content);
    content[size] = 0;

    var lines = std.mem.splitScalar(u8, content, '\n');

    var config_text = std.ArrayList(u8).init(allocator);
    defer config_text.deinit();
    var code = std.ArrayList(u8).init(allocator);
    defer code.deinit();
    var expects = std.ArrayList(u8).init(allocator);
    defer expects.deinit();
    var errors = std.ArrayList(u8).init(allocator);
    defer errors.deinit();

    var section: Section = .None;
    var test_count: usize = 0;

    while (lines.next()) |line| {
        if (line.len == 0 or eql(u8, line, "\r")) continue;

        // Skip comments
        if (std.mem.startsWith(u8, std.mem.trimLeft(u8, line, " "), "//")) continue;

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
            try code.append(0);

            const tmp_file = try dir.createFile("tmp.rv", .{});
            try tmp_file.writeAll(code.items[0 .. code.items.len - 1 :0]);
            tmp_file.close();
            // std.process.exit(0);
            defer dir.deleteFile("tmp.rv") catch unreachable;

            run_test(
                dir,
                expects.items,
                errors.items,
            ) catch |e| {
                print("Error in test {} in file {s}\n\n", .{ test_count + 1, file_path });
                return e;
            };

            code.clearRetainingCapacity();
            expects.clearRetainingCapacity();
            errors.clearRetainingCapacity();
            section = .None;
            test_count += 1;
            continue;
        }

        var writer = switch (section) {
            .Code => code.writer(),
            .Config => config_text.writer(),
            .Expect => expects.writer(),
            .Err => errors.writer(),
            .None => continue,
        };

        _ = try writer.write(std.mem.trimRight(u8, line, "\r"));
        _ = try writer.write("\n");
    }
}

pub fn run_test(cwd: *std.fs.Dir, exp: []const u8, errors: []const u8) !void {
    _ = errors;

    const path_to_exe = try std.fs.path.join(allocator, &[_][]const u8{
        "..", "..", "zig-out", "bin", "rover",
    });
    defer allocator.free(path_to_exe);

    const res = try std.process.Child.run(.{
        .allocator = allocator,
        .cwd_dir = cwd.*,
        .argv = &[_][]const u8{
            path_to_exe,
            "tmp.rv",
            "--print-ast",
            "--exit-on-print",
        },
    });
    defer allocator.free(res.stdout);
    defer allocator.free(res.stderr);

    std.debug.print("ERR: {s}\n", .{res.stderr});

    if (exp.len > 0) {
        expect(eql(u8, res.stdout, exp)) catch |e| {
            const color_dif = try colorize_dif(exp, res.stdout);
            defer allocator.free(color_dif);

            print("expect:\n{s}\n", .{exp});
            print("got:\n{s}\n", .{color_dif});
            return e;
        };
    }
    // else if (errors.len > 0) {
    //     var all = std.mem.splitScalar(u8, errors, '\n');
    //
    //     var i: usize = 0;
    //     while (all.next()) |err| : (i += 1) {
    //         if (err.len == 0) continue;
    //
    //         var line = std.mem.splitScalar(u8, err, ',');
    //         // There is at least one element, the error name
    //         const err_name = line.next().?;
    //
    //         expect(i < test_data.reports.len) catch |e| {
    //             print("assertion {}, expect to find a {}th report but only {} were generated\n", .{ i, i + 1, test_data.reports.len });
    //             return e;
    //         };
    //         const report = test_data.reports[i];
    //
    //         const got_name = @tagName(report);
    //         expect(eql(u8, err_name, got_name)) catch |e| {
    //             print("assertion {}, expect error: {s}, got {s}\n", .{ i, err_name, got_name });
    //             return e;
    //         };
    //
    //         // We look for all the fields
    //         inline for (fields(Report)) |field| {
    //             // If we find the current and that it's a non-void (assuming it's a struct)
    //             if (field.type != void and eql(u8, field.name, @tagName(std.meta.activeTag(report)))) {
    //                 const fv = @field(report, field.name);
    //
    //                 // We check all the fields of the structure to test them
    //                 inline for (fields(field.type)) |subf| {
    //                     const subv = @field(fv, subf.name);
    //
    //                     if (line.next()) |extra| {
    //                         expect(eql(u8, std.mem.trimLeft(u8, extra, " "), subv)) catch |e| {
    //                             print("assertion {}, expect error extra arg: {s}, got {s}\n", .{ i, extra, subv });
    //                             return e;
    //                         };
    //                     } else {
    //                         print("expect extra argument '{s}' for report '{s}'\n", .{ subf.name, field.name });
    //                         return error.TestUnexpectedResult;
    //                     }
    //                 }
    //             }
    //         }
    //     }
    //
    //     // i - 1 because end of while loop executed the continuation expression
    //     if (i - 1 != test_data.reports.len) {
    //         print(
    //             "Error: incoherent number of errors/warnings, execution had {} and test file had {}\n",
    //             .{ test_data.reports.len, i - 1 },
    //         );
    //         print("Execution:\n{any}\nTest file:\n{s}\n", .{ test_data.reports, errors });
    //         return error.TestUnexpectedResult;
    //     }
    // } else {
    //     print("Error, no expect and no erros in test\n", .{});
    //     return error.TestUnexpectedResult;
    // }
}

test "runtime" {
    const exe_name = if (builtin.os.tag == .windows) "rover.exe" else "rover";

    switch (test_config.stage) {
        .all => {},
        .lexer => {},
        .parser => try run_stage_tests(exe_name, "parser"),
        .analyzer => {},
        .compiler => try run_stage_tests(),
        .vm => try run_vm_tests(exe_name),
    }

    // try run_test(&[_][]const u8{ "tests", "vm" });

    // var cwd = std.fs.cwd();
    //
    // const path = try std.fs.path.join(allocator, &[_][]const u8{
    //     "tests", "vm",
    // });
    // defer allocator.free(path);
    //
    // var test_dir = try cwd.openDir(path, .{ .iterate = true });
    // defer test_dir.close();
    //
    // var base_walker = try test_dir.walk(allocator);
    // defer base_walker.deinit();
    //
    // while (try base_walker.next()) |*item| {
    //     if (std.mem.endsWith(u8, item.path, ".rv")) {
    //         const exe_name = if (builtin.os.tag == .windows) "rover.exe" else "rover";
    //
    //         const path_to_exe = try std.fs.path.join(allocator, &[_][]const u8{
    //             "zig-out", "bin", exe_name,
    //         });
    //         defer allocator.free(path_to_exe);
    //
    //         const file_path = try std.fs.path.join(allocator, &[_][]const u8{
    //             "tests", "vm", item.path,
    //         });
    //         defer allocator.free(file_path);
    //
    //         const res = try std.process.Child.run(.{
    //             .allocator = allocator,
    //             .cwd_dir = cwd,
    //             .argv = &[_][]const u8{
    //                 path_to_exe,
    //                 "-f",
    //                 file_path,
    //             },
    //         });
    //         defer allocator.free(res.stdout);
    //         defer allocator.free(res.stderr);
    //
    //         var got_expects = std.mem.splitScalar(u8, res.stdout, '\n');
    //
    //         // Read file
    //         const file = try test_dir.openFile(item.path, .{ .mode = .read_only });
    //         defer file.close();
    //
    //         const size = try file.getEndPos();
    //         const content = try allocator.alloc(u8, size);
    //         defer allocator.free(content);
    //
    //         _ = try file.readAll(content);
    //         var lines = std.mem.splitScalar(u8, content, '\n');
    //
    //         var expects = std.ArrayList(u8).init(allocator);
    //         defer expects.deinit();
    //
    //         var i: usize = 0;
    //
    //         while (lines.next()) |line| : (i += 1) {
    //             const trimmed = std.mem.trimRight(u8, line, "\r");
    //
    //             if (std.mem.containsAtLeast(u8, trimmed, 1, "expect:")) {
    //                 var split = std.mem.splitSequence(u8, trimmed, "expect: ");
    //                 _ = split.next();
    //                 const exp = split.next().?;
    //
    //                 const got = got_expects.next().?;
    //                 expect(std.mem.eql(u8, got, exp)) catch |e| {
    //                     print("Error in file: {s} line: {}\n", .{ item.path, i });
    //                     print("expect:\n{s}\n", .{exp});
    //                     print("got:\n{s}\n", .{got});
    //                     print("\nStderr: {s}\n", .{res.stderr});
    //                     return e;
    //                 };
    //             }
    //         }
    //     }
    // }
}
