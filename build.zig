const std = @import("std");

const Stage = enum { all, parser, analyzer, compiler, vm };

pub fn build(b: *std.Build) !void {
    const options = b.addOptions();

    const print_instr = b.option(bool, "print-instr", "prints the current instruction") orelse false;
    options.addOption(bool, "print_instr", print_instr);

    const print_stack = b.option(bool, "print-stack", "prints the stack on each instruction") orelse false;
    options.addOption(bool, "print_stack", print_stack);

    const log_gc = b.option(bool, "log-gc", "logs each GC actions (alloc and free)") orelse false;
    options.addOption(bool, "log_gc", log_gc);

    const tracy_enabled = b.option(bool, "tracy", "Enable tracy client library") orelse false;

    // const log_analyzer = b.option(bool, "log-analyzer", "prints the analyzer's logs") orelse false;
    // options.addOption(bool, "log_analyzer", log_analyzer);

    // Tracy module or empty one
    const tracy_dep = if (tracy_enabled) b.dependency("tracy", .{ .tracy_no_exit = true }) else undefined;
    const tracy_mod = if (tracy_enabled) tracy_dep.module("tracy") else b.createModule(.{
        .root_source_file = b.path("src/tracy_noop.zig"),
    });

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "rover",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const clap = b.dependency("clap", .{});
    exe.root_module.addImport("clap", clap.module("clap"));
    exe.root_module.addImport("tracy", tracy_mod);
    exe.root_module.addOptions("config", options);

    // For tracy, as it's a cpp project
    if (tracy_enabled) {
        exe.linkLibrary(tracy_dep.artifact("tracy"));
        exe.linkLibCpp();
    }

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // --------
    // For ZLS
    // --------
    const exe_check = b.addExecutable(.{
        .name = "foo",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
    });
    exe_check.root_module.addImport("clap", clap.module("clap"));
    exe_check.root_module.addImport("tracy", tracy_mod);
    exe_check.root_module.addOptions("config", options);

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // -------
    //  Tests
    // -------
    const test_step = b.step("test", "Run unit tests");

    // Tester
    const tester_exe = b.addExecutable(.{
        .name = "rover-tester",
        .root_source_file = b.path("tests/tester.zig"),
        .target = target,
    });

    tester_exe.root_module.addImport("clap", clap.module("clap"));
    b.installArtifact(tester_exe);
    const run_tester = b.addRunArtifact(tester_exe);
    run_tester.step.dependOn(b.getInstallStep());
    test_step.dependOn(&run_tester.step);

    if (b.args) |args|
        run_tester.addArgs(args)
    else
        run_tester.addArg("--stage=all");
}

// fn get_stage_from_file(file_path: []const u8) Stage {
//     return if (std.mem.indexOf(u8, file_path, "parser")) |_|
//         .parser
//     else if (std.mem.indexOf(u8, file_path, "analyzer")) |_|
//         .analyzer
//     else if (std.mem.indexOf(u8, file_path, "compiler")) |_|
//         .compiler
//     else if (std.mem.indexOf(u8, file_path, "vm")) |_|
//         .vm
//     else
//         .all;
// }
