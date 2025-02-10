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
        .name = "rover-lang",
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

    // Options
    const test_options = b.addOptions();

    var stage = b.option(Stage, "stage", "pipeline stage to test, defaults to all") orelse .all;

    const file_path = b.option([]const u8, "file", "specific file to test, defaults to all") orelse "";
    test_options.addOption([]const u8, "file", file_path);

    if (!std.mem.eql(u8, file_path, "")) {
        stage = get_stage_from_file(file_path);
    }

    test_options.addOption(Stage, "stage", stage);

    // All unit tests within source code
    const tests_exe = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_tests = b.addRunArtifact(tests_exe);
    tests_exe.root_module.addOptions("test_config", test_options);

    tests_exe.root_module.addOptions("config", options);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_tests.step);

    // Enable Vm tests
    if (stage == .vm or stage == .all) {
        // Vm tests use the compiled Vm to run, with a special main
        const runtime_tests_exe = b.addTest(.{
            .root_source_file = b.path("tests/vm/runtime_tester.zig"),
            .target = target,
            .optimize = optimize,
        });

        const run_runtime_tests_exe = b.addRunArtifact(runtime_tests_exe);

        test_step.dependOn(&run_runtime_tests_exe.step);

        // Compiles the exe for the runtime tests
        test_step.dependOn(b.getInstallStep());
    }
}

fn get_stage_from_file(file_path: []const u8) Stage {
    return if (std.mem.indexOf(u8, file_path, "parser")) |_|
        .parser
    else if (std.mem.indexOf(u8, file_path, "analyzer")) |_|
        .analyzer
    else if (std.mem.indexOf(u8, file_path, "compiler")) |_|
        .compiler
    else if (std.mem.indexOf(u8, file_path, "vm")) |_|
        .vm
    else
        .all;
}
