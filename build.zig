const std = @import("std");

pub fn build(b: *std.Build) !void {
    const options = b.addOptions();

    const print_instr = b.option(bool, "print-instr", "prints the current instruction") orelse false;
    options.addOption(bool, "print_instr", print_instr);

    const print_stack = b.option(bool, "print-stack", "prints the stack on each instruction") orelse false;
    options.addOption(bool, "print_stack", print_stack);

    const log_gc = b.option(bool, "log-gc", "logs each GC actions (alloc and free)") orelse false;
    options.addOption(bool, "log_gc", log_gc);

    const stress_gc = b.option(bool, "stress-gc", "logs each GC actions (alloc, free, mark, sweep, ...)") orelse false;
    options.addOption(bool, "stress_gc", stress_gc);

    const test_mode = b.option(bool, "test-mode", "Compiles in test mode to enable certain behaviors") orelse false;
    options.addOption(bool, "test_mode", test_mode);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const rover_mod = b.addModule("rover", .{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/main.zig"),
    });

    const exe = b.addExecutable(.{
        .name = "rover",
        .root_module = rover_mod,
    });

    const misc_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/misc/misc.zig"),
    });

    const clarg = b.dependency("clarg", .{
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("clarg", clarg.module("clarg"));
    exe.root_module.addImport("misc", misc_mod);
    exe.root_module.addOptions("options", options);

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
        .root_module = rover_mod,
    });
    exe_check.root_module.addImport("clarg", clarg.module("clarg"));
    exe_check.root_module.addOptions("options", options);

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // -------
    //  Tests
    // -------
    const test_step = b.step("test", "Run unit tests");

    // Unit tests
    const exe_tests = b.addTest(.{ .root_module = exe.root_module });
    const run_exe_tests = b.addRunArtifact(exe_tests);
    test_step.dependOn(&run_exe_tests.step);

    // Unit tests on misc module
    const misc_tests = b.addTest(.{ .root_module = misc_mod });
    const run_misc_tests = b.addRunArtifact(misc_tests);
    test_step.dependOn(&run_misc_tests.step);

    // Custom unit tests runner for language
    const tester_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("tests/tester.zig"),
    });
    const tester_exe = b.addExecutable(.{
        .name = "rover-tester",
        .root_module = tester_mod,
    });

    tester_exe.root_module.addImport("clarg", clarg.module("clarg"));
    const install_tester = b.addInstallArtifact(tester_exe, .{});
    const run_tester = b.addRunArtifact(tester_exe);
    run_tester.step.dependOn(&install_tester.step);
    run_tester.step.dependOn(b.getInstallStep());

    test_step.dependOn(&run_tester.step);

    if (b.args) |args|
        run_tester.addArgs(args)
    else
        run_tester.addArg("--stage=all");
}
