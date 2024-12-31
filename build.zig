const std = @import("std");

pub fn build(b: *std.Build) void {
    const options = b.addOptions();

    const print_instr = b.option(bool, "print-instr", "prints the current instruction") orelse false;
    options.addOption(bool, "print_instr", print_instr);

    const print_stack = b.option(bool, "print-stack", "prints the stack on each instruction") orelse false;
    options.addOption(bool, "print_stack", print_stack);

    const log_gc = b.option(bool, "log-gc", "logs each GC actions (alloc and free)") orelse false;
    options.addOption(bool, "log_gc", log_gc);

    // const log_analyzer = b.option(bool, "log-analyzer", "prints the analyzer's logs") orelse false;
    // options.addOption(bool, "log_analyzer", log_analyzer);

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
    exe.root_module.addOptions("config", options);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // For ZLS
    const exe_check = b.addExecutable(.{
        .name = "foo",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
    });
    exe_check.root_module.addImport("clap", clap.module("clap"));
    exe_check.root_module.addOptions("config", options);

    // These two lines you might want to copy
    // (make sure to rename 'exe_check')
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // -------
    //  Tests
    // -------
    // All unit tests within source code
    const tests_exe = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_tests = b.addRunArtifact(tests_exe);
    tests_exe.root_module.addOptions("config", options);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_tests.step);

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
