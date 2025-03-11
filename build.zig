const std = @import("std");

pub fn build(b: *std.Build) !void {
    const options = b.addOptions();

    const print_instr = b.option(bool, "print-instr", "prints the current instruction") orelse false;
    options.addOption(bool, "print_instr", print_instr);

    const print_stack = b.option(bool, "print-stack", "prints the stack on each instruction") orelse false;
    options.addOption(bool, "print_stack", print_stack);

    const log_gc = b.option(bool, "log-gc", "logs each GC actions (alloc and free)") orelse false;
    options.addOption(bool, "log_gc", log_gc);

    const test_mode = b.option(bool, "test-mode", "Compiles in test mode to enable certain behaviors") orelse false;
    options.addOption(bool, "test_mode", test_mode);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/main.zig"),
    });

    const exe = b.addExecutable(.{
        .name = "rover",
        .root_module = exe_mod,
    });

    const rover_std_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("std/std.zig"),
    });

    const clap = b.dependency("clap", .{});
    exe.root_module.addImport("clap", clap.module("clap"));
    exe.root_module.addImport("rover-std", rover_std_mod);
    exe.root_module.addOptions("config", options);

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
        .root_module = exe_mod,
    });
    exe_check.root_module.addImport("clap", clap.module("clap"));
    exe_check.root_module.addImport("rover-std", rover_std_mod);
    exe_check.root_module.addOptions("config", options);

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // -------
    //  Tests
    // -------
    const test_step = b.step("test", "Run unit tests");

    // Tester
    const tester_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("tests/tester.zig"),
    });
    const tester_exe = b.addExecutable(.{
        .name = "rover-tester",
        .root_module = tester_mod,
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
