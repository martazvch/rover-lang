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

    const rover_mod = b.addModule("rover", .{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("src/main.zig"),
    });

    const exe = b.addExecutable(.{
        .name = "rover",
        .root_module = rover_mod,
    });

    const rover_std_mod = b.createModule(.{
        .optimize = optimize,
        .target = target,
        .root_source_file = b.path("std/std.zig"),
    });

    const clap = b.dependency("clap", .{});
    exe.root_module.addImport("clap", clap.module("clap"));
    exe.root_module.addImport("rover-std", rover_std_mod);
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
    exe_check.root_module.addImport("clap", clap.module("clap"));
    exe_check.root_module.addImport("rover-std", rover_std_mod);
    exe_check.root_module.addOptions("options", options);

    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // -------
    //  Tests
    // -------

    const test_stepv2 = b.step("test", "Run new tests");

    const testerv2_mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("tests/tester.zig"),
    });
    const testerv2_exe = b.addExecutable(.{
        .name = "rover-tester",
        .root_module = testerv2_mod,
    });

    testerv2_exe.root_module.addImport("clap", clap.module("clap"));
    const install_testerv2 = b.addInstallArtifact(testerv2_exe, .{});
    const run_testerv2 = b.addRunArtifact(testerv2_exe);
    run_testerv2.step.dependOn(&install_testerv2.step);
    run_testerv2.step.dependOn(b.getInstallStep());
    test_stepv2.dependOn(&run_testerv2.step);

    if (b.args) |args|
        run_testerv2.addArgs(args)
    else
        run_testerv2.addArg("--stage=all");
}
