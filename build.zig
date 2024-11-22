const std = @import("std");

// pub const BuildOptions = struct {
//     print_instr: bool = false,
//     print_stack: bool = false,
//     print_code: bool = false,
//
//     fn from_builder(b: *std.Build) BuildOptions {
//         const print_instr = b.option(bool, "print_instr", "prints the current instruction") orelse false;
//         const print_stack = b.option(bool, "print_stack", "prints the stack on each instruction") orelse false;
//
//         return .{ .print_instr = print_instr, .print_stack = print_stack };
//     }
// };

pub fn build(b: *std.Build) void {
    const options = b.addOptions();
    // const opts = BuildOptions.from_builder(b);

    const print_instr = b.option(bool, "print-instr", "prints the current instruction") orelse false;
    options.addOption(bool, "print_instr", print_instr);

    const print_stack = b.option(bool, "print-stack", "prints the stack on each instruction") orelse false;
    options.addOption(bool, "print_stack", print_stack);

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
        .target = b.host,
    });
    exe_check.root_module.addImport("clap", clap.module("clap"));
    exe_check.root_module.addOptions("config", options);

    // These two lines you might want to copy
    // (make sure to rename 'exe_check')
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

    // Tests
    const exe_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_tests = b.addRunArtifact(exe_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    //     // the `zig build --help` menu, providing a way for the user to request
    //         // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_tests.step);
}
