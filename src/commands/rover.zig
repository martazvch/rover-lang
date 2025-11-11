const std = @import("std");
const Allocator = std.mem.Allocator;

const Pipeline = @import("../core/pipeline/Pipeline.zig");
const Vm = @import("../core/runtime/Vm.zig");
const State = @import("../core/pipeline/State.zig");

pub fn run(allocator: Allocator, file_path: []const u8, config: State.Config) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    var state: State = .new(arena_alloc, config);
    state.registerNatives(arena_alloc, @import("../core/builtins/builtins.zig"));
    // state.registerNatives(arena_alloc, @import("../core/builtins/file.zig"));
    // state.registerNatives(arena_alloc, @import("../core/builtins/array.zig"));

    var vm: Vm = undefined;
    vm.init(allocator, &state);
    defer vm.deinit();

    const entry_point = mod: {
        const file_content = std.fs.cwd().readFileAllocOptions(allocator, file_path, 100_000, null, .of(u8), 0) catch |err| {
            // TODO: Rover error
            std.debug.print("Error: {}, unable to open file at: {s}\n", .{ err, file_path });
            std.process.exit(0);
        };
        defer allocator.free(file_content);

        var pipeline: Pipeline = .init(arena_alloc, &vm, &state);

        const entry_point = pipeline.run(file_path, ".", file_content) catch |e| switch (e) {
            error.ExitOnPrint => return,
            else => return e,
        };

        break :mod entry_point;
    };

    try vm.run(entry_point, state.module_interner.compiled.values());
}
