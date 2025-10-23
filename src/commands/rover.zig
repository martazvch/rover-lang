const std = @import("std");
const Allocator = std.mem.Allocator;

const Pipeline = @import("../core/pipeline/Pipeline.zig");
const Vm = @import("../core/runtime/Vm.zig");
const State = @import("../core/pipeline/State.zig");

const file = @import("misc").file;

pub fn run(allocator: Allocator, file_path: []const u8, config: State.Config) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_alloc = arena.allocator();
    defer arena.deinit();

    var state: State = .new(arena_alloc, config);
    state.registerNatives(arena_alloc, @import("../core/builtins/builtins.zig"));

    var vm: Vm = undefined;
    vm.init(allocator, &state);
    defer vm.deinit();

    const module = mod: {
        const file_content = try file.read(allocator, file_path);
        defer allocator.free(file_content);

        var pipeline: Pipeline = .init(arena_alloc, &vm, &state);

        const module = pipeline.run(file_path, ".", file_content) catch |e| switch (e) {
            error.ExitOnPrint => return,
            else => return e,
        };

        break :mod module;
    };

    try vm.run(module, state.module_interner.compiled.values());
}
