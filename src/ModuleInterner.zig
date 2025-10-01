const std = @import("std");
const Allocator = std.mem.Allocator;

const AnalyzedModule = @import("core/analyzer/Analyzer.zig").AnalyzedModule;
const CompiledModule = @import("core/compiler/compiler.zig").CompiledModule;
const InternerIndex = @import("misc").Interner.Index;
const oom = @import("misc").oom;

const Self = @This();

allocator: Allocator,
analyzed: std.AutoArrayHashMapUnmanaged(InternerIndex, AnalyzedModule),
compiled: std.AutoArrayHashMapUnmanaged(InternerIndex, CompiledModule),

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator, .analyzed = .empty, .compiled = .empty };
}

pub fn deinit(self: *Self) void {
    self.analyzed.deinit(self.allocator);
    for (self.compiled.values()) |*mod| {
        mod.deinit(self.allocator);
    }
    self.compiled.deinit(self.allocator);
}

pub fn add(self: *Self, name: InternerIndex, analyzed: AnalyzedModule, compiled: CompiledModule) void {
    self.analyzed.put(self.allocator, name, analyzed) catch oom();
    self.compiled.put(self.allocator, name, compiled) catch oom();
}

pub fn getAnalyzed(self: *const Self, name: InternerIndex) ?*AnalyzedModule {
    return self.analyzed.getPtr(name);
}
