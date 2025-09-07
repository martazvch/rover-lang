const std = @import("std");
const Allocator = std.mem.Allocator;

const AnalyzedModule = @import("frontend/Analyzer.zig").AnalyzedModule;
const CompiledModule = @import("backend/compiler.zig").CompiledModule;
const InternerIndex = @import("Interner.zig").Index;
const oom = @import("utils.zig").oom;

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

// pub fn reserve(self: *Self, name: InternerIndex) usize {
//     self.analyzed.put(self.allocator, name, undefined) catch oom();
//     self.compiled.put(self.allocator, name, undefined) catch oom();
//     return self.analyzed.count();
// }

pub fn add(self: *Self, name: InternerIndex, analyzed: AnalyzedModule, compiled: CompiledModule) void {
    self.analyzed.put(self.allocator, name, analyzed) catch oom();
    self.compiled.put(self.allocator, name, compiled) catch oom();
}

// pub fn setAt(self: *Self, index: usize, analyzed: AnalyzedModule, compiled: CompiledModule) void {
//     self.analyzed.values()[index] = analyzed;
//     self.compiled.values()[index] = compiled;
// }

pub fn getAnalyzed(self: *const Self, name: InternerIndex) ?*AnalyzedModule {
    return self.analyzed.getPtr(name);
}
