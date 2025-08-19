const std = @import("std");
const Allocator = std.mem.Allocator;

const AnalyzedModule = @import("frontend/Analyzer.zig").AnalyzedModule;
const CompiledModule = @import("backend/compiler.zig").CompiledModule;
const InternerIndex = @import("Interner.zig").Index;
const oom = @import("utils.zig").oom;

const Self = @This();
pub const Module = struct { analyzed: AnalyzedModule, compiled: CompiledModule };

arena: std.heap.ArenaAllocator,
modules: std.AutoHashMapUnmanaged(InternerIndex, Module),

pub fn init(allocator: Allocator) Self {
    return .{ .arena = .init(allocator), .modules = .empty };
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub fn add(self: *Self, name: InternerIndex, module: Module) void {
    self.modules.put(self.arena.allocator(), name, module) catch oom();
}

pub fn get(self: *const Self, name: InternerIndex) ?*Module {
    return self.modules.getPtr(name);
}

pub fn getKind(self: *const Self, name: InternerIndex, comptime kind: std.meta.FieldEnum(Module)) ?*@FieldType(Module, @tagName(kind)) {
    var module = self.get(name) orelse return null;
    return &@field(module, @tagName(kind));
}
