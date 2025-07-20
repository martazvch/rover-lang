const std = @import("std");
const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;

const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const oom = @import("../utils.zig").oom;

const Self = @This();
pub const Mode = union(enum) {
    add,
    add_no_alloc,
    setAt: usize,
};

allocator: Allocator,
instructions: MultiArrayList(Instruction),

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator, .instructions = .{} };
}

/// Reserves an empty slot and returns its index
pub fn reserveInstr(self: *Self) usize {
    return self.instructions.addOne(self.allocator) catch oom();
}

pub fn ensureUnusedSize(self: *Self, size: usize) void {
    self.instructions.ensureUnusedCapacity(self.allocator, size) catch oom();
}

pub fn emit(self: *Self, instr: Instruction, mode: Mode) void {
    switch (mode) {
        .add => self.addInstr(instr),
        .add_no_alloc => self.addInstrNoAlloc(instr),
        .setAt => |idx| self.setInstr(idx, instr),
    }
}

/// Adds a new instruction and add it's `start` field and returns its index.
fn addInstr(self: *Self, instr: Instruction) void {
    self.instructions.append(self.allocator, instr) catch oom();
}

/// Adds a new instruction and add it's `start` field and returns its index.
fn addInstrNoAlloc(self: *Self, instr: Instruction) void {
    self.instructions.appendAssumeCapacity(instr);
}

/// Sets the instruction at the given idnex
fn setInstr(self: *Self, index: usize, instr: Instruction) void {
    self.instructions.set(index, instr);
}
