const std = @import("std");
const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;

const Instruction = @import("rir.zig").Instruction;
const oom = @import("../utils.zig").oom;

const Self = @This();
pub const Mode = union(enum) {
    add,
    add_no_alloc,
    set_at: usize,
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
        .set_at => |idx| self.setInstr(idx, instr),
    }
}

pub fn count(self: *const Self) usize {
    return self.instructions.len;
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

/// Converts instructions offsets to line numbers
pub fn computeLineFromOffsets(self: *Self, source: [:0]const u8) []const usize {
    const offsets = self.instructions.items(.offset);
    var list: std.ArrayListUnmanaged(usize) = .{};
    list.ensureUnusedCapacity(self.allocator, offsets.len) catch oom();

    var line: usize = 1;
    var offset_index: usize = 0;

    for (source, 0..) |c, i| {
        if (c == '\n') {
            for (offsets[offset_index..offsets.len]) |o| {
                if (o <= i) {
                    offset_index += 1;
                    list.appendAssumeCapacity(line);
                }
            }

            line += 1;

            if (offset_index == offsets.len) break;
        }
    }

    return list.toOwnedSlice(self.allocator) catch oom();
}
