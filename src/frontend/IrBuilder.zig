const std = @import("std");
const Allocator = std.mem.Allocator;
const MultiArrayList = std.MultiArrayList;

const rir = @import("rir.zig");
const Instruction = rir.Instruction;
const oom = @import("../utils.zig").oom;

const Self = @This();
pub const Mode = union(enum) {
    add,
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

/// Adds a new instruction and add it's `start` field and returns its index.
fn addInstr(self: *Self, instr: Instruction) void {
    self.instructions.append(self.allocator, instr) catch oom();
}

/// Sets the instruction at the given idnex
fn setInstr(self: *Self, index: usize, instr: Instruction) void {
    self.instructions.set(index, instr);
}

fn finishInstruction(self: *Self, instr: Instruction, mode: Mode) void {
    switch (mode) {
        .add => self.addInstr(instr),
        .setAt => |idx| self.setInstr(idx, instr),
    }
}

pub fn declareVariable(self: *Self, has_value: bool, cast: bool, index: usize, global: bool, mode: Mode) void {
    const instr: Instruction = .{ .data = .{ .var_decl = .{
        .has_value = has_value,
        .cast = cast,
        .variable = .{ .index = index, .scope = if (global) .global else .local },
    } } };

    self.finishInstruction(instr, mode);
}

pub fn declareFunction(self: *Self, body_len: usize, default_count: usize, return_kind: rir.ReturnKind, mode: Mode) void {
    self.finishInstruction(
        .{ .data = .{ .fn_decl = .{ .body_len = body_len, .default_params = default_count, .return_kind = return_kind } } },
        mode,
    );
}
pub fn identifier(self: *Self, index: usize, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .identifier_id = .{ .index = index, .rc_action = undefined } } }, mode);
}

pub fn block(self: *Self, len: usize, pop_count: u8, is_expr: bool, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .block = .{ .length = len, .pop_count = pop_count, .is_expr = is_expr } } }, mode);
}

pub fn print(self: *Self, mode: Mode) void {
    const instr: Instruction = .{ .data = .print };
    self.finishInstruction(instr, mode);
}

pub fn boolLiteral(self: *Self, value: bool, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .bool = value } }, mode);
}

pub fn intLiteral(self: *Self, value: i64, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .int = value } }, mode);
}

pub fn floatLiteral(self: *Self, value: f64, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .float = value } }, mode);
}

pub fn strLiteral(self: *Self, value: usize, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .string = value } }, mode);
}

pub fn nullLiteral(self: *Self, mode: Mode) void {
    self.finishInstruction(.{ .data = .null }, mode);
}

pub fn castTo(self: *Self, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .cast = .float } }, mode);
}

// TODO: delete and merge this into other Instructions
pub fn name(self: *Self, name_idx: usize, mode: Mode) void {
    self.finishInstruction(.{ .data = .{ .name = name_idx } }, mode);
}
