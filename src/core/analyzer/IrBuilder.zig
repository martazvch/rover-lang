const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const MultiArrayList = std.MultiArrayList;

const ir = @import("ir.zig");
const Instruction = ir.Instruction;
const oom = @import("misc").oom;

const Self = @This();
pub const Mode = union(enum) {
    add,
    add_no_alloc,
    set_at: usize,
};

allocator: Allocator,
instructions: MultiArrayList(Instruction),
roots: ArrayList(ir.Index),

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator, .instructions = .empty, .roots = .empty };
}

pub fn count(self: *const Self) usize {
    return self.instructions.len;
}

pub fn addInstr(self: *Self, instr_data: Instruction.Data, offset: usize) usize {
    self.instructions.append(self.allocator, .{ .data = instr_data, .offset = offset }) catch oom();
    return self.instructions.len - 1;
}

pub fn addRootInstr(self: *Self, index: ir.Index) void {
    self.roots.append(self.allocator, index) catch oom();
}

pub fn wrapPreviousInstr(self: *Self, comptime instr: std.meta.FieldEnum(Instruction.Data)) usize {
    return self.wrapInstr(instr, self.instructions.len - 1);
}

pub fn wrapInstr(self: *Self, comptime instr: std.meta.FieldEnum(Instruction.Data), index: usize) usize {
    if (@FieldType(Instruction.Data, @tagName(instr)) != ir.Index) {
        @compileError("Can only wrap instructions that have a single instruction as a payload");
    }

    const prev_offset = self.instructions.items(.offset)[index];

    const wrap = @unionInit(Instruction.Data, @tagName(instr), index);
    self.instructions.append(self.allocator, .{ .data = wrap, .offset = prev_offset }) catch oom();

    return self.instructions.len - 1;
}

pub fn wrapInstrInplace(self: *Self, comptime instr: std.meta.FieldEnum(Instruction.Data), index: usize) void {
    if (@FieldType(Instruction.Data, @tagName(instr)) != ir.Index) {
        @compileError("Can only wrap instructions that have a single instruction as a payload");
    }

    const prev_offset = self.instructions.items(.offset)[index];

    const wrap = @unionInit(Instruction.Data, @tagName(instr), index);
    self.instructions.set(index, .{ .data = wrap, .offset = prev_offset });
}

/// Converts instructions offsets to line numbers
pub fn computeLineFromOffsets(self: *Self, source: [:0]const u8) []const usize {
    const offsets = self.instructions.items(.offset);
    var list: std.ArrayList(usize) = .empty;
    list.ensureUnusedCapacity(self.allocator, offsets.len) catch oom();

    var line: usize = 1;
    var offset_index: usize = 0;

    for (source, 0..) |c, i| {
        if (c == '\n') {
            var prev: usize = 0;

            for (offsets[offset_index..]) |offset| {
                // If offset is lower than previous, it means it has been added manually
                // after some instructions. We append 0 so when it's rendered in debug mode,
                // as 0 will be lower than previous line it will just render '|'
                if (prev != 0 and offset < prev) {
                    list.appendAssumeCapacity(0);
                } else if (offset <= i) {
                    list.appendAssumeCapacity(line);
                } else {
                    break;
                }

                prev = offset;
                offset_index += 1;
            }

            line += 1;

            if (offset_index == offsets.len) break;
        }
    }

    return list.toOwnedSlice(self.allocator) catch oom();
}
