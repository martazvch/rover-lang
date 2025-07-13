const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const Terminal = @import("terminal/Terminal.zig");
const WinTerm = @import("terminal/WinTerm.zig");
const oom = @import("../utils.zig").oom;

const Self = @This();
const Prompts = std.ArrayListUnmanaged([:0]const u8);
const Error = error{ BadRead, BadWrite, TooManyIndents, Empty, EndOfFile } || std.fs.File.Writer.Error;

const MAX_IDENT = 64;
const INDENT_SIZE = 4;
const SPACES = " " ** (INDENT_SIZE * MAX_IDENT);

const Vec2 = struct {
    x: usize,
    y: usize,

    pub const zero: Vec2 = .{ .x = 0, .y = 0 };
};

terminal: Terminal,
stdin: std.fs.File.Reader,
stdout: std.fs.File.Writer,
prompts: Prompts = .{},
cursor_pos: Vec2 = .zero,
indent_level: usize = 0,

// TODO: forced to keep a reference?
var win_term: @import("terminal/WinTerm.zig") = undefined;

pub fn init() Self {
    var terminal = terminal: {
        if (builtin.os.tag == .windows) {
            win_term = WinTerm.init() catch unreachable;

            break :terminal win_term.terminal();
        } else unreachable;
    };

    terminal.enableRawMode() catch unreachable;

    return .{
        .terminal = terminal,
        .stdin = std.io.getStdIn().reader(),
        .stdout = std.io.getStdOut().writer(),
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.terminal.disableRawMode();

    for (self.prompts.items) |prompt| {
        allocator.free(prompt);
    }
    self.prompts.deinit(allocator);
}

pub fn logInfos(self: *Self) Error!void {
    _ = self.stdout.write("\t\tRover language REPL\n  Type 'quit' or Ctrl+C to exit\n\n") catch return error.BadWrite;
}

pub fn getPrompt(self: *Self, allocator: Allocator) Error![:0]const u8 {
    var input: std.ArrayListUnmanaged(u8) = .{};
    errdefer input.deinit(allocator);

    var line_offset: usize = 0;
    self.cursor_pos.x = 0;

    self.printPs1();

    while (true) {
        const key = self.terminal.getKey() catch |e| switch (e) {
            error.NonAsciiChar => {
                self.stdout.writeAll("Error: accept only ascii characters\n") catch unreachable;
                self.printPs1();
                continue;
            },
            else => unreachable,
        };

        switch (key.value) {
            .up => {
                if (self.cursor_pos.y == 0) continue;
                self.cursor_pos.y -= 1;
                self.restoreHistory(allocator, self.cursor_pos.y, &input, line_offset);
            },
            .down => {
                // If end of history, reset line
                if (self.cursor_pos.y == self.prompts.items.len - 1) {
                    self.cursor_pos.y += 1;
                    self.clearLineMoveStartOfLine();
                    self.printPs1();
                    continue;
                } else if (self.cursor_pos.y == self.prompts.items.len) {
                    continue;
                } else {
                    self.cursor_pos.y += 1;
                    self.restoreHistory(allocator, self.cursor_pos.y, &input, line_offset);
                }
            },
            .left => {
                if (self.cursor_pos.x > 0) {
                    self.moveCursor(.left, 1);
                }
            },
            .right => {
                if (self.cursor_pos.x < input.items.len) {
                    self.moveCursor(.right, 1);
                }
            },
            .back => {
                if (self.cursor_pos.x == 0) continue;

                const line = input.items[line_offset + self.cursor_pos.x - 1 ..];

                std.mem.copyForwards(u8, line[0..], line[1..]);
                _ = input.pop();
                self.moveCursor(.left, 1);
                var len = self.stdout.write(input.items[line_offset + self.cursor_pos.x ..]) catch unreachable;
                // Erase the last character
                len += self.stdout.write(" ") catch unreachable;

                self.cursor_pos.x += len;
                self.moveCursor(.left, len);
            },
            .delete => {},
            .tab => {
                self.stdout.writeAll(SPACES[0..INDENT_SIZE]) catch unreachable;
                input.appendSlice(allocator, SPACES[0..INDENT_SIZE]) catch oom();
                self.cursor_pos.x += INDENT_SIZE;
            },
            .enter => {
                const trimmed = std.mem.trimRight(u8, input.items[line_offset..], " ");

                if (trimmed.len == 0) {
                    self.lineReturnContinue(allocator, &input);
                    self.cursor_pos.x = 0;
                    line_offset = input.items.len;
                    continue;
                }

                if (std.mem.eql(u8, trimmed, "quit")) {
                    return error.EndOfFile;
                }

                const prev_indent = self.indent_level;
                try self.checkIndent(trimmed);

                if (prev_indent < self.indent_level or prev_indent > self.indent_level) {
                    self.moveCursorEndOfLine(input.items.len - line_offset);
                    input.appendSlice(allocator, "\n") catch oom();
                    _ = self.stdout.writeAll("\n") catch unreachable;
                    self.printPs1();
                    line_offset = input.items.len;
                    const indent_len = self.indent(allocator, &input);
                    _ = self.stdout.writeAll(input.items[line_offset..]) catch unreachable;
                    self.cursor_pos.x = indent_len;
                } else {
                    if (self.indent_level == 0) {
                        self.lineReturn(allocator, &input, true);
                        self.prompts.append(allocator, input.toOwnedSliceSentinel(allocator, 0) catch oom()) catch oom();
                        self.cursor_pos.y += 1;
                        return self.prompts.getLast();
                    } else {
                        self.moveCursorEndOfLine(input.items[line_offset..].len);

                        input.appendSlice(allocator, "\n") catch oom();
                        _ = self.stdout.writeAll("\n") catch unreachable;
                        self.printPs1();
                        line_offset = input.items.len;

                        const indent_len = self.indent(allocator, &input);
                        _ = self.stdout.writeAll(input.items[line_offset..]) catch unreachable;
                        self.cursor_pos.x = indent_len;

                        continue;
                    }
                }
            },
            .char => |char| {
                if (char == 'c' and key.ctrl) {
                    return error.EndOfFile;
                }

                input.insert(allocator, line_offset + self.cursor_pos.x, char) catch oom();
                self.writeAndMoveCursor(char);
            },
        }

        if (self.cursor_pos.x != input.items.len - line_offset) {
            // Rewrite tail after cursor
            try self.stdout.writeAll(input.items[self.cursor_pos.x..]);
            // Move cursor back to original position
            const diff = input.items.len - self.cursor_pos.x;
            try self.stdout.print("\x1b[{}D", .{diff});
        }
    }

    unreachable;
}

fn writeAndMoveCursor(self: *Self, char: u8) void {
    self.stdout.writeByte(char) catch unreachable;
    self.cursor_pos.x += 1;
}

fn printPs1(self: *Self) void {
    _ = self.stdout.write(if (self.indent_level == 0) "> " else "| ") catch unreachable;
}

fn indent(self: *Self, allocator: Allocator, line: *std.ArrayListUnmanaged(u8)) usize {
    const indent_len = self.indent_level * INDENT_SIZE;
    const indent_chars = SPACES[0..indent_len];
    line.appendSlice(allocator, indent_chars) catch oom();

    return indent_len;
}

fn moveCursor(self: *Self, dir: enum { left, right }, amount: usize) void {
    if (dir == .left) {
        self.cursor_pos.x -= amount;
        self.stdout.print("\x1b[{}D", .{amount}) catch unreachable;
    } else {
        self.cursor_pos.x += amount;
        self.stdout.print("\x1b[{}C", .{amount}) catch unreachable;
    }
}

fn moveCursorEndOfLine(self: *Self, line_len: usize) void {
    if (self.cursor_pos.x < line_len) {
        self.moveCursor(.right, line_len - self.cursor_pos.x);
    }
}

fn clearLineMoveStartOfLine(self: *Self) void {
    self.stdout.writeAll("\x1b[2K\r") catch unreachable;
}

/// Prints a new line and Ps1
fn lineReturn(self: *Self, allocator: Allocator, line: *std.ArrayListUnmanaged(u8), write: bool) void {
    if (self.cursor_pos.x < line.items.len) {
        self.moveCursor(.right, line.items.len - self.cursor_pos.x);
    }

    self.stdout.writeAll("\n") catch unreachable;

    if (write) {
        line.appendSlice(allocator, "\n") catch oom();
    }
}

fn lineReturnContinue(self: *Self, allocator: Allocator, line: *std.ArrayListUnmanaged(u8)) void {
    if (self.cursor_pos.x < line.items.len) {
        self.moveCursor(.right, line.items.len - self.cursor_pos.x);
    }

    line.appendSlice(allocator, "\n") catch oom();
    _ = self.stdout.writeAll("\n") catch unreachable;
    self.printPs1();
    const index = line.items.len;
    const indent_len = self.indent(allocator, line);
    _ = self.stdout.writeAll(line.items[index..]) catch unreachable;
    self.cursor_pos.x = indent_len;
}

fn checkIndent(self: *Self, input: []const u8) Error!void {
    for (input) |c| {
        if (c == '{')
            self.indent_level += 1
        else if (c == '}')
            // TODO: protect?
            self.indent_level -= 1;
    }

    if (self.indent_level > MAX_IDENT) return error.TooManyIndents;
}

fn restoreHistory(self: *Self, allocator: Allocator, index: usize, line: *std.ArrayListUnmanaged(u8), offset: usize) void {
    // Switching while having edited current line
    if (line.items.len != offset) {
        line.shrinkRetainingCapacity(offset);
        self.cursor_pos.x = 0;
        // Delete all line and move cursor back to beginning of line
        self.clearLineMoveStartOfLine();
        self.printPs1();
    }

    const prev = self.prompts.items[index];
    const prev_no_return = prev[0 .. prev.len - 1];
    line.appendSlice(allocator, prev_no_return) catch oom();
    self.stdout.writeAll(prev_no_return) catch unreachable;
    self.cursor_pos.x = prev_no_return.len;
}
