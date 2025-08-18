const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const Terminal = @import("terminal/Terminal.zig");
const WinTerm = @import("terminal/WinTerm.zig");
const Pipeline = @import("../Pipeline.zig");
const Vm = @import("Vm.zig");
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
prompts: Prompts,
cursor_pos: Vec2,
indent_level: usize,
allocator: Allocator,
pipeline: Pipeline,
vm: Vm,

pub fn init(self: *Self, allocator: Allocator, config: Vm.Config) void {
    var terminal = terminal: {
        if (builtin.os.tag == .windows) {
            const term = WinTerm.init() catch unreachable;

            break :terminal term.terminal();
        } else unreachable;
    };
    terminal.enableRawMode() catch unreachable;

    self.terminal = terminal;
    self.stdin = std.io.getStdIn().reader();
    self.stdout = std.io.getStdOut().writer();
    self.prompts = .{};
    self.cursor_pos = .zero;
    self.indent_level = 0;
    self.allocator = allocator;
    self.pipeline = undefined;
    self.pipeline.init(&self.vm, config);
    self.vm = undefined;
    self.vm.init(allocator, config);

    self.logInfos();
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

pub fn run(self: *Self) !void {
    while (true) {
        const prompt = self.getPrompt(self.allocator) catch |e| switch (e) {
            error.Empty => continue,
            error.EndOfFile => break,
            else => {
                std.debug.print("REPL error: {s}\n", .{@errorName(e)});
                return;
            },
        };

        const module = self.pipeline.run("stdin", prompt) catch |e| switch (e) {
            error.ExitOnPrint => return,
            else => return e,
        };
        try self.vm.run(module);
    }
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
                line_offset = self.restoreHistory(allocator, self.cursor_pos.y, &input, line_offset);
            },
            .down => {
                // If end of history, reset line
                if (self.cursor_pos.y == self.prompts.items.len - 1) {
                    self.cursor_pos.y += 1;
                    self.resetLine(&input, 0);
                    line_offset = 0;
                    continue;
                } else if (self.cursor_pos.y == self.prompts.items.len) {
                    continue;
                } else {
                    self.cursor_pos.y += 1;
                    line_offset = self.restoreHistory(allocator, self.cursor_pos.y, &input, line_offset);
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
                    try self.stdout.writeAll("\n");

                    if (self.indent_level > 0) {
                        input.appendSlice(allocator, "\n") catch oom();
                    }

                    self.printPs1();
                    line_offset = input.items.len;
                    self.cursor_pos.x = self.indent(allocator, &input);
                    self.stdout.writeAll(input.items[line_offset..]) catch unreachable;
                    continue;
                }

                if (std.mem.eql(u8, trimmed, "quit")) {
                    return error.EndOfFile;
                }

                try self.checkIndent(input.items);

                if (self.indent_level == 0) {
                    self.lineReturn(allocator, &input, true);
                    self.prompts.append(allocator, input.toOwnedSliceSentinel(allocator, 0) catch oom()) catch oom();
                    self.cursor_pos.y += 1;
                    return self.prompts.getLast();
                } else {
                    line_offset = self.lineReturnContinue(allocator, &input);
                    continue;
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
            try self.stdout.writeAll(input.items[line_offset + self.cursor_pos.x ..]);
            // Move cursor back to original position
            const diff = input.items.len - line_offset - self.cursor_pos.x;
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

fn lineReturn(self: *Self, allocator: Allocator, line: *std.ArrayListUnmanaged(u8), write: bool) void {
    self.moveCursorEndOfLine(line.items.len);
    self.stdout.writeAll("\n") catch unreachable;

    if (write) {
        line.appendSlice(allocator, "\n") catch oom();
    }
}

fn lineReturnContinue(self: *Self, allocator: Allocator, line: *std.ArrayListUnmanaged(u8)) usize {
    self.moveCursorEndOfLine(line.items.len);
    line.appendSlice(allocator, "\n") catch oom();
    _ = self.stdout.writeAll("\n") catch unreachable;
    self.printPs1();

    const line_offset = line.items.len;
    const indent_len = self.indent(allocator, line);
    _ = self.stdout.writeAll(line.items[line_offset..]) catch unreachable;
    self.cursor_pos.x = indent_len;

    return line_offset;
}

fn checkIndent(self: *Self, input: []const u8) Error!void {
    self.indent_level = 0;

    for (input) |c| {
        if (c == '{')
            self.indent_level += 1
        else if (c == '}')
            // TODO: protect?
            self.indent_level -= 1;
    }

    if (self.indent_level > MAX_IDENT) return error.TooManyIndents;
}

fn restoreHistory(self: *Self, allocator: Allocator, index: usize, line: *std.ArrayListUnmanaged(u8), offset: usize) usize {
    self.resetLine(line, offset);
    const prev = self.prompts.items[index];
    const prev_no_return = prev[0 .. prev.len - 1];
    line.appendSlice(allocator, prev_no_return) catch oom();
    self.stdout.writeAll(prev_no_return) catch unreachable;
    const start_of_last_line = std.mem.lastIndexOf(u8, prev_no_return, "\n") orelse 0;
    self.cursor_pos.x = prev_no_return.len - start_of_last_line;

    return start_of_last_line;
}

/// Resets the line by erasing content from console and line buffer and prints Ps1 back
fn resetLine(self: *Self, line: *std.ArrayListUnmanaged(u8), offset: usize) void {
    if (line.items.len != offset) {
        const line_count = std.mem.count(u8, line.items, "\n");

        self.clearLineMoveStartOfLine();
        for (0..line_count) |_| {
            // Move up
            self.stdout.writeAll("\x1b[A") catch unreachable;
            self.clearLineMoveStartOfLine();
        }

        self.printPs1();

        line.shrinkRetainingCapacity(offset);
        self.cursor_pos.x = 0;
    }
}
