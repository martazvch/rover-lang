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
const IDENT_SIZE = 4;
const SPACES = " " ** (IDENT_SIZE * MAX_IDENT);

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
    _ = self.stdout.write("\t\tRover language REPL\n  Type 'quit' to exit\n\n") catch return error.BadWrite;
}

pub fn getPrompt(self: *Self, allocator: Allocator) Error![:0]const u8 {
    self.cursor_pos.x = 0;

    var input: std.ArrayListUnmanaged(u8) = .{};
    errdefer input.deinit(allocator);

    self.printPs1();

    while (true) {
        const key = self.terminal.getKey() catch unreachable;

        switch (key.value) {
            .up => {},
            .down => {},
            .left => {
                if (self.cursor_pos.x > 0) {
                    self.cursor_pos.x -= 1;
                    self.moveCursor(.left, 1);
                }
            },
            .right => {
                if (self.cursor_pos.x < input.items.len) {
                    self.cursor_pos.x += 1;
                    self.moveCursor(.right, 1);
                }
            },
            .enter => {
                const trimmed = std.mem.trimRight(u8, input.items, " ");

                if (trimmed.len == 0) {
                    self.lineReturn(allocator, &input, false, true);
                    continue;
                }

                if (std.mem.eql(u8, trimmed, "quit")) {
                    return error.EndOfFile;
                }

                switch (trimmed[trimmed.len - 1]) {
                    '{' => {
                        self.indent_level += 1;
                        self.lineReturn(allocator, &input, true, true);
                    },
                    '}' => {
                        self.indent_level -= 1;
                        self.lineReturn(allocator, &input, true, true);
                    },
                    else => {
                        self.lineReturn(allocator, &input, true, false);
                        self.prompts.append(allocator, input.toOwnedSliceSentinel(allocator, 0) catch oom()) catch oom();
                        return self.prompts.getLast();
                    },
                }
            },
            .chars => |chars| {
                input.insertSlice(allocator, self.cursor_pos.x, chars) catch oom();
                self.cursor_pos.x += chars.len;
                try self.stdout.writeAll(chars);
            },
        }

        if (self.cursor_pos.x != input.items.len) {
            // Rewrite tail after cursor
            try self.stdout.writeAll(input.items[self.cursor_pos.x..]);
            // Move cursor back to original position
            const diff = input.items.len - self.cursor_pos.x;
            try self.stdout.print("\x1b[{}D", .{diff});
        }
    }

    unreachable;
}

fn printPs1(self: *Self) void {
    _ = self.stdout.write(if (self.indent_level == 0) "> " else "| ") catch unreachable;
    _ = self.stdout.write(SPACES[0 .. self.indent_level * IDENT_SIZE]) catch unreachable;
}

fn moveCursor(self: *Self, dir: enum { left, right }, amount: usize) void {
    if (dir == .left) {
        self.stdout.print("\x1b[{}D", .{amount}) catch unreachable;
    } else {
        self.stdout.print("\x1b[{}C", .{amount}) catch unreachable;
    }
}

/// Prints a new line and Ps1
fn lineReturn(self: *Self, allocator: Allocator, line: *std.ArrayListUnmanaged(u8), write: bool, print_ps1: bool) void {
    if (self.cursor_pos.x < line.items.len) {
        self.moveCursor(.right, line.items.len - self.cursor_pos.x);
    }

    self.stdout.writeAll("\n") catch unreachable;

    if (write) line.appendSlice(allocator, "\n") catch oom();
    if (print_ps1) self.printPs1();
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
