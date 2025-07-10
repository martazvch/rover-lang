const std = @import("std");
const Allocator = std.mem.Allocator;
const Prompts = std.ArrayListUnmanaged([]const u8);

const Tty = @import("Tty.zig");
const oom = @import("../utils.zig").oom;

tty: Tty,
stdin: std.fs.File.Reader,
stdout: std.fs.File.Writer,
prompts: Prompts = .{},
indent_level: usize = 0,

const Self = @This();
const MAX_IDENT = 64;
const IDENT_SIZE = 4;
const SPACES = " " ** (IDENT_SIZE * MAX_IDENT);
const Error = error{ BadRead, BadWrite, TooManyIndents, Empty, EndOfFile };

pub fn init() Self {
    return .{
        .tty = Tty.init() catch unreachable,
        .stdin = std.io.getStdIn().reader(),
        .stdout = std.io.getStdOut().writer(),
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.tty.deinit();

    for (self.prompts.items) |prompt| {
        allocator.free(prompt);
    }
    self.prompts.deinit(allocator);
}

pub fn logInfos(self: *Self) Error!void {
    _ = self.stdout.write("\t\tRover language REPL\n  Type 'quit' to exit\n\n") catch return error.BadWrite;
}

pub fn getPrompt(self: *Self, allocator: Allocator) Error![:0]const u8 {
    _ = allocator;

    const event = self.tty.getEvent() catch return error.EndOfFile;
    _ = event; // autofix

    // const prompt = self.tty.getPrompt() catch return error.EndOfFile;
    // _ = prompt; // autofix

    return "";

    // var input: std.ArrayListUnmanaged(u8) = .{};
    // errdefer input.deinit(allocator);

    // const prev_level = self.indent_level;
    // while (true) {
    //     const prev_written = input.items.len;
    //     try self.printPs1();
    //     self.stdin.streamUntilDelimiter(input.writer(allocator), '\n', null) catch return error.BadRead;
    //     try self.checkIndent(input.items[prev_written..]);
    //
    //     if (self.indent_level == prev_level) break;
    // }
    //
    // if (input.items.len == 1) {
    //     return error.Empty;
    // } else if (input.items.len >= 4 and std.mem.eql(u8, input.items[0..4], "quit")) {
    //     return error.EndOfFile;
    // }
    //
    // input.append(allocator, '\n') catch oom();
    // input.append(allocator, 0) catch oom();
    // const len = input.items.len - 1;
    // self.prompts.append(allocator, input.toOwnedSlice(allocator) catch oom()) catch oom();
    //
    // return self.prompts.items[self.prompts.items.len - 1][0..len :0];
}

fn printPs1(self: *Self) Error!void {
    _ = self.stdout.write(if (self.indent_level == 0) "> " else "| ") catch return error.BadWrite;
    _ = self.stdout.write(SPACES[0 .. self.indent_level * IDENT_SIZE]) catch return error.BadWrite;
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
