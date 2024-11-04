const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;

const BoxChar = enum {
    BottomLeft,
    BottomRight,
    Horitzontal,
    LeftT,
    UnderT,
    UpperLeft,
    UpperRight,
    Vertical,
};

fn box_char(kind: BoxChar) []const u8 {
    return switch (kind) {
        .BottomLeft => "╰",
        .BottomRight => "╯",
        .Horitzontal => "─",
        .LeftT => "├",
        .UnderT => "┬",
        .UpperLeft => "╭",
        .UpperRight => "╮",
        .Vertical => "│",
    };
}

const Color = enum {
    Blue,
    Cyan,
    Green,
    NoColor,
    Red,
    Yellow,
};

fn color(clr: Color) []const u8 {
    return switch (clr) {
        .Blue => "\x1b[34m",
        .Cyan => "\x1b[96m",
        .Green => "\x1b[32m",
        .NoColor => "\x1b[0m",
        .Red => "\x1b[31m",
        .Yellow => "\x1b[33m",
    };
}

const err_msg = color(.Red) ++ "Error:" ++ color(.NoColor);
const warning_msg = color(.Yellow) ++ "Warning:" ++ color(.NoColor);

pub const Report = struct {
    msg: []const u8,
    start: usize,
    len: usize,
    hint: ?[]const u8,
    level: Level,

    pub const Level = enum {
        Error,
        Info,
        Warning,

        pub fn get_msg(self: *const Level) []const u8 {
            return switch (self.*) {
                .Error => err_msg,
                .Info => @panic("not implemented yet"),
                .Warning => warning_msg,
            };
        }
    };

    const Self = @This();

    pub fn new(level: Level, msg: []const u8, start: usize, len: usize, hint: ?[]const u8) Self {
        return .{
            .level = level,
            .msg = msg,
            .start = start,
            .len = len,
            .hint = hint,
        };
    }

    pub fn display(self: *const Self, allocator: Allocator, source: []const u8) !void {
        var current: usize = 0;
        var line_start: usize = 0;
        var line_count: usize = 0;
        var previous_line: ?[]const u8 = null;

        while (true) {
            if (source[current] == '\n') {
                if (current >= self.start) break;

                line_count += 1;
                previous_line = source[line_start..current];
                line_start = current;
            }

            current += 1;
        }

        print("{s} {s}\n", .{ self.level.get_msg(), self.msg });

        if (previous_line) |pl| {
            print(" {} | {s}\n", .{ line_count - 1, pl });
        }

        print("{s} {} | {s}\n", .{ box_char(.UpperLeft), line_count, source[line_start..current] });

        const h_count = self.start - line_start;
        const buf = try allocator.alloc(u8, h_count);
        defer allocator.free(buf);
        // @memset(buf, box_char(.Horitzontal));

        print("{s}{s}\n", .{ box_char(.LeftT), buf });
    }
};

// NOTE:
// This works, using utf-8 representation. Adjust the allocated size with the
// code points number and here we go
// const std = @import("std");
//
// pub fn main() !void {
//     const allocator = std.heap.page_allocator; // or your chosen allocator
//     const line_start = 0; // Example value
//     const self_start = 10; // Example value
//     const h_count = self_start - line_start; // The size of the buffer
//
//     // Allocate the buffer
//     const buf = try allocator.alloc(u8, h_count * 3);
//     defer allocator.free(buf);
//
//     // Define the UTF-8 representation of the Unicode character "─"
//     const char_to_fill: [3]u8 = [_]u8{ 0xE2, 0x94, 0x80 }; // UTF-8 for "─"
//
//     // Fill the buffer with the character
//     for (buf, 0..) |*byte, index| {
//         const char_index = index % 3; // Cycle through the character bytes
//         byte.* = char_to_fill[char_index];
//     }
//
//     // Optional: Print the buffer to verify it contains the desired character
//     const stdout = std.io.getStdOut().writer();
//     try stdout.writeAll(buf[0..h_count*3]); // Write the buffer to stdout
//     try stdout.writeAll("\n"); // Write a newline for clarity
// }
