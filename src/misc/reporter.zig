const std = @import("std");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const assert = std.debug.assert;
const Writer = std.Io.Writer;
const builtin = @import("builtin");

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

fn boxChar(kind: BoxChar) []const u8 {
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

fn generateMsg(comptime msg: []const u8, comptime clr: Color) []const u8 {
    return color(clr) ++ msg ++ color(.NoColor);
}

const err_msg = generateMsg("Error:", .Red);
const help_msg = generateMsg("help:", .Green);
const warning_msg = generateMsg("Warning:", .Yellow);
const corner_to_hint = boxChar(.BottomLeft) ++ boxChar(.Horitzontal) ** 4;
const corner_to_end = boxChar(.BottomLeft) ++ boxChar(.Horitzontal) ** 2;

/// Reports all the reports of type *Report*
pub fn reportAll(
    Report: type,
    reports: []const GenReport(Report),
    verbose: bool,
    file_name: []const u8,
    source: [:0]const u8,
) !void {
    const prev_cp = if (builtin.os.tag == .windows) cp: {
        const prev = std.os.windows.kernel32.GetConsoleOutputCP();
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
        break :cp prev;
    } else 0;

    defer if (builtin.os.tag == .windows) {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(prev_cp);
    };

    var stderr_buf: [2048]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
    const stderr = &stderr_writer.interface;

    if (verbose) {
        for (reports) |*report| {
            try display(Report, report, stderr, file_name, source);
        }
    } else {
        for (reports) |report| {
            try report.toStr(stderr);
            try stderr.writeAll("\n");
        }
    }

    try stderr.flush();
}

fn display(Report: type, report: *const GenReport(Report), writer: *Writer, file_name: []const u8, source: [:0]const u8) !void {
    // Prints the error part
    //  Error: <err-msg>
    try writer.print("{s} ", .{report.level.get_level_msg()});
    try report.getMsg(writer);
    _ = try writer.write("\n");

    // If there is visual indication on text
    if (report.end > 0) {
        var current: usize = 0;
        var line_start: usize = 0;
        var line_count: usize = 0;
        var previous_line: ?[]const u8 = null;

        // Looking for current line where it occured and buffers the previous one
        // for context
        while (true) : (current += 1) {
            if (current >= source.len) break;

            if (source[current] == '\n') {
                if (current >= report.start) break;

                // line_start > 0 otherwise if first line of file is \n, current - 1 crashes
                if (line_start > 0) {
                    const end = if (source[current - 1] == '\r') current - 1 else current;
                    previous_line = source[line_start..end];
                }

                line_count += 1;
                // Skip the \n
                line_start = current + 1;
            }
        }

        // Line index start to 1
        line_count += 1;

        var buf: [10]u8 = undefined;
        // We consider the maximum line number being 99 999. The extra space
        // is for space between line number and gutter and the one at the beginning
        const buf2: [7]u8 = [_]u8{' '} ** 7;

        // Gets line number digit count
        const written = try std.fmt.bufPrint(&buf, "{}", .{line_count});
        const line_digit_count = written.len;
        const left_padding = buf2[0 .. written.len + 2];

        // Prints file name and location infos
        //  ╭─[file_name.rv:1:5]
        try writer.print(
            "{s}{s}{s}[{s}{s}{s}:{}:{}]\n",
            .{
                left_padding,
                boxChar(.UpperLeft),
                boxChar(.Horitzontal),
                color(.Blue),
                file_name,
                color(.NoColor),
                line_count,
                report.end - line_start + 1,
            },
        );
        // Prints previous line number, separation and line itself
        //  56 | var a = 3
        if (previous_line) |pl| {
            try print_line(writer, line_count - 1, pl, line_digit_count);
        }

        // Prints current line number, separation and line
        //  57 | fn add(a, b c)
        try print_line(writer, line_count, source[line_start..current], line_digit_count);

        // Underlines the problem
        // Takes padding into account + separator + space
        //  <space><space> |
        try writer.print("{s}{s} ", .{ left_padding, boxChar(.Vertical) });

        // We get the length of the error code and the half to underline it
        var space_buf: [1024]u8 = [_]u8{' '} ** 1024;
        const start_space = report.start - line_start;
        const lexeme_len = @max(report.end - report.start, 1);
        const half = @divFloor(lexeme_len, 2);

        // Prints initial space
        _ = try writer.write(space_buf[0..start_space]);

        // We write in yellow
        _ = try writer.write(color(.Yellow));

        // Prints ─┬─
        for (0..lexeme_len) |i| {
            if (i == half) {
                _ = try writer.write(boxChar(.UnderT));
            } else {
                _ = try writer.write(boxChar(.Horitzontal));
            }
        }
        _ = try writer.write("\n");

        // We switch back to no color
        _ = try writer.write(color(.NoColor));

        // Prints to indication (written state is the good one at this stage
        // for the beginning of the sequence to print)
        //  <space><space> | ╰─── <indication txt>
        try writer.print("{s}{s} ", .{ left_padding, boxChar(.Vertical) });
        _ = try writer.write(space_buf[0 .. start_space + half]);

        _ = try writer.write(color(.Yellow));

        try writer.print("{s} ", .{corner_to_hint});
        _ = try report.getHint(writer);
        _ = try writer.write("\n");
        _ = try writer.write(color(.NoColor));

        _ = try writer.write(left_padding);
        try writer.print("{s}\n", .{corner_to_end});
    }

    var buf: [1024]u8 = undefined;
    var fbw = std.Io.Writer.fixed(&buf);

    try report.getHelp(&fbw);

    if (fbw.buffered().len > 0) {
        try writer.print("  {s} {s}\n", .{ help_msg, fbw.buffered() });
    }

    _ = try writer.write("\n");
}

// Limitation of Zig, can only use comptime known strings for formatting...
// TODO: dynamic formatting
fn print_line(writer: *Writer, line_nb: usize, line: []const u8, digit_count: usize) !void {
    try switch (digit_count) {
        1 => writer.print(" {:>1} {s} {s}\n", .{ line_nb, boxChar(.Vertical), line }),
        2 => writer.print(" {:>2} {s} {s}\n", .{ line_nb, boxChar(.Vertical), line }),
        3 => writer.print(" {:>3} {s} {s}\n", .{ line_nb, boxChar(.Vertical), line }),
        4 => writer.print(" {:>4} {s} {s}\n", .{ line_nb, boxChar(.Vertical), line }),
        5 => writer.print(" {:>5} {s} {s}\n", .{ line_nb, boxChar(.Vertical), line }),
        else => unreachable,
    };
}

/// Error report used en each step of the Rover language:
/// lexing, parsing, compiling, executing, ...
/// It has:
///  - report: structure that has the message data
///  - level: warning or error
///  - start: starting byte offset from source of the error
///  - end: ending byte offset from source of the error
pub fn GenReport(comptime T: type) type {
    assert(@typeInfo(T) == .@"union");
    assert(@hasDecl(T, "getMsg"));
    assert(@hasDecl(T, "getHint"));
    assert(@hasDecl(T, "getHelp"));

    return struct {
        report: T,
        level: Level,
        start: usize,
        end: usize,

        pub const Level = enum {
            @"error",
            info,
            warning,

            pub fn get_level_msg(self: Level) []const u8 {
                return switch (self) {
                    .@"error" => err_msg,
                    .info => @panic("not implemented yet"),
                    .warning => warning_msg,
                };
            }
        };

        const Self = @This();

        pub fn init(report: T, level: Level, start: usize, end: usize) Self {
            return .{
                .report = report,
                .level = level,
                .start = start,
                .end = end,
            };
        }

        /// Creates an error associated with the tag
        pub fn err(report: T, start: usize, end: usize) Self {
            return Self.init(report, .@"error", start, end);
        }

        /// Creates warning associated with the tag
        pub fn warn(report: T, start: usize, end: usize) Self {
            return Self.init(report, .warning, start, end);
        }

        pub fn getMsg(self: *const Self, writer: anytype) !void {
            return self.report.getMsg(writer);
        }

        pub fn getHint(self: *const Self, writer: anytype) !void {
            return self.report.getHint(writer);
        }

        pub fn getHelp(self: *const Self, writer: anytype) !void {
            return self.report.getHelp(writer);
        }

        /// Used in test mode when we only want error name and associated data
        pub fn toStr(self: *const Self, writer: anytype) !void {
            const name = @tagName(self.report);
            try writer.writeAll(name);

            inline for (std.meta.fields(T)) |field| {
                if (field.type != void and std.mem.eql(u8, field.name, name)) {
                    const field_info = @field(self.report, field.name);

                    inline for (std.meta.fields(field.type)) |subf| {
                        const subv = @field(field_info, subf.name);

                        switch (@TypeOf(subv)) {
                            usize => try writer.print(", {}", .{subv}),
                            else => try writer.print(", {s}", .{subv}),
                        }
                    }
                }
            }
        }
    };
}
