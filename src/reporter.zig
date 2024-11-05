const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Writer = std.fs.File.Writer;
const WriteError = std.posix.WriteError;

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

pub const Reporter = struct {
    source: []const u8,
    writer: WriterType,

    pub const WriterType = union(enum) {
        StdOut: std.fs.File.Writer,
        Custom: WindowsReporter,

        pub fn write(self: *const WriterType, bytes: []const u8) !usize {
            return switch (self.*) {
                inline else => |writer| writer.write(bytes),
            };
        }
    };

    pub fn init(source: []const u8) Reporter {
        var writer: WriterType = undefined;

        if (builtin.os.tag == .windows) {
            writer = .{ .Custom = WindowsReporter.init() };
        } else {
            writer = .{ .StdOut = std.io.getStdOut().writer() };
        }

        return .{ .writer = writer, .source = source };
    }

    pub fn report_all(self: *const Reporter, reports: []const Report) !void {
        for (reports) |report| {
            try report.display(self.source, "stdin", &self.writer);
        }
    }
};

const WindowsReporter = struct {
    prev_cp: c_uint,

    const Self = @This();

    pub fn init() Self {
        return .{ .prev_cp = std.os.windows.kernel32.GetConsoleOutputCP() };
    }

    pub fn write(self: Self, bytes: []const u8) !usize {
        const stdout = std.io.getStdOut().writer();

        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
        const count = try stdout.write(bytes);
        _ = std.os.windows.kernel32.SetConsoleOutputCP(self.prev_cp);
        return count;
    }
};

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

    pub fn display(
        self: *const Self,
        source: []const u8,
        file_name: []const u8,
        writer: *const Reporter.WriterType,
    ) !void {
        var current: usize = 0;
        var line_start: usize = 0;
        var line_count: usize = 0;
        var previous_line: ?[]const u8 = null;

        // Looking for current line where it occured and buffers the previous one
        // for context
        while (true) {
            if (source[current] == '\n') {
                if (current >= self.start) break;

                line_count += 1;
                previous_line = source[line_start..current];
                line_start = current;
            }

            current += 1;
        }

        var buf: [1024]u8 = undefined;
        // We consider the maximum line number being 99 999. The extra space
        // is for space between line number and gutter
        const buf2: [6]u8 = [_]u8{' '} ** 6;

        // Gets line number digit count
        var written = try std.fmt.bufPrint(&buf, "{}", .{line_count});
        const line_digit_count = written.len;
        const left_padding = buf2[0 .. written.len + 1];

        // Prints the error part
        //  Error: <err-msg>
        written = try std.fmt.bufPrint(&buf, "{s} {s}\n", .{ self.level.get_msg(), self.msg });
        _ = try writer.write(written);

        // Prints file name and location infos
        //  ╭─[file_name.rz:1:5]
        written = try std.fmt.bufPrint(
            buf[0..],
            "{s}{s}{s}[{s}.rz:{}:{}]\n",
            .{ left_padding, box_char(.UpperLeft), box_char(.Horitzontal), file_name, line_count, line_start },
        );
        _ = try writer.write(written);

        // Prints previous line number, separation and line itself
        //  56 | var a = 3
        if (previous_line) |pl| {
            try Report.print_line(line_count - 1, pl, line_digit_count, writer);
        }

        // Prints current line number, separation and line
        //  57 | fn add(a, b c)
        try Report.print_line(line_count, source[line_start..current], line_digit_count, writer);

        // Underlines the problem
        // Takes padding into account + separator + space
        written = try std.fmt.bufPrint(buf[0..], "{s}{s} ", .{ left_padding, box_char(.Vertical) });
        _ = try writer.write(written);

        // We get the length of the error code and the half to underline it
        const h_count = @max(self.start - line_start, 1);
        const half = @divFloor(h_count, 2);

        for (0..h_count) |i| {
            if (i == half) {
                _ = try writer.write(box_char(.UnderT));
            } else {
                _ = try writer.write(box_char(.Horitzontal));
            }
        }
        _ = try writer.write("\n");
    }

    // Limitation of Zig, can only use comptime known strings for formatting...
    fn print_line(line_nb: usize, line: []const u8, digit_count: usize, writer: *const Reporter.WriterType) !void {
        var buf: [1024]u8 = undefined;

        const written = switch (digit_count) {
            1 => try std.fmt.bufPrint(&buf, "{:>1} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            2 => try std.fmt.bufPrint(&buf, "{:>2} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            3 => try std.fmt.bufPrint(&buf, "{:>3} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            4 => try std.fmt.bufPrint(&buf, "{:>4} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            5 => try std.fmt.bufPrint(&buf, "{:>5} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            else => unreachable,
        };

        _ = try writer.write(written);
    }
};
