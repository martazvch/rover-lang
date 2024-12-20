const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const assert = std.debug.assert;
const Writer = std.fs.File.Writer;
const Span = @import("frontend/ast.zig").Span;
const UnsafeIterStr = @import("unsafe_iter.zig").UnsafeIterStr;

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

fn generate_msg(comptime msg: []const u8, comptime clr: Color) []const u8 {
    return color(clr) ++ msg ++ color(.NoColor);
}

const err_msg = generate_msg("Error:", .Red);
const help_msg = generate_msg("help:", .Green);
const warning_msg = generate_msg("Warning:", .Yellow);
const corner_to_hint = box_char(.BottomLeft) ++ box_char(.Horitzontal) ** 4;
const corner_to_end = box_char(.BottomLeft) ++ box_char(.Horitzontal) ** 2;

/// Generic reporter type. The *Item* is the type of object to refer to get extra data
/// and *Reporter* is the enum that holds the messages. *Report* must have methods:
/// - get_msg
/// - get_hint
/// - get_help
pub fn GenReporter(comptime Report: type) type {
    return struct {
        source: [:0]const u8,
        writer: WriterType,
        // extra: UnsafeIterStr,

        const Self = @This();

        const WindowsReporter = struct {
            prev_cp: c_uint,
            writer: std.fs.File.Writer,

            const InnerSelf = @This();

            pub fn init(writer: std.fs.File.Writer) InnerSelf {
                return .{ .writer = writer, .prev_cp = std.os.windows.kernel32.GetConsoleOutputCP() };
            }

            pub fn write(self: InnerSelf, bytes: []const u8) !usize {
                _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
                const count = try self.writer.write(bytes);
                _ = std.os.windows.kernel32.SetConsoleOutputCP(self.prev_cp);
                return count;
            }

            pub fn print(self: InnerSelf, comptime format: []const u8, args: anytype) !void {
                _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
                const count = try self.writer.print(format, args);
                _ = std.os.windows.kernel32.SetConsoleOutputCP(self.prev_cp);
                return count;
            }
        };

        const WriterType = union(enum) {
            StdOut: std.fs.File.Writer,
            Custom: WindowsReporter,

            pub fn write(self: *const WriterType, bytes: []const u8) !usize {
                return switch (self.*) {
                    inline else => |writer| writer.write(bytes),
                };
            }

            pub fn print(self: *const WriterType, comptime format: []const u8, args: anytype) !void {
                return switch (self.*) {
                    inline else => |writer| writer.print(format, args),
                };
            }
        };

        // pub fn init(source: [:0]const u8, extra: []const []const u8) Self {
        pub fn init(source: [:0]const u8) Self {
            var writer: WriterType = undefined;
            const stdout = std.io.getStdOut().writer();

            if (builtin.os.tag == .windows) {
                writer = .{ .Custom = WindowsReporter.init(stdout) };
            } else {
                writer = .{ .StdOut = stdout };
            }

            // return .{ .writer = writer, .source = source, .extra = UnsafeIterStr.init(extra) };
            return .{ .writer = writer, .source = source };
        }

        /// Reports all the reports of type *Report*
        pub fn report_all(self: *Self, file_name: []const u8, reports: []const GenReport(Report)) !void {
            const sep = if (builtin.os.tag == .windows) '\\' else '/';
            var iter = std.mem.splitBackwardsScalar(u8, file_name, sep);
            const name = iter.first();

            for (reports) |*report| {
                try self.display(report, name);
            }
        }

        fn display(self: *Self, report: *const GenReport(Report), file_name: []const u8) !void {
            var current: usize = 0;
            var line_start: usize = 0;
            var line_count: usize = 0;
            var previous_line: ?[]const u8 = null;

            // Looking for current line where it occured and buffers the previous one
            // for context
            while (true) {
                if (self.source[current] == '\n') {
                    if (current >= report.start) break;

                    const end = if (self.source[current - 1] == '\r') current - 1 else current;
                    previous_line = self.source[line_start..end];

                    line_count += 1;
                    // Skip the \n
                    line_start = current + 1;
                }

                current += 1;
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

            // Prints the error part
            //  Error: <err-msg>
            try self.writer.print("{s} ", .{report.level.get_level_msg()});
            try report.get_msg(self.writer);
            _ = try self.writer.write("\n");

            // Prints file name and location infos
            //  ╭─[file_name.rv:1:5]
            try self.writer.print(
                "{s}{s}{s}[{s}{s}{s}:{}:{}]\n",
                .{
                    left_padding,
                    box_char(.UpperLeft),
                    box_char(.Horitzontal),
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
                try self.print_line(line_count - 1, pl, line_digit_count);
            }

            // Prints current line number, separation and line
            //  57 | fn add(a, b c)
            try self.print_line(line_count, self.source[line_start..current], line_digit_count);

            // Underlines the problem
            // Takes padding into account + separator + space
            //  <space><space> |
            try self.writer.print("{s}{s} ", .{ left_padding, box_char(.Vertical) });

            // We get the length of the error code and the half to underline it
            var space_buf: [1024]u8 = [_]u8{' '} ** 1024;
            const start_space = report.start - line_start;
            const lexeme_len = @max(report.end - report.start, 1);
            const half = @divFloor(lexeme_len, 2);

            // Prints initial space
            _ = try self.writer.write(space_buf[0..start_space]);

            // We write in yellow
            _ = try self.writer.write(color(.Yellow));

            // Prints ─┬─
            for (0..lexeme_len) |i| {
                if (i == half) {
                    _ = try self.writer.write(box_char(.UnderT));
                } else {
                    _ = try self.writer.write(box_char(.Horitzontal));
                }
            }
            _ = try self.writer.write("\n");

            // We switch back to no color
            _ = try self.writer.write(color(.NoColor));

            // Prints to indication (written state is the good one at this stage
            // for the beginning of the sequence to print)
            //  <space><space> | ╰─── <indication txt>
            try self.writer.print("{s}{s} ", .{ left_padding, box_char(.Vertical) });
            _ = try self.writer.write(space_buf[0 .. start_space + half]);

            _ = try self.writer.write(color(.Yellow));

            try self.writer.print("{s} ", .{corner_to_hint});
            _ = try report.get_hint(self.writer);
            _ = try self.writer.write("\n");
            _ = try self.writer.write(color(.NoColor));

            _ = try self.writer.write(left_padding);
            try self.writer.print("{s}\n", .{corner_to_end});

            var fba = try std.BoundedArray(u8, 1000).init(0);
            try report.get_help(fba.writer());

            if (fba.slice().len > 0) {
                // try self.writer.print("  {s} {s}\n", .{ help_msg, fba.slice()[0..help_bytes] });
                try self.writer.print("  {s} {s}\n", .{ help_msg, fba.slice()[0..fba.slice().len] });
            }

            _ = try self.writer.write("\n");
        }

        // Limitation of Zig, can only use comptime known strings for formatting...
        // TODO: dynamic formatting
        fn print_line(self: *const Self, line_nb: usize, line: []const u8, digit_count: usize) !void {
            try switch (digit_count) {
                1 => self.writer.print(" {:>1} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
                2 => self.writer.print(" {:>2} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
                3 => self.writer.print(" {:>3} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
                4 => self.writer.print(" {:>4} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
                5 => self.writer.print(" {:>5} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
                else => unreachable,
            };
        }
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
    assert(@hasDecl(T, "get_msg"));
    assert(@hasDecl(T, "get_hint"));
    assert(@hasDecl(T, "get_help"));

    return struct {
        report: T,
        level: Level,
        start: usize,
        end: usize,

        pub const Level = enum {
            Error,
            Info,
            Warning,

            pub fn get_level_msg(self: Level) []const u8 {
                return switch (self) {
                    .Error => err_msg,
                    .Info => @panic("not implemented yet"),
                    .Warning => warning_msg,
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
        pub fn err(report: T, span: Span) Self {
            return Self.init(report, .Error, span.start, span.end);
        }

        /// Creates warning associated with the tag
        pub fn warn(report: T, span: Span) Self {
            return Self.init(report, .Warning, span.start, span.end);
        }

        pub fn get_msg(self: *const Self, writer: anytype) !void {
            return self.report.get_msg(writer);
        }

        pub fn get_hint(self: *const Self, writer: anytype) !void {
            return self.report.get_hint(writer);
        }

        pub fn get_help(self: *const Self, writer: anytype) !void {
            return self.report.get_help(writer);
        }
    };
}
