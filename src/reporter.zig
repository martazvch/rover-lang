const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Writer = std.fs.File.Writer;
const WriteError = std.posix.WriteError;
const ErrorKind = @import("errors.zig").ErrKind;
const Token = @import("frontend/lexer.zig").Token;
const IterList = @import("iter_list.zig").IterList;

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

pub const Reporter = struct {
    source: []const u8,
    writer: WriterType,
    extra: IterList([]const u8),

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

        return .{ .writer = writer, .source = source, .extra = undefined };
    }

    pub fn report_all(
        self: *Reporter,
        file_name: []const u8,
        reports: []const Report,
        extra_infos: []const []const u8,
    ) !void {
        const sep = if (builtin.os.tag == .windows) '\\' else '/';
        var iter = std.mem.splitBackwardsScalar(u8, file_name, sep);
        const name = iter.first();

        self.extra = IterList([]const u8).init(extra_infos);

        for (reports) |*report| {
            try report.display(self.source, name, &self.writer, &self.extra);
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

/// Error report used en each step of the Rover language:
/// lexing, parsing, compiling, executing, ...
/// It has:
///  - kind: error kind of type *ErrorKind*
///  - level: warning or error
///  - start: starting byte offset from source of the error
///  - end: ending byte offset from source of the error
///  - hint: message under the error
///  - help: optional message at the end to help solve
pub const Report = struct {
    kind: ErrorKind,
    level: Level,
    start: usize,
    end: usize,
    msg: ?[]const u8,

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

    pub fn init(
        kind: ErrorKind,
        level: Level,
        start: usize,
        end: usize,
        msg: ?[]const u8,
    ) Self {
        return .{
            .kind = kind,
            .level = level,
            .start = start,
            .end = end,
            .msg = msg,
        };
    }

    /// Creates an error associated with the kind. If *msg* is not null
    /// overrides the message in the template.
    // TODO: make last agrs being arguments for formatting
    pub fn err(
        kind: ErrorKind,
        start: usize,
        end: usize,
        msg: ?[]const u8,
    ) Self {
        return Report.init(kind, .Error, start, end, msg);
    }

    /// Creates an error at the given token
    pub fn err_at_token(kind: ErrorKind, token: *const Token, msg: ?[]const u8) Self {
        return Self.err(kind, token.start, token.start + token.lexeme.len, msg);
    }

    pub fn display(
        self: *const Self,
        source: []const u8,
        file_name: []const u8,
        writer: *const Reporter.WriterType,
        extra: *IterList([]const u8),
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

                const end = if (source[current - 1] == '\r') current - 1 else current;
                previous_line = source[line_start..end];

                line_count += 1;
                // Skip the \n
                line_start = current + 1;
            }

            current += 1;
        }
        // Line index start to 1
        line_count += 1;

        var buf: [1024]u8 = undefined;
        // We consider the maximum line number being 99 999. The extra space
        // is for space between line number and gutter and the one at the beginning
        const buf2: [7]u8 = [_]u8{' '} ** 7;

        // Gets line number digit count
        var written = try std.fmt.bufPrint(&buf, "{}", .{line_count});
        const line_digit_count = written.len;
        const left_padding = buf2[0 .. written.len + 2];

        const infos = self.kind.get_infos();

        // Prints the error part
        //  Error: <err-msg>
        written = try std.fmt.bufPrint(&buf, "{s} {s}", .{ self.level.get_msg(), infos.msg });
        _ = try writer.write(written);

        // Could be extra informations, like token lexeme
        if (try self.kind.extra(&buf, extra)) |w| {
            _ = try writer.write(w);
        }
        _ = try writer.write("\n");

        // Prints file name and location infos
        //  ╭─[file_name.rv:1:5]
        written = try std.fmt.bufPrint(
            buf[0..],
            "{s}{s}{s}[{s}{s}{s}:{}:{}]\n",
            .{
                left_padding,
                box_char(.UpperLeft),
                box_char(.Horitzontal),
                color(.Blue),
                file_name,
                color(.NoColor),
                line_count,
                self.end - line_start + 1,
            },
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
        //  <space><space> |
        var buf3: [1024]u8 = undefined;
        written = try std.fmt.bufPrint(&buf3, "{s}{s} ", .{ left_padding, box_char(.Vertical) });
        _ = try writer.write(written);

        // We get the length of the error code and the half to underline it
        var buf4: [1024]u8 = [_]u8{' '} ** 1024;
        const start_space = self.start - line_start;
        const lexeme_len = @max(self.end - self.start, 1);
        const half = @divFloor(lexeme_len, 2);

        // Prints initial space
        _ = try writer.write(buf4[0..start_space]);

        // We write in yellow
        _ = try writer.write(color(.Yellow));

        // Prints ─┬─
        for (0..lexeme_len) |i| {
            if (i == half) {
                _ = try writer.write(box_char(.UnderT));
            } else {
                _ = try writer.write(box_char(.Horitzontal));
            }
        }
        _ = try writer.write("\n");

        // We switch back to no color
        _ = try writer.write(color(.NoColor));

        // Prints to indication (written state is the good one at this stage
        // for the beginning of the sequence to print)
        //  <space><space> | ╰─── <indication txt>
        _ = try writer.write(written);
        _ = try writer.write(buf4[0..start_space]);

        _ = try writer.write(color(.Yellow));
        const hint = try std.fmt.bufPrint(&buf, "{s} {s}\n", .{ corner_to_hint, infos.hint });
        _ = try writer.write(hint);
        _ = try writer.write(color(.NoColor));

        _ = try writer.write(left_padding);
        const corner = try std.fmt.bufPrint(&buf, "{s}\n", .{corner_to_end});
        _ = try writer.write(corner);

        if (infos.help) |h_msg| {
            const help = try std.fmt.bufPrint(&buf, "  {s} {s}\n", .{ help_msg, h_msg });
            _ = try writer.write(help);
        }
    }

    // Limitation of Zig, can only use comptime known strings for formatting...
    fn print_line(line_nb: usize, line: []const u8, digit_count: usize, writer: *const Reporter.WriterType) !void {
        var buf: [1024]u8 = undefined;

        const written = switch (digit_count) {
            1 => try std.fmt.bufPrint(&buf, " {:>1} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            2 => try std.fmt.bufPrint(&buf, " {:>2} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            3 => try std.fmt.bufPrint(&buf, " {:>3} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            4 => try std.fmt.bufPrint(&buf, " {:>4} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            5 => try std.fmt.bufPrint(&buf, " {:>5} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            else => unreachable,
        };

        _ = try writer.write(written);
    }
};
