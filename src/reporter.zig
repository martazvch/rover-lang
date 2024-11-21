const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const print = std.debug.print;
const Writer = std.fs.File.Writer;
const WriteError = std.posix.WriteError;
const Token = @import("frontend/lexer.zig").Token;

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
    source: [:0]const u8,
    writer: WriterType,
    tokens: []const Token,

    pub const WriterType = union(enum) {
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

    pub fn init(source: [:0]const u8, tokens: []const Token) Reporter {
        var writer: WriterType = undefined;
        const stdout = std.io.getStdOut().writer();

        if (builtin.os.tag == .windows) {
            writer = .{ .Custom = WindowsReporter.init(stdout) };
        } else {
            writer = .{ .StdOut = stdout };
        }

        return .{ .writer = writer, .source = source, .tokens = tokens };
    }

    pub fn report_all(
        self: *Reporter,
        file_name: []const u8,
        reports: []const Report,
    ) !void {
        const sep = if (builtin.os.tag == .windows) '\\' else '/';
        var iter = std.mem.splitBackwardsScalar(u8, file_name, sep);
        const name = iter.first();

        for (reports) |*report| {
            try report.display(self.source, name, self.tokens, &self.writer);
        }
    }
};

const WindowsReporter = struct {
    prev_cp: c_uint,
    writer: std.fs.File.Writer,

    const Self = @This();

    pub fn init(writer: std.fs.File.Writer) Self {
        return .{ .writer = writer, .prev_cp = std.os.windows.kernel32.GetConsoleOutputCP() };
    }

    pub fn write(self: Self, bytes: []const u8) !usize {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
        const count = try self.writer.write(bytes);
        _ = std.os.windows.kernel32.SetConsoleOutputCP(self.prev_cp);
        return count;
    }

    pub fn print(self: Self, comptime format: []const u8, args: anytype) !void {
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
        const count = try self.writer.print(format, args);
        _ = std.os.windows.kernel32.SetConsoleOutputCP(self.prev_cp);
        return count;
    }
};

/// Error report used en each step of the Rover language:
/// lexing, parsing, compiling, executing, ...
/// It has:
///  - tag: error tag
///  - level: warning or error
///  - start: starting byte offset from source of the error
///  - end: ending byte offset from source of the error
///  - extra_id: token id for extra info to display
pub const Report = struct {
    tag: Tag,
    level: Level,
    start: usize,
    end: usize,
    extra_id: ?usize,

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

    pub const Tag = enum {
        // Lexer
        UnterminatedStr,
        UnexpectedChar,

        // Parser
        ChainingCmpOp,
        ExpectExpr,
        UnclosedParen,
        UnexpectedEof,

        // Compiler
        TooManyConst,
    };

    const Self = @This();

    pub fn init(
        tag: Tag,
        level: Level,
        start: usize,
        end: usize,
        extra_id: ?usize,
    ) Self {
        return .{
            .tag = tag,
            .level = level,
            .start = start,
            .end = end,
            .extra_id = extra_id,
        };
    }

    /// Creates an error associated with the tag. If *msg* is not null
    /// overrides the message in the template.
    pub fn err(tag: Tag, start: usize, end: usize, extra: ?usize) Self {
        return Report.init(tag, .Error, start, end, extra);
    }

    /// Creates an error at the given token
    pub fn err_at_token(tag: Tag, token: *const Token, extra: ?usize) Self {
        return Self.err(tag, token.loc.start, token.loc.end, extra);
    }

    fn get_msg(self: *const Self, writer: anytype) !usize {
        return switch (self.tag) {
            // Lexer
            .UnterminatedStr => writer.write("unterminated string"),
            .UnexpectedChar => writer.write("unexpected character"),

            // Parser
            .ChainingCmpOp => writer.write("chaining comparison operators"),
            .ExpectExpr => writer.write("expected expression, found "),
            .UnclosedParen => writer.write("unclosed parenthesis"),
            .UnexpectedEof => writer.write("unexpected end of file"),

            // Compiler
            .TooManyConst => writer.write("too many constant in this chunk (max 256)"),
        };
    }

    fn get_hint(self: *const Self, writer: anytype) !usize {
        return switch (self.tag) {
            // Lexer
            .UnterminatedStr, .UnexpectedChar, .ExpectExpr, .UnclosedParen => writer.write("here"),

            // Parser
            .ChainingCmpOp => writer.write("this one is not allowed"),

            // Compiler
            .TooManyConst => writer.write("this one"),
            else => 0,
        };
    }

    fn get_help(self: *const Self, writer: anytype) !usize {
        return switch (self.tag) {
            // Lexer
            .UnterminatedStr => writer.write("close the opening quote"),

            // Parser
            .ChainingCmpOp => writer.write("split your comparison with 'and' and 'or' operators"),
            .UnclosedParen => writer.write("close the opening parenthesis"),

            // Compiler
            .TooManyConst => writer.write("try to split your code into smaller chunks"),
            else => 0,
        };
    }

    pub fn display(
        self: *const Self,
        source: [:0]const u8,
        file_name: []const u8,
        tokens: []const Token,
        writer: *Reporter.WriterType,
    ) !void {
        _ = tokens;

        var current: usize = 0;
        var line_start: usize = 0;
        var line_count: usize = 0;
        var previous_line: ?[]const u8 = null;

        // Looking for current line where it occured and buffers the previous one
        // for context
        while (true) {
            if (source[current] == 0) {
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
        try writer.print("{s} ", .{self.level.get_level_msg()});
        _ = try self.get_msg(writer);
        _ = try writer.write("\n");

        // Prints file name and location infos
        //  ╭─[file_name.rv:1:5]
        try writer.print(
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
        try writer.print("{s}{s} ", .{ left_padding, box_char(.Vertical) });

        // We get the length of the error code and the half to underline it
        var space_buf: [1024]u8 = [_]u8{' '} ** 1024;
        const start_space = self.start - line_start;
        const lexeme_len = @max(self.end - self.start, 1);
        const half = @divFloor(lexeme_len, 2);

        // Prints initial space
        _ = try writer.write(space_buf[0..start_space]);

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
        try writer.print("{s}{s} ", .{ left_padding, box_char(.Vertical) });
        _ = try writer.write(space_buf[0..start_space]);

        _ = try writer.write(color(.Yellow));

        try writer.print("{s} ", .{corner_to_hint});
        _ = try self.get_hint(writer);
        _ = try writer.write("\n");
        _ = try writer.write(color(.NoColor));

        _ = try writer.write(left_padding);
        try writer.print("{s}\n", .{corner_to_end});

        var fba = try std.BoundedArray(u8, 10000).init(0);
        const help_bytes = try self.get_help(fba.writer());
        if (help_bytes > 0) {
            try writer.print("  {s} {s}\n", .{ help_msg, fba.slice()[0..help_bytes] });
        }
    }

    // Limitation of Zig, can only use comptime known strings for formatting...
    fn print_line(line_nb: usize, line: []const u8, digit_count: usize, writer: anytype) !void {
        try switch (digit_count) {
            1 => writer.print(" {:>1} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            2 => writer.print(" {:>2} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            3 => writer.print(" {:>3} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            4 => writer.print(" {:>4} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            5 => writer.print(" {:>5} {s} {s}\n", .{ line_nb, box_char(.Vertical), line }),
            else => unreachable,
        };
    }
};
