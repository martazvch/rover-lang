const std = @import("std");
const win = std.os.windows;

const DWORD = u32;
const WORD = u16;
const WCHAR = u16;
const BOOL = i32;
const HANDLE = win.HANDLE;

const ENABLE_LINE_INPUT = 0x0002;
const ENABLE_ECHO_INPUT = 0x0004;
const ENABLE_PROCESSED_INPUT = 0x0001;

const KEY_EVENT = 0x0001;
const VK_UP = 0x26;
const VK_DOWN = 0x28;
const VK_LEFT = 0x25;
const VK_RIGHT = 0x27;
const VK_RETURN = 0x0D;

extern "kernel32" fn ReadConsoleInputW(hConsoleInput: HANDLE, lpBuffer: *INPUT_RECORD, nLength: DWORD, lpNumberOfEventsRead: *DWORD) BOOL;

const CP_UTF8 = 65001;

const KEY_EVENT_RECORD = extern struct {
    bKeyDown: BOOL,
    wRepeatCount: WORD,
    wVirtualKeyCode: WORD,
    wVirtualScanCode: WORD,
    uChar: extern union {
        UnicodeChar: WCHAR,
        AsciiChar: u8,
    },
    dwControlKeyState: DWORD,
};

const INPUT_RECORD = extern struct {
    EventType: WORD,
    Event: extern union {
        KeyEvent: KEY_EVENT_RECORD,
        _padding: [16]u8, // ensure size matches Windows definition
    },
};

const Self = @This();

prev_cp: win.UINT,
input_handle: HANDLE,
original_mode: DWORD,
stdout: std.fs.File.Writer,
cursor_pos: usize,

pub fn init() !Self {
    // Set console output to UTF-8
    const prev_cp = win.kernel32.GetConsoleOutputCP();
    _ = win.kernel32.SetConsoleOutputCP(CP_UTF8);

    const input_handle = win.kernel32.GetStdHandle(win.STD_INPUT_HANDLE) orelse {
        return error.InvalidHandle;
    };

    var original_mode: DWORD = 0;
    if (win.kernel32.GetConsoleMode(input_handle, &original_mode) == 0)
        return error.GetConsoleModeFailed;

    const raw_mode = original_mode & ~@as(DWORD, ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    if (win.kernel32.SetConsoleMode(input_handle, raw_mode) == 0)
        return error.SetConsoleModeFailed;

    return .{
        .prev_cp = prev_cp,
        .input_handle = input_handle,
        .original_mode = original_mode,
        .stdout = std.io.getStdOut().writer(),
        .cursor_pos = 0,
    };
}

pub fn deinit(self: *Self) void {
    _ = win.kernel32.SetConsoleOutputCP(self.prev_cp);
    _ = win.kernel32.SetConsoleMode(self.input_handle, self.original_mode);
}

pub const Event = union(enum) {
    up,
    down,
    left,
    right,
    enter,
    char: u16,
};

pub fn getEvent(self: *Self) !KEY_EVENT_RECORD {
    var record: INPUT_RECORD = undefined;
    var read_count: DWORD = 0;

    if (ReadConsoleInputW(self.input_handle, &record, 1, &read_count) == 0) {
        return error.ReadConsoleInputFailed;
    }

    if (record.EventType != KEY_EVENT) return error.NotAnEvent;

    const key = record.Event.KeyEvent;
    if (key.bKeyDown == 0) return error.NotAnEvent;

    return key;
}

fn moveCursor(self: *Self, dir: enum { left, right }, max: usize) !void {
    if (dir == .left) {
        if (self.cursor_pos > 0) {
            self.cursor_pos -= 1;
            try self.stdout.writeAll("\x1b[D");
        }
    } else {
        if (self.cursor_pos < max) {
            self.cursor_pos += 1;
            try self.stdout.writeAll("\x1b[C");
        }
    }
}

pub fn getPrompt(self: *Self) ![]const u8 {
    const stdout = std.io.getStdOut().writer();

    var buffer: [1024]u8 = undefined;
    var line: []u8 = buffer[0..0]; // current line

    while (true) {
        const key = self.getEvent() catch |e| switch (e) {
            error.NotAnEvent => continue,
            else => |narrowed| return narrowed,
        };

        switch (key.wVirtualKeyCode) {
            VK_LEFT => try self.moveCursor(.left, 0),
            VK_RIGHT => try self.moveCursor(.right, line.len),
            VK_RETURN => {
                try stdout.writeAll("\n");
                return line;
            },
            else => {
                const ch = key.uChar.UnicodeChar;

                if (ch != 0) {
                    if (ch == 'q') return error.Eof;

                    // Encode character at the end of line
                    // const write_ptr = buffer[line.len..];
                    var write_ptr: [4]u8 = undefined;
                    const bytes_to_insert = try std.unicode.utf8Encode(ch, &write_ptr);
                    // Create a slice pointing to the newly written bytes
                    const inserted_bytes = write_ptr[0..bytes_to_insert];

                    // Shift right the tail to make space
                    std.mem.copyBackwards(u8, buffer[self.cursor_pos + bytes_to_insert .. line.len + bytes_to_insert], buffer[self.cursor_pos..line.len]);

                    // Insert new bytes
                    @memcpy(buffer[self.cursor_pos .. self.cursor_pos + bytes_to_insert], inserted_bytes);

                    // Update line and cursor
                    line = buffer[0 .. line.len + bytes_to_insert];
                    self.cursor_pos += bytes_to_insert;

                    try stdout.writeAll(inserted_bytes);
                    if (self.cursor_pos != line.len) {
                        // Rewrite tail after cursor
                        try stdout.writeAll(line[self.cursor_pos..]);
                        // Move cursor back to original position
                        const diff = line.len - self.cursor_pos;
                        try stdout.print("\x1b[{}D", .{diff});
                    }
                }
            },
        }
    }
}
