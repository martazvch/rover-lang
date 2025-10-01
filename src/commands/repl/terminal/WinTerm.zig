const std = @import("std");
const win = std.os.windows;
const Terminal = @import("Terminal.zig");

const DWORD = u32;
const WORD = u16;
const WCHAR = u16;
const BOOL = i32;
const HANDLE = win.HANDLE;

const ENABLE_LINE_INPUT = 0x0002;
const ENABLE_ECHO_INPUT = 0x0004;
const ENABLE_PROCESSED_INPUT = 0x0001;

const KEY_EVENT = 0x0001;
const VK_TAB = 0x09;
const VK_UP = 0x26;
const VK_DOWN = 0x28;
const VK_LEFT = 0x25;
const VK_RIGHT = 0x27;
const VK_RETURN = 0x0D;
const VK_BACK = 0x08;
const VK_DELETE = 0x2E;

const RIGHT_ALT_PRESSED = 0x0001;
const LEFT_ALT_PRESSED = 0x0002;
const ALT_PRESSED = RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED;

const RIGHT_CTRL_PRESSED = 0x0004;
const LEFT_CTRL_PRESSED = 0x0008;
const CTRL_PRESSED = LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED;

const SHIFT_PRESSED: u32 = 0x0010;

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

pub fn init() Terminal.Error!Self {
    // Set console output to UTF-8
    const prev_cp = win.kernel32.GetConsoleOutputCP();
    _ = win.kernel32.SetConsoleOutputCP(CP_UTF8);
    const input_handle = win.kernel32.GetStdHandle(win.STD_INPUT_HANDLE) orelse {
        return error.InitFail;
    };

    var original_mode: DWORD = 0;
    if (win.kernel32.GetConsoleMode(input_handle, &original_mode) == 0) {
        return error.InitFail;
    }

    return .{
        .prev_cp = prev_cp,
        .input_handle = input_handle,
        .original_mode = original_mode,
        .stdout = std.io.getStdOut().writer(),
        .cursor_pos = 0,
    };
}

pub fn enableRawMode(ctx: *anyopaque) Terminal.Error!void {
    const self: *const Self = @ptrCast(@alignCast(ctx));
    const raw_mode = self.original_mode & ~@as(DWORD, ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    if (win.kernel32.SetConsoleMode(self.input_handle, raw_mode) == 0) {
        return error.RawModeFail;
    }
}

pub fn disableRawMode(ctx: *anyopaque) void {
    const self: *const Self = @ptrCast(@alignCast(ctx));

    _ = win.kernel32.SetConsoleOutputCP(self.prev_cp);
    _ = win.kernel32.SetConsoleMode(self.input_handle, self.original_mode);
}

fn getEvent(self: *const Self) Terminal.Error!KEY_EVENT_RECORD {
    var record: INPUT_RECORD = undefined;
    var read_count: DWORD = 0;

    if (ReadConsoleInputW(self.input_handle, &record, 1, &read_count) == 0) {
        return error.ReadInputError;
    }

    if (record.EventType != KEY_EVENT) return error.NotAnEvent;

    const key = record.Event.KeyEvent;
    if (key.bKeyDown == 0) return error.NotAnEvent;

    return key;
}

pub fn getKey(ctx: *anyopaque) Terminal.Error!Terminal.Key {
    const self: *const Self = @ptrCast(@alignCast(ctx));

    while (true) {
        const key = self.getEvent() catch |e| switch (e) {
            error.NotAnEvent => continue,
            else => |narrowed| return narrowed,
        };

        const ctrl = key.dwControlKeyState & CTRL_PRESSED != 0;
        const only_shift = key.dwControlKeyState & ~SHIFT_PRESSED == 0;

        if (only_shift) continue;

        return switch (key.wVirtualKeyCode) {
            VK_UP => .{ .value = .up, .ctrl = ctrl },
            VK_DOWN => .{ .value = .down, .ctrl = ctrl },
            VK_LEFT => .{ .value = .left, .ctrl = ctrl },
            VK_RIGHT => .{ .value = .right, .ctrl = ctrl },
            VK_RETURN => .{ .value = .enter, .ctrl = ctrl },
            VK_BACK => .{ .value = .back, .ctrl = ctrl },
            VK_DELETE => .{ .value = .delete, .ctrl = ctrl },
            VK_TAB => .{ .value = .tab, .ctrl = ctrl },
            else => {
                const ch = key.uChar.UnicodeChar;

                if (ch == 0) continue;
                if (ch > 0x7F) return error.NonAsciiChar;

                // Ctrl-C
                if (ch == 0x03) {
                    return .{ .value = .{ .char = 'c' }, .ctrl = true };
                }

                // Encode character at the end of line
                // var write_ptr: [4]u8 = undefined;
                // const bytes_to_insert = std.unicode.utf8Encode(ch, &write_ptr) catch return error.ReadInputError;
                // return .{ .value = .{ .chars = write_ptr[0..bytes_to_insert] }, .ctrl = ctrl };
                return .{ .value = .{ .char = @intCast(ch) }, .ctrl = ctrl };
            },
        };
    }
}

pub fn terminal(self: *Self) Terminal {
    return .{
        .ptr = self,
        .vtable = &.{
            .enableRawMode = enableRawMode,
            .disableRawMode = disableRawMode,
            .getKey = getKey,
        },
    };
}
