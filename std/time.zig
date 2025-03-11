const std = @import("std");

pub fn _clock() f64 {
    return @as(f64, @floatFromInt(std.time.milliTimestamp())) / 1000.0;
}
