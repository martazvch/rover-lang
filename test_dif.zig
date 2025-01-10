const std = @import("std");
const builtin = @import("builtin");
const print = std.debug.print;

const RED = "\x1b[31m";
const NORMAL = "\x1b[0m";

const str1 =
    \\[Fn declaration add, type void, arity 1
    \\    params:
    \\        arg1, type int
    \\    body:
    \\        [Block]
    \\]
    \\[Fn declaration add, type void, arity 1
    \\    params:
    \\        arg1, type int
    \\    body:
    \\        [Block]
    \\]
    \\[Fn declaration add, type void, arity 2
    \\    params:
    \\        param1, type int
    \\        param2, type Person
    \\    body:
    \\        [Block]
    \\]
;

const str2 =
    \\[Fn declaration add, type void, arity 1
    \\    params:
    \\        arg1, type int
    \\    body:
    \\        [Block]
    \\]
    \\[Fn declaration add, type void, arity 1
    \\    params:
    \\        arg1, type float
    \\    body:
    \\        [Bloc]
    \\]
    \\[Fn declaration add, type void, arity 2
    \\    params:
    \\        param1, type int
    \\        param2, type Person
;

pub fn main() !void {
    if (!std.mem.eql(u8, str1, str2)) {
        var res = std.ArrayList(u8).init(std.heap.page_allocator);
        defer res.deinit();

        const State = enum {
            normal,
            until_nl1,
            until_nl2,
        };

        var i: usize = 0;
        var j: usize = 0;
        dif: switch (State.normal) {
            .normal => {
                if (i >= str1.len) break :dif;
                if (j >= str2.len) break :dif;

                if (str1[i] != str2[j]) {
                    try res.appendSlice(RED);
                    try res.append(str2[j]);
                    i += 1;
                    j += 1;
                    continue :dif .until_nl1;
                }

                try res.append(str2[j]);
                i += 1;
                j += 1;
                continue :dif .normal;
            },
            .until_nl1 => {
                if (i >= str1.len) break :dif;

                if (str1[i] == '\n') {
                    i += 1;
                    continue :dif .until_nl2;
                }

                i += 1;
                continue :dif .until_nl1;
            },
            .until_nl2 => {
                if (j >= str2.len) break :dif;

                if (str2[j] == '\n') {
                    try res.appendSlice(NORMAL);
                    try res.append(str2[j]);
                    j += 1;
                    continue :dif .normal;
                }

                try res.append(str2[j]);
                j += 1;
                continue :dif .until_nl2;
            },
        }

        print("Res:\n{s}\n", .{res.items});
    }
}
