const std = @import("std");

pub const Foo = struct { a: i64, bar: f64 };

pub fn main() void {
    const infos = @typeInfo(Foo);

    inline for (infos.@"struct".fields) |f| {
        if (comptime std.mem.eql(u8, "a", f.name)) {
            @compileLog("Here");

            if ("a".len == f.name.len) {
                @compileLog("Same length");
            }
        }
    }
}
