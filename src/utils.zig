const std = @import("std");

pub fn oom() noreturn {
    std.debug.print("out of memory\n", .{});
    std.process.exit(1);
}

/// Creates an enum containing each fields of the structure
pub fn EnumFromStruct(comptime Structure: type) type {
    const info = @typeInfo(Structure).@"struct";
    var arr: [info.fields.len]std.builtin.Type.EnumField = undefined;
    for (info.fields, 0..) |f, i| {
        arr[i] = .{ .name = f.name, .value = i };
    }

    return @Type(.{ .@"enum" = .{
        .tag_type = u32,
        .fields = &arr,
        .decls = &.{},
        .is_exhaustive = true,
    } });
}
