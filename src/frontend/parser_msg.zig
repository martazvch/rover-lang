const UnsafeIterStr = @import("../unsafe_iter.zig").UnsafeIterStr;

pub const ParserMsg = enum {
    ChainingCmpOp,
    ExpectExpr,
    UnclosedParen,
    UnexpectedEof,

    const Self = @This();

    pub fn get_msg(self: Self, writer: anytype, uis: *UnsafeIterStr) !void {
        // switch (self) {
        //     .ChainingCmpOp => _ = try writer.write("chaining comparison operators"),
        //     .ExpectExpr => try writer.print("expected expression, found \"{s}\"", .{uis.next()}),
        //     .UnclosedParen => _ = try writer.write("unclosed parenthesis"),
        //     .UnexpectedEof => _ = try writer.write("unexpected end of file"),
        // }

        // Trick for handling both !usize and !void return type
        _ = try switch (self) {
            .ChainingCmpOp => writer.write("chaining comparison operators"),
            .UnclosedParen => writer.write("unclosed parenthesis"),
            .UnexpectedEof => writer.write("unexpected end of file"),
            else => |e| blk: {
                switch (e) {
                    .ExpectExpr => try writer.print("expected expression, found \"{s}\"", .{uis.next()}),
                    else => unreachable,
                }

                break :blk 0;
            },
        };
    }

    pub fn get_hint(self: Self, writer: anytype, _: *UnsafeIterStr) !usize {
        return switch (self) {
            .ExpectExpr, .UnclosedParen, .UnexpectedEof => writer.write("here"),
            .ChainingCmpOp => writer.write("this one is not allowed"),
        };
    }

    pub fn get_help(self: Self, writer: anytype, _: *UnsafeIterStr) !usize {
        return switch (self) {
            .ChainingCmpOp => writer.write("split your comparison with 'and' and 'or' operators"),
            .UnclosedParen => writer.write("close the opening parenthesis"),
            else => 0,
        };
    }
};
