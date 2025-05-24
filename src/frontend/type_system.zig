const std = @import("std");
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

// 0000    0     0    0    0   0000  0000000000 0000000000
// |--|    |     |    |    |   |--|  |-------------------|
// Extra  Save  Ref  Nul  Var  Kind          Value

// Types are 32 bits long
const TypeSize = u32;
pub const Type = enum(TypeSize) {
    void,
    null,
    int,
    float,
    bool,
    str,
    self,
    _,

    const Self = @This();

    pub fn toIdx(self: Self) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn fromIdx(index: usize) Self {
        return @enumFromInt(index);
    }

    /// Creates a type from kind and value information
    pub fn create(kind: Kind, extra: Extra, value: Value) Self {
        const tmp: u32 = @intCast(kind.toIdx());
        const tmp2: u32 = @intCast(extra.toIdx());
        // 0x1 for variable, reference, nullable and reserve. All false
        return @enumFromInt(tmp2 << 28 | 0 << 24 | tmp << 20 | value);
    }

    /// Creates a type from kind and value information
    pub fn createVar(kind: Kind, extra: Extra, value: Value) Self {
        const tmp: u32 = @intCast(kind.toIdx());
        const tmp2: u32 = @intCast(extra.toIdx());
        // 0x1 for variable, reference, nullable and reserve. Activates only variable
        return @enumFromInt(tmp2 << 28 | 1 << 24 | tmp << 20 | value);
    }

    /// Get a type kind, discarding extra and value information bits
    pub fn getKind(self: Self) Kind {
        const erased = self.toIdx() & 0xf00000;
        return @enumFromInt(erased >> 20);
    }

    /// Get a type kind, discarding extra and value information bits
    pub fn setKind(self: *Self, kind: Kind) void {
        const erased = self.toIdx() & 0xff0fffff;
        self.* = @enumFromInt(erased | (kind.toIdx() << 28));
    }

    /// Get extra information bits about a type
    pub fn getExtra(self: Self) Extra {
        return @enumFromInt(self.toIdx() >> 28);
    }

    /// Get a type kind, discarding extra and value information bits
    pub fn setExtra(self: *Self, extra: Extra) void {
        const erased = self.toIdx() & 0x0fffffff;
        self.* = @enumFromInt(erased | extra.toIdx() << 28);
    }

    /// Extract the value bits associated to a type
    pub fn getValue(self: Self) Value {
        const erased = self.toIdx() & 0x000fffff;
        return @as(Value, @intCast(erased));
    }

    /// Checks if a type is of a certain kind
    pub fn is(self: Self, kind: Kind) bool {
        return getKind(self) == kind;
    }

    /// Sets the 'Var' bit of the type
    pub fn setVar(self: *Self) void {
        const erased = self.toIdx() & 0xf0ffffff;
        self.* = @enumFromInt(erased | (1 << 24));
    }

    /// Returns if the type is an instance
    pub fn isVar(self: *const Self) bool {
        return self.toIdx() & 0x01000000 == 1;
    }
};

// 4 first bits (16 values) are for:
pub const Kind = enum(u4) {
    none,
    func,
    array,
    tuple,
    @"enum",
    map,
    @"error",
    @"struct",
    module,
    _,

    pub fn toIdx(self: Kind) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn fromIdx(index: usize) Kind {
        return @enumFromInt(index);
    }

    /// Renders Kind as a string
    pub fn toStr(kind: Kind) []const u8 {
        return switch (kind) {
            .func => "function",
            else => |k| @tagName(k),
        };
    }
};

// 20 other allow 1048575 different types
pub const Value = u20;
// 1 bit for variable or not
// 1 bit for reference or not
// 1 bit for nullable or not
// 1 bit of reserve

// 4 next bits are for extra infos:
pub const Extra = enum(u4) {
    none,
    builtin,
    bound_method,
    imported,
    _,

    pub fn toIdx(self: Extra) usize {
        return @as(usize, @intFromEnum(self));
    }

    pub fn fromIdx(index: usize) Extra {
        return @enumFromInt(index);
    }
};

// Custom types
pub const TypeInfo = union(enum) {
    func: FnInfo,
    @"struct": StructInfo,

    /// Sets the module reference if it has been imported
    pub fn setModule(self: *TypeInfo, module_index: usize, type_index: usize) void {
        switch (self.*) {
            inline else => |*t| t.module = .{
                .import_index = module_index,
                .type_index = type_index,
            },
        }
    }
};

pub const FnInfo = struct {
    params: []const Type,
    return_type: Type,
    tag: Tag = .function,
    module: ?ModuleRef = null,

    pub const Tag = enum { builtin, function };
};

pub const StructInfo = struct {
    functions: AutoHashMapUnmanaged(usize, MemberInfo),
    fields: AutoHashMapUnmanaged(usize, MemberInfo),
    default_value_fields: usize,
    module: ?ModuleRef = null,

    pub fn proto(self: *const StructInfo, allocator: std.mem.Allocator) std.AutoHashMapUnmanaged(usize, bool) {
        var res = std.AutoHashMapUnmanaged(usize, bool){};
        res.ensureTotalCapacity(allocator, self.fields.capacity()) catch @panic("oom");

        var kv = self.fields.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.default) {
                res.putAssumeCapacity(entry.key_ptr.*, false);
            }
        }

        return res;
    }
};

pub const MemberInfo = struct {
    /// Order of declaration
    index: usize,
    /// Field's type
    type: Type,
    /// Has a default value
    default: bool = false,
};

pub const ModuleRef = struct {
    /// Modules from which it has been imported
    import_index: usize = 0,
    /// Index in type manager
    type_index: usize = 0,
};

pub const Symbols = AutoArrayHashMapUnmanaged(usize, MemberInfo);

test "types" {
    const expect = @import("std").testing.expect;
    const var1: Type = .int;
    const var2: Type = .str;

    try expect(var1.is(.variable));
    try expect(!var2.is(.func));

    const str: Type = .create(.variable, 0, .str);
    try expect(str == .str);
    try expect(str.is(.variable));
    try expect(str.getValue() == .str);

    const func: Type = .create(.func, .builtin, 16777214);
    try expect(func.is(.func));
    try expect(func.isBuiltin());
    try expect(func.getValue() == 16777214);
}
