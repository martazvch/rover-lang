const std = @import("std");
const Allocator = std.mem.Allocator;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const oom = @import("../utils.zig").oom;

// 0000    0     0     0    0   0000  0000000000 0000000000
// |--|    |     |     |    |   |--|  |-------------------|
// Extra  Save  Save  Ref  Nul  Kind          Value

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
    const EXTRA_MASK: TypeSize = 0xf0000000;
    const REF_MASK: TypeSize = 0x02000000;
    const NUL_MASK: TypeSize = 0x01000000;
    const KIND_MASK: TypeSize = 0x00f00000;
    const VAL_MASK: TypeSize = 0x000fffff;

    pub inline fn toIdx(self: Self) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) Self {
        return @enumFromInt(index);
    }

    /// Creates a type from kind and value information
    pub inline fn create(kind: Kind, extra: Extra, value: Value) Self {
        return @enumFromInt(extra.toIdx() << 28 | 0 << 24 | kind.toIdx() << 20 | value);
    }

    /// Get a type kind, discarding extra and value information bits
    pub inline fn getKind(self: Self) Kind {
        return @enumFromInt((self.toIdx() & KIND_MASK) >> 20);
    }

    /// Get a type kind, discarding extra and value information bits
    pub inline fn setKind(self: *Self, kind: Kind) void {
        const erased = self.toIdx() & ~KIND_MASK;
        self.* = @enumFromInt(erased | (kind.toIdx() << 20));
    }

    /// Get extra information bits about a type
    pub inline fn getExtra(self: Self) Extra {
        return @enumFromInt(self.toIdx() >> 28);
    }

    /// Get a type kind, discarding extra and value information bits
    pub inline fn setExtra(self: *Self, extra: Extra) void {
        const erased = self.toIdx() & ~EXTRA_MASK;
        self.* = @enumFromInt(erased | extra.toIdx() << 28);
    }

    /// Extract the value bits associated to a type
    pub inline fn getValue(self: Self) Value {
        return @as(Value, @intCast(self.toIdx() & VAL_MASK));
    }

    /// Checks if a type is of a certain kind
    pub inline fn is(self: Self, kind: Kind) bool {
        return getKind(self) == kind;
    }

    /// Checks if the type is a heap allocated object
    pub inline fn isHeap(self: Self) bool {
        const kind = self.getKind();
        return kind == .@"struct" or kind == .array;
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

    pub inline fn toIdx(self: Kind) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) Kind {
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

// 1 bit for reference or not
// 1 bit for nullable or not
// 1 bit of reserve
// 1 bit of reserve

// 4 next bits are for extra infos:
pub const Extra = enum(u4) {
    none,
    builtin,
    bound_method,
    imported,
    _,

    pub inline fn toIdx(self: Extra) TypeSize {
        return @as(TypeSize, @intFromEnum(self));
    }

    pub inline fn fromIdx(index: TypeSize) Extra {
        return @enumFromInt(index);
    }
};

// Custom types
pub const TypeInfo = union(enum) {
    array: ArrayInfo,
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

pub const ArrayInfo = struct {
    child: Type,
    module: ?ModuleRef = null,
};

pub const FnInfo = struct {
    params: AutoArrayHashMapUnmanaged(usize, ParamInfo),
    return_type: Type,
    tag: Tag = .function,
    module: ?ModuleRef = null,

    pub const Tag = enum { builtin, function };

    pub const ParamInfo = struct {
        /// Order of declaration
        index: usize,
        /// Field's type
        type: Type,
        /// Has a default value
        default: bool = false,
    };

    pub fn proto(self: *const FnInfo, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
        var res = AutoArrayHashMapUnmanaged(usize, bool){};
        res.ensureTotalCapacity(allocator, self.params.count()) catch oom();

        var kv = self.params.iterator();
        while (kv.next()) |entry| {
            res.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.default);
        }

        return res;
    }
};

pub const StructInfo = struct {
    functions: AutoHashMapUnmanaged(usize, MemberInfo),
    fields: AutoArrayHashMapUnmanaged(usize, MemberInfo),
    default_value_fields: usize,
    module: ?ModuleRef = null,

    pub const MemberInfo = struct {
        /// Order of declaration
        index: usize,
        /// Field's type
        type: Type,
        /// Has a default value
        default: bool = false,
    };

    pub fn proto(self: *const StructInfo, allocator: Allocator) AutoArrayHashMapUnmanaged(usize, bool) {
        var res = AutoArrayHashMapUnmanaged(usize, bool){};
        res.ensureTotalCapacity(allocator, self.fields.count()) catch oom();

        var kv = self.fields.iterator();
        while (kv.next()) |entry| {
            if (!entry.value_ptr.default) {
                res.putAssumeCapacity(entry.key_ptr.*, false);
            }
        }

        return res;
    }
};

pub const ModuleRef = struct {
    /// Modules from which it has been imported
    import_index: usize = 0,
    /// Index in type manager
    type_index: usize = 0,
};

pub const Symbols = AutoArrayHashMapUnmanaged(usize, StructInfo.MemberInfo);

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
