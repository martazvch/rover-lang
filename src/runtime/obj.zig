const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const options = @import("options");
const Vm = @import("vm.zig").Vm;
const Value = @import("values.zig").Value;
const Chunk = @import("../backend/chunk.zig").Chunk;
const NativeFn = @import("../std/meta.zig").NativeFn;

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,
    is_marked: bool,

    const ObjKind = enum {
        // BoundMethod,
        Fn,
        // Instance,
        // Iter,
        NativeFn,
        String,
        Struct,
    };

    pub fn allocate(vm: *Vm, comptime T: type, kind: ObjKind) Allocator.Error!*T {
        comptime assert(@hasField(T, "obj"));
        comptime assert(@hasDecl(T, "as_obj"));

        const ptr = try vm.gc_alloc.create(T);
        ptr.obj = .{
            .kind = kind,
            .next = vm.objects,
            .is_marked = false,
        };

        vm.objects = &ptr.obj;

        if (comptime options.log_gc) {
            std.debug.print("{*} allocate {} bytes for: ", .{ ptr, @sizeOf(T) });
        }

        return ptr;
    }

    pub fn destroy(self: *Obj, vm: *Vm) void {
        switch (self.kind) {
            // .BoundMethod => self.as(ObjBoundMethod).deinit(vm.allocator),
            .Fn => {
                const function = self.as(ObjFunction);
                function.deinit(vm.gc_alloc);
            },
            // .Instance => {
            //     const instance = self.as(ObjInstance);
            //     instance.deinit(vm.allocator);
            // },
            // .Iter => {
            //     const iter = self.as(ObjIter);
            //     vm.allocator.destroy(iter);
            // },
            .NativeFn => {
                const function = self.as(ObjNativeFn);
                function.deinit(vm.gc_alloc);
            },
            .String => self.as(ObjString).deinit(vm.gc_alloc),
            .Struct => {
                const structure = self.as(ObjStruct);
                structure.deinit(vm.gc_alloc);
            },
        }
    }

    pub fn as(self: *Obj, comptime T: type) *T {
        comptime assert(@hasField(T, "obj"));

        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn print(self: *Obj, writer: anytype) !void {
        try switch (self.kind) {
            // .BoundMethod => self.as(ObjBoundMethod).method.function.print(writer),
            // .Closure => self.as(ObjClosure).function.print(writer),
            .Fn => self.as(ObjFunction).print(writer),
            // .Instance => writer.print("<instance of {s}>", .{self.as(ObjInstance).parent.name.chars}),
            // .Iter => {
            //     const iter = self.as(ObjIter);
            //     try writer.print("iter: {} -> {}", .{ iter.current, iter.end });
            // },
            .NativeFn => writer.print("<native fn>", .{}),
            .String => writer.print("\"{s}\"", .{self.as(ObjString).chars}),
            .Struct => writer.print("<structure {s}>", .{self.as(ObjStruct).name.chars}),
        };
    }

    pub fn log(self: *Obj) void {
        switch (self.kind) {
            // .BoundMethod => self.as(ObjBoundMethod).method.function.print(writer),
            // .Closure => self.as(ObjClosure).function.print(writer),
            .Fn => self.as(ObjFunction).log(),
            // .Instance => writer.print("<instance of {s}>", .{self.as(ObjInstance).parent.name.chars}),
            // .Iter => {
            //     const iter = self.as(ObjIter);
            //     try writer.print("iter: {} -> {}", .{ iter.current, iter.end });
            // },
            .NativeFn => std.debug.print("<native fn>", .{}),
            .String => std.debug.print("\"{s}\"", .{self.as(ObjString).chars}),
            .Struct => std.debug.print("<structure {s}>", .{self.as(ObjStruct).name.chars}),
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    const Self = @This();

    // PERF: flexible array member: https://craftinginterpreters.com/strings.html#challenges
    fn create(vm: *Vm, str: []const u8, hash: u32) Allocator.Error!*ObjString {
        var obj = try Obj.allocate(vm, Self, .String);
        obj.chars = str;
        obj.hash = hash;

        // The set method can trigger a GC to grow hashmap before
        // inserting. We put the value on the stack so that it is marked
        // as a root
        vm.stack.push(Value.obj(obj.as_obj()));
        _ = try vm.strings.set(obj, Value.null_());
        _ = vm.stack.pop();

        if (options.log_gc) {
            print("{s}\n", .{str});
        }

        return obj;
    }

    pub fn copy(vm: *Vm, str: []const u8) Allocator.Error!*ObjString {
        const hash = ObjString.hash_string(str);
        const interned = vm.strings.find_string(str, hash);

        if (interned) |i| return i;

        const chars = try vm.gc_alloc.alloc(u8, str.len);
        @memcpy(chars, str);

        if (options.log_gc) {
            std.debug.print(
                "{*} allocate {} bytes for copying: {s}\n",
                .{ chars.ptr, chars.len, chars },
            );
        }

        return ObjString.create(vm, chars, hash);
    }

    // Take a string allocated by calling Vm. If interned already, free
    // the memory and return the interned one
    pub fn take(vm: *Vm, str: []const u8) Allocator.Error!*ObjString {
        const hash = ObjString.hash_string(str);
        const interned = vm.strings.find_string(str, hash);

        if (interned) |i| {
            vm.gc_alloc.free(str);
            return i;
        }

        return ObjString.create(vm, str, hash);
    }

    pub fn as_obj(self: *ObjString) *Obj {
        return &self.obj;
    }

    fn hash_string(chars: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (chars) |c| {
            hash ^= c;
            hash *%= 16777619;
        }

        return hash;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.chars);
        allocator.destroy(self);
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*ObjString,

    const Self = @This();

    pub fn create(vm: *Vm, name: ?*ObjString) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .Fn);

        obj.arity = 0;
        obj.chunk = Chunk.init(vm.allocator);
        obj.name = name;

        if (options.log_gc) {
            const display_name = if (name) |n| n.chars else "";
            std.debug.print("{s}\n", .{display_name});
        }

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn print(self: *const Self, writer: anytype) (std.fs.File.WriteError || Allocator.Error)!void {
        if (self.name) |n| {
            try writer.print("<fn {s}>", .{n.chars});
        } else {
            try writer.print("<fn script>", .{});
        }
    }

    pub fn log(self: *const Self) void {
        std.debug.print("<fn {s}>", .{if (self.name) |n| n.chars else "script"});
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.chunk.deinit();

        // Name already in the linked list, don't free manually
        allocator.destroy(self);
    }
};

pub const ObjNativeFn = struct {
    obj: Obj,
    function: NativeFn,

    const Self = @This();

    pub fn create(vm: *Vm, function: NativeFn) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .NativeFn);
        obj.function = function;

        if (options.log_gc) {
            std.debug.print("native fn\n", .{});
        }

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }

    pub fn as_obj(self: *ObjNativeFn) *Obj {
        return &self.obj;
    }
};

pub const ObjStruct = struct {
    obj: Obj,
    name: *ObjString,
    // methods: Table,

    const Self = @This();

    pub fn create(vm: *Vm, name: *ObjString) Allocator.Error!*Self {
        const obj = try Obj.allocate(vm, Self, .Struct);
        obj.name = name;
        // obj.methods = Table.init(vm.allocator);

        if (options.log_gc) std.debug.print("<struct {s}>\n", .{name.chars});

        return obj;
    }

    pub fn as_obj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        // self.methods.deinit();
        allocator.destroy(self);
    }
};
