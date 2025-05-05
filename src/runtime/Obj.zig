const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const options = @import("options");

const Chunk = @import("../backend/Chunk.zig");
const NativeFn = @import("../std/meta.zig").NativeFn;
const Value = @import("values.zig").Value;
const Vm = @import("Vm.zig");
const oom = @import("../utils.zig").oom;

kind: ObjKind,
next: ?*Obj,
is_marked: bool,

const Obj = @This();

const ObjKind = enum {
    bound_method,
    func,
    instance,
    // Iter,
    native_fn,
    string,
    @"struct",
};

pub fn allocate(vm: *Vm, comptime T: type, kind: ObjKind) *T {
    comptime assert(@hasField(T, "obj"));
    comptime assert(@hasDecl(T, "asObj"));

    const ptr = vm.gc_alloc.create(T) catch oom();
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
        .bound_method => self.as(ObjBoundMethod).deinit(vm.allocator),
        .func => {
            const function = self.as(ObjFunction);
            function.deinit(vm.gc_alloc);
        },
        .instance => {
            const instance = self.as(ObjInstance);
            instance.deinit(vm.gc_alloc);
        },
        // .Iter => {
        //     const iter = self.as(ObjIter);
        //     vm.allocator.destroy(iter);
        // },
        .native_fn => {
            const function = self.as(ObjNativeFn);
            function.deinit(vm.gc_alloc);
        },
        .string => self.as(ObjString).deinit(vm.gc_alloc),
        .@"struct" => {
            const structure = self.as(ObjStruct);
            structure.deinit(vm);
        },
    }
}

pub fn as(self: *Obj, comptime T: type) *T {
    comptime assert(@hasField(T, "obj"));

    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn print(self: *Obj, writer: anytype) !void {
    try switch (self.kind) {
        .bound_method => self.as(ObjBoundMethod).method.log(),
        // .Closure => self.as(ObjClosure).function.print(writer),
        .func => self.as(ObjFunction).print(writer),
        .instance => writer.print("<instance of {s}>", .{self.as(ObjInstance).parent.name.chars}),
        // .Iter => {
        //     const iter = self.as(ObjIter);
        //     try writer.print("iter: {} -> {}", .{ iter.current, iter.end });
        // },
        .native_fn => writer.print("<native fn>", .{}),
        .string => writer.print("\"{s}\"", .{self.as(ObjString).chars}),
        .@"struct" => writer.print("<structure {s}>", .{self.as(ObjStruct).name.chars}),
    };
}

pub fn log(self: *Obj) void {
    switch (self.kind) {
        .bound_method => self.as(ObjBoundMethod).method.log(),
        // .Closure => self.as(ObjClosure).function.print(writer),
        .func => self.as(ObjFunction).log(),
        .instance => std.debug.print("<instance of {s}>", .{self.as(ObjInstance).parent.name.chars}),
        // .Iter => {
        //     const iter = self.as(ObjIter);
        //     try writer.print("iter: {} -> {}", .{ iter.current, iter.end });
        // },
        .native_fn => std.debug.print("<native fn>", .{}),
        .string => std.debug.print("\"{s}\"", .{self.as(ObjString).chars}),
        .@"struct" => std.debug.print("<structure {s}>", .{self.as(ObjStruct).name.chars}),
    }
}

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    const Self = @This();

    // PERF: flexible array member: https://craftinginterpreters.com/strings.html#challenges
    fn create(vm: *Vm, str: []const u8, hash: u32) *ObjString {
        var obj = Obj.allocate(vm, Self, .string);
        obj.chars = str;
        obj.hash = hash;

        // The set method can trigger a GC to grow hashmap before
        // inserting. We put the value on the stack so that it is marked
        // as a root
        vm.stack.push(Value.obj(obj.asObj()));
        _ = vm.strings.set(obj, Value.null_());
        _ = vm.stack.pop();

        if (options.log_gc) {
            std.debug.print("{s}\n", .{str});
        }

        return obj;
    }

    pub fn copy(vm: *Vm, str: []const u8) *ObjString {
        const hash = ObjString.hashString(str);
        const interned = vm.strings.findString(str, hash);

        if (interned) |i| return i;

        const chars = vm.gc_alloc.alloc(u8, str.len) catch oom();
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
    pub fn take(vm: *Vm, str: []const u8) *ObjString {
        const hash = ObjString.hashString(str);
        const interned = vm.strings.findString(str, hash);

        if (interned) |i| {
            vm.gc_alloc.free(str);
            return i;
        }

        return ObjString.create(vm, str, hash);
    }

    pub fn asObj(self: *ObjString) *Obj {
        return &self.obj;
    }

    fn hashString(chars: []const u8) u32 {
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

    pub fn create(vm: *Vm, name: ?*ObjString) *Self {
        const obj = Obj.allocate(vm, Self, .func);

        obj.arity = 0;
        obj.chunk = Chunk.init(vm.allocator);
        obj.name = name;

        if (options.log_gc) {
            const display_name = if (name) |n| n.chars else "";
            std.debug.print("{s}\n", .{display_name});
        }

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
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

    pub fn create(vm: *Vm, function: NativeFn) *Self {
        const obj = Obj.allocate(vm, Self, .native_fn);
        obj.function = function;

        if (options.log_gc) {
            std.debug.print("native fn\n", .{});
        }

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }

    pub fn asObj(self: *ObjNativeFn) *Obj {
        return &self.obj;
    }
};

pub const ObjStruct = struct {
    obj: Obj,
    name: *ObjString,
    field_count: usize,
    methods: []*ObjFunction,

    const Self = @This();

    pub fn create(vm: *Vm, name: *ObjString, field_count: usize, methods: []*ObjFunction) *Self {
        const obj = Obj.allocate(vm, Self, .@"struct");
        obj.name = name;
        obj.field_count = field_count;
        obj.methods = methods;

        if (options.log_gc) std.debug.print("<struct {s}>\n", .{name.chars});

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    // Functions are freed because they are on the main minked list of objects in the VM
    // The memory of the array is owned though
    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.allocator.free(self.methods);
        vm.gc_alloc.destroy(self);
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    parent: *ObjStruct,
    fields: []Value,

    const Self = @This();

    pub fn create(vm: *Vm, parent: *ObjStruct) *Self {
        // Fields first for GC because other wise allocating fields after creation
        // of the instance may trigger GC in between
        const alloc_fields = vm.gc_alloc.alloc(Value, parent.field_count) catch oom();
        const obj = Obj.allocate(vm, Self, .instance);

        obj.parent = parent;
        obj.fields = alloc_fields;

        if (options.log_gc)
            std.debug.print("<instance of {s}>\n", .{parent.name.chars});

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
        allocator.destroy(self);
    }
};

pub const ObjBoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *ObjFunction,

    const Self = @This();

    pub fn create(vm: *Vm, receiver: Value, method: *ObjFunction) *Self {
        const obj = Obj.allocate(vm, Self, .bound_method);

        obj.receiver = receiver;
        obj.method = method;

        if (options.log_gc)
            method.log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};
