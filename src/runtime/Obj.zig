const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;

const options = @import("options");

const Chunk = @import("../backend/Chunk.zig");
const CompiledModule = @import("../backend/compiler.zig").CompiledModule;
const NativeFn = @import("../std/meta.zig").NativeFn;
const oom = @import("../utils.zig").oom;
const Value = @import("values.zig").Value;
const Vm = @import("Vm.zig");

kind: Kind,
next: ?*Obj,
is_marked: bool = false,
ref_count: usize = 0,

const Obj = @This();

const Kind = enum {
    array,
    box,
    closure,
    function,
    instance,
    module,
    native_fn,
    string,
    structure,
};

pub fn allocate(vm: *Vm, comptime T: type, kind: Kind) *T {
    comptime assert(@hasField(T, "obj"));
    comptime assert(@hasDecl(T, "asObj"));

    const ptr = vm.gc_alloc.create(T) catch oom();
    ptr.obj = .{ .kind = kind, .next = vm.objects };

    vm.objects = &ptr.obj;

    if (comptime options.log_gc) {
        std.debug.print("{*} allocate {} bytes for: ", .{ ptr, @sizeOf(T) });
    }

    return ptr;
}

pub fn deepCopy(self: *Obj, vm: *Vm) *Obj {
    return switch (self.kind) {
        .array => self.as(Array).deepCopy(vm).asObj(),
        .instance => self.as(Instance).deepCopy(vm).asObj(),
        // Immutable, shallow copy ok
        .box, .closure, .function, .module, .native_fn, .string, .structure => self,
    };
}

pub fn destroy(self: *Obj, vm: *Vm) void {
    switch (self.kind) {
        .array => self.as(Array).deinit(vm),
        .box => self.as(Box).deinit(vm),
        .closure => self.as(Closure).deinit(vm),
        .function => {
            const function = self.as(Function);
            function.deinit(vm);
        },
        .instance => {
            const instance = self.as(Instance);
            instance.deinit(vm.gc_alloc);
        },
        .module => self.as(Module).deinit(vm.gc_alloc),
        .native_fn => {
            const function = self.as(NativeFunction);
            function.deinit(vm.gc_alloc);
        },
        .string => self.as(String).deinit(vm.gc_alloc),
        .structure => {
            const structure = self.as(Structure);
            structure.deinit(vm);
        },
    }
}

pub inline fn as(self: *Obj, comptime T: type) *T {
    comptime assert(@hasField(T, "obj"));

    return @alignCast(@fieldParentPtr("obj", self));
}

pub fn print(self: *Obj, writer: *Writer) Writer.Error!void {
    switch (self.kind) {
        .array => {
            const array = self.as(Array);
            try writer.writeAll("[");
            for (array.values.items, 0..) |val, i| {
                val.print(writer);
                if (i < array.values.items.len - 1) try writer.writeAll(", ");
            }
            try writer.writeAll("]");
        },
        .box => {
            const box = self.as(Box);
            try writer.writeAll("Box ");
            box.value.print(writer);
        },
        .closure => {
            const closure = self.as(Closure);

            if (comptime @import("builtin").mode == .Debug) {
                try writer.print("<closure {s}>", .{closure.function.name.chars});
            } else {
                try writer.print("<fn {s}>", .{closure.function.name.chars});
            }
        },
        .function => {
            const function = self.as(Function);
            try writer.print("<function {s}>", .{function.name.chars});
        },
        .instance => try writer.print("<instance of {s}>", .{self.as(Instance).parent.name.chars}),
        .module => try writer.print("<module {s}>", .{self.as(Module).module.name}),
        .native_fn => try writer.print("<native fn>", .{}),
        .string => try writer.print("{s}", .{self.as(String).chars}),
        .structure => try writer.print("<structure {s}>", .{self.as(Structure).name.chars}),
    }
}

pub fn log(self: *Obj) void {
    switch (self.kind) {
        .array => std.debug.print("<array>", .{}),
        .box => std.debug.print("box", .{}),
        .closure => std.debug.print("<closure {s}>", .{self.as(Closure).function.name.chars}),
        .function => std.debug.print("<fn {s}>", .{self.as(Function).name.chars}),
        .instance => std.debug.print("<instance of {s}>", .{self.as(Instance).parent.name.chars}),
        .module => std.debug.print("<module {s}>", .{self.as(Module).module.name}),
        .native_fn => std.debug.print("<native fn>", .{}),
        .string => std.debug.print("{s}", .{self.as(String).chars}),
        .structure => std.debug.print("<structure {s}>", .{self.as(Structure).name.chars}),
    }
}

pub fn loadDefaultValues(self: *Obj, vm: *Vm, index: usize) void {
    vm.r3 = switch (self.kind) {
        .function => self.as(Function).default_values,
        .instance => self.as(Instance).parent.methods[index].default_values,
        .module => self.as(Module).module.globals[index].obj.as(Function).default_values,
        .structure => self.as(Structure).default_values,
        else => unreachable,
    };
}

pub fn invoke(self: *Obj, vm: *Vm, index: usize) struct { *Function, bool } {
    return switch (self.kind) {
        .instance => .{ self.as(Instance).parent.methods[index], false },
        .module => b: {
            const module = self.as(Module).module;
            vm.updateModule(module);
            break :b .{ module.globals[index].obj.as(Function), true };
        },
        .structure => .{ self.as(Structure).methods[index], false },
        else => unreachable,
    };
}

pub fn structLiteral(self: *Obj, vm: *Vm) *Instance {
    return switch (self.kind) {
        .structure => Instance.create(vm, self.as(Structure)),
        else => unreachable,
    };
}

pub const Array = struct {
    obj: Obj,
    values: ArrayList(Value),

    const Self = @This();

    pub fn create(vm: *Vm, values: []Value) *Self {
        const obj = Obj.allocate(vm, Self, .array);
        obj.values = .{};
        obj.values.ensureTotalCapacity(vm.allocator, values.len) catch oom();
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();

        for (values) |val| {
            obj.values.appendAssumeCapacity(val);
        }

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deepCopy(self: *const Self, vm: *Vm) *Self {
        return Self.create(vm, self.values.items);
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        // We don't own the values, just the array
        self.values.deinit(vm.allocator);
        vm.allocator.destroy(self);
    }
};

pub const String = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    const Self = @This();

    // PERF: flexible array member: https://craftinginterpreters.com/strings.html#challenges
    fn create(vm: *Vm, str: []const u8, hash: u32) *String {
        var obj = Obj.allocate(vm, Self, .string);
        obj.chars = str;
        obj.hash = hash;

        // The set method can trigger a GC to grow hashmap before
        // inserting. We put the value on the stack so that it is marked
        // as a root
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();
        _ = vm.strings.set(obj, Value.null_);

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn copy(vm: *Vm, str: []const u8) *String {
        const hash = String.hashString(str);
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

        return String.create(vm, chars, hash);
    }

    // Take a string allocated by calling Vm. If interned already, free
    // the memory and return the interned one
    pub fn take(vm: *Vm, str: []const u8) *String {
        const hash = String.hashString(str);
        const interned = vm.strings.findString(str, hash);

        if (interned) |i| {
            vm.gc_alloc.free(str);
            return i;
        }

        return String.create(vm, str, hash);
    }

    pub fn asObj(self: *String) *Obj {
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

pub const Function = struct {
    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: *String,
    default_values: []Value,
    module_index: usize,

    const Self = @This();

    pub fn create(vm: *Vm, name: *String, default_count: usize, module_index: usize) *Self {
        const obj = Obj.allocate(vm, Self, .function);
        obj.arity = 0;
        obj.chunk = Chunk.init(vm.allocator);
        obj.name = name;
        obj.default_values = vm.allocator.alloc(Value, default_count) catch oom();
        obj.module_index = module_index;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        self.chunk.deinit(vm.allocator);
        vm.allocator.free(self.default_values);

        // Name already in the linked list, don't free manually
        vm.gc_alloc.destroy(self);
    }
};

pub const Closure = struct {
    obj: Obj,
    function: *Function,
    captures: []Value,

    const Self = @This();

    pub fn create(vm: *Vm, function: *Function, captures: []Value) *Self {
        const obj = Obj.allocate(vm, Self, .closure);
        obj.function = function;
        obj.captures = vm.gc_alloc.alloc(Value, captures.len) catch oom();
        @memcpy(obj.captures, captures);

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.free(self.captures);
        vm.gc_alloc.destroy(self);
    }
};

pub const Box = struct {
    obj: Obj,
    value: Value,

    const Self = @This();

    pub fn create(vm: *Vm, value: Value) *Self {
        const obj = Obj.allocate(vm, Self, .box);
        obj.value = value;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }
};

pub const NativeFunction = struct {
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

    pub fn asObj(self: *NativeFunction) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};

pub const Structure = struct {
    obj: Obj,
    name: *String,
    field_count: usize,
    default_values: []Value,
    methods: []*Function,

    const Self = @This();

    pub fn create(vm: *Vm, name: *String, field_count: usize, default_count: usize, methods: []*Function) *Self {
        const obj = Obj.allocate(vm, Self, .structure);
        obj.name = name;
        obj.field_count = field_count;
        obj.default_values = vm.allocator.alloc(Value, default_count) catch oom();
        obj.methods = methods;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    // Functions are freed because they are on the main minked list of objects in the VM
    // The memory of the array is owned though
    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.allocator.free(self.methods);
        vm.allocator.free(self.default_values);
        vm.gc_alloc.destroy(self);
    }
};

pub const Instance = struct {
    obj: Obj,
    parent: *Structure,
    fields: []Value,

    const Self = @This();

    pub fn create(vm: *Vm, parent: *Structure) *Self {
        // Fields first for GC because other wise allocating fields after creation
        // of the instance may trigger GC in between
        const alloc_fields = vm.gc_alloc.alloc(Value, parent.field_count) catch oom();
        const obj = Obj.allocate(vm, Self, .instance);

        obj.parent = parent;
        obj.fields = alloc_fields;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deepCopy(self: *Self, vm: *Vm) *Self {
        var obj = Self.create(vm, self.parent);
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();

        for (self.fields, 0..) |*field, i| {
            obj.fields[i] = field.deepCopy(vm);
        }

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
        allocator.destroy(self);
    }
};

pub const Module = struct {
    obj: Obj,
    module: *CompiledModule,

    const Self = @This();

    pub fn create(vm: *Vm, module: *CompiledModule) *Self {
        const obj = Obj.allocate(vm, Self, .module);
        obj.module = module;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};
