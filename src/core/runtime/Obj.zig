const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Writer = std.Io.Writer;

const options = @import("options");

const TypeId = @import("../analyzer/types.zig").TypeId;
const Chunk = @import("../compiler/Chunk.zig");
const CompiledModule = @import("../compiler/compiler.zig").CompiledModule;
const ffi = @import("../builtins/ffi.zig");
const oom = @import("misc").oom;
const Value = @import("values.zig").Value;
const Vm = @import("Vm.zig");

kind: Kind,
next: ?*Obj,
type_id: TypeId,
is_marked: bool = false,
ref_count: usize = 0,

const Obj = @This();

const Kind = enum {
    array,
    box,
    closure,
    @"enum",
    enum_instance,
    function,
    instance,
    native_fn,
    native_obj,
    string,
    structure,

    pub fn fromType(T: type) Kind {
        return switch (T) {
            Array => .array,
            Box => .box,
            Closure => .closure,
            Enum => .@"enum",
            EnumInstance => .enum_instance,
            Function => .function,
            Instance => .instance,
            NativeFunction => .native_fn,
            NativeObj => .native_obj,
            String => .string,
            Structure => .structure,
            else => @compileError(@typeName(T) ++ " isn't a runtime object type"),
        };
    }
};

// TODO: report a runtime error for OOM
pub fn allocate(vm: *Vm, comptime T: type, type_id: TypeId) *T {
    comptime assert(@hasField(T, "obj"));
    comptime assert(@hasDecl(T, "asObj"));

    const ptr = vm.gc_alloc.create(T) catch oom();
    ptr.obj = .{ .kind = .fromType(T), .next = vm.objects, .type_id = type_id };

    vm.objects = &ptr.obj;

    if (comptime options.log_gc) {
        std.debug.print("{*} allocate {} bytes for: ", .{ ptr, @sizeOf(T) });
    }

    return ptr;
}

/// Another version of allocation but don't register the object into the VM linked list.
/// Used for objects that live for ever like symbols which are created at compile time
/// Dedicated function so that we don't add a bool check at runtime when allocating with `allocate`
// TODO: report a runtime error for OOM
fn allocateComptime(allocator: Allocator, comptime T: type, type_id: TypeId) *T {
    comptime assert(@hasField(T, "obj"));
    comptime assert(@hasDecl(T, "asObj"));

    const ptr = allocator.create(T) catch oom();
    ptr.obj = .{ .kind = .fromType(T), .next = null, .type_id = type_id };

    if (comptime options.log_gc) {
        std.debug.print("{*} allocate {} bytes for: ", .{ ptr, @sizeOf(T) });
    }

    return ptr;
}

pub fn deepCopy(self: *Obj, vm: *Vm) *Obj {
    return switch (self.kind) {
        .array => self.as(Array).deepCopy(vm).asObj(),
        .enum_instance => @panic("TODO"),
        .instance => self.as(Instance).deepCopy(vm).asObj(),
        // Immutable, shallow copy ok
        .box, .closure, .@"enum", .function, .native_fn, .native_obj, .string, .structure => self,
    };
}

pub fn destroy(self: *Obj, vm: *Vm) void {
    switch (self.kind) {
        .array => self.as(Array).deinit(vm),
        .box => self.as(Box).deinit(vm),
        .closure => self.as(Closure).deinit(vm),
        .@"enum" => self.as(Enum).deinit(vm),
        .enum_instance => self.as(EnumInstance).deinit(vm),
        .function => {
            const function = self.as(Function);
            function.deinit(vm);
        },
        .instance => {
            const instance = self.as(Instance);
            instance.deinit(vm.gc_alloc);
        },
        .native_fn => {
            const function = self.as(NativeFunction);
            function.deinit(vm.gc_alloc);
        },
        .native_obj => {
            const object = self.as(NativeObj);
            object.deinit(vm.gc_alloc);
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
                try writer.print("<closure {s}>", .{closure.function.name});
            } else {
                try writer.print("<fn {s}>", .{closure.function.name.chars});
            }
        },
        .@"enum" => try writer.print("<enum {s}>", .{self.as(Enum).name}),
        .enum_instance => {
            const instance = self.as(EnumInstance);
            try writer.print("<enum {s}, tag {}>", .{ instance.parent.name, instance.tag_id });
        },
        .function => {
            const function = self.as(Function);
            try writer.print("<function {s}>", .{function.name});
        },
        .instance => try writer.print("<instance of {s}>", .{self.as(Instance).parent.name}),
        .native_fn => try writer.print("<native fn {s}>", .{self.as(NativeFunction).name}),
        .native_obj => try writer.print("<native object {s}>", .{self.as(NativeObj).name}),
        .string => try writer.print("{s}", .{self.as(String).chars}),
        .structure => try writer.print("<structure {s}>", .{self.as(Structure).name}),
    }
}

pub fn log(self: *Obj) void {
    switch (self.kind) {
        .array => std.debug.print("<array>", .{}),
        .box => std.debug.print("box", .{}),
        .closure => std.debug.print("<closure {s}>", .{self.as(Closure).function.name}),
        .function => std.debug.print("<fn {s}>", .{self.as(Function).name}),
        .instance => std.debug.print("<instance of {s}>", .{self.as(Instance).parent.name}),
        .native_fn => std.debug.print("<native function {s}>", .{self.as(NativeFunction).name}),
        .string => std.debug.print("{s}", .{self.as(String).chars}),
        .structure => std.debug.print("<structure {s}>", .{self.as(Structure).name}),
    }
}

pub fn getFn(self: *Obj, index: usize) Value {
    return switch (self.kind) {
        .@"enum" => self.as(Enum).functions[index],
        .enum_instance => self.as(EnumInstance).parent.functions[index],
        .structure => self.as(Structure).functions[index],
        .instance => self.as(Instance).parent.functions[index],
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
        const obj = Obj.allocate(vm, Self, undefined);
        obj.values = .empty;

        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();

        obj.values.ensureTotalCapacity(vm.gc_alloc, values.len) catch oom();

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
        var values: ArrayList(Value) = .empty;
        values.ensureTotalCapacity(vm.allocator, self.values.items.len) catch oom();

        const index = vm.gc.tmp_roots.items.len;
        defer vm.gc.tmp_roots.shrinkRetainingCapacity(index);

        vm.gc.tmp_roots.ensureUnusedCapacity(vm.allocator, self.values.items.len) catch oom();

        for (self.values.items) |val| {
            const value = val.deepCopy(vm);
            values.appendAssumeCapacity(value);
            if (value.asObj()) |obj| vm.gc.tmp_roots.appendAssumeCapacity(obj);
        }

        return Self.create(vm, values.toOwnedSlice(vm.allocator) catch oom());
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        // We don't own the values, just the array
        self.values.deinit(vm.gc_alloc);
        vm.gc_alloc.destroy(self);
    }
};

pub const String = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    const Self = @This();

    // PERF: flexible array member: https://craftinginterpreters.com/strings.html#challenges
    fn create(vm: *Vm, str: []const u8, hash: u32) *String {
        var obj = Obj.allocate(vm, Self, undefined);
        obj.chars = str;
        obj.hash = hash;

        // The set method can trigger a GC to grow hashmap before
        // inserting. We put the value on the stack so that it is marked
        // as a root
        vm.gc.pushTmpRoot(obj.asObj());
        defer vm.gc.popTmpRoot();
        vm.strings.put(hash, obj) catch oom();

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    /// **Warning**: Meant to be used at compile time only
    pub fn comptimeCopy(vm: *Vm, str: []const u8) *String {
        const hash = String.hashString(str);
        const gop = vm.strings.getOrPut(hash) catch oom();
        if (gop.found_existing) {
            return gop.value_ptr.*;
        }

        const chars = vm.allocator.alloc(u8, str.len) catch oom();
        @memcpy(chars, str);

        var obj = Obj.allocateComptime(vm.allocator, Self, undefined);
        obj.chars = chars;
        obj.hash = hash;

        gop.value_ptr.* = obj;

        return obj;
    }

    // Take a string allocated by calling Vm. If interned already, free
    // the memory and return the interned one
    pub fn take(vm: *Vm, str: []const u8) *String {
        const hash = String.hashString(str);
        if (vm.strings.get(hash)) |interned| {
            vm.gc_alloc.free(str);
            return interned;
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
    chunk: Chunk,
    name: []const u8,
    module_index: usize,
    // defaults: []u8,

    const Self = @This();

    // pub fn create(allocator: Allocator, name: []const u8, type_id: TypeId, defaults: usize, module_index: usize) *Self {
    pub fn create(allocator: Allocator, name: []const u8, type_id: TypeId, module_index: usize) *Self {
        const obj = Obj.allocateComptime(allocator, Self, type_id);
        obj.chunk = .empty;
        obj.name = allocator.dupe(u8, name) catch oom();
        // obj.defaults = allocator.alloc(u8, defaults) catch oom();
        obj.module_index = module_index;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        self.chunk.deinit(vm.allocator);
        // vm.allocator.free(self.defaults);
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
        const obj = Obj.allocate(vm, Self, undefined);
        vm.gc.pushTmpRoot(&obj.obj);
        defer vm.gc.popTmpRoot();

        // Fix the size to zero to avoid GC bug
        obj.captures.len = 0;
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
        const obj = Obj.allocate(vm, Self, undefined);
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
    name: []const u8,
    function: ffi.ZigFn,

    const Self = @This();

    pub fn create(allocator: Allocator, name: []const u8, function: ffi.ZigFn) *Self {
        const obj = Obj.allocateComptime(allocator, Self, undefined);
        obj.name = name;
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
    name: []const u8,
    // defaults: []u8,
    field_count: usize,
    functions: []Value,

    const Self = @This();

    // pub fn create(allocator: Allocator, name: []const u8, type_id: TypeId, defaults: usize, field_count: usize, functions: []Value) *Self {
    pub fn create(allocator: Allocator, name: []const u8, type_id: TypeId, field_count: usize, functions: []Value) *Self {
        const obj = Obj.allocateComptime(allocator, Self, type_id);
        obj.name = allocator.dupe(u8, name) catch oom();
        // obj.defaults = allocator.alloc(u8, defaults) catch oom();
        obj.field_count = field_count;
        obj.functions = functions;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    // Functions aren't freed because they are on the main linked list of objects in the VM
    // The memory of the array is owned though
    pub fn deinit(self: *Self, vm: *Vm) void {
        // vm.allocator.free(self.defaults);
        vm.allocator.free(self.functions);
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
        const obj = Obj.allocate(vm, Self, undefined);

        obj.parent = parent;
        obj.fields = alloc_fields;
        obj.asObj().type_id = parent.asObj().type_id;

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

        obj.fields.len = 0;
        for (self.fields, 0..) |*field, i| {
            const value = field.deepCopy(vm);
            obj.fields.len += 1;
            obj.fields[i] = value;
        }

        return obj;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.free(self.fields);
        allocator.destroy(self);
    }
};

pub const Enum = struct {
    obj: Obj,
    name: []const u8,
    // tags: []const []const u8,
    functions: []Value,

    const Self = @This();

    pub fn create(allocator: Allocator, name: []const u8, functions: []Value) *Self {
        const obj = Obj.allocateComptime(allocator, Self, undefined);
        obj.name = allocator.dupe(u8, name) catch oom();
        // obj.tags = tags;
        obj.functions = functions;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    // Functions aren't freed because they are on the main linked list of objects in the VM
    // The memory of the array is owned though
    pub fn deinit(self: *Self, vm: *Vm) void {
        // for (self.tags) |tag| {
        //     vm.allocator.free(tag);
        // }
        // vm.allocator.free(self.tags);
        // vm.allocator.free(self.functions);
        vm.gc_alloc.destroy(self);
    }
};

pub const EnumInstance = struct {
    obj: Obj,
    parent: *const Enum,
    tag_id: u8,
    payload: Value,

    const Self = @This();

    pub fn create(allocator: Allocator, parent: *const Enum, tag_id: u8, payload: Value) *Self {
        const obj = Obj.allocateComptime(allocator, Self, undefined);
        obj.parent = parent;
        obj.tag_id = tag_id;
        obj.payload = payload;

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn deinit(self: *Self, vm: *Vm) void {
        vm.gc_alloc.destroy(self);
    }
};

pub const NativeObj = struct {
    obj: Obj,
    name: []const u8,
    child: *anyopaque,
    // info: ffi.ZigStruct,

    const Self = @This();

    // pub fn create(vm: *Vm, name: []const u8, child: *anyopaque, info: ffi.ZigStruct) *Self {
    pub fn create(vm: *Vm, name: []const u8, child: *anyopaque) *Self {
        // Fields first for GC because other wise allocating fields after creation
        // of the instance may trigger GC in between
        const obj = Obj.allocate(vm, Self, undefined);
        obj.name = name;
        obj.child = child;
        // obj.info = info;

        if (options.log_gc) obj.asObj().log();

        return obj;
    }

    pub fn asObj(self: *Self) *Obj {
        return &self.obj;
    }

    pub fn getFunc(self: *Self, index: usize) Value {
        _ = self; // autofix
        _ = index; // autofix
        unreachable;
    }

    // TODO: destroy child too?
    pub fn deinit(self: *Self, allocator: Allocator) void {
        allocator.destroy(self);
    }
};
