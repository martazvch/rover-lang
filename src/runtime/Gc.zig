const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const options = @import("options");

const Obj = @import("Obj.zig");
const Value = @import("values.zig").Value;
const Vm = @import("Vm.zig");

const ObjArray = Obj.ObjArray;
const ObjFunction = Obj.ObjFunction;
const ObjStruct = Obj.ObjStruct;
const ObjInstance = Obj.ObjInstance;
const ObjBoundMethod = Obj.ObjBoundMethod;
const Module = @import("../Pipeline.zig").Module;

vm: *Vm,
parent_allocator: Allocator,
grays: std.ArrayList(*Obj),
bytes_allocated: usize,
next_gc: usize,
active: bool,

const Self = @This();
const GROW_FACTOR = 2;

pub fn init(parent_allocator: Allocator) Self {
    return .{
        .vm = undefined,
        .parent_allocator = parent_allocator,
        .grays = std.ArrayList(*Obj).init(parent_allocator),
        .bytes_allocated = 0,
        .next_gc = 1024 * 1024,
        .active = false,
    };
}

pub fn deinit(self: *Self) void {
    self.grays.deinit();
}

pub fn link(self: *Self, vm: *Vm) void {
    self.vm = vm;
}

pub fn collect(self: *Self) Allocator.Error!void {
    if (options.log_gc) {
        print("\n-- GC begin\n", .{});
    }

    const bytes_before = self.bytes_allocated;

    try self.markRoots();
    try self.traceRef();
    self.sweep();

    self.next_gc = self.bytes_allocated * GROW_FACTOR;

    if (options.log_gc) {
        print("-- GC end\n", .{});
        print(" ->  collected {} bytes (from {} to {}), next at {}\n\n", .{
            bytes_before - self.bytes_allocated,
            bytes_before,
            self.bytes_allocated,
            self.next_gc,
        });
    }
}

fn markRoots(self: *Self) Allocator.Error!void {
    var value = self.vm.stack.values[0..].ptr;

    while (value != self.vm.stack.top) : (value += 1) {
        try self.markValue(&value[0]);
    }

    try self.markModule(self.vm.module);
    for (self.vm.module_chain.items) |mod| {
        try self.markModule(mod);
    }
}

fn markModule(self: *Self, module: *Module) Allocator.Error!void {
    // No need to mark the function, it's never gonna be called. We use only
    // the global symbols
    try self.markArray(module.globals);

    for (module.imports) |*mod| {
        try self.markModule(mod);
    }
}

fn traceRef(self: *Self) Allocator.Error!void {
    while (self.grays.items.len > 0) {
        const obj = self.grays.pop().?;
        try self.blackenObject(obj);
    }
}

fn blackenObject(self: *Self, obj: *Obj) Allocator.Error!void {
    if (options.log_gc) {
        print("  {*} blacken ", .{obj});
        obj.log();
        print("\n", .{});
    }

    switch (obj.kind) {
        .array => {
            const array = obj.as(ObjArray);
            try self.markArray(array.values.items);
        },
        .bound_method => {
            const bound = obj.as(ObjBoundMethod);
            try self.markValue(&bound.receiver);
            try self.markObject(bound.method.asObj());
        },
        .func => {
            const function = obj.as(ObjFunction);
            if (function.name) |name| {
                try self.markObject(name.asObj());
            }
            try self.markArray(&function.chunk.constants);
        },
        .instance => {
            const instance = obj.as(ObjInstance);
            try self.markObject(instance.parent.asObj());
            try self.markArray(instance.fields);
        },
        .@"struct" => {
            const structure = obj.as(ObjStruct);
            try self.markObject(structure.name.asObj());
            try self.markArray(structure.default_values);
            for (structure.methods) |m| {
                try self.markObject(m.asObj());
            }
        },
        .native_fn, .string => {},
    }
}

fn sweep(self: *Self) void {
    var previous: ?*Obj = null;
    var object: ?*Obj = self.vm.objects;

    while (object) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = object;
            object = obj.next;
        } else {
            const unreached = obj;
            object = obj.next;

            if (previous) |prev| {
                prev.next = object;
            } else {
                self.vm.objects = object;
            }

            if (options.log_gc) {
                print("  {*} sweep: ", .{unreached});
                unreached.log();
                print("\n", .{});
            }

            unreached.destroy(self.vm);
        }
    }
}

fn markValue(self: *Self, value: *const Value) Allocator.Error!void {
    if (value.asObj()) |obj|
        try self.markObject(obj);
}

fn markObject(self: *Self, obj: ?*Obj) Allocator.Error!void {
    if (obj) |o| {
        if (o.is_marked) return;

        if (options.log_gc) {
            print("  {*} mark ", .{o});
            o.log();
            print("\n", .{});
        }

        o.is_marked = true;

        try self.grays.append(o);
    }
}

fn markArray(self: *Self, array: []const Value) Allocator.Error!void {
    for (array) |*value|
        try self.markValue(value);
}

/// Calling alloc triggers the GC before allocating
pub fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    self.bytes_allocated += len;
    if (self.active and (self.bytes_allocated > self.next_gc or options.stress_gc)) {
        // TODO: error?
        self.collect() catch return null;
    }

    return self.parent_allocator.rawAlloc(len, alignment, ret_addr);
}

pub fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
    const self: *Self = @ptrCast(@alignCast(ctx));

    // TODO: can never shrink instead of expand?
    self.bytes_allocated += new_len - memory.len;

    if (self.active and (self.bytes_allocated > self.next_gc or options.stress_gc)) {
        self.collect() catch return false;
    }

    return self.parent_allocator.rawResize(memory, alignment, new_len, ret_addr);
}

// TODO: see if this is well implemented
pub fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));

    if (new_len >= memory.len)
        self.bytes_allocated += new_len - memory.len
    else
        self.bytes_allocated -= memory.len - new_len;

    if (self.active and (self.bytes_allocated > self.next_gc or options.stress_gc)) {
        self.collect() catch return null;
    }

    return self.parent_allocator.rawRemap(memory, alignment, new_len, ret_addr);
}

pub fn free(ctx: *anyopaque, buf: []u8, alignment: Alignment, ret_addr: usize) void {
    const self: *Self = @ptrCast(@alignCast(ctx));

    self.bytes_allocated -= buf.len;
    self.parent_allocator.rawFree(buf, alignment, ret_addr);
}

/// Returns the Gc as an allocator
pub fn allocator(self: *Self) Allocator {
    return .{ .ptr = self, .vtable = &.{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    } };
}
