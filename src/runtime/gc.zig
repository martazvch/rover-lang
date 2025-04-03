const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const options = @import("options");

const Obj = @import("obj.zig").Obj;
const Value = @import("values.zig").Value;
const Vm = @import("vm.zig").Vm;

const ObjFunction = @import("obj.zig").ObjFunction;
const ObjStruct = @import("obj.zig").ObjStruct;
// const ObjInstance = @import("obj.zig").ObjInstance;
// const ObjBoundMethod = @import("obj.zig").ObjBoundMethod;
// const Table = @import("table.zig").Table;
// const Compiler = @import("compiler.zig").Compiler;

pub const Gc = struct {
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

    pub fn collect_garbage(self: *Self) Allocator.Error!void {
        if (options.log_gc) {
            print("\n-- GC begin\n", .{});
        }

        const bytes_before = self.bytes_allocated;

        try self.mark_roots();
        try self.trace_reference();
        self.sweep();

        self.next_gc = self.bytes_allocated * Gc.GROW_FACTOR;

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

    fn mark_roots(self: *Self) Allocator.Error!void {
        var value = self.vm.stack.values[0..].ptr;

        while (value != self.vm.stack.top) : (value += 1) {
            try self.mark_value(&value[0]);
        }

        for (self.vm.frame_stack.frames[0..self.vm.frame_stack.count]) |*frame| {
            try self.mark_object(frame.function.as_obj());
        }
    }

    fn trace_reference(self: *Self) Allocator.Error!void {
        while (self.grays.items.len > 0) {
            const obj = self.grays.pop().?;
            try self.blacken_object(obj);
        }
    }

    fn blacken_object(self: *Self, obj: *Obj) Allocator.Error!void {
        if (options.log_gc) {
            print("  {*} blacken ", .{obj});
            obj.log();
            print("\n", .{});
        }

        switch (obj.kind) {
            // .BoundMethod => {
            //     const bound = obj.as(ObjBoundMethod);
            //     try self.mark_value(&bound.receiver);
            //     try self.mark_object(bound.method.as_obj());
            // },
            .Fn => {
                const function = obj.as(ObjFunction);
                if (function.name) |name| {
                    try self.mark_object(name.as_obj());
                }
                try self.mark_array(&function.chunk.constants);
            },
            // .Instance => {
            //     const instance = obj.as(ObjInstance);
            //     try self.mark_object(instance.parent.name.as_obj());
            //     try self.mark_table(&instance.fields);
            // },
            .Struct => {
                const structure = obj.as(ObjStruct);
                try self.mark_object(structure.name.as_obj());
                // try self.mark_table(&structure.methods);
            },
            // .NativeFn, .String, .Iter => {},
            .NativeFn, .String => {},
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

    fn mark_value(self: *Self, value: *Value) Allocator.Error!void {
        if (value.as_obj()) |obj| try self.mark_object(obj);
    }

    // pub because called once by the compiler
    pub fn mark_object(self: *Self, obj: ?*Obj) Allocator.Error!void {
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

    // fn mark_array(self: *Self, array: *std.ArrayList(Value)) Allocator.Error!void {
    fn mark_array(self: *Self, array: []Value) Allocator.Error!void {
        for (array) |*value|
            try self.mark_value(value);
    }

    /// Calling alloc triggers the GC before allocating
    pub fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.bytes_allocated += len;
        if (self.active and (self.bytes_allocated > self.next_gc or options.stress_gc)) {
            self.collect_garbage() catch return null;
        }

        return self.parent_allocator.rawAlloc(len, alignment, ret_addr);
    }

    pub fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));

        // TODO: can never shrink instead of expand?
        self.bytes_allocated += new_len - memory.len;

        if (self.active and (self.bytes_allocated > self.next_gc or options.stress_gc)) {
            self.collect_garbage() catch return false;
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
            self.collect_garbage() catch return null;
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
};
