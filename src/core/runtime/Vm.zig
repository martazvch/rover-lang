const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const options = @import("options");

const OpCode = @import("../compiler/Chunk.zig").OpCode;
const CompiledModule = @import("../compiler/compiler.zig").CompiledModule;
const Disassembler = @import("../compiler/Disassembler.zig");
const oom = @import("misc").oom;
const Gc = @import("Gc.zig");
const Obj = @import("Obj.zig");
const Value = @import("values.zig").Value;
const State = @import("../pipeline/State.zig");

gc: Gc,
stack: Stack,
frame_stack: FrameStack,
ip: [*]u8,
allocator: Allocator,
arena_comptime: std.heap.ArenaAllocator,
gc_alloc: Allocator,
strings: std.AutoHashMap(usize, *Obj.String),
objects: ?*Obj,
modules: []CompiledModule,
natives: []Value,

/// Holds default variables of current call
// TODO: put inside the frame too
r3: []Value = undefined,

const Self = @This();
const Error = error{StackOverflow} || Allocator.Error;

pub fn init(self: *Self, allocator: Allocator, state: *const State) void {
    self.arena_comptime = .init(allocator);
    self.allocator = self.arena_comptime.allocator();

    // TODO: pass an ObjectPoolAlloc?
    self.gc = .init(self, allocator);
    self.gc_alloc = self.gc.allocator();
    self.stack.init();
    self.strings = .init(self.allocator);
    self.stack = .empty;
    self.stack.init();
    self.frame_stack = .empty;
    self.objects = null;

    self.createNatives(state);

    // In REPL mode, we won't call the main function (there is not)
    // so we increment ourself the frame stack (discaring the first one)
    // but the count is coherent of what is expected below, for example
    // for function call we exit if the frame stack count == 1. In REPL
    // it would be always true
    if (state.config.embedded) {
        self.frame_stack.count += 1;
    }
}

fn createNatives(self: *Self, state: *const State) void {
    self.natives = self.allocator.alloc(Value, state.native_reg.funcs.items.len) catch oom();

    for (state.native_reg.funcs.items, 0..) |func, i| {
        self.natives[i] = .makeObj(Obj.NativeFunction.create(self, func.name, func.func).asObj());
    }
}

pub fn deinit(self: *Self) void {
    self.arena_comptime.deinit();
    self.gc.deinit();
    self.freeObjects();
}

fn freeObjects(self: *Self) void {
    var object = self.objects;
    while (object) |obj| {
        const next = obj.next;
        obj.destroy(self);
        object = next;
    }
}

/// Returns the error gave in `kind` parameter and prints backtrace
fn err(self: *Self, kind: Error) Error {
    for (0..self.frame_stack.count) |i| {
        const idx = self.frame_stack.count - i - 1;
        const frame = self.frame_stack.frames[idx];
        const function = frame.function;
        const instr = self.instructionNb();

        var buf: [1024]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&buf);
        const stderr = &stderr_writer.interface;
        defer stderr.flush() catch oom();

        stderr.print("[line {}] in ", .{function.chunk.offsets.items[instr]});

        if (function.name) |name| {
            stderr.print("{s}()\n", .{name});
        } else stderr.print("script\n", .{});
    }

    return kind;
}

fn instructionNb(self: *const Self) usize {
    const frame = &self.frame_stack.frames[self.frame_stack.count - 1];
    const addr1 = @intFromPtr(frame.ip);
    const addr2 = @intFromPtr(frame.function.chunk.code.items.ptr);
    return addr1 - addr2;
}

pub fn run(self: *Self, module: CompiledModule, modules: []CompiledModule) !void {
    self.modules = modules;
    self.gc.active = true;
    try self.execute(&module);
}

fn execute(self: *Self, entry_module: *const CompiledModule) !void {
    var buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buf);
    const stdout = &stdout_writer.interface;

    var frame = try self.frame_stack.new();
    frame.initCall(entry_module.function.asObj(), &self.stack, 0, self.modules);

    while (true) {
        if (comptime options.print_stack) {
            // TODO: return an internal error?
            self.stack.print(stdout, frame) catch oom();
        }

        if (comptime options.print_instr) {
            var dis = Disassembler.init(&frame.function.chunk, frame.module, .normal);
            const instr_nb = self.instructionNb();
            _ = dis.disInstruction(stdout, instr_nb);
        }

        const instruction = frame.readByte();
        const op: OpCode = @enumFromInt(instruction);

        switch (op) {
            .add_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float += rhs;
            },
            .add_int => {
                const rhs = self.stack.pop().int;
                self.stack.peekRef(0).int += rhs;
            },
            .array_new => {
                const len = frame.readByte();
                const array = Obj.Array.create(self, (self.stack.top - len)[0..len]);
                self.stack.top -= len;
                self.stack.push(Value.makeObj(array.asObj()));
            },
            .array_get => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const final = checkArrayIndex(array.values.items.len, index);
                self.stack.push(array.values.items[final]);
            },
            .array_get_chain => {
                const depth = frame.readByte();
                var array = self.stack.peekRef(depth).obj.as(Obj.Array);

                for (0..depth - 1) |i| {
                    const index = checkArrayIndex(array.values.items.len, self.stack.peek(i).int);
                    array = array.values.items[index].obj.as(Obj.Array);
                }

                const index = checkArrayIndex(array.values.items.len, self.stack.peek(depth - 1).int);
                const value = array.values.items[index];

                self.stack.peekRef(depth).* = value;
                self.stack.top -= depth;
            },
            .array_get_chain_cow => {
                const depth = frame.readByte();
                var value = self.stack.peek(depth);
                var array = value.obj.as(Obj.Array);

                for (0..depth - 1) |i| {
                    const index = checkArrayIndex(array.values.items.len, self.stack.peek(i).int);
                    value = array.values.items[index];
                    value.obj = self.cow(value.obj);
                    array = value.obj.as(Obj.Array);
                }

                const index = checkArrayIndex(array.values.items.len, self.stack.peek(depth - 1).int);
                array.values.items[index].obj = self.cow(array.values.items[index].obj);

                self.stack.peekRef(depth).* = array.values.items[index];
                self.stack.top -= depth;
            },
            // PERF: don't pop, just decrement rsp (check all pops, maybe they aren't usefull at all)
            .array_set => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const value = self.stack.pop();

                const final = checkArrayIndex(array.values.items.len, index);
                array.values.items[final] = value;
            },
            .array_set_chain => {
                // Array on top of stack already cowed
                const depth = frame.readByte();
                var array = self.stack.peekRef(depth).obj.as(Obj.Array);

                for (0..depth - 1) |i| {
                    const index = checkArrayIndex(array.values.items.len, self.stack.peek(i).int);
                    var value = array.values.items[index];
                    value.obj = self.cow(value.obj);
                    array = value.obj.as(Obj.Array);
                }

                const index = checkArrayIndex(array.values.items.len, self.stack.peek(depth - 1).int);
                array.values.items[index] = self.stack.peek(depth + 1);
                self.stack.top -= depth + 2;
            },
            .box => {
                const to_box = self.stack.pop();
                const boxed = Value.makeObj(Obj.Box.create(self, to_box).asObj());
                self.stack.push(boxed);
            },
            .call => {
                const args_count = frame.readByte();
                frame = try self.frame_stack.newKeepMod();
                frame.initCall(self.stack.peekRef(args_count).obj, &self.stack, args_count, self.modules);
            },
            .call_native => {
                const args_count = frame.readByte();
                const native = self.stack.peekRef(args_count).obj.as(Obj.NativeFunction).function;
                const result = native.call((self.stack.top - args_count)[0..args_count]);

                self.stack.top -= args_count + 1;
                if (result) |res| self.stack.push(res);
            },
            .closure => {
                const captures_count = frame.readByte();
                const closure = Obj.Closure.create(
                    self,
                    self.stack.peekRef(captures_count).obj.as(Obj.Function),
                    (self.stack.top - captures_count)[0..captures_count],
                );
                // Discard the function
                self.stack.top -= captures_count + 1;
                self.stack.push(Value.makeObj(closure.asObj()));
            },
            .constant => self.stack.push(frame.readConstant()),
            .def_global => {
                const idx = frame.readByte();
                frame.module.globals[idx] = self.stack.pop();
            },
            .div_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float /= rhs;
            },
            .div_int => {
                const rhs = self.stack.pop().int;
                const lhs = self.stack.pop().int;
                self.stack.push(Value.makeInt(@divTrunc(lhs, rhs)));
            },
            .enum_create => {
                self.stack.peekRef(0).* = .makeObj(Obj.EnumInstance.create(
                    self.allocator,
                    self.stack.peek(0).obj.as(Obj.Enum),
                    frame.readByte(),
                    .null,
                ).asObj());
            },
            .eq_bool => self.stack.push(Value.makeBool(self.stack.pop().bool == self.stack.pop().bool)),
            .eq_float => self.stack.push(Value.makeBool(self.stack.pop().float == self.stack.pop().float)),
            .eq_int => self.stack.push(Value.makeBool(self.stack.pop().int == self.stack.pop().int)),
            .eq_null => self.stack.push(Value.makeBool(self.stack.pop() == .null)),
            .eq_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(Obj.String) == self.stack.pop().obj.as(Obj.String))),
            .exit_repl => {
                // Here, there is no value to pop for now, no implicit null is
                // put on top of the stack
                self.frame_stack.count -= 1;
                break;
            },
            .ge_float => self.stack.push(Value.makeBool(self.stack.pop().float <= self.stack.pop().float)),
            .ge_int => self.stack.push(Value.makeBool(self.stack.pop().int <= self.stack.pop().int)),
            .get_capt_frame => {
                // Get a capture in the current call frame, not on stack
                const index = frame.readByte();
                self.stack.push(frame.captures[index]);
            },
            .get_capt_local => {
                // Local captured are determined by their index relative to start of call frame
                const index = frame.readByte();
                self.stack.push((frame.slots + index)[0]);
            },
            .get_default => self.stack.push(self.r3[frame.readByte()]),
            .get_field => {
                const field_idx = frame.readByte();
                self.stack.peekRef(0).* = self.stack.peekRef(0).obj.as(Obj.Instance).fields[field_idx];
            },
            .get_field_cow => {
                const field_idx = frame.readByte();
                const field = &self.stack.peekRef(0).obj.as(Obj.Instance).fields[field_idx];
                field.obj = self.cow(field.obj);
                self.stack.peekRef(0).* = field.*;
            },
            .get_global => {
                const idx = frame.readByte();
                self.stack.push(frame.module.globals[idx]);
            },
            .get_global_cow => {
                const idx = frame.readByte();
                const value = &frame.module.globals[idx];
                value.obj = self.cow(value.obj);
                self.stack.push(value.*);
            },
            // TODO: see if same compiler bug as get_global
            .get_local => self.stack.push(frame.slots[frame.readByte()]),
            .get_local_cow => {
                const index = frame.readByte();
                const value = &frame.slots[index];
                value.obj = self.cow(value.obj);
                self.stack.push(value.*);
            },
            .get_method => {
                const index = frame.readByte();
                self.stack.push(self.stack.peek(0));
                self.stack.peekRef(1).* = Value.makeObj(self.stack.peekRef(0).obj.as(Obj.Instance).parent.methods[index].asObj());
                self.stack.peekRef(1).obj.loadDefaultValues(self, 0);
            },
            // TODO: same as above
            .get_static_method => {
                const method_idx = frame.readByte();
                const top = self.stack.peekRef(0);
                const structure = top.obj.as(Obj.Structure);
                const method = structure.methods[method_idx];
                top.* = Value.makeObj(method.asObj());
                self.stack.peekRef(0).obj.loadDefaultValues(self, 0);
            },
            .get_tag => self.stack.peekRef(0).* = .makeInt(self.stack.peek(0).obj.as(Obj.EnumInstance).tag_id),
            .gt_float => self.stack.push(Value.makeBool(self.stack.pop().float < self.stack.pop().float)),
            .gt_int => self.stack.push(Value.makeBool(self.stack.pop().int < self.stack.pop().int)),
            .incr_ref => self.stack.peekRef(0).obj.ref_count += 1,
            .is_bool => self.stack.peekRef(0).* = .makeBool(self.stack.peek(0) == .bool),
            .is_float => self.stack.peekRef(0).* = .makeBool(self.stack.peek(0) == .float),
            .is_int => self.stack.peekRef(0).* = .makeBool(self.stack.peek(0) == .int),
            .is_str => {
                const top = self.stack.peekRef(0);
                if (top.* != .obj) top.* = .false_;
                self.stack.peekRef(0).* = .makeBool(top.obj.kind == .string);
            },
            .is_type => {
                const type_id = frame.readByte();
                self.stack.push(.makeBool(self.stack.peek(0).obj.type_id == type_id));
            },
            .jump => {
                const jump = frame.readShort();
                frame.ip += jump;
            },
            .jump_false => {
                const jump = frame.readShort();
                if (!self.stack.peek(0).bool) frame.ip += jump;
            },
            .jump_true => {
                const jump = frame.readShort();
                if (self.stack.peek(0).bool) frame.ip += jump;
            },
            .lt_float => self.stack.push(Value.makeBool(self.stack.pop().float > self.stack.pop().float)),
            .lt_int => self.stack.push(Value.makeBool(self.stack.pop().int > self.stack.pop().int)),
            .le_float => self.stack.push(Value.makeBool(self.stack.pop().float >= self.stack.pop().float)),
            .le_int => self.stack.push(Value.makeBool(self.stack.pop().int >= self.stack.pop().int)),

            .load_blk_val => self.stack.push(frame.blk_val),

            .load_extern_sym => {
                const module_index = frame.readByte();
                const symbol_index = frame.readByte();
                const module = self.modules[module_index];
                const symbol = module.symbols[symbol_index];
                self.stack.push(symbol);
                self.stack.peekRef(0).obj.loadDefaultValues(self, 0);
            },
            .load_builtin => {
                const symbol_idx = frame.readByte();
                self.stack.push(self.natives[symbol_idx]);
            },
            .load_sym => {
                const symbol_idx = frame.readByte();
                self.stack.push(frame.module.symbols[symbol_idx]);
                self.stack.peekRef(0).obj.loadDefaultValues(self, 0);
            },

            .loop => {
                const jump = frame.readShort();
                frame.ip -= jump;
            },
            .mul_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float *= rhs;
            },
            .mul_int => {
                const rhs = self.stack.pop().int;
                self.stack.peekRef(0).int *= rhs;
            },
            // PERF: no push/pop, only pointer manipulation (same for eq_)
            .ne_bool => self.stack.push(Value.makeBool(self.stack.pop().bool != self.stack.pop().bool)),
            .ne_int => self.stack.push(Value.makeBool(self.stack.pop().int != self.stack.pop().int)),
            .ne_float => self.stack.push(Value.makeBool(self.stack.pop().float != self.stack.pop().float)),
            .ne_null => self.stack.push(Value.makeBool(self.stack.pop() != .null)),
            .ne_null_push => self.stack.push(Value.makeBool(self.stack.peek(0) != .null)),
            .ne_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(Obj.String) != self.stack.pop().obj.as(Obj.String))),
            .neg_float => self.stack.peekRef(0).float *= -1,
            .neg_int => self.stack.peekRef(0).int *= -1,
            .not => self.stack.peekRef(0).not(),
            .pop => _ = self.stack.pop(),
            .print => {
                self.stack.pop().print(stdout);
                stdout.writeAll("\n") catch oom();
                stdout.flush() catch oom();
            },
            .push_false => self.stack.push(Value.false_),
            .push_null => self.stack.push(Value.null_),
            .push_true => self.stack.push(Value.true_),
            .ret => {
                const result = self.stack.pop();
                self.frame_stack.count -= 1;

                // The last standing frame is the artificial one created when we run
                // the global scope at the very beginning
                // TODO: avoid logic at runtime, just emit a special OpCode for `main` return
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = frame.slots - 1;
                self.stack.push(result);

                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .ret_naked => {
                self.frame_stack.count -= 1;

                // The last standing frame is the artificial one created when we run
                // the global scope at the very beginning
                // TODO: avoid logic at runtime, just emit a special OpCode for `main` naked return
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = frame.slots - 1;
                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .set_field => {
                const field_idx = frame.readByte();
                const instance = self.stack.pop().obj.as(Obj.Instance);
                const value = self.stack.pop();
                instance.fields[field_idx] = value;
            },
            .set_global => {
                const idx = frame.readByte();
                frame.module.globals[idx] = self.stack.pop();
            },
            .set_local => frame.slots[frame.readByte()] = self.stack.pop(),
            .set_local_box => {
                const index = frame.readByte();
                frame.slots[index].obj.as(Obj.Box).value = self.stack.pop();
            },

            .store_blk_val => frame.blk_val = self.stack.pop(),

            .str_cat => self.strConcat(),
            .str_mul => self.strMul(self.stack.peekRef(0).obj.as(Obj.String), self.stack.peekRef(1).int),
            .struct_lit => {
                const arity = frame.readByte();
                // PERF: do we need a function call?
                var instance = self.stack.peekRef(arity).obj.structLiteral(self);

                for (0..arity) |i| {
                    instance.fields[i] = self.stack.peek(arity - i - 1);
                }

                self.stack.peekRef(arity).* = Value.makeObj(instance.asObj());
                self.stack.top -= arity;
            },
            .sub_float => {
                const rhs = self.stack.pop().float;
                self.stack.peekRef(0).float -= rhs;
            },
            .sub_int => {
                const rhs = self.stack.pop().int;
                self.stack.peekRef(0).int -= rhs;
            },
            .unbox => self.stack.peekRef(0).* = self.stack.peekRef(0).obj.as(Obj.Box).value,
        }
    }
}

/// Checks clone on write
/// TODO: move this to Obj
fn cow(self: *Self, obj: *Obj) *Obj {
    if (obj.ref_count > 0) {
        obj.ref_count -= 1;
        return obj.deepCopy(self);
    }

    return obj;
}

fn strConcat(self: *Self) void {
    const s2 = self.stack.peekRef(0).obj.as(Obj.String);
    const s1 = self.stack.peekRef(1).obj.as(Obj.String);

    const res = self.gc_alloc.alloc(u8, s1.chars.len + s2.chars.len) catch oom();
    @memcpy(res[0..s1.chars.len], s1.chars);
    @memcpy(res[s1.chars.len..], s2.chars);

    self.stack.peekRef(1).* = Value.makeObj(Obj.String.take(self, res).asObj());
    self.stack.top -= 1;
}

fn strMul(self: *Self, str: *const Obj.String, factor: i64) void {
    // BUG: Check if factor is positive
    const f = @as(usize, @intCast(factor));
    const res = self.gc_alloc.alloc(u8, str.chars.len * f) catch oom();
    for (0..f) |i| {
        @memcpy(res[i * str.chars.len .. (i + 1) * str.chars.len], str.chars);
    }

    self.stack.peekRef(1).* = Value.makeObj(Obj.String.take(self, res).asObj());
    self.stack.top -= 1;
}

fn checkArrayIndex(array_len: usize, index: i64) usize {
    // TODO: runtime error desactivable with release fast mode
    if (index > array_len - 1) @panic("Out of bound access");

    return if (index >= 0)
        @intCast(index)
    else b: {
        const tmp: usize = @abs(index);
        if (tmp > array_len) @panic("Out of bound");

        break :b array_len - tmp;
    };
}

// PERF: inline methods?
const Stack = struct {
    values: [STACK_SIZE]Value,
    top: [*]Value,

    const STACK_SIZE: u16 = @as(u16, FrameStack.FRAMES_MAX) * @as(u16, std.math.maxInt(u8));
    pub const empty: Stack = .{ .values = undefined, .top = undefined };

    pub fn init(self: *Stack) void {
        self.top = self.values[0..].ptr;
    }

    pub fn push(self: *Stack, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Stack) Value {
        self.top -= 1;
        return self.top[0];
    }

    fn peek(self: *const Stack, distance: usize) Value {
        return (self.top - 1 - distance)[0];
    }

    pub fn peekRef(self: *Stack, distance: usize) *Value {
        return &(self.top - 1 - distance)[0];
    }

    pub fn print(self: *const Stack, writer: *Writer, frame: *CallFrame) Writer.Error!void {
        try writer.writeAll("          ");
        var value = self.values[0..].ptr;

        while (value != self.top) : (value += 1) {
            // Start of call frame
            if (value == frame.slots - 1) try writer.writeAll(">");

            try writer.writeAll("[");
            value[0].print(writer);
            try writer.writeAll("] ");
        }
        try writer.writeAll("\n");
        try writer.flush();
    }
};

pub const CallFrame = struct {
    function: *Obj.Function,
    module: *CompiledModule,
    ip: [*]u8,
    slots: [*]Value,
    captures: []Value,
    blk_val: Value,

    pub fn readByte(self: *CallFrame) u8 {
        defer self.ip += 1;
        return self.ip[0];
    }

    pub fn readConstant(self: *CallFrame) Value {
        // Compiler bug: https://github.com/ziglang/zig/issues/13938
        return (&self.function.chunk.constants)[self.readByte()];
    }

    pub fn readShort(self: *CallFrame) u16 {
        const part1 = self.readByte();
        const part2 = self.readByte();

        return (@as(u16, part1) << 8) | part2;
    }

    // PERF: preshot the closure or function?
    pub fn initCall(self: *CallFrame, callee: *Obj, stack: *Stack, args_count: usize, modules: []CompiledModule) void {
        self.slots = stack.top - args_count;

        const function = switch (callee.kind) {
            .closure => b: {
                const closure = callee.as(Obj.Closure);
                const capt_len = closure.captures.len;

                std.mem.copyBackwards(Value, self.slots[capt_len .. capt_len + args_count], self.slots[0..args_count]);
                @memcpy(self.slots, closure.captures);
                stack.top += capt_len;

                self.captures = closure.captures;
                break :b closure.function;
            },
            .function => b: {
                const function = callee.as(Obj.Function);
                self.module = &modules[function.module_index];
                break :b function;
            },
            else => unreachable,
        };

        self.function = function;
        self.ip = function.chunk.code.items.ptr;
    }
};

const FrameStack = struct {
    frames: [FRAMES_MAX]CallFrame,
    count: usize,

    const FRAMES_MAX: u8 = 64;

    pub const empty: FrameStack = .{ .frames = undefined, .count = 0 };

    pub fn new(self: *FrameStack) Error!*CallFrame {
        if (self.count == FRAMES_MAX) {
            return error.StackOverflow;
        }

        const new_frame = &self.frames[self.count];
        self.count += 1;

        return new_frame;
    }

    /// Opens a new frame while keeping the same module at the one before
    /// Assumes that there is one before
    pub fn newKeepMod(self: *FrameStack) Error!*CallFrame {
        if (self.count == FRAMES_MAX) {
            return error.StackOverflow;
        }

        const new_frame = &self.frames[self.count];
        new_frame.module = self.frames[self.count - 1].module;
        self.count += 1;

        return new_frame;
    }
};
