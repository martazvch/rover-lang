const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const options = @import("options");

const Chunk = @import("../backend/Chunk.zig");
const OpCode = Chunk.OpCode;
const Disassembler = @import("../backend/Disassembler.zig");
const Interner = @import("../Interner.zig");
const Pipeline = @import("../Pipeline.zig");
const Module = Pipeline.Module;
const oom = @import("../utils.zig").oom;
const Gc = @import("Gc.zig");
const Obj = @import("Obj.zig");
const ObjArray = Obj.ObjArray;
const ObjFunction = Obj.ObjFunction;
const ObjInstance = Obj.ObjInstance;
const ObjNativeFn = Obj.ObjNativeFn;
const ObjString = Obj.ObjString;
const ObjStruct = Obj.ObjStruct;
const ObjBoundMethod = Obj.ObjBoundMethod;
const Table = @import("Table.zig");
const Value = @import("values.zig").Value;

pipeline: Pipeline,
gc: Gc,
start_module: Module,
module: *Module,
module_chain: std.ArrayListUnmanaged(*Module),
stack: Stack,
frame_stack: FrameStack,
ip: [*]u8,
allocator: Allocator,
gc_alloc: Allocator,
stdout: std.fs.File.Writer,
interner: Interner,
// TODO: not Zig's hashmap?
strings: Table,
objects: ?*Obj,
heap_vars: []Value,
// TODO: can only be object but must put *Module from Value to *Obj first
// because used in 'get_symbol'
r1: *Value = undefined,
r2: Value = .null_,

const Self = @This();
const Error = error{StackOverflow} || Allocator.Error;

pub const Config = struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
};

pub const empty: Self = .{
    .pipeline = .empty,
    .gc = undefined,
    .gc_alloc = undefined,
    .module = undefined,
    .start_module = undefined,
    .module_chain = .{},
    .stack = .empty,
    .frame_stack = .empty,
    .ip = undefined,
    .allocator = undefined,
    .stdout = undefined,
    .interner = undefined,
    .strings = undefined,
    .objects = null,
    .heap_vars = undefined,
};

pub fn init(self: *Self, allocator: Allocator, config: Config) void {
    self.allocator = allocator;
    // TODO: pass self instead of calling link after
    // TODO: pass an ObjectPoolAlloc?
    self.gc = .init(allocator);
    self.gc_alloc = self.gc.allocator();
    self.stdout = std.io.getStdOut().writer();
    self.interner = .init(allocator);
    self.gc.link(self);
    self.pipeline.init(self, config);
    self.stack.init();
    self.strings = .init(self.allocator);
    // Init on first address to avoid nullable pointer (used only in print stack mode)
    self.r1 = &self.stack.values[0];

    // In REPL mode, we won't call the main function (there is not)
    // so we increment ourself the frame stack (discaring the first one)
    // but the count is coherent of what is expected below, for example
    // for function call we exit if the frame stack count == 1. In REPL
    // it would be always true
    if (config.embedded) self.frame_stack.count += 1;
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.heap_vars);
    self.start_module.deinit(self.allocator);
    self.module_chain.deinit(self.allocator);
    self.interner.deinit();
    self.gc.deinit();
    self.strings.deinit();
    self.freeObjects();
    self.pipeline.deinit();
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
        self.stdout.print("[line {}] in ", .{function.chunk.offsets.items[instr]});

        if (function.name) |name| {
            print("{s}()\n", .{name});
        } else print("script\n", .{});
    }

    return kind;
}

fn instructionNb(self: *const Self) usize {
    const frame = &self.frame_stack.frames[self.frame_stack.count - 1];
    const addr1 = @intFromPtr(frame.ip);
    const addr2 = @intFromPtr(frame.function.chunk.code.items.ptr);
    return addr1 - addr2;
}

pub fn run(self: *Self, filename: []const u8, source: [:0]const u8) !void {
    self.start_module = self.pipeline.run(filename, source) catch |e| switch (e) {
        error.ExitOnPrint => return,
        else => return e,
    };

    self.module = &self.start_module;
    self.gc.active = true;
    try self.execute(self.module.function);
}

// TODO: put outside of VM, in another module
pub fn runRepl(self: *Self) !void {
    const stdin = std.io.getStdIn().reader();

    // Keep all prompts because all of the pipeline uses []const u8 wich point
    // to the user input. Needed for errors and interner
    var prompts = std.ArrayList([]const u8).init(self.allocator);
    defer prompts.deinit();

    var input = std.ArrayList(u8).init(self.allocator);

    _ = try self.stdout.write("\t\tRover language REPL\n");

    while (true) {
        _ = try self.stdout.write("\n> ");
        try stdin.streamUntilDelimiter(input.writer(), '\n', null);

        try input.append('\n');
        try input.append(0);
        const len = input.items.len - 1;
        try prompts.append(try input.toOwnedSlice());

        try self.run("stdin", prompts.items[prompts.items.len - 1][0..len :0]);
    }
}

fn checkArrayIndex(array: *const ObjArray, index: i64) usize {
    // TODO: runtime error desactivable with release fast mode
    if (index > array.values.items.len - 1) @panic("Out of bound access");

    return if (index >= 0)
        @intCast(index)
    else b: {
        const tmp: usize = @abs(index);
        if (tmp > array.values.items.len) @panic("Out of bound");

        break :b array.values.items.len - tmp;
    };
}

fn execute(self: *Self, entry_point: *ObjFunction) !void {
    var frame: *CallFrame = undefined;
    try self.call(&frame, entry_point, 0);

    while (true) {
        if (comptime options.print_stack) {
            if (self.r1 != &self.stack.values[0]) {
                print("    R1: [", .{});
                try self.r1.print(self.stdout);
                print("]  |  ", .{});
            } else {
                print("          ", .{});
            }

            var value = self.stack.values[0..].ptr;

            while (value != self.stack.top) : (value += 1) {
                // Start of call frame
                if (value == frame.slots) print(">", .{});

                print("[", .{});
                try value[0].print(self.stdout);
                print("] ", .{});
            }
            print("\n", .{});
        }

        if (comptime options.print_instr) {
            var dis = Disassembler.init(self.allocator, &frame.function.chunk, self.module.globals, .Normal);
            defer dis.deinit();
            const instr_nb = self.instructionNb();
            _ = try dis.disInstruction(instr_nb, self.stdout);

            switch (@as(OpCode, @enumFromInt(frame.ip[0]))) {
                .get_symbol => _ = try dis.disInstruction(instr_nb + 2, self.stdout),
                else => {},
            }
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
            .array => {
                const len = frame.readByte();
                const array = ObjArray.create(self, (self.stack.top - len)[0..len]);
                self.stack.top -= len;
                self.stack.push(Value.makeObj(array.asObj()));
            },
            .array_access => {
                const index = self.stack.pop().int;
                const array = self.r1.obj.as(ObjArray);
                const final = checkArrayIndex(array, index);

                self.stack.push(array.values.items[final]);
            },
            .array_access_reg => {
                const index = self.stack.pop().int;
                const array = self.r1.obj.as(ObjArray);
                const final = checkArrayIndex(array, index);

                self.r1 = &array.values.items[final];
            },
            .array_access_chain => {
                const depth = frame.readByte();
                var tmp: *ObjArray = self.r1.obj.as(ObjArray);

                for (0..depth - 1) |_| {
                    const idx = checkArrayIndex(tmp, self.stack.pop().int);
                    tmp = tmp.values.items[idx].obj.as(ObjArray);
                }

                const idx = checkArrayIndex(tmp, self.stack.pop().int);
                self.stack.push(tmp.values.items[idx]);
            },
            .array_access_chain_reg => {
                const depth = frame.readByte();
                var tmp: *ObjArray = self.r1.obj.as(ObjArray);

                for (0..depth - 1) |_| {
                    const idx = checkArrayIndex(tmp, self.stack.pop().int);
                    tmp = tmp.values.items[idx].obj.as(ObjArray);
                }

                const idx = checkArrayIndex(tmp, self.stack.pop().int);
                self.r1 = &tmp.values.items[idx];
            },
            .array_assign => {
                const index = self.stack.pop().int;
                self.r1.obj = self.cow(self.r1.obj);
                const array = self.r1.obj.as(ObjArray);

                const final = checkArrayIndex(array, index);
                const value = self.stack.pop();
                array.obj.as(ObjArray).values.items[final] = value;
            },
            .array_assign_chain => {
                const depth = frame.readByte();
                var tmp: *ObjArray = self.r1.obj.as(ObjArray);
                var last: **ObjArray = &tmp;

                for (0..depth - 1) |_| {
                    const idx = checkArrayIndex(last.*, self.stack.pop().int);
                    last = @constCast(&last.*.values.items[idx].obj);
                }

                last.* = self.cow(last.*.asObj()).as(ObjArray);

                const idx = checkArrayIndex(last.*, self.stack.pop().int);
                last.*.values.items[idx] = self.stack.pop();
            },
            .bound_method => {
                // TODO: bound method obj could only keep an *Obj (because only struct have methods)
                const method_idx = frame.readByte();
                const receiver = self.r1.*;
                const method = receiver.obj.as(ObjInstance).parent.methods[method_idx];
                const bound = ObjBoundMethod.create(self, receiver, method);
                self.stack.push(Value.makeObj(bound.asObj()));
            },
            .bound_method_call => {
                const args_count = frame.readByte();
                const bound = self.stack.peekRef(args_count).obj.as(ObjBoundMethod);
                try self.callBoundMethod(&frame, args_count, bound.receiver, bound.method);
            },
            .call => {
                const args_count = frame.readByte();
                const callee = self.stack.peekRef(args_count).obj.as(ObjFunction);
                try self.call(&frame, callee, args_count);
            },
            // TODO: Cast could only 'transmute' or bitcast the value on stack to change the union tag
            .cast_to_float => self.stack.push(Value.makeFloat(@floatFromInt(self.stack.pop().int))),
            .constant => self.stack.push(frame.readConstant()),
            .define_heap_var => {
                const idx = frame.readByte();
                self.heap_vars[idx] = self.stack.pop();
            },
            .define_global => {
                const idx = frame.readByte();
                self.module.globals[idx] = self.stack.pop();
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
            .eq_bool => self.stack.push(Value.makeBool(self.stack.pop().bool == self.stack.pop().bool)),
            .eq_float => self.stack.push(Value.makeBool(self.stack.pop().float == self.stack.pop().float)),
            .eq_int => self.stack.push(Value.makeBool(self.stack.pop().int == self.stack.pop().int)),
            .eq_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(ObjString) == self.stack.pop().obj.as(ObjString))),
            .exit_repl => {
                // Here, there is no value to pop for now, no implicit null is
                // put on top of the stack
                self.frame_stack.count -= 1;
                break;
            },
            .false => self.stack.push(Value.false_),
            .get_field => {
                const field_idx = frame.readByte();
                self.stack.push(self.r1.obj.as(ObjInstance).fields[field_idx]);
            },
            .get_field_reg => {
                const field_idx = frame.readByte();
                self.r1 = &self.r1.obj.as(ObjInstance).fields[field_idx];
            },
            .get_global => {
                const idx = frame.readByte();
                self.stack.push(self.module.globals[idx]);
            },
            .get_global_reg => {
                const idx = frame.readByte();
                self.r1 = &self.module.globals[idx];
            },
            // TODO: see if same compiler bug as get_global
            .get_heap => self.stack.push(self.heap_vars[frame.readByte()]),
            // TODO: see if same compiler bug as get_global
            .get_local => self.stack.push(frame.slots[frame.readByte()]),
            .get_local_reg => {
                self.r1 = &frame.slots[frame.readByte()];
                self.r1.obj = self.cow(self.r1.obj);
            },
            .get_local_absolute => self.stack.push(self.stack.values[frame.readByte()]),
            .get_fn_default => self.getDefaultValue(frame, ObjFunction),
            // TODO: use a dedicated register
            .get_method_default => {
                const obj_idx = frame.readByte();
                const method_idx = frame.readByte();
                const default_idx = frame.readByte();
                self.stack.push(self.stack.peekRef(obj_idx).obj.as(ObjInstance).parent.methods[method_idx].default_values[default_idx]);
            },
            .get_static_method => {
                const method_idx = frame.readByte();
                const structure = self.r1.obj.as(ObjStruct);
                const method = structure.methods[method_idx];
                self.stack.push(Value.makeObj(method.asObj()));
            },
            .get_struct_default => self.getDefaultValue(frame, ObjStruct),
            .get_symbol => {
                const symbol_idx = frame.readByte();
                self.stack.push(self.r1.module.globals[symbol_idx]);
            },
            .gt_float => self.stack.push(Value.makeBool(self.stack.pop().float < self.stack.pop().float)),
            .gt_int => self.stack.push(Value.makeBool(self.stack.pop().int < self.stack.pop().int)),
            .ge_float => self.stack.push(Value.makeBool(self.stack.pop().float <= self.stack.pop().float)),
            .ge_int => self.stack.push(Value.makeBool(self.stack.pop().int <= self.stack.pop().int)),
            .incr_ref_count => self.stack.peekRef(0).obj.ref_count += 1,
            .import_call => {
                const args_count = frame.readByte();
                // TODO: put it in a reg?
                const module = self.stack.pop().module;
                self.updateModule(module);
                const callee = self.stack.peekRef(args_count).obj.as(ObjFunction);
                try self.call(&frame, callee, args_count);
            },
            .import_item => {
                const module_idx = frame.readByte();
                const field_idx = frame.readByte();
                self.stack.push(self.module.imports[module_idx].globals[field_idx]);
            },
            .invoke => {
                const args_count = frame.readByte();
                const method_idx = frame.readByte();
                const receiver = self.stack.peek(args_count);
                const method = receiver.obj.as(ObjInstance).parent.methods[method_idx];
                try self.callBoundMethod(&frame, args_count, receiver, method);
            },
            .invoke_import => {
                const args_count = frame.readByte();
                const symbol = frame.readByte();
                const module = self.stack.peek(args_count).module;
                self.updateModule(module);
                const imported = self.stack.peek(args_count).module.globals[symbol];
                try self.call(&frame, imported.obj.as(ObjFunction), args_count);
            },
            .invoke_static => {
                const args_count = frame.readByte();
                const method_idx = frame.readByte();
                const structure = self.stack.peekRef(args_count).obj.as(ObjStruct);
                const function = structure.methods[method_idx];
                try self.call(&frame, function, args_count);
            },
            .jump => {
                const jump = frame.readShort();
                frame.ip += jump;
            },
            .jump_if_false => {
                const jump = frame.readShort();
                if (!self.stack.peek(0).bool) frame.ip += jump;
            },
            .jump_if_true => {
                const jump = frame.readShort();
                if (self.stack.peek(0).bool) frame.ip += jump;
            },
            .lt_float => self.stack.push(Value.makeBool(self.stack.pop().float > self.stack.pop().float)),
            .lt_int => self.stack.push(Value.makeBool(self.stack.pop().int > self.stack.pop().int)),
            .le_float => self.stack.push(Value.makeBool(self.stack.pop().float >= self.stack.pop().float)),
            .le_int => self.stack.push(Value.makeBool(self.stack.pop().int >= self.stack.pop().int)),
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
            .naked_return => {
                self.frame_stack.count -= 1;

                // The last standing frame is the artificial one created when we run
                // the global scope at the very beginning
                // TODO: avoid logic at runtime, just emit a special OpCode for `main` naked return
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = frame.slots;
                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .native_fn_call => {
                const args_count = frame.readByte();
                const native = self.stack.peekRef(args_count).obj.as(ObjNativeFn).function;
                const result = native((self.stack.top - args_count)[0..args_count]);

                self.stack.top -= args_count + 1;
                self.stack.push(result);
            },
            .ne_bool => self.stack.push(Value.makeBool(self.stack.pop().bool != self.stack.pop().bool)),
            .ne_int => self.stack.push(Value.makeBool(self.stack.pop().int != self.stack.pop().int)),
            .ne_float => self.stack.push(Value.makeBool(self.stack.pop().float != self.stack.pop().float)),
            .ne_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(ObjString) != self.stack.pop().obj.as(ObjString))),
            .negate_float => self.stack.peekRef(0).float *= -1,
            .negate_int => self.stack.peekRef(0).int *= -1,
            .not => self.stack.peekRef(0).not(),
            .null => self.stack.push(Value.null_),
            .pop => _ = self.stack.pop(),
            .print => {
                try self.stack.pop().print(self.stdout);
                _ = try self.stdout.write("\n");
            },
            .push_module => {
                const index = frame.readByte();
                const module = &self.module.imports[index];
                self.stack.push(.makeModule(module));
            },
            .reg_push => {
                self.r2 = self.stack.pop();
                self.r1 = &self.r2;
            },
            .reg_assign => self.r1.* = self.stack.pop(),
            .reg_assign_cow => {
                self.r1.obj = self.cow(self.r1.obj);
                self.r1.* = self.stack.pop();
            },
            .@"return" => {
                const result = self.stack.pop();
                self.frame_stack.count -= 1;

                // The last standing frame is the artificial one created when we run
                // the global scope at the very beginning
                // TODO: avoid logic at runtime, just emit a special OpCode for `main` return
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = frame.slots;
                self.stack.push(result);

                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .scope_return => {
                const locals_count = frame.readByte();
                const res = self.stack.pop();
                self.stack.top -= locals_count;

                // TODO: put in register too?
                self.stack.push(res);
            },
            .set_global => {
                const idx = frame.readByte();
                self.module.globals[idx] = self.stack.pop();
            },
            .set_heap => self.heap_vars[frame.readByte()] = self.stack.pop(),
            .set_local => frame.slots[frame.readByte()] = self.stack.pop(),
            .str_cat => self.strConcat(),
            .str_mul_l => self.strMul(self.stack.peekRef(0).obj.as(ObjString), self.stack.peekRef(1).int),
            .str_mul_r => self.strMul(self.stack.peekRef(1).obj.as(ObjString), self.stack.peekRef(0).int),
            .struct_literal => {
                const arity = frame.readByte();
                var instance = ObjInstance.create(self, self.stack.peekRef(arity).obj.as(ObjStruct));

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
            .true => self.stack.push(Value.true_),
            .unload_module => self.module = self.module_chain.pop().?,
        }
    }
}

/// Checks clone on write
fn cow(self: *Self, obj: *Obj) *Obj {
    if (obj.ref_count > 0) {
        obj.ref_count -= 1;
        return obj.deepCopy(self);
    }

    return obj;
}

/// Read the two next bytes as `scope` then `index` then get value from according scope
inline fn getValueFromScope(self: *Self, frame: *CallFrame) *Value {
    const scope: OpCode = @enumFromInt(frame.readByte());
    const idx = frame.readByte();

    return if (scope == .get_local or scope == .get_local_reg)
        &frame.slots[idx]
    else if (scope == .get_global)
        &self.module.globals[idx]
    else
        &self.heap_vars[idx];
}

fn callBoundMethod(self: *Self, frame: **CallFrame, args_count: usize, receiver: Value, method: *ObjFunction) !void {
    self.stack.peekRef(args_count).* = receiver;
    try self.call(frame, method, args_count);
}

fn updateModule(self: *Self, module: *Module) void {
    self.module_chain.append(self.allocator, self.module) catch oom();
    self.module = module;
}

/// Gets and pushes on the stack a default value. Should be used for types `ObjFunction` and `ObjStruct`
fn getDefaultValue(self: *Self, frame: *CallFrame, T: type) void {
    if (!@hasField(T, "default_values")) {
        @compileError("Type " ++ @typeName(T) ++ " doesn't have field 'default_values'");
    }

    const obj_idx = frame.readByte();
    const default_idx = frame.readByte();
    self.stack.push(self.stack.peekRef(obj_idx).obj.as(T).default_values[default_idx]);
}

fn strConcat(self: *Self) void {
    const s2 = self.stack.peekRef(0).obj.as(ObjString);
    const s1 = self.stack.peekRef(1).obj.as(ObjString);

    const res = self.gc_alloc.alloc(u8, s1.chars.len + s2.chars.len) catch oom();
    @memcpy(res[0..s1.chars.len], s1.chars);
    @memcpy(res[s1.chars.len..], s2.chars);

    // 'Pop' the two strings
    self.stack.top -= 2;
    self.stack.push(Value.makeObj(ObjString.take(self, res).asObj()));
}

fn strMul(self: *Self, str: *const ObjString, factor: i64) void {
    // BUG: Check if factor is positive
    const f = @as(usize, @intCast(factor));
    const res = self.gc_alloc.alloc(u8, str.chars.len * f) catch oom();
    for (0..f) |i| {
        @memcpy(res[i * str.chars.len .. (i + 1) * str.chars.len], str.chars);
    }

    // pop after alloc in case of GC trigger
    _ = self.stack.pop();
    _ = self.stack.pop();

    self.stack.push(Value.makeObj(ObjString.take(self, res).asObj()));
}

fn call(self: *Self, frame: **CallFrame, callee: *ObjFunction, args_count: usize) Error!void {
    if (self.frame_stack.count == FrameStack.FRAMES_MAX) {
        return error.StackOverflow;
    }

    const new_frame = &self.frame_stack.frames[self.frame_stack.count];
    self.frame_stack.count += 1;
    new_frame.function = callee;
    new_frame.ip = callee.chunk.code.items.ptr;
    // -1 for the function itself
    new_frame.slots = self.stack.top - args_count - 1;

    frame.* = &self.frame_stack.frames[self.frame_stack.count - 1];
}

// PERF: bench avec BoundedArray
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

    fn peekRef(self: *Stack, distance: usize) *Value {
        return &(self.top - 1 - distance)[0];
    }
};

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,

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
};

const FrameStack = struct {
    frames: [FRAMES_MAX]CallFrame,
    count: usize,

    const FRAMES_MAX: u8 = 64;

    pub const empty: FrameStack = .{ .frames = undefined, .count = 0 };
};
