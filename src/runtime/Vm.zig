const std = @import("std");
const print = std.debug.print;
const Allocator = std.mem.Allocator;

const options = @import("options");

const Chunk = @import("../backend/Chunk.zig");
const OpCode = Chunk.OpCode;
const Disassembler = @import("../backend/Disassembler.zig");
const Pipeline = @import("../Pipeline.zig");
const Gc = @import("Gc.zig");
const Obj = @import("Obj.zig");
const ObjFunction = Obj.ObjFunction;
const ObjInstance = Obj.ObjInstance;
const ObjNativeFn = Obj.ObjNativeFn;
const ObjString = Obj.ObjString;
const ObjStruct = Obj.ObjStruct;
const Table = @import("Table.zig");
const Value = @import("values.zig").Value;

pipeline: Pipeline,
gc: Gc,
stack: Stack,
frame_stack: FrameStack,
ip: [*]u8,
allocator: Allocator,
gc_alloc: Allocator,
stdout: std.fs.File.Writer,
// TODO: not Zig's hashmap?
strings: Table,
objects: ?*Obj,
globals: [256]Value, // TODO: ArrayList or constant, not hard written
heap_vars: []Value,

const Self = @This();
const Error = error{StackOverflow} || Allocator.Error;

pub const Config = struct {
    embedded: bool,
    print_ast: bool,
    print_bytecode: bool,
    static_analyzis: bool,
    print_ir: bool,

    pub const default: Config = .{
        .embedded = false,
        .print_ast = false,
        .print_bytecode = false,
        .static_analyzis = false,
        .print_ir = false,
    };
};

pub const empty: Self = .{
    .pipeline = undefined,
    .gc = undefined,
    .gc_alloc = undefined,
    .stack = .empty,
    .frame_stack = .empty,
    .ip = undefined,
    .allocator = undefined,
    .stdout = undefined,
    .strings = undefined,
    .objects = null,
    .globals = undefined,
    .heap_vars = undefined,
};

pub fn init(self: *Self, allocator: Allocator, config: Config) void {
    self.allocator = allocator;
    // TODO: pass self instead of calling link after
    // TODO: pass an ObjectPoolAlloc?
    self.gc = .init(allocator);
    self.gc_alloc = self.gc.allocator();
    self.stdout = std.io.getStdOut().writer();
    self.gc.link(self);
    self.pipeline.init(self, config);
    self.stack.init();
    self.strings = .init(self.allocator);

    // In REPL mode, we won't call the main function (there is not)
    // so we increment ourself the frame stack (discaring the first one)
    // but the count is coherent of what is expected below, for example
    // for function call we exit if the frame stack count == 1. In REPL
    // it would be always true
    if (config.embedded) self.frame_stack.count += 1;
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.heap_vars);
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
    const func = self.pipeline.run(filename, source) catch |e| switch (e) {
        error.ExitOnPrint => return,
        else => return e,
    };
    try self.call(func, 0);
    self.gc.active = true;
    try self.execute();
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

fn execute(self: *Self) !void {
    var frame = &self.frame_stack.frames[self.frame_stack.count - 1];

    while (true) {
        if (comptime options.print_stack) {
            print("          ", .{});

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
            var dis = Disassembler.init(&frame.function.chunk, self.allocator, .Normal);
            defer dis.deinit();
            _ = try dis.disInstruction(self.instructionNb(), self.stdout);
        }

        const instruction = frame.readByte();
        const op: OpCode = @enumFromInt(instruction);

        switch (op) {
            .AddFloat => {
                const rhs = self.stack.pop().Float;
                self.stack.peekRef(0).Float += rhs;
            },
            .AddInt => {
                const rhs = self.stack.pop().Int;
                self.stack.peekRef(0).Int += rhs;
            },
            .call => {
                const args_count = frame.readByte();
                const callee = self.stack.peekRef(args_count).Obj.as(ObjFunction);
                try self.call(callee, args_count);

                // If call success, we need to to set frame pointer back
                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },

            // TODO: Cast could only 'transmute' or bitcast the value on stack to change the union tag
            .CastToFloat => self.stack.push(Value.float(@floatFromInt(self.stack.pop().Int))),
            .Constant => self.stack.push(frame.readConstant()),
            .DefineHeapVar => {
                const idx = frame.readByte();
                self.heap_vars[idx] = self.stack.pop();
            },
            .DefineGlobal => {
                const idx = frame.readByte();
                self.globals[idx] = self.stack.pop();
            },
            .DivFloat => {
                const rhs = self.stack.pop().Float;
                self.stack.peekRef(0).Float /= rhs;
            },
            .DivInt => {
                const rhs = self.stack.pop().Int;
                const lhs = self.stack.pop().Int;
                self.stack.push(Value.int(@divTrunc(lhs, rhs)));
            },
            .EqBool => self.stack.push(Value.bool_(self.stack.pop().Bool == self.stack.pop().Bool)),
            .EqFloat => self.stack.push(Value.bool_(self.stack.pop().Float == self.stack.pop().Float)),
            .EqInt => self.stack.push(Value.bool_(self.stack.pop().Int == self.stack.pop().Int)),
            .EqStr => self.stack.push(Value.bool_(self.stack.pop().Obj.as(ObjString) == self.stack.pop().Obj.as(ObjString))),
            .false => self.stack.push(Value.bool_(false)),
            .field_assign => {
                frame.ip += 1;
                const field = self.getField(frame);
                field.* = self.stack.pop();
            },
            .get_field => {
                const value = self.getField(frame);
                self.stack.push(value.*);
            },
            // Compiler bug: https://github.com/ziglang/zig/issues/13938
            .GetGlobal => self.stack.push((&self.globals)[frame.readByte()]),
            .GetHeap => self.stack.push(self.heap_vars[frame.readByte()]),
            // TODO: see if same compiler bug as GetGlobal
            .GetLocal => self.stack.push(frame.slots[frame.readByte()]),
            .GtFloat => self.stack.push(Value.bool_(self.stack.pop().Float < self.stack.pop().Float)),
            .GtInt => self.stack.push(Value.bool_(self.stack.pop().Int < self.stack.pop().Int)),
            .GeFloat => self.stack.push(Value.bool_(self.stack.pop().Float <= self.stack.pop().Float)),
            .GeInt => self.stack.push(Value.bool_(self.stack.pop().Int <= self.stack.pop().Int)),
            .Jump => {
                const jump = frame.readShort();
                frame.ip += jump;
            },
            .JumpIfFalse => {
                const jump = frame.readShort();
                if (!self.stack.peek(0).Bool) frame.ip += jump;
            },
            .JumpIfTrue => {
                const jump = frame.readShort();
                if (self.stack.peek(0).Bool) frame.ip += jump;
            },
            .LtFloat => self.stack.push(Value.bool_(self.stack.pop().Float > self.stack.pop().Float)),
            .LtInt => self.stack.push(Value.bool_(self.stack.pop().Int > self.stack.pop().Int)),
            .LeFloat => self.stack.push(Value.bool_(self.stack.pop().Float >= self.stack.pop().Float)),
            .LeInt => self.stack.push(Value.bool_(self.stack.pop().Int >= self.stack.pop().Int)),
            .Loop => {
                const jump = frame.readShort();
                frame.ip -= jump;
            },
            .MulFloat => {
                const rhs = self.stack.pop().Float;
                self.stack.peekRef(0).Float *= rhs;
            },
            .MulInt => {
                const rhs = self.stack.pop().Int;
                self.stack.peekRef(0).Int *= rhs;
            },
            .NakedReturn => {
                self.frame_stack.count -= 1;

                // The last standing frame is the artificial one created when we run
                // the global scope at the very beginning
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = frame.slots;
                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .NativeFnCall => {
                const args_count = frame.readByte();
                const native = self.stack.peekRef(args_count).Obj.as(ObjNativeFn).function;
                const result = native((self.stack.top - args_count)[0..args_count]);

                self.stack.top -= args_count + 1;
                self.stack.push(result);
            },
            .NeBool => self.stack.push(Value.bool_(self.stack.pop().Bool != self.stack.pop().Bool)),
            .NeInt => self.stack.push(Value.bool_(self.stack.pop().Int != self.stack.pop().Int)),
            .NeFloat => self.stack.push(Value.bool_(self.stack.pop().Float != self.stack.pop().Float)),
            .NeStr => self.stack.push(Value.bool_(self.stack.pop().Obj.as(ObjString) != self.stack.pop().Obj.as(ObjString))),
            .NegateFloat => self.stack.peekRef(0).Float *= -1,
            .NegateInt => self.stack.peekRef(0).Int *= -1,
            .not => self.stack.peekRef(0).not(),
            .null => self.stack.push(Value.null_()),
            .Pop => _ = self.stack.pop(),
            .print => {
                try self.stack.pop().print(self.stdout);
                _ = try self.stdout.write("\n");
            },
            .ExitRepl => {
                // Here, there is no value to pop for now, no implicit null is
                // put on top of the stack
                self.frame_stack.count -= 1;
                break;
            },
            .@"return" => {
                const result = self.stack.pop();
                self.frame_stack.count -= 1;

                // The last standing frame is the artificial one created when we run
                // the global scope at the very beginning
                if (self.frame_stack.count == 1) {
                    _ = self.stack.pop();
                    break;
                }

                self.stack.top = frame.slots;
                self.stack.push(result);
                frame = &self.frame_stack.frames[self.frame_stack.count - 1];
            },
            .ScopeReturn => {
                const locals_count = frame.readByte();
                const res = self.stack.pop();
                self.stack.top -= locals_count;
                self.stack.push(res);
            },
            .SetGlobal => self.globals[frame.readByte()] = self.stack.pop(),
            .SetHeap => self.heap_vars[frame.readByte()] = self.stack.pop(),
            .SetLocal => frame.slots[frame.readByte()] = self.stack.pop(),
            .StrCat => try self.strConcat(),
            .StrMulL => try self.strMul(self.stack.peekRef(0).Obj.as(ObjString), self.stack.peekRef(1).Int),
            .StrMulR => try self.strMul(self.stack.peekRef(1).Obj.as(ObjString), self.stack.peekRef(0).Int),
            .struct_literal => {
                const arity = frame.readByte();
                const scope: OpCode = @enumFromInt(frame.readByte());
                const idx = frame.readByte();

                const structure = switch (scope) {
                    .GetLocal => frame.slots[idx].Obj.as(ObjStruct),
                    .GetGlobal => self.globals[idx].Obj.as(ObjStruct),
                    else => unreachable,
                };

                var instance = try ObjInstance.create(self, structure);

                for (0..arity) |i| {
                    instance.fields[i] = self.stack.peek(arity - i - 1);
                }

                self.stack.top -= arity;
                self.stack.push(Value.obj(instance.asObj()));
            },
            .SubFloat => {
                const rhs = self.stack.pop().Float;
                self.stack.peekRef(0).Float -= rhs;
            },
            .SubInt => {
                const rhs = self.stack.pop().Int;
                self.stack.peekRef(0).Int -= rhs;
            },
            // PERF: we could avoid a function call here
            .true => self.stack.push(Value.bool_(true)),
        }
    }
}

// TODO: make this treatment in earlier stages to avoid runtime checks?
// Rucursion here avoid to push and pop from stack like: a.b.c pushing a.b on stack
// then pop it to access c to push it back
fn getField(self: *Self, frame: *CallFrame) *Value {
    const field_idx = frame.readByte();

    const instance = blk: {
        const op: OpCode = @enumFromInt(frame.readByte());

        if (op == .get_field)
            break :blk self.getField(frame);

        const idx = frame.readByte();

        break :blk if (op == .GetGlobal)
            &self.globals[idx]
        else if (op == .GetHeap)
            &self.heap_vars[idx]
        else
            &frame.slots[idx];
    };

    return &instance.Obj.as(ObjInstance).fields[field_idx];
}

fn strConcat(self: *Self) !void {
    const s2 = self.stack.peekRef(0).Obj.as(ObjString);
    const s1 = self.stack.peekRef(1).Obj.as(ObjString);

    const res = try self.gc_alloc.alloc(u8, s1.chars.len + s2.chars.len);
    @memcpy(res[0..s1.chars.len], s1.chars);
    @memcpy(res[s1.chars.len..], s2.chars);

    // Pop after alloc in case of GC trigger
    _ = self.stack.pop();
    _ = self.stack.pop();

    self.stack.push(Value.obj((try ObjString.take(self, res)).asObj()));
}

fn strMul(self: *Self, str: *const ObjString, factor: i64) !void {
    // BUG: Check if factor is positive
    const f = @as(usize, @intCast(factor));
    const res = try self.gc_alloc.alloc(u8, str.chars.len * f);
    for (0..f) |i| {
        @memcpy(res[i * str.chars.len .. (i + 1) * str.chars.len], str.chars);
    }

    // Pop after alloc in case of GC trigger
    _ = self.stack.pop();
    _ = self.stack.pop();

    self.stack.push(Value.obj((try ObjString.take(self, res)).asObj()));
}

fn call(self: *Self, callee: *ObjFunction, args_count: usize) Error!void {
    if (self.frame_stack.count == FrameStack.FRAMES_MAX) {
        return error.StackOverflow;
    }

    const frame = &self.frame_stack.frames[self.frame_stack.count];
    self.frame_stack.count += 1;
    frame.function = callee;
    frame.ip = callee.chunk.code.items.ptr;
    // -1 for the function itself
    frame.slots = self.stack.top - args_count - 1;
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
