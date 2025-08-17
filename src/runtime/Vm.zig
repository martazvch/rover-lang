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
const Repl = @import("Repl.zig");
const Table = @import("Table.zig");
const Value = @import("values.zig").Value;

pipeline: Pipeline,
gc: Gc,
repl: Repl,
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
symbols: []Value,

/// Holds default variables of current call
r3: []Value = undefined,

const Self = @This();
const Error = error{StackOverflow} || Allocator.Error;

pub const Config = struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
};

pub fn init(self: *Self, allocator: Allocator, config: Config) void {
    self.allocator = allocator;
    // TODO: pass an ObjectPoolAlloc?
    self.gc = .init(self, allocator);
    self.gc_alloc = self.gc.allocator();
    self.stdout = std.io.getStdOut().writer();
    self.interner = .init(allocator);
    self.pipeline = undefined;
    self.pipeline.init(self, config);
    self.stack.init();
    self.strings = .init(self.allocator);
    self.stack = .empty;
    self.stack.init();
    self.frame_stack = .empty;
    self.objects = null;
    self.module_chain = .{};

    // In REPL mode, we won't call the main function (there is not)
    // so we increment ourself the frame stack (discaring the first one)
    // but the count is coherent of what is expected below, for example
    // for function call we exit if the frame stack count == 1. In REPL
    // it would be always true
    if (config.embedded) {
        self.frame_stack.count += 1;
        self.repl = .init();
    }
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.symbols);

    self.module_chain.deinit(self.allocator);
    self.interner.deinit();
    self.gc.deinit();
    self.strings.deinit();
    self.freeObjects();
    self.pipeline.deinit();
    if (self.pipeline.config.embedded) self.repl.deinit(self.allocator);
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
    self.symbols = self.start_module.symbols;

    self.module = &self.start_module;
    self.gc.active = true;

    // Init on dummy address to avoid nullable pointer (used only in print stack mode)
    try self.execute(self.module.function);
}

pub fn runRepl(self: *Self) !void {
    try self.repl.logInfos();

    while (true) {
        const prompt = self.repl.getPrompt(self.allocator) catch |e| switch (e) {
            error.Empty => continue,
            error.EndOfFile => break,
            else => {
                std.debug.print("REPL error: {s}\n", .{@errorName(e)});
                return;
            },
        };

        try self.run("stdin", prompt);
    }
}

fn checkArrayIndex(array: *const Obj.Array, index: i64) usize {
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

fn execute(self: *Self, entry_point: *Obj.Function) !void {
    var frame = try self.frame_stack.new();
    frame.initCall(entry_point.asObj(), &self.stack, 0);

    while (true) {
        if (comptime options.print_stack) {
            print("          ", .{});
            var value = self.stack.values[0..].ptr;

            while (value != self.stack.top) : (value += 1) {
                // Start of call frame
                if (value == frame.slots - 1) print(">", .{});

                print("[", .{});
                try value[0].print(self.stdout);
                print("] ", .{});
            }
            print("\n", .{});
        }

        if (comptime options.print_instr) {
            var dis = Disassembler.init(self.allocator, &frame.function.chunk, self.module.globals, .normal);
            defer dis.deinit();
            const instr_nb = self.instructionNb();
            _ = try dis.disInstruction(instr_nb, self.stdout);
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
                const array = Obj.Array.create(self, (self.stack.top - len)[0..len]);
                self.stack.top -= len;
                self.stack.push(Value.makeObj(array.asObj()));
            },
            .array_access => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const final = checkArrayIndex(array, index);
                self.stack.push(array.values.items[final]);
            },
            .array_access_cow => {
                const index = self.stack.pop().int;
                const array = self.stack.pop().obj.as(Obj.Array);
                const final = checkArrayIndex(array, index);
                const value = &array.values.items[final];
                value.obj = self.cow(value.obj);
                self.stack.push(value.*);
            },
            // .array_access_reg_cow => {
            //     const index = self.stack.pop().int;
            //     const array = self.r1.obj.as(Obj.Array);
            //     const final = checkArrayIndex(array, index);
            //
            //     self.r1 = &array.values.items[final];
            //     self.r1.obj = self.cow(self.r1.obj);
            // },
            .array_access_chain => unreachable,
            // .array_access_chain => {
            //     const depth = frame.readByte();
            //     var tmp: *Obj.Array = self.r1.obj.as(Obj.Array);
            //
            //     for (0..depth - 1) |_| {
            //         const idx = checkArrayIndex(tmp, self.stack.pop().int);
            //         tmp = tmp.values.items[idx].obj.as(Obj.Array);
            //     }
            //
            //     const idx = checkArrayIndex(tmp, self.stack.pop().int);
            //     self.stack.push(tmp.values.items[idx]);
            // },
            .array_assign => {
                const index = self.stack.pop().int;
                // const array = self.stack.pop().obj.as(Obj.Array);
                const array = self.stack.pop().obj.as(Obj.Array);
                const value = self.stack.pop();

                const final = checkArrayIndex(array, index);
                array.values.items[final] = value;
            },
            .array_assign_chain => unreachable,
            // .array_assign_chain => {
            //     const depth = frame.readByte();
            //     var tmp: *Obj.Array = self.r1.obj.as(Obj.Array);
            //     var last: **Obj.Array = &tmp;
            //
            //     for (0..depth - 1) |_| {
            //         const idx = checkArrayIndex(last.*, self.stack.pop().int);
            //         last = &last.*.values.items[idx].obj;
            //         last.* = self.cow(last.*.asObj()).as(Obj.Array);
            //     }
            //
            //     const idx = checkArrayIndex(last.*, self.stack.pop().int);
            //     last.*.values.items[idx] = self.stack.pop();
            // },
            .bound_import => unreachable,
            // .bound_import => {
            //     const symbol_idx = frame.readByte();
            //     const bound = Obj.BoundImport.create(
            //         self,
            //         self.r1.obj.as(Obj.ObjModule),
            //         self.r1.obj.as(Obj.ObjModule).module.globals[symbol_idx].obj,
            //     );
            //     self.stack.push(Value.makeObj(bound.asObj()));
            // },
            .box => {
                const to_box = self.stack.pop();
                const boxed = Value.makeObj(Obj.Box.create(self, to_box).asObj());
                self.stack.push(boxed);
            },
            .call => {
                const args_count = frame.readByte();
                frame = try self.frame_stack.new();
                frame.initCall(self.stack.peekRef(args_count).obj, &self.stack, args_count);
            },
            .call_native => {
                // const args_count = frame.readByte();
                // const native = self.stack.peekRef(args_count).obj.as(NativeFunction).function;
                // const result = native((self.stack.top - args_count)[0..args_count]);
                //
                // self.stack.top -= args_count + 1;
                // self.stack.push(result);
            },
            .cast_to_float => self.stack.peekRef(0).* = Value.makeFloat(@floatFromInt(self.stack.peekRef(0).int)),
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
            .dup => self.stack.push(self.stack.peek(0)),
            .eq_bool => self.stack.push(Value.makeBool(self.stack.pop().bool == self.stack.pop().bool)),
            .eq_float => self.stack.push(Value.makeBool(self.stack.pop().float == self.stack.pop().float)),
            .eq_int => self.stack.push(Value.makeBool(self.stack.pop().int == self.stack.pop().int)),
            .eq_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(Obj.String) == self.stack.pop().obj.as(Obj.String))),
            .exit_repl => {
                // Here, there is no value to pop for now, no implicit null is
                // put on top of the stack
                self.frame_stack.count -= 1;
                break;
            },
            .false => self.stack.push(Value.false_),
            .ge_float => self.stack.push(Value.makeBool(self.stack.pop().float <= self.stack.pop().float)),
            .ge_int => self.stack.push(Value.makeBool(self.stack.pop().int <= self.stack.pop().int)),
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
                self.stack.push(self.module.globals[idx]);
            },
            .get_global_cow => {
                const idx = frame.readByte();
                const value = &self.module.globals[idx];
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
            .get_method => self.stack.peekRef(0).* = Value.makeObj(self.stack.peekRef(0).obj.as(Obj.Instance).parent.methods[frame.readByte()].asObj()),
            // TODO: same as above
            .get_static_method => {
                const method_idx = frame.readByte();
                const top = self.stack.peekRef(0);
                const structure = top.obj.as(Obj.Structure);
                const method = structure.methods[method_idx];
                top.* = Value.makeObj(method.asObj());
            },
            .get_symbol => {
                const symbol_idx = frame.readByte();
                self.stack.push(self.symbols[symbol_idx]);
            },
            .gt_float => self.stack.push(Value.makeBool(self.stack.pop().float < self.stack.pop().float)),
            .gt_int => self.stack.push(Value.makeBool(self.stack.pop().int < self.stack.pop().int)),
            .incr_ref_count => self.stack.peekRef(0).obj.ref_count += 1,
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

            .load_fn_default => self.stack.peekRef(0).obj.loadDefaultValues(self, 0),
            .load_invoke_default => self.stack.peekRef(0).obj.loadDefaultValues(self, frame.readByte()),
            .load_struct_def => self.r3 = self.stack.peekRef(0).obj.as(Obj.Structure).default_values,

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

                if (frame.imported) {
                    self.module = self.module_chain.pop().?;
                }

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
            .ne_bool => self.stack.push(Value.makeBool(self.stack.pop().bool != self.stack.pop().bool)),
            .ne_int => self.stack.push(Value.makeBool(self.stack.pop().int != self.stack.pop().int)),
            .ne_float => self.stack.push(Value.makeBool(self.stack.pop().float != self.stack.pop().float)),
            .ne_str => self.stack.push(Value.makeBool(self.stack.pop().obj.as(Obj.String) != self.stack.pop().obj.as(Obj.String))),
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
                self.stack.push(Value.makeObj(Obj.ObjModule.create(self, module).asObj()));
            },
            .@"return" => {
                const result = self.stack.pop();
                self.frame_stack.count -= 1;

                if (frame.imported) {
                    self.module = self.module_chain.pop().?;
                }

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
            .scope_return => {
                const locals_count = frame.readByte();
                const res = self.stack.pop();
                self.stack.top -= locals_count;
                self.stack.push(res);
            },
            .set_field => {
                const field_idx = frame.readByte();
                const instance = self.stack.pop().obj.as(Obj.Instance);
                const value = self.stack.pop();
                instance.fields[field_idx] = value;
            },
            .set_global => {
                const idx = frame.readByte();
                self.module.globals[idx] = self.stack.pop();
            },
            .set_local => frame.slots[frame.readByte()] = self.stack.pop(),
            .set_local_box => {
                const index = frame.readByte();
                frame.slots[index].obj.as(Obj.Box).value = self.stack.pop();
            },
            .str_cat => self.strConcat(),
            .str_mul_l => self.strMul(self.stack.peekRef(0).obj.as(Obj.String), self.stack.peekRef(1).int),
            .str_mul_r => self.strMul(self.stack.peekRef(1).obj.as(Obj.String), self.stack.peekRef(0).int),
            .struct_literal => {
                const arity = frame.readByte();
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
            .swap => {
                const a = self.stack.peekRef(0);
                const b = self.stack.peekRef(1);
                const tmp = a.*;
                a.* = b.*;
                b.* = tmp;
            },
            .true => self.stack.push(Value.true_),
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

// TODO: ArrayList?
// fn call(self: *Self, frame: **CallFrame, callee: *Obj.Function, args_count: usize, imported: bool) Error!void {
// fn call(self: *Self, frame: *CallFrame, args_count: usize) Error!void {
// if (self.frame_stack.count == FrameStack.FRAMES_MAX) {
//     return error.StackOverflow;
// }

// const new_frame = &self.frame_stack.frames[self.frame_stack.count];
// self.frame_stack.count += 1;
// new_frame.function = callee;
// new_frame.ip = callee.chunk.code.items.ptr;
// -1 for the function itself
// new_frame.slots = self.stack.top - args_count - 1;
// new_frame.slots = self.stack.top - args_count;
// frame.slots = self.stack.top - args_count;
// new_frame.imported = imported;
// new_frame.imported = false;
// frame.imported = false;

// frame.* = &self.frame_stack.frames[self.frame_stack.count - 1];
// }

pub fn updateModule(self: *Self, module: *Module) void {
    self.module_chain.append(self.allocator, self.module) catch oom();
    self.module = module;
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
};

pub const CallFrame = struct {
    function: *Obj.Function,
    ip: [*]u8,
    slots: [*]Value,
    // TODO: useless?
    imported: bool,

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
    pub fn initCall(self: *CallFrame, callee: *Obj, stack: *Stack, args_count: usize) void {
        self.imported = false;
        self.slots = stack.top - args_count;

        const function = switch (callee.kind) {
            .closure => b: {
                const closure = callee.as(Obj.Closure);
                const capt_len = closure.captures.len;

                std.mem.copyBackwards(Value, self.slots[capt_len .. capt_len + args_count], self.slots[0..args_count]);
                @memcpy(self.slots, closure.captures);
                stack.top += capt_len;

                break :b closure.function;
            },
            .function => callee.as(Obj.Function),
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
};
