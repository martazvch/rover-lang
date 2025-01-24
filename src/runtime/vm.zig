const std = @import("std");
const print = std.debug.print;
const config = @import("config");
const Allocator = std.mem.Allocator;
const Stmt = @import("../frontend/ast.zig").Stmt;
const AnalyzedStmt = @import("../frontend/analyzed_ast.zig").AnalyzedStmt;
const Gc = @import("gc.zig").Gc;
const Value = @import("values.zig").Value;
const Chunk = @import("../backend/chunk.zig").Chunk;
const OpCode = @import("../backend/chunk.zig").OpCode;
const Table = @import("table.zig").Table;
const Obj = @import("obj.zig").Obj;
const ObjString = @import("obj.zig").ObjString;
const ObjFunction = @import("obj.zig").ObjFunction;
const ObjNativeFn = @import("obj.zig").ObjNativeFn;
const Disassembler = @import("../backend/disassembler.zig").Disassembler;

// PERF: bench avec BoundedArray
const Stack = struct {
    values: [STACK_SIZE]Value,
    top: [*]Value,

    const STACK_SIZE: u16 = @as(u16, FrameStack.FRAMES_MAX) * @as(u16, std.math.maxInt(u8));
    const Self = @This();

    pub fn new() Self {
        return .{
            .values = undefined,
            .top = undefined,
        };
    }

    pub fn init(self: *Self) void {
        self.top = self.values[0..].ptr;
    }

    pub fn push(self: *Self, value: Value) void {
        self.top[0] = value;
        self.top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.top -= 1;
        return self.top[0];
    }

    fn peek(self: *const Self, distance: usize) Value {
        return (self.top - 1 - distance)[0];
    }

    fn peek_ref(self: *Self, distance: usize) *Value {
        return &(self.top - 1 - distance)[0];
    }
};

const CallFrame = struct {
    // closure: *ObjClosure,
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,

    const Self = @This();

    pub fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn read_constant(self: *Self) Value {
        return self.function.chunk.constants[self.read_byte()];
    }

    pub fn read_string(self: *Self) *ObjString {
        return self.read_constant().as_obj().?.as(ObjString);
    }

    pub fn read_short(self: *Self) u16 {
        const part1 = self.read_byte();
        const part2 = self.read_byte();

        return (@as(u16, part1) << 8) | part2;
    }
};

const FrameStack = struct {
    frames: [FRAMES_MAX]CallFrame,
    count: usize,

    const Self = @This();
    const FRAMES_MAX: u8 = 64;

    pub fn new() Self {
        return .{
            .frames = undefined,
            .count = 0,
        };
    }
};

pub const Vm = struct {
    gc: Gc,
    stack: Stack,
    frame_stack: FrameStack,
    ip: [*]u8,
    allocator: Allocator,
    stdout: std.fs.File.Writer,
    strings: Table,
    init_string: *ObjString,
    objects: ?*Obj,
    globals: [256]Value, // TODO: ArrayList or constant, not hard written

    const Self = @This();
    const Error = error{StackOverflow} || Allocator.Error;

    pub fn new(allocator: Allocator) Self {
        return .{
            .gc = Gc.init(allocator),
            .stack = Stack.new(),
            .frame_stack = FrameStack.new(),
            .ip = undefined,
            .allocator = undefined,
            .stdout = std.io.getStdOut().writer(),
            .strings = undefined,
            .init_string = undefined,
            .objects = null,
            .globals = undefined,
        };
    }

    pub fn init(self: *Self, repl: bool) !void {
        self.gc.link(self);
        self.allocator = self.gc.allocator();
        self.stack.init();
        self.strings = Table.init(self.allocator);
        self.init_string = try ObjString.copy(self, "init");

        // In REPL mode, we won't call the main function (there is not)
        // so we increment ourself the frame stack (discaring the first one)
        // but the count is coherent of what is expected below, for example
        // for function call we exit if the frame stack count == 1. In REPL
        // it would be always true
        if (repl) self.frame_stack.count += 1;
    }

    pub fn deinit(self: *Self) void {
        self.gc.deinit();
        self.strings.deinit();
        self.free_objects();
    }

    fn free_objects(self: *Self) void {
        var object = self.objects;
        while (object) |obj| {
            const next = obj.next;
            obj.destroy(self);
            object = next;
        }
    }

    /// Returns the error gave in *kind* parameter and prints backtrace
    fn err(self: *Self, kind: Error) Error {
        for (0..self.frame_stack.count) |i| {
            const idx = self.frame_stack.count - i - 1;
            const frame = self.frame_stack.frames[idx];
            const function = frame.function;

            const instr = self.instruction_nb();
            self.stdout.print("[line {}] in ", .{function.chunk.offsets.items[instr]});

            if (function.name) |name| {
                print("{s}()\n", .{name});
            } else print("script\n", .{});
        }

        return kind;
    }

    fn instruction_nb(self: *const Self) usize {
        const frame = &self.frame_stack.frames[self.frame_stack.count - 1];
        const addr1 = @intFromPtr(frame.ip);
        const addr2 = @intFromPtr(frame.function.chunk.code.items.ptr);
        return addr1 - addr2;
    }

    pub fn run(self: *Self, func: *ObjFunction) !void {
        // Initialize with the global scope
        try self.call(func, 0);
        try self.execute();
    }

    fn execute(self: *Self) !void {
        var frame = &self.frame_stack.frames[self.frame_stack.count - 1];

        while (true) {
            if (comptime config.print_stack) {
                print("          ", .{});

                var value = self.stack.values[0..].ptr;

                while (value != self.stack.top) : (value += 1) {
                    print("[", .{});
                    try value[0].print(self.stdout);
                    print("] ", .{});
                }
                print("\n", .{});
            }

            if (comptime config.print_instr) {
                var dis = Disassembler.init(&frame.function.chunk, self.allocator, false);
                defer dis.deinit();
                _ = try dis.dis_instruction(self.instruction_nb(), self.stdout);
            }

            const instruction = frame.read_byte();
            const op: OpCode = @enumFromInt(instruction);

            switch (op) {
                .AddFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref(0).Float += rhs;
                },
                .AddInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref(0).Int += rhs;
                },
                .CastToFloat => self.stack.push(Value.float(@floatFromInt(self.stack.pop().Int))),
                .Constant => self.stack.push(frame.read_constant()),
                .DifferentInt => self.stack.push(Value.bool_(self.stack.pop().Int != self.stack.pop().Int)),
                .DifferentFloat => self.stack.push(Value.bool_(self.stack.pop().Float != self.stack.pop().Float)),
                .DefineGlobal => {
                    const idx = frame.read_byte();
                    self.globals[idx] = self.stack.pop();
                },
                .DivideFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref(0).Float /= rhs;
                },
                .DivideInt => {
                    const rhs = self.stack.pop().Int;
                    const lhs = self.stack.pop().Int;
                    self.stack.push(Value.int(@divTrunc(lhs, rhs)));
                },
                .EqualInt => self.stack.push(Value.bool_(self.stack.pop().Int == self.stack.pop().Int)),
                .EqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float == self.stack.pop().Float)),
                .EqualStr => self.stack.push(Value.bool_(self.stack.pop().Obj.as(ObjString) == self.stack.pop().Obj.as(ObjString))),
                .False => self.stack.push(Value.bool_(false)),
                .FnCall => {
                    const args_count = frame.read_byte();
                    const callee = self.stack.peek_ref(args_count).Obj.as(ObjFunction);
                    try self.call(callee, args_count);

                    // If call success, we need to to set frame pointer back
                    frame = &self.frame_stack.frames[self.frame_stack.count - 1];
                },
                .GetGlobal => self.stack.push(self.globals[frame.read_byte()]),
                .GetLocal => self.stack.push(frame.slots[frame.read_byte()]),
                .GreaterInt => self.stack.push(Value.bool_(self.stack.pop().Int < self.stack.pop().Int)),
                .GreaterFloat => self.stack.push(Value.bool_(self.stack.pop().Float < self.stack.pop().Float)),
                .GreaterEqualInt => self.stack.push(Value.bool_(self.stack.pop().Int <= self.stack.pop().Int)),
                .GreaterEqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float <= self.stack.pop().Float)),
                .Jump => {
                    const jump = frame.read_short();
                    frame.ip += jump;
                },
                .JumpIfFalse => {
                    const jump = frame.read_short();
                    if (!self.stack.peek(0).Bool) frame.ip += jump;
                },
                .JumpIfTrue => {
                    const jump = frame.read_short();
                    if (self.stack.peek(0).Bool) frame.ip += jump;
                },
                .LessInt => self.stack.push(Value.bool_(self.stack.pop().Int > self.stack.pop().Int)),
                .LessFloat => self.stack.push(Value.bool_(self.stack.pop().Float > self.stack.pop().Float)),
                .LessEqualInt => self.stack.push(Value.bool_(self.stack.pop().Int >= self.stack.pop().Int)),
                .LessEqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float >= self.stack.pop().Float)),
                .Loop => {
                    const jump = frame.read_short();
                    frame.ip -= jump;
                },
                .MultiplyFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref(0).Float *= rhs;
                },
                .MultiplyInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref(0).Int *= rhs;
                },
                .NativeFnCall => {
                    const args_count = frame.read_byte();
                    const native = self.stack.peek_ref(args_count).Obj.as(ObjNativeFn).function;
                    const result = native((self.stack.top - args_count)[0..args_count]);

                    self.stack.top -= args_count + 1;
                    self.stack.push(result);
                },
                .NegateFloat => self.stack.peek_ref(0).Float *= -1,
                .NegateInt => self.stack.peek_ref(0).Int *= -1,
                .Not => self.stack.peek_ref(0).not(),
                .Null => self.stack.push(Value.null_()),
                .Pop => _ = self.stack.pop(),
                .Print => {
                    try self.stack.pop().print(self.stdout);
                    _ = try self.stdout.write("\n");
                },
                .ExitRepl => {
                    // Here, there is no value to pop for now, no implicit null is
                    // put on top of the stack
                    self.frame_stack.count -= 1;

                    // Implicit print the last expression in REPL mode. For now, analyzer
                    // dosen't allow to write an expression returning a value without
                    // assignment or so

                    // if (self.stack.top == self.stack.values[0..].ptr) {
                    //     const res = self.stack.pop();
                    //     try res.print(self.stdout);
                    // }

                    break;
                },
                .Return => {
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
                    const locals_count = frame.read_byte();
                    const res = self.stack.pop();

                    for (0..locals_count) |_| _ = self.stack.pop();
                    self.stack.push(res);
                },
                .SetGlobal => self.globals[frame.read_byte()] = self.stack.pop(),
                .SetLocal => frame.slots[frame.read_byte()] = self.stack.pop(),
                .StrCat => try self.str_concat(),
                .StrMulL => try self.str_mul(self.stack.peek_ref(0).Obj.as(ObjString), self.stack.peek_ref(1).Int),
                .StrMulR => try self.str_mul(self.stack.peek_ref(1).Obj.as(ObjString), self.stack.peek_ref(0).Int),
                .SubtractFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref(0).Float -= rhs;
                },
                .SubtractInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref(0).Int -= rhs;
                },
                .True => self.stack.push(Value.bool_(true)),
            }
        }
    }

    fn str_concat(self: *Self) !void {
        const s2 = self.stack.peek_ref(0).Obj.as(ObjString);
        const s1 = self.stack.peek_ref(1).Obj.as(ObjString);

        const res = try self.allocator.alloc(u8, s1.chars.len + s2.chars.len);
        @memcpy(res[0..s1.chars.len], s1.chars);
        @memcpy(res[s1.chars.len..], s2.chars);

        // Pop after alloc in case of GC trigger
        _ = self.stack.pop();
        _ = self.stack.pop();

        self.stack.push(Value.obj((try ObjString.take(self, res)).as_obj()));
    }

    fn str_mul(self: *Self, str: *const ObjString, factor: i64) !void {
        // BUG: Check if factor is positive
        const f = @as(usize, @intCast(factor));
        const res = try self.allocator.alloc(u8, str.chars.len * f);
        for (0..f) |i| {
            @memcpy(res[i * str.chars.len .. (i + 1) * str.chars.len], str.chars);
        }

        // Pop after alloc in case of GC trigger
        _ = self.stack.pop();
        _ = self.stack.pop();

        self.stack.push(Value.obj((try ObjString.take(self, res)).as_obj()));
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
};
