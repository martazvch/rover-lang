const std = @import("std");
const print = std.debug.print;
const config = @import("config");
const Allocator = std.mem.Allocator;
const BoundedArray = std.BoundedArray;
const Value = @import("values.zig").Value;
const Chunk = @import("../backend/chunk.zig").Chunk;
const OpCode = @import("../backend/chunk.zig").OpCode;

// PERF: bench avec BoundedArray
const Stack = struct {
    values: [STACK_SIZE]Value,
    top: [*]Value,

    const STACK_SIZE = std.math.maxInt(u8) + 1;
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
        // Pointer arithmetic
        return (self.top - 1 - distance)[0];
    }
};

pub const Vm = struct {
    stack: Stack,
    chunk: *Chunk,
    ip: [*]u8,
    allocator: Allocator,
    stdout: std.fs.File.Writer,

    const Self = @This();

    pub fn new(allocator: Allocator, chunk: *Chunk) Self {
        return .{
            .stack = Stack.new(),
            .chunk = chunk,
            .ip = undefined,
            .allocator = allocator,
            .stdout = std.io.getStdOut().writer(),
        };
    }

    pub fn init(self: *Self) void {
        self.stack.init();
        self.ip = self.chunk.code.items.ptr;
    }

    pub fn deinit(self: *Self) void {
        _ = &self;
    }

    pub fn read_byte(self: *Self) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn read_constant(self: *Self) Value {
        return self.chunk.constants[self.read_byte()];
    }

    fn instruction_nb(self: *const Self) usize {
        const addr1 = @intFromPtr(self.ip);
        const addr2 = @intFromPtr(self.chunk.code.items.ptr);
        return addr1 - addr2;
    }

    pub fn run(self: *Self) !void {
        while (true) {
            if (config.print_stack) {
                print("          ", .{});

                var value = self.stack.values[0..].ptr;
                while (value != self.stack.top) : (value += 1) {
                    print("[", .{});
                    value[0].log();
                    print("] ", .{});
                }
                print("\n", .{});
            }

            if (config.print_instr) {
                const Disassembler = @import("../backend/disassembler.zig").Disassembler;
                const dis = Disassembler.init(self.chunk);
                _ = dis.dis_instruction(self.instruction_nb());
            }

            const instruction = self.read_byte();
            const op: OpCode = @enumFromInt(instruction);

            switch (op) {
                .Add => self.stack.push(Value.int(self.stack.pop().Int + self.stack.pop().Int)),
                .Constant => self.stack.push(self.read_constant()),
                .Divide => self.stack.push(Value.int(@divTrunc(self.stack.pop().Int, self.stack.pop().Int))),
                .Multiply => self.stack.push(Value.int(self.stack.pop().Int * self.stack.pop().Int)),
                .Negate => self.stack.push(Value.int(-1 * self.stack.pop().Int)),
                .Print => unreachable,
                .Return => {
                    self.stack.pop().log();
                    break;
                },
                .Subtract => self.stack.push(Value.int(self.stack.pop().Int - self.stack.pop().Int)),
            }
        }
    }
};
