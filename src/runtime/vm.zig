const std = @import("std");
const print = std.debug.print;
const config = @import("config");
const Allocator = std.mem.Allocator;
const BoundedArray = std.BoundedArray;
const Value = @import("values.zig").Value;
const Chunk = @import("../backend/chunk.zig").Chunk;
const OpCode = @import("../backend/chunk.zig").OpCode;
const BinOpType = @import("../backend/chunk.zig").BinOpType;

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

    fn peek_ref(self: *Self) *Value {
        return &(self.top - 1)[0];
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
            if (comptime config.print_stack) {
                print("          ", .{});

                var value = self.stack.values[0..].ptr;
                while (value != self.stack.top) : (value += 1) {
                    print("[", .{});
                    try value[0].log(self.stdout);
                    print("] ", .{});
                }
                print("\n", .{});
            }

            if (comptime config.print_instr) {
                const Disassembler = @import("../backend/disassembler.zig").Disassembler;
                const dis = Disassembler.init(self.chunk, self.allocator, false);
                defer dis.deinit();
                print("{s}\n", .{dis.disassembled.items});
                _ = try dis.dis_instruction(self.instruction_nb());
            }

            const instruction = self.read_byte();
            const op: OpCode = @enumFromInt(instruction);

            switch (op) {
                .AddFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref().Float += rhs;
                },
                .AddInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref().Int += rhs;
                },
                .CastToFloat => self.stack.push(Value.float(@floatFromInt(self.stack.pop().Int))),
                .Constant => self.stack.push(self.read_constant()),
                .DifferentInt => self.stack.push(Value.bool_(self.stack.pop().Int != self.stack.pop().Int)),
                .DifferentFloat => self.stack.push(Value.bool_(self.stack.pop().Float != self.stack.pop().Float)),
                .DivideFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref().Float /= rhs;
                },
                .DivideInt => {
                    const rhs = self.stack.pop().Int;
                    const lhs = self.stack.pop().Int;
                    self.stack.push(Value.int(@divTrunc(lhs, rhs)));
                },
                .EqualInt => self.stack.push(Value.bool_(self.stack.pop().Int == self.stack.pop().Int)),
                .EqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float == self.stack.pop().Float)),
                .GreaterInt => self.stack.push(Value.bool_(self.stack.pop().Int > self.stack.pop().Int)),
                .GreaterFloat => self.stack.push(Value.bool_(self.stack.pop().Float > self.stack.pop().Float)),
                .GreaterEqualInt => self.stack.push(Value.bool_(self.stack.pop().Int >= self.stack.pop().Int)),
                .GreaterEqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float >= self.stack.pop().Float)),
                .LessInt => self.stack.push(Value.bool_(self.stack.pop().Int < self.stack.pop().Int)),
                .LessFloat => self.stack.push(Value.bool_(self.stack.pop().Float < self.stack.pop().Float)),
                .LessEqualInt => self.stack.push(Value.bool_(self.stack.pop().Int <= self.stack.pop().Int)),
                .LessEqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float <= self.stack.pop().Float)),
                .False => self.stack.push(Value.bool_(false)),
                .MultiplyFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref().Float *= rhs;
                },
                .MultiplyInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref().Int *= rhs;
                },
                .NegateFloat => self.stack.peek_ref().Float *= -1,
                .NegateInt => self.stack.peek_ref().Int *= -1,
                .Not => self.stack.peek_ref().not(),
                .Null => self.stack.push(Value.null_()),
                .Print => unreachable,
                .Return => {
                    try self.stack.pop().log(self.stdout);
                    break;
                },
                .SubtractFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref().Float -= rhs;
                },
                .SubtractInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref().Int -= rhs;
                },
                .True => self.stack.push(Value.bool_(true)),
            }
        }
    }
};
