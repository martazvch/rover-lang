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
                    value[0].log();
                    print("] ", .{});
                }
                print("\n", .{});
            }

            if (comptime config.print_instr) {
                const Disassembler = @import("../backend/disassembler.zig").Disassembler;
                const dis = Disassembler.init(self.chunk);
                _ = dis.dis_instruction(self.instruction_nb());
            }

            const instruction = self.read_byte();
            const op: OpCode = @enumFromInt(instruction);

            switch (op) {
                .Add => self.binop('+'),
                .Constant => self.stack.push(self.read_constant()),
                .Divide => self.binop('/'),
                .False => self.stack.push(Value.bool_(false)),
                .Multiply => self.binop('*'),
                .Negate => self.stack.push(Value.int(-1 * self.stack.pop().Int)),
                .Not => self.stack.push(Value.bool_(!self.stack.pop().Bool)),
                .Null => self.stack.push(Value.null_()),
                .Print => unreachable,
                .Return => {
                    self.stack.pop().log();
                    break;
                },
                .Subtract => self.binop('-'),
                .True => self.stack.push(Value.bool_(true)),
            }
        }
    }

    fn binop(self: *Self, op: u8) void {
        const rhs = self.stack.pop();
        const lhs = self.stack.pop();
        const binop_type = self.read_byte();

        const res = switch (@as(BinOpType, @enumFromInt(binop_type))) {
            .IntInt => Value.int(compute_binop(i64, lhs.Int, rhs.Int, op)),
            .FloatFloat => Value.float(compute_binop(f64, lhs.Float, rhs.Float, op)),
            .UintUint => Value.uint(compute_binop(u64, lhs.Uint, rhs.Uint, op)),
            .IntFloat => blk: {
                const cast: f64 = @floatFromInt(lhs.Int);
                break :blk Value.float(compute_binop(f64, cast, rhs.Float, op));
            },
            .IntUint => blk: {
                const cast: i64 = @intCast(rhs.Uint);
                break :blk Value.int(compute_binop(i64, lhs.Int, cast, op));
            },
            .FloatInt => blk: {
                const cast: f64 = @floatFromInt(rhs.Int);
                break :blk Value.float(compute_binop(f64, lhs.Float, cast, op));
            },
            .UintInt => blk: {
                const cast: i64 = @intCast(lhs.Uint);
                break :blk Value.int(compute_binop(i64, cast, rhs.Int, op));
            },
            .UintFloat => blk: {
                const cast: f64 = @floatFromInt(lhs.Uint);
                break :blk Value.float(compute_binop(f64, cast, rhs.Float, op));
            },
            .FloatUint => blk: {
                const cast: f64 = @floatFromInt(rhs.Int);
                break :blk Value.float(compute_binop(f64, lhs.Float, cast, op));
            },
            .Other => @panic("not implemented yet"),
        };

        self.stack.push(res);
    }
};

fn compute_binop(comptime T: type, v1: T, v2: T, op: u8) T {
    return switch (op) {
        '+' => v1 + v2,
        '-' => v1 - v2,
        '*' => v1 * v2,
        '/' => switch (@typeInfo(T)) {
            .int => @divTrunc(v1, v2),
            .float => v1 / v2,
            else => @compileError("unsupported type for division"),
        },
        else => unreachable,
    };
}
