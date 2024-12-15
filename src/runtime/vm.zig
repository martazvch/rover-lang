const std = @import("std");
const print = std.debug.print;
const config = @import("config");
const Allocator = std.mem.Allocator;
const Gc = @import("gc.zig").Gc;
const Value = @import("values.zig").Value;
const Chunk = @import("../backend/chunk.zig").Chunk;
const OpCode = @import("../backend/chunk.zig").OpCode;
const Table = @import("table.zig").Table;
const Obj = @import("obj.zig").Obj;
const ObjString = @import("obj.zig").ObjString;

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
        return (self.top - 1 - distance)[0];
    }

    fn peek_ref(self: *Self, distance: usize) *Value {
        return &(self.top - 1 - distance)[0];
    }
};

pub const Vm = struct {
    gc: Gc,
    stack: Stack,
    chunk: *const Chunk,
    ip: [*]u8,
    allocator: Allocator,
    stdout: std.fs.File.Writer,
    strings: Table,
    init_string: *ObjString,
    objects: ?*Obj,
    globals: [256]Value, // TODO: ArrayList or constant, not hard written

    const Self = @This();

    pub fn new(allocator: Allocator) Self {
        return .{
            .gc = Gc.init(allocator),
            .stack = Stack.new(),
            .chunk = undefined,
            .ip = undefined,
            .allocator = undefined,
            .stdout = std.io.getStdOut().writer(),
            .strings = undefined,
            .init_string = undefined,
            .objects = null,
            .globals = undefined,
        };
    }

    pub fn init(self: *Self) !void {
        self.gc.link(self);
        self.allocator = self.gc.allocator();

        self.stack.init();

        self.strings = Table.init(self.allocator);
        self.init_string = try ObjString.copy(self, "init");
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

    pub fn run(self: *Self, chunk: *const Chunk) !void {
        self.chunk = chunk;
        self.ip = self.chunk.code.items.ptr;
        try self.execute();
    }

    pub fn run_slice(self: *Self, chunk: *const Chunk, start: usize) !void {
        self.chunk = chunk;
        self.ip = self.chunk.code.items[start..].ptr;
        try self.execute();
    }

    fn execute(self: *Self) !void {
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
                const Disassembler = @import("../backend/disassembler.zig").Disassembler;
                var dis = Disassembler.init(self.chunk, self.allocator, false);
                defer dis.deinit();
                print("{s}\n", .{dis.disassembled.items});
                _ = try dis.dis_instruction(self.instruction_nb(), self.stdout);
            }

            const instruction = self.read_byte();
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
                .Constant => self.stack.push(self.read_constant()),
                .DifferentInt => self.stack.push(Value.bool_(self.stack.pop().Int != self.stack.pop().Int)),
                .DifferentFloat => self.stack.push(Value.bool_(self.stack.pop().Float != self.stack.pop().Float)),
                .DefineGlobal => {
                    const idx = self.read_byte();
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
                .GetGlobal => self.stack.push(self.globals[self.read_byte()]),
                .GreaterInt => self.stack.push(Value.bool_(self.stack.pop().Int < self.stack.pop().Int)),
                .GreaterFloat => self.stack.push(Value.bool_(self.stack.pop().Float < self.stack.pop().Float)),
                .GreaterEqualInt => self.stack.push(Value.bool_(self.stack.pop().Int <= self.stack.pop().Int)),
                .GreaterEqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float <= self.stack.pop().Float)),
                .LessInt => self.stack.push(Value.bool_(self.stack.pop().Int > self.stack.pop().Int)),
                .LessFloat => self.stack.push(Value.bool_(self.stack.pop().Float > self.stack.pop().Float)),
                .LessEqualInt => self.stack.push(Value.bool_(self.stack.pop().Int >= self.stack.pop().Int)),
                .LessEqualFloat => self.stack.push(Value.bool_(self.stack.pop().Float >= self.stack.pop().Float)),
                .False => self.stack.push(Value.bool_(false)),
                .MultiplyFloat => {
                    const rhs = self.stack.pop().Float;
                    self.stack.peek_ref(0).Float *= rhs;
                },
                .MultiplyInt => {
                    const rhs = self.stack.pop().Int;
                    self.stack.peek_ref(0).Int *= rhs;
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
                .Return => break,
                .SetGlobal => self.globals[self.read_byte()] = self.stack.pop(),
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
};
