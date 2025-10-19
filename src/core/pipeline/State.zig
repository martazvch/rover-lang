const std = @import("std");
const Allocator = std.mem.Allocator;

const TypeInterner = @import("../analyzer/types.zig").TypeInterner;
const ModuleInterner = @import("ModuleInterner.zig");
const NativeRegister = @import("NativesRegister.zig");

const misc = @import("misc");
const Interner = misc.Interner;
const Sb = misc.StringBuilder;

config: Config,
interner: Interner,
type_interner: TypeInterner,
path_builder: Sb,
module_interner: ModuleInterner,
native_reg: NativeRegister,

const Self = @This();

pub fn new(allocator: Allocator, config: Config) Self {
    var ctx: Self = .{
        .config = config,
        .interner = .init(allocator),
        .type_interner = .init(allocator),
        .path_builder = .empty,
        .module_interner = .init(allocator),
        .native_reg = .empty,
    };
    ctx.type_interner.cacheFrequentTypes();

    return ctx;
}

pub fn registerNatives(self: *Self, allocator: Allocator, Module: type) void {
    self.native_reg.register(allocator, &self.interner, &self.type_interner, Module);
}

pub const Config = struct {
    embedded: bool = false,
    print_ast: bool = false,
    print_bytecode: bool = false,
    static_analyzis: bool = false,
    print_ir: bool = false,
};
