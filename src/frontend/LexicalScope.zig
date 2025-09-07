const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const AnalyzedModule = @import("Analyzer.zig").AnalyzedModule;
const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const Type = @import("types.zig").Type;
const TypeInterner = @import("types.zig").TypeInterner;
const oom = @import("../utils.zig").oom;

const Self = @This();

pub const Variable = struct {
    type: *const Type,
    kind: enum { local, global },
    initialized: bool,
    index: Index = 0,
    captured: bool = false,
    constant: bool,

    pub const Index = usize;
};

pub const Symbol = struct { type: *const Type, index: usize };
pub const ExternSymbol = struct { module_index: usize, symbol: Symbol };

pub const VariableMap = AutoHashMapUnmanaged(InternerIdx, Variable);
pub const SymbolArrMap = AutoArrayHashMapUnmanaged(InternerIdx, Symbol);
pub const ExternMap = AutoHashMapUnmanaged(InternerIdx, ExternSymbol);

scopes: ArrayListUnmanaged(Scope),
current: *Scope,
builtins: AutoHashMapUnmanaged(InternerIdx, *const Type),
symbol_count: usize,

pub const empty: Self = .{ .scopes = .{}, .current = undefined, .builtins = .{}, .symbol_count = 0 };

pub const Scope = struct {
    variables: VariableMap = .{},
    symbols: SymbolArrMap = .{},
    extern_symbols: ExternMap = .{},
    /// First is the interned identifier and second is the interned module's path key of module interner
    modules: AutoHashMapUnmanaged(InternerIdx, *const Type) = .{},
    /// Offset to apply to any index in this scope. Correspond to the numbers of locals
    /// in parent scopes (represents stack at runtime)
    offset: usize,
};

pub fn open(self: *Self, allocator: Allocator, offset_from_child: bool) void {
    const offset = if (offset_from_child) self.current.variables.count() + self.current.offset else 0;
    self.scopes.append(allocator, .{ .offset = offset }) catch oom();
    self.updateCurrent();
}

pub fn close(self: *Self) usize {
    const popped = self.scopes.pop().?;
    self.updateCurrent();
    return popped.variables.count();
}

pub fn initGlobalScope(self: *Self, allocator: Allocator, interner: *Interner, type_interner: *const TypeInterner) void {
    self.open(allocator, false);
    const builtins = std.meta.fields(TypeInterner.Cache);
    self.builtins.ensureUnusedCapacity(allocator, builtins.len) catch oom();

    inline for (builtins) |builtin| {
        self.builtins.putAssumeCapacity(interner.intern(builtin.name), @field(type_interner.cache, builtin.name));
    }
}

fn updateCurrent(self: *Self) void {
    self.current = &self.scopes.items[self.scopes.items.len - 1];
}

pub fn isGlobal(self: *Self) bool {
    return self.scopes.items.len == 1;
}

pub fn declareVar(
    self: *Self,
    allocator: Allocator,
    name: InternerIdx,
    ty: *const Type,
    captured: bool,
    initialized: bool,
    constant: bool,
) error{TooManyLocals}!usize {
    if (self.current.variables.count() == 255 and !self.isGlobal()) {
        return error.TooManyLocals;
    }

    const index = self.current.variables.count();
    self.current.variables.put(allocator, name, .{
        .type = ty,
        .kind = if (self.isGlobal()) .global else .local,
        .initialized = initialized,
        .index = index,
        .captured = captured,
        .constant = constant,
    }) catch oom();

    return index;
}

/// Tries to retreive a variable from scopes and the local offset of its scope
pub fn getVariable(self: *const Self, name: InternerIdx) ?struct { *Variable, usize } {
    var i = self.scopes.items.len;

    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        if (scope.variables.getPtr(name)) |variable| {
            return .{ variable, scope.offset };
        }
    }

    return null;
}

/// Forward declares a symbol without incrementing global symbol count
pub fn forwardDeclareSymbol(self: *Self, allocator: Allocator, name: InternerIdx) *Symbol {
    self.current.symbols.put(allocator, name, .{ .type = undefined, .index = self.symbol_count }) catch oom();
    self.symbol_count += 1;

    return self.current.symbols.getPtr(name).?;
}

/// Removes symbol name from **current** scope
pub fn removeSymbolFromScope(self: *Self, name: InternerIdx) ?Symbol {
    const sym = self.current.symbols.fetchOrderedRemove(name) orelse return null;
    return sym.value;
}

pub fn getSymbol(self: *const Self, name: InternerIdx) ?*Symbol {
    var i = self.scopes.items.len;

    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        if (scope.symbols.getPtr(name)) |sym| {
            return sym;
        }
    }

    return null;
}

pub fn declareExternSymbol(
    self: *Self,
    allocator: Allocator,
    name: InternerIdx,
    module_index: usize,
    symbol: Symbol,
) void {
    self.current.extern_symbols.put(
        allocator,
        name,
        .{ .module_index = module_index, .symbol = symbol },
    ) catch oom();
}

pub fn getExternSymbol(self: *const Self, name: InternerIdx) ?*ExternSymbol {
    var i = self.scopes.items.len;

    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        if (scope.extern_symbols.getPtr(name)) |ext| {
            return ext;
        }
    }

    return null;
}

pub fn declareModule(self: *Self, allocator: Allocator, name: InternerIdx, ty: *const Type) void {
    self.current.modules.put(allocator, name, ty) catch oom();
}

// TODO: merge all iterators in this file
pub fn getModule(self: *const Self, name: InternerIdx) ?*const Type {
    var i = self.scopes.items.len;

    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        if (scope.modules.get(name)) |ty| {
            return ty;
        }
    }

    return null;
}

pub fn getType(self: *Self, name: InternerIdx) ?*const Type {
    if (self.builtins.get(name)) |builtin| {
        return builtin;
    } else if (self.getSymbol(name)) |sym| {
        return sym.type;
    }

    return null;
}

pub fn isVarOrSymInCurrentScope(self: *const Self, name: InternerIdx) bool {
    return self.current.variables.get(name) != null or
        self.current.symbols.get(name) != null;
}

pub fn isModuleImported(self: *const Self, name: InternerIdx) bool {
    return self.getModule(name) != null;
}
