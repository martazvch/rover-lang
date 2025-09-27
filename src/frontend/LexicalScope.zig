const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const AnalyzedModule = @import("Analyzer.zig").AnalyzedModule;
const Interner = @import("../Interner.zig");
const InternerIdx = Interner.Index;
const Type = @import("types.zig").Type;
const TypeInterner = @import("types.zig").TypeInterner;
const InstrIndex = @import("rir.zig").Index;
const Span = @import("Lexer.zig").Span;
const oom = @import("../utils.zig").oom;

const Self = @This();

pub const Variable = struct {
    type: *const Type,
    kind: enum { local, global },
    initialized: bool,
    index: Index,
    captured: bool,
    constant: bool,
    comp_time: bool,

    pub const Index = usize;
};

pub const Break = struct {
    instr: InstrIndex,
    type: *const Type,
    span: Span,
};

pub const Symbol = struct { type: *const Type, index: usize };
pub const ExternSymbol = struct { module_index: usize, symbol: Symbol };

pub const VariableMap = AutoHashMapUnmanaged(InternerIdx, Variable);
pub const SymbolArrMap = AutoArrayHashMapUnmanaged(InternerIdx, Symbol);
pub const ExternMap = AutoHashMapUnmanaged(InternerIdx, ExternSymbol);

scopes: ArrayList(Scope),
current: *Scope,
builtins: AutoHashMapUnmanaged(InternerIdx, *const Type),
symbol_count: usize,

pub const empty: Self = .{ .scopes = .empty, .current = undefined, .builtins = .{}, .symbol_count = 0 };

pub const Scope = struct {
    name: ?InternerIdx,
    variables: VariableMap = .empty,
    forwarded: VariableMap = .empty,
    symbols: SymbolArrMap = .empty,
    extern_symbols: ExternMap = .empty,
    /// First is the interned identifier and second is the interned module's path key of module interner
    modules: AutoHashMapUnmanaged(InternerIdx, *const Type) = .empty,
    breaks: ArrayList(Break) = .empty,
    /// Offset to apply to any index in this scope. Correspond to the numbers of locals
    /// in parent scopes (represents stack at runtime)
    offset: usize,
};

// pub fn open(self: *Self, allocator: Allocator, offset_from_parent: bool) void {
// pub fn open(self: *Self, allocator: Allocator, fn_body: bool, name: ?InternerIdx) void {
pub fn open(self: *Self, allocator: Allocator, fn_body: bool, name: ?InternerIdx) void {
    // TODO: invert name and usage
    const offset = if (fn_body) self.current.variables.count() + self.current.offset else 0;
    var scope: Scope = .{ .name = name, .offset = offset };

    // If variables have been forwarded, for declare them now
    if (self.scopes.items.len > 0) {
        scope.variables.ensureUnusedCapacity(allocator, self.current.forwarded.count()) catch oom();

        var it = self.current.forwarded.iterator();

        while (it.next()) |entry| {
            scope.variables.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        self.current.forwarded.clearRetainingCapacity();
    }

    self.scopes.append(allocator, scope) catch oom();
    self.updateCurrent();
}

pub fn close(self: *Self) struct { usize, []const Break } {
    const popped = self.scopes.pop().?;
    self.updateCurrent();
    return .{ popped.variables.count(), popped.breaks.items };
}

pub fn initGlobalScope(self: *Self, allocator: Allocator, interner: *Interner, type_interner: *const TypeInterner) void {
    self.open(allocator, false, null);
    const builtins = std.meta.fields(TypeInterner.Cache);
    self.builtins.ensureUnusedCapacity(allocator, builtins.len) catch oom();

    inline for (builtins) |builtin| {
        self.builtins.putAssumeCapacity(interner.intern(builtin.name), @field(type_interner.cache, builtin.name));
    }
}

/// Update `current` field to last scope. **Assumes** that there is at least one scope
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
    comp_time: bool,
) error{TooManyLocals}!usize {
    const index = self.current.variables.count();
    if (index == 255 and !self.isGlobal()) return error.TooManyLocals;

    self.current.variables.put(allocator, name, .{
        .type = ty,
        .kind = if (self.isGlobal()) .global else .local,
        .initialized = initialized,
        .index = index,
        .captured = captured,
        .constant = constant,
        .comp_time = comp_time,
    }) catch oom();

    return index;
}

pub fn declareVarInFutureScope(self: *Self, allocator: Allocator, name: InternerIdx, ty: *const Type, captured: bool) error{TooManyLocals}!void {
    const index = self.current.forwarded.count();
    if (index == 255) return error.TooManyLocals;

    self.current.forwarded.put(allocator, name, .{
        .type = ty,
        .kind = .local,
        .initialized = true,
        .index = index,
        .captured = captured,
        .constant = true,
        .comp_time = false,
    }) catch oom();
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

pub fn addBreak(self: *Self, allocator: Allocator, brk: Break, label: ?InternerIdx) error{UnknownLabel}!void {
    const lbl = label orelse {
        self.current.breaks.append(allocator, brk) catch oom();
        return;
    };

    var i = self.scopes.items.len;

    while (i > 0) {
        i -= 1;
        const scope = &self.scopes.items[i];

        if (scope.name) |sn| {
            if (sn != lbl) continue;

            scope.breaks.append(allocator, brk) catch oom();
        }

        return error.UnknownLabel;
    }
}

// TODO: shouldn't it be in type interner?
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
