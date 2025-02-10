pub const Scope = enum { Builtin, Global, Local };
pub const ReturnKind = enum { Explicit, ImplicitValue, ImplicitVoid };

pub const Instruction = union(enum) {
    Bool: bool,
    Float: f64,
    Int: isize,
    Null,
    String: usize,
};
