// TODO: remove heap
pub const Scope = enum { builtin, global, heap, local };
pub const Type = enum(u2) { float, int };
pub const ReturnKind = enum(u2) { explicit, implicit_value, implicit_void };
pub const RcAction = enum { increment, cow, none };

pub const Instruction = struct {
    data: Data,
    offset: usize,

    pub const Data = union(enum) {
        array: Array,
        array_access,
        array_access_chain: ArrayAccessChain,
        assignment: Assignment,
        binop: Binop,
        block: Block,
        bool: bool,
        bound_method: usize,
        call: Call,
        capture: Capture,
        cast: Type,
        discard,
        field: Field,
        float: f64,
        fn_decl: FnDecl,
        identifier: Variable,
        @"if": If,
        int: i64,
        load_symbol: LoadSymbol,
        multiple_var_decl: usize,
        name: usize,
        null,
        pop,
        print,
        @"return": Return,
        string: usize,
        struct_decl: StructDecl,
        default_value: usize,
        struct_literal: StructLiteral,
        unary: Unary,
        value: Value,
        var_decl: VarDecl,
        @"while",
    };

    pub const Binop = struct {
        cast: Cast = .none,
        op: Op,

        pub const Cast = enum(u2) { lhs, rhs, none };
        pub const Op = enum(u6) {
            add_float,
            add_int,
            add_str,
            @"and",
            div_float,
            div_int,
            eq_bool,
            eq_float,
            eq_int,
            eq_str,
            ge_float,
            ge_int,
            gt_float,
            gt_int,
            le_float,
            le_int,
            lt_float,
            lt_int,
            mul_float,
            mul_int,
            mul_str,
            ne_bool,
            ne_float,
            ne_int,
            ne_str,
            @"or",
            sub_float,
            sub_int,
        };
    };

    pub const Array = struct {
        elems: []const Elem,

        pub const Elem = struct { cast: bool, incr_rc: bool };
    };
    // TODO: see if cow/rc actually used
    pub const ArrayAccessChain = struct { depth: usize };
    pub const Assignment = struct {
        /// Casts to float the value before assignment
        cast: bool,
        /// When the assigne is a heap allocated object, we check if there are shallow copy
        /// of it before mutation. If so, check the reference count and perform a deep copy if
        /// it is referenced
        cow: bool,
        /// Increment assigned value reference count
        incr_rc: bool,
    };
    pub const Block = struct { length: usize, pop_count: u8, is_expr: bool };
    pub const Call = struct { arity: u8, implicit_first: bool };
    pub const Capture = struct { index: usize, is_local: bool };
    pub const FnDecl = struct {
        kind: Kind,
        name: ?usize,
        cast: bool,
        body_len: u64,
        default_params: u8,
        captures_count: usize,
        return_kind: ReturnKind,

        pub const Kind = union(enum) { closure, symbol: usize };
    };
    pub const Field = struct {
        index: usize,
        kind: Kind,

        pub const Kind = enum { method, field, static_method, symbol };
    };
    pub const If = struct {
        cast: Cast,
        has_else: bool,
        incr_rc_then: bool,
        incr_rc_else: bool,

        pub const Cast = enum(u2) { then, @"else", none };
    };
    pub const LoadSymbol = struct {
        module_index: ?usize,
        symbol_index: u8,
    };
    pub const Return = struct { value: bool, cast: bool };
    pub const StructDecl = struct { index: usize, fields_count: usize, default_fields: usize, func_count: usize };
    pub const StructLiteral = struct { fields_count: u8 };
    pub const Value = struct {
        value_instr: usize,
        cast: bool,
        box: bool,
        incr_rc: bool,
    };
    pub const Unary = struct {
        op: Op,
        typ: Type,

        pub const Op = enum { minus, bang };
    };
    pub const VarDecl = struct {
        box: bool,
        cast: bool,
        variable: Variable,
        has_value: bool,
        /// Increment reference count of value
        incr_rc: bool,
    };
    pub const Variable = struct {
        index: u64,
        scope: Scope,
        unbox: bool,
    };
};
