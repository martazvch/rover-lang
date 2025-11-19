pub const Scope = enum { builtin, global, local };
pub const Type = enum(u2) { float, int };
pub const RcAction = enum { increment, cow, none };

pub const Index = usize;
pub const IndexVoid = @import("std").math.maxInt(Index);

pub const TypeId = @import("types.zig").TypeId;
pub const SymbolIndex = usize;

pub const Instruction = struct {
    data: Data,
    offset: usize,

    pub const Data = union(enum) {
        array: Array,
        array_access: ArrayAccess,
        assignment: Assignment,
        binop: Binop,
        block: Block,
        bool: bool,
        box: Index,
        bound_method: BoundMethod,
        @"break": Break,
        call: Call,
        constant: Constant,
        discard: Index,
        enum_create: EnumCreate,
        enum_decl: EnumDecl,
        field: Field,
        float: f64,
        fn_decl: FnDecl,
        identifier: Variable,
        @"if": If,
        incr_rc: Index,
        int: i64,
        load_symbol: LoadSymbol,
        load_builtin: usize,
        match: Match,
        multiple_var_decl: MultiVarDecl,
        null,
        pat_nullable: Index,
        pop: Index,
        print: Index,
        @"return": Return,
        string: usize,
        struct_decl: StructDecl,
        struct_literal: StructLiteral,
        unary: Unary,
        unbox: Index,
        var_decl: VarDecl,
        when: When,
        @"while": While,

        noop, // Used only by 'use' statements as they don't produce any instructions
    };

    pub const ArrayAccess = struct { array: Index, indicies: []const Index };
    pub const Binop = struct {
        lhs: Index,
        rhs: Index,
        op: Op,

        pub const Op = enum {
            add_float,
            add_int,
            add_str,
            @"and",
            div_float,
            div_int,
            eq_bool,
            eq_float,
            eq_int,
            eq_null,
            eq_str,
            eq_tag,
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
            ne_null,
            ne_str,
            ne_tag,
            @"or",
            sub_float,
            sub_int,
        };
    };

    pub const Array = struct {
        values: []const Index,
    };
    pub const Assignment = struct {
        assigne: Index,
        value: Index,
        cow: bool,
    };
    pub const Block = struct {
        instrs: []const Index,
        pop_count: u8,
        is_expr: bool,
    };
    pub const BoundMethod = struct { structure: Index, index: usize };
    pub const Break = struct { instr: ?Index, depth: usize };
    pub const Call = struct {
        callee: Index,
        args: []const Arg,
        ext_mod: ?usize,
        native: bool,
    };
    pub const Arg = union(enum) {
        instr: Index,
        default: struct { const_index: usize, mod: ?usize },
    };
    pub const Constant = struct {
        index: usize,
        instr: Index,
    };
    pub const EnumCreate = struct {
        sym: LoadSymbol,
        tag_index: usize,
    };
    pub const EnumDecl = struct {
        name: usize,
        sym_index: SymbolIndex,
        functions: []const Index,
    };
    pub const Field = struct {
        structure: Index,
        index: usize,
        kind: Kind,

        pub const Kind = enum { field, function };
    };
    pub const FnDecl = struct {
        sym_index: SymbolIndex,
        type_id: TypeId,
        name: ?usize,
        body: []const Index,
        defaults: []const Index,
        captures: []const Capture,
        returns: bool,

        pub const Capture = struct { index: usize, local: bool };
    };
    pub const If = struct {
        cond: Index,
        then: Index,
        @"else": ?Index,
    };
    pub const LoadSymbol = struct {
        module_index: ?usize,
        symbol_index: u8,
    };
    pub const Match = struct {
        expr: Index,
        arms: []const Arm,
        is_expr: bool,

        pub const Arm = struct { expr: Index, body: Index };
    };
    pub const MultiVarDecl = struct { decls: []const Index };
    pub const Return = struct { value: ?Index };
    pub const StructDecl = struct {
        // Interner index
        name: usize,
        sym_index: SymbolIndex,
        type_id: TypeId,
        fields_count: usize,
        default_fields: []const Index,
        functions: []const Index,
    };
    pub const StructLiteral = struct {
        structure: Index,
        values: []const Arg,
    };
    pub const Unary = struct {
        op: Op,
        typ: Type,
        instr: Index,

        pub const Op = enum { minus, bang };
    };
    pub const VarDecl = struct {
        box: bool,
        variable: Variable,
        value: ?Index,
    };
    pub const Variable = struct {
        index: u64,
        scope: Scope,
    };
    pub const When = struct {
        expr: Index,
        arms: []const Arm,
        is_expr: bool,

        pub const Arm = struct { type_id: TypeId, body: Index };
    };
    pub const While = struct { cond: Index, body: Index };
};
