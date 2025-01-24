const std = @import("std");
const Analyzer = @import("analyzer.zig").Analyzer;
const TypeSys = @import("type_system.zig");
const native_clock = @import("../../std/time/time.zig").native_clock;

// Re-export constants
const Void = TypeSys.Void;
const Null = TypeSys.Null;
const Int = TypeSys.Int;
const Float = TypeSys.Float;
const Bool = TypeSys.Bool;
const Str = TypeSys.Str;
