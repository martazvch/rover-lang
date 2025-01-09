/// Documentation baby
const Foo = struct {

    pub fn fn1() void { }
    pub fn fn2() void {
        // f
    }
};

const Foo = struct {
    const Self = @This();

    pub fn fn0() void { }
    pub fn fn1(self: @This()) void { }
    pub fn fn2(self: Self) void { }
    pub fn fn3(self: Foo) void {}
    pub fn fn4() void {
    }
};

