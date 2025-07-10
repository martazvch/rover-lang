# Various Notes

## Spec

### Expressions

- Variable should be declared with const if they aren't mutated
- Last expression of a block is an implicit return
- Assignment is not an expression, we can't chain. It's because we don't want that an assignment
at the end of a scope being the returned value
- Each control flow is an expression, all branches must return the same value:
   - If
   - loop
   - Match
- Regarding returned value:
    - If:
        - Each branch must return the same type unless exit scope
        - Each branch can exit the current scope with a `return` statement
    - While:
        - Can be exited with a `break` statement with a value after or not
    - loop:
        - Same as while
    - Match:
        - Each branch must return the same type
- While can't be an expression because of the fact that you may never enter the loop

### Variables

Immutable variables are declared with "let" and mutables ones with "var"
Variable declared without value are declared as "uninit" automatically, no need 
to use manually the keyword

### Struct

Structures fields can be explicitly marked as private `priv`
Methods that uses the instance must reference it with the "self" as first arg
Methods that do not need `self` can be called on the type itself (allow defining
multiple constructor for example)
The type `Self` if available to refer to the structure itself
Struct can be constructed as:

```rust
struct Point {x, y: int}
let p = Point{ x = 1, y = 2 }
```

They can have an `init` method thats called automatically on creation and allow
the structor name to be called as a constructor

```rust
struct Point {
    x, y: int

    fn init(x, y) -> Self {}
}
let p = Point(1, 2)
```

### Boilerplate reduction

- Allow rust-like structure creation redudant field name

```rust
struct Point { x: i64, y: i64 }
let x = 5
let y = 10
let p = Point { x, y } // instead of Point { x = x, y = y };
```

- If variables of the same name are in scope, allow something like:

```rust
struct Point { x: i64, y: i64 }
let x = 4
let y = 10
let pt = Point { ... }

```

- Chaining type

```rust
struct Point { x, y: i64 }
```

- Constructor arguments. The init method is the only one that can do that
by automatically taking the args to match those in 'self'. If no body to init
all non-default value must have a match in args

```rust
struct Point {
    x, y: int

    fn init(x, y) Self {}
}
```

- To sum up, all of this is supported:

```rust
struct Point {
    x, y: int

    fn init(x, y: int) Self {
        .{ x, y }
    }
}

fn main() {
    var x, y = 5, 6

    // Auto mapping
    var pt = Point(...)

    // Call to `init`
    var pt = Point(x, y)

    // No constructor
    var pt = Point { x = x, y = y }

    // No constructor shortcut
    var pt = Point { x, y }

    // No constructor shortcut++
    var pt = Point {...}
```

### Macro

Implement a macro system, one of the key args of the language. Macro use the same language
for it to be easy to write / read and manipulate the AST. Derive macro with '#'

```rust
macro constructor(s: struct) {
    if s.has_method("init") do compile_err("Already a constructor")

    args = [(field.name, field.kind) for field in s.fields()]
    s.add_method("init", args, null) // null is for return type which is optional
}

#constructor
struct Point { x, y: i64 }
let p = Point(x, y)
```

If multiple macro, declare as a tuple

```rust
#(constructor, default)
struct Foo {}
```

### List comprehension

Language should support list comprehension because it's too powerfull

### Union type

> [!Note]
> cf. bottom of spec

Union can be defined in two ways and can be aliased. Error union follows the same rule but
separate the types from the error with `!` like:

```rust
type ShelterRes = Dog | Cat ! NoPet | Burnt

fn add() int!Error {}
```

- Inlined union

If you wanna use type union in a function's declaration of a structure's field for example,
you can define it as:

```rust
struct Point {
    coord: (int, int)|Vec2,
}
fn addOne(value: int|Vec2) {}

// Alias
type ScalarOrVec = int|Vec2

```

- Heavy union

You can define an union like you would define an enum but you can attach methods on it

```rust
union {
    int,
    Vec2,
    
    fn addOne(self) {}
}
```

It allows to define methods on it, apply traits, derive macro, ...

### Error

Error propagation is done with "!" and throw up the call stack the error
For propagation, error types must match
Error unions can be merged as any union type with '&':

```rust
error ShelterErr { NoPet } & HumanErr
```

Error can have a payload and **implicitly derives #constructor**

```rust
error ParserErr {
     InvalidToken: Token // Token is a struct defined elsewhere: struct Token { start: int, end: int }
     SyntaxeErr // implicit "void" payload equivalent to no payload
     UnclosedQuote: (int, str) // Tuple payload with no argument name
     InvalidType: struct { found: str, expect: str } // Anonymus struct payload for arg name
} & ShelterErr

fn parse(source: str) -> int ! ParserErr {
    if source[0] == "%" {
        ParserErr.InvalidToken(Token(source[0])
    } else if source[0] == "{" and not source[1] == "}" {
        ParserErr.InvalidType({ found = "int", expect = "float"})
    } else {
        0
    }
}

// propagation
fn compile(source: str) -> ! CompilerErr|ParserErr { // error union inplace fusion
    let parsed = parse()! // propagation
    ...
}
```

### Null and error fallbacks

To provide fallback values in case of null variable or error union variable, there are the two
operators `??` and `!!`.

```rust
fn add() -> int!Err {}
fn mul() -> ?int {}

let res = add() !! 5
let res = mul() ?? 5
```

### Extract operator

You can use the extract operator `::` to extract in several place, or sometimes alias.

- Control flow:

You can alias for the entier construct by extracting at the end of the condition or extract on each branch

```rust
match get_pet().name :: n {
    "Rodrigo de la suerte" => print(fmt("I'm gonna kill you {}!", n))
    _ => print("Who are you {}?", n)
}

when pet :: p {
    Cat => print(fmt("I'm gonna kill you {}!", p.cat_method()))
    _ => print("What type are you {}?", p)
}

// Alias branches (mostly useful for errors' payload)
when err { // with when, we match the error type
    ParserErr.InvalidToken :: tk => {
        print("got {}", tk.lexeme)
        5
    }
    _ => return err
}

```

For `if`, you can use it to extract a non-null value or a non-error value. In the latter case, the `else`
branch can extract the error

```rust
let maybeNull: ?int = 0

if maybeNull :: res {
    // Here `res` is an int
    print(res)
}

let maybeErr: int!Error = 0

if maybeErr :: val {
    // Here val is an int
    print(val)
} else :: err {
    // Here err is an error
    print("Error: {}", err) 
}
```

For `while`, it's the same, a non-null value can be extracted in the condition like:

```rust
while maybeNull() :: value {}
```

### Trap keyword

To work with errors, there is the `trap` keyword. It can extract the error and is considered as an *expression*.
It means that it can provide a fallback value as `!!` with the possibility to interact with the error

```rust
let value = maybeErr() trap 5
let value = maybeErr() trap { 5 }
let value = maybeErr() trap :: err {
    print err
    5
}
let value = maybeErr() trap :: err {
    match err {
        MathErr => 5,
        _ => return err
    }
}
```

### Named loops

- loop aliases are done with `@` before loop keyword so we can mix aliases and loop
aliases without any confusion:

```rust
@outter while true {
    @inner loop {
        if today() == "monday" do break @outter
    }
}

let last = @outter loop {
    while get_token() :: tk {
        if tk.kind == .plus do break @outter tk
    }
}
```

Can alias types while working with generics implementation (espacially with union type)

### Nullable

Nullable is part of a type. In can be unsafe collapsed with `.?` operator and can be extracted
in different control flow constructs.

```rust
let value: ?int = 0
let value_int = value.?
```

### Closure

Closure exist as first class object in Rover. However, the way the variables are captured is once again explicit.
User has to define which variables get captured with the `bind` keyword in function declaration like. Capture is
done by reference but you can refer to outer scope variables, they will be copied.

```rust
var a = 1
var b = 2

fn print_later(bind b) {
    print(a, b)
}

a = 4
b = 9
print_later() // expect: 1, 9
```

As we bind from a declared variable, we can infer type and not require it in function definition

### Trait

There is no OOP here, only composition done via trait system
Traits can be generic
Can have default methods implementation

```rust
trait Add<T, R> {
    fn add(self, other: T) -> R
}
```

### When

When allow pattern matching on types
Can alias too

```rust
type Animal = Dog | Cat
var pet: Animal = Dog {}

when pet {
    Dog => // here use pet as a Dog
    Cat => // idem for Cat
}
```

Allow a powerful feature is defining traits on union types

```rust
type Animal = Dog | Cat

trait Speaker {
    fn speak(self) -> str
    fn die(self)
}

impl Speaker for Animal {
    // For each type
    fn speak(self) -> str when self {
        Dog => "waf waf"
        Cat => "meow meow"
    }

    // For all types
    fn die(self) { print("dead") }
}

// Other example
trait Add<T, R> {
    fn add(self, other: T) -> R
}

struct Vec2 { x, y: float }

impl Add<T, Vec2> for Vec2 {
    fn add(self, other: T) -> Vec2 when T {
        int|float => Vec2 { x = self.x + other.x, y = self.y + other.y }
        Vec2 => Vec2 { x = self.x + other.x, y = self.y + other.y }
        _ => panic()
    }
}
```

### Uninit

Special keyword "uninit" that allows to specify that the value is not initialized yet
Use of a uninit var is a runtime panic

### Pointers

Support references and dereference for optimization purpose
"&" for adressing
Returning a reference to a local variable is forbidden
Returning a reference to a member is allowed
Reference of reference `&&` are forbidden

```rust
struct Node {
    next: &Node
}

var node = Node { next = uninit }

// Later
node.next = &other_node
// Dereference
print(node.next.*)
```

### Dot coercion

When the type excepted in an enum or an error, we can use like in Zig the dot coercion and extend to methods
22/12/24 : Dot coercion for enums only like in pattern matching

```zig
const Token = enum {
    Number, Indentifier,

    fn init() Self {}
};

fn main() {
    const tk: Token = .Number;
    const tk: Token = .init()
}
```
### Bundles deconstruction

You can deconstruct structs and tuples, ... via syntax like:

```rust
struct Node {
    lhs, rhs: int
}

var node = Node {lhs=0, rhs=0}
match node {
    Node {lhs = 1, _} :: n => print(n)
}

// Assignemnt
let Node {lhs :: l, rhs :: r} = node
// Use l and r as local variables

```

### Still to figure out

- loop aliasing, maybe a bit cluttered for now, consider:
    - Optional "with" keyword:
    break outter with tk
    - Explicit dereferencing:
    break @outter tk

- String interpolation with a `fmt` function in the `std` or the `%` syntaxe
    -> `fmt` function

## Thoughts
The idea is to bring back old operators working with errors (!) and nullable (?) and 
rework how we work with errors

### Functions

So, we keep "!" in function's prototype to separate "ok" channel from "err" channel

```rust
fn add() -> int ! ArithmeticErr {}
```

Returning into the error channel is done with "return!". Ok return are normal return

```rust
fn add(a: int) -> int ! ArithmeticErr {
    if a < 0 do return! ArithmeticErr.NegativeNb(a)

    return a + 2 // useless keyword here btw
}
```

If a function returns only an error, no need to use return!, there is only one channel

### Propagation

To propagate error, simply use "!" right after a function call

```rust
fn add(a: int) -> int ! ArithmeticErr { }

fn calculate(a, b: int) -> int ! ArithmeticErr {
    let res = add(a)!
    res
}
```

It can be chained by design

```rust
fn add(a: int) -> int ! ArithmeticErr { }

fn calculate(a, b: int) -> int ! ArithmeticErr {
    let res = add(a)!.add(a)!
    res
}
```

### Collapse

We should have a mechanism to collapse the error into a non-error (unwrap) resulting in a panic
at runtime if it was an error. Maybe `variable.!` that would match the same syntaxe to unwrap
a nullable `variable.?`

```rust
fn add(a: int) -> int ! ArithmeticErr { }

fn main() {
    let val = get().!
}
```

My only concern is the lack of readability between error propagation and error unwrapping

```rust
fn add(a: int) -> int ! ArithmeticErr { }

fn calculate(a: int) int ! Error {
    let val = get().! // unwraps
    let val = get()! // propagates

    let val = get()!.get().! // is it readable?
}
```

## Archives

### Specialized if

To work with nullable and error, we introduce `if?` and `if!`. It allow to use control flow
to extract the value in case of non-null or non-error value. Also, `if!` provides an 
`else` branch that can alias the error to use it

```rust
fn get() -> ?int {}
fn get2() -> int ! Err {}

fn main() {
    let val: ?int = get()
    let val2: int ! Err = get2()

    // Explicit aliasing
    if? val @v {
        print(v)
    }

    // Do we allow implicit aliasing?
    if? val {
        // Here use `val` as a non-null value
        print(val)
    }
    
    // Same question about implicit aliasing
    if! val2 {
        // Here, val2 is a non-error
    } else {
        // Here val2 is an error
    }

    if! val2 {
        // Here, val2 is a non-error
    } else err { // No need for aliasing? Just a variable available here?
        // Here err is an error
    }
}
```

### Specialized else

To work with nullable and error, we introduce `else?` and `else!`. It allow to define a fallback value

```rust
fn get() -> ?int {}
fn get2() -> int ! Err {}

fn main() {
    let val = get() else? 5
    let val2 = get2() else! 5
}
```

### Different aliasing

I was also wondering about `@` for aliasing. It reads quite naturally as `@` is often "at"
but I find the symbol noisy. I really like the `::` symbol and it's not used anywhere
in current grammer. Maybe it's the right spot? If not, I'd like to find a spot for it

```rust
fn get() -> ?int {}
fn get2() -> int ! Err {}

fn main() {
    let val: ?int = get()
    let val2: int ! Err = get2()

    // Explicit aliasing
    if? val::v {
        print(v)
    }

    if? val :: v { // Better with spaces?
        print(v)
    }
}
```

### (v0) ifnull and iferr

Those keywords are meant to provide a fallback value in case of null/error value.
For erros, we can pattern match on it with *when*
Combination of *iferr* and *match* allow to write only once *err* (reduce boilerplate)

```rust
fn add() -> int!Err {}

let res = add() iferr 5 // fallback value

let res = add() iferr :: err { // fallback value + error usage
    print(err)
    5
}

let res = add() iferr :: err { // custom handling + error propagation
    print("you really messed up")
    return err
}

let res = add() iferr :: err when err { // with when, we match the error type
    ParserErr.InvalidToken :: tk => {
        print("got {}", tk.lexeme)
        5
    }
    ParserErr.InvalidType => {
        print("expected {}, found {}", err.found, err.expect)
        10
    }
    _ => return err
}

fn add() -> ?int {}
let res = add() ifnull 10
```

## Debugger

> [!Note]
> the whole discussion is in my chatgpt, but he approuved the idea

Allow the user the debug the program with a CLI debugger. The user whould be able to inspect
any variable of any scope currently available including:

- Stack
- Constants
- Upvalues

Objectives:

- Don't slow down the Vm when don't used in debug mode
- Allow CLI usage with a REPL mode like GDB

Solution:

- Create a Debugger structure that owns a Vm instance
- In the Vm, add a compile flag to enable check of compile hook at each instruction
- The debugger checks those hooks and checks if current line is in its breakpoints list
- If yes, in enters REPL mode

That means that we generate two binaries, one is the interpreter without those debug hooks
and the other contains those, meaning that it will check them unconditionaly


# Unions rewrite
```rust
fn main() {
    // Here anonymus union
    fn add(self, other: Vec2 | int) {
        // 'when' to pattern math on types and '::' to extrat the value
        when other {
            Vec2 :: v => todo!(),
            int :: i => todo!(),
        }
    }

    // Enums can't have pyaload but can have methods
    enum Expr {
        binop,
        block,
    }

    fn check(expr: Expr) {
        // 'match' to match on a regular value
        match expr {
            binop => todo!(),
            _ => todo!()
        }
    }

    // Can define unions like this, tag: Type
    union Geom {
        vec: Vec2,
        scalar: int
    }

    fn add(self, other: Geom) {
        // 'when' to pattern match on tagged unions type + extract value
        when other {
            Vec2 :: v => todo!(),
            int :: i => todo!(),
        }

        // 'match' to pattern match on tag value + extraction
        match other {
            vec :: v => todo!(),
            _ => unreachable,
        }
    }

    union Nested {
        geom: Geom,
        other, // equivalent to void
    }

    fn nested(other: Nested) {
        // Here we use 'when' so we can interact with types and nested types. If analyzer
        // recognized that we match on a union inside a union, allow a syntaxe to match nested
        // levels
        when other {
            Geom:Vec2 :: v => ,
            Geom:int :: i => ,
        }

        match other {
            geom :: g => , // here we match on tag so we cannot interact with types and nested
            // types
        }
    }
}
```

# Ref count rules

Cow rules
- Each invoke triggers a cow
     - If this is an instance
     - Later, check if function mutates the instance and emit only when needed
- In assignment, trigger a cow for every thing as it could be referenced and modifying
  the end of field chain modify all references to upper level in chain
     - Cow for last element of chain is not necessary if it's not a heap object
- In RHS, trigger a cow only if there is an invoke in the chain, otherwise it's only access
- Simple assignment triggers cow if variable is a heap object, for locals and globals
     - What happens if the variable isn't init? The cow looks for r1.obj

Ref count rules
- Each heap object gets incremented when they are referenced
- Function arguments aren't because they are immutable by definition unless they are references
  but in this case it is meant to modify the original object

```rust
//                No cow, only access
//                GET_FIELD_REG, GET_FIELD
//                     |
foo.bar.baz   =   toto.titi.tata
//   |
// GET_FIELD_REG_COW, ASSIGN_REG_COW
// Cow each step because it could be ref count

//                No cow, only access
//                     |
foo.bar.baz   =   toto.titi[1].tata
//   |
// Cow each step because it could be ref count

//                Cow the index call
//                     |
foo.bar.baz   =   toto.titi[toto.getIdx()].tata
//   |
// Cow each step because it could be ref count

//  No cow
//    |
foo = bar
// cow lhs -> SET_LOCAL_COW
```
