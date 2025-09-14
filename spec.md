# Various Notes

## Intro

**Rover** is an interpreted language written in **Zig** that runs on an embeddable stack-based VM.
It's strictly typed but thanks to type inference you rarely write types.
Rover is **not** OOP and use `unions` and pattern matching to create composition.

There is no implicit reference to heap allocated objects.
It uses a COW system to avoid unecessary copies at runtime, meaning that when you pass around heap allocated objects
like arrays, instances, ... it clones it only if there is a mutation on it.

```rust
var arr = [1, 2]
var arr2 = arr

// No copy occured
print(&arr)  // 0x000324565
print(&arr2) // 0x000324565

arr[1] = 56
// On mutation a copy has been created
print(&arr)  // 0x000334541
print(&arr2) // 0x000324565
```

## Spec

### Entry point

Each program written in **Rover** must define a `main` function that serves as an entry point.
It is planned to have some sort of package manager and a project configuration file that
will know where the `main` is so that the project can be launched with `rover run`.
You can also give a file path like so: `rover main.rv`.

Having to define an entry point disallow writting declarative code in file global scope
that would be executed each time the file is read/imported.

While you can't write runtime logic in global scope, you can still declare constants and mutable
variables and even initialize them with a compile time known value.

### Expressions

- Each block is an expression
- Last expression of a block is an implicit return
- Each control flow is an expression, all branches must return the same value:
   - if
   - loop
   - match
- Regarding returned value:
    - if:
        - Each branch must return the same type unless exit scope
        - Each branch can exit the current scope with a `return` statement
    - loop:
        - Same as while can be exited with a value after or not
    - match:
        - Each branch must return the same type
- Unused values are automatically popped from the stack but you can explictly discard them
- Automatically discarded errors is a warning

```rust
1 + 1 // result is discarded
_ = 1 + 1 // explicit discard
mayFail() // warning
```

### Statements

- All symbols declaration are statements and can be done in any scope
- While can't be an expression because of the fact that you may never enter the loop

### Variables

- Immutable variables are declared with `let` and mutables ones with `var`
- Variable declared without value are declared as `null`. Static anylyzis pass try to warn the user in case of using an uninit variable.
- Assignment is not an expression
- Variable names can shadow other variable with the same name if it's not within the same scope
- Variable's type is declared after `:` after the name
- Variable's type can be infered if initialized with a value
- Either the type or a value must be provided
- You can chain variable declarations

All of those syntax are valid:

```rust
var counter = 0 // infered as `int`
var counter: int = 0
var counter: int

let point = makePoint() // infered as Point
let point: Point = makePoint()

var i, j, k: int // all are `null`
var i, j, k = 0 // i = 0, j = 0, k = 0
var i, j, k = 1, 2, 3 // i = 1, j = 2, k = 3
var i, j, k = false, 56, 1.2 // i: bool = false, j: int = 56, k: float = 1.2
```

### Functions

- They can be defined in global and local scopes
- They can return explicitly with `return` or implictly as the body is a block
- Return type is defined after `->`
- If the function doesn't return anything, you can just omit the type and the arrow (you can explicitly write `-> void`)
- Parameters can share types
- Parameters can have default values
- Parameters can share default values
- If a default value is provided, the type can be omitted
- Parameters can be named during calls to but all positional arguments must be before named ones
- Function are first class objects

All of those syntax are valid:

```rust
fn add(x, y: int) -> int { x + y }
fn sayHi(name = "Tom") { print name }

fn work(who, why: str, urgent: bool, v1, v2 = 0.5, payed = false) { .. }

// Can be called:
work("Tom", "to live", false) // using default values for other paramters
work(urgent=false, why="to live", who="Tom") // naming for clarity
work("Tom", why="to live", urgent=true, v2=0.8) // mixing

// First class object means that you can use them as any other object:
let addFn = add // `addFn` is of type `fn(int, int) -> int`
print(addFn(1, 2)) // can be called as a regular function

```

### Closure

- Closures captures their environment so that they can use variables outside their scope whenever they are called
- They are runtime values so they must be defined in a local scope
- Every local function can capture it environment but you can also define closure with the short syntax: `|| {}` (usefull to pass around)

```rust
fn createPow(exposant=2) -> fn(int) -> int {
    // Here `pow` refers to exposant so latter when it will be called is has to be able to have access to it
    fn pow(x: int) -> int {
        x ** exposant
    }
    pow // implicit return of `pow` function itself
}

let powFn: fn(int) -> int = createPow(3)
print(powFn(3)) // prints 9

// Other syntax
let closure = |x, y: int| {
    print x + y
}
// Easy to pass around
filter(data, |x| -> bool { x % 2 == 0 })
```

### Structures

- Structures fields can have a default value
- Structures fields can be explicitly marked as private `priv`
- Methods that uses the instance must reference it with the `self` as first parameter
- The parameter `self` is implictly passed as first argument when calling a method, allowing chaining
- Methods that do not need `self` can be called on the type itself as static method (allow defining multiple constructor for example)
- The type `Self` if available to refer to the structure itself
- Struct can be constructed as:

```rust
struct Point {x, y: int}
let p = Point{ x = 1, y = 2 }
```

 - *Not sure:* They can have an `init` method thats called automatically on creation and allow
the structor name to be called as a constructor

```rust
struct Point {
    x, y: int

    fn init(x, y: int) -> Self {}
}
let p = Point(1, 2)
```

- If an identifer in scope shares the same as field of the structure being instanciated, you can refer to it as:

```rust
struct Point { x: i64, y: i64 }
let x = 5
let y = 10
let p = Point { x, y } // instead of Point { x = x, y = y };
```

- If variables of the same name are in scope, it is also allowed to refer to them automatically:

```rust
struct Point { x: i64, y: i64 }
let x = 4
let y = 10
let pt = Point { ... }

```

- Fields can share the same type and the same default values

```rust
struct Point { x, y: i64 }
struct Point { x, y = 5 }
```

- *Not sure*: Constructor arguments. The init method is the only one that can do that
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


### List comprehension

Language should support list comprehension because it's too powerfull

### Pattern matching

A powerfull feature of Rover is pattern matching. You can either match on **values** with the keyword `match` or on types
with the keyword `when`.

- You can match on ranges
- You can extract the matched value with the operator `::`
- You can match default case with `_`
- Both `match` and `when` has to be exhaustive

```rust
let counter = 10

match counter {
    0..3 -> ..., // ranges
    4, 5, 6 -> ..., // several values
    _ -> ... // default case
}

match counter {
    0..10 :: v -> print v, // extract the value with `::` into a variable named `v`
    _ -> ...,
}

// Match on types
when counter {
    int -> addInt(counter, other),
    float -> addFloat(counter, other),
    _ -> unreachable,
}
```

### Enums

- Enums are defined with `enum`
- They can't have payload
- They can have methods
- You can associate integer values to each field

```rust
enum Token {
    period,
    comma,
    colon = 90,
}
```

### Union type

- Used to pack together several possible defnintion of a variable
- Can only be one at the same time
- Used for pattern matching
- Fields can have payloads
- Methods can be attached to them

```rust
union Data {
    vec: Vec2,
    scalar: int

    // Attach methods
    fn getValue(self) -> int {
        // match on tag name
        match self {
            vec :: v -> v.x + v.y,
            scalar :: i -> i,
        }
    }
}

let data = Data.vec(Vec2(1, 2)) // build like this
```

- You can define anonymus unions with the syntax `|` (handy to pass around)
- You can't have methods on anonymus unions

```rust
fn add(value: int | Vec2) -> int { ... }
```

- *Not sure:* You can use `when` with this syntax to match on nested types:

```rust
union Nested {
    geom: Geom, // nested one
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
}
```

### Error

- Error propagation is done with "!" and throw up the call stack the error
- For propagation, error types must match
- Error unions can be merged as any union type with '&':
- Error can have a payload
- Function returning errors are defined as: `OkType ! ErrorType`

```rust
error ShelterErr { NoPet, NoRoof }
```

```rust
error ParserErr {
     InvalidToken: Token // Token is a struct defined elsewhere: struct Token { start, end: int }
     SyntaxeErr // implicit "void" payload equivalent to no payload
     UnclosedQuote: (int, str) // Tuple payload with no argument name
     InvalidType: struct { found: str, expect: str } // Anonymus struct payload for arg name
} & ShelterErr // merges the two sets

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

// chain
fn add(a: int) -> int ! ArithmeticErr { }

fn calculate(a, b: int) -> int ! ArithmeticErr {
// chain propagation
    let res = add(a)!.add(a)!
    res
}
```

### Nullable

- Nullability is directly embedded in types with the syntax `?` before the type
- You can safely unwrap nullables with the extract operator `::`
- You can unsafe unwrap nullables with postfix operator `.?` resulting in a runtime panic in case of `null` value
- `if` and `while` recognize nullable and allow to use it as a condition
- To provide fallback values in case of null variable or error union variable, there are the two
operators `??` and `!!`.

```rust
fn add() -> int!Err {}
fn mul() -> ?int {}

// Fallbacks
let res = add() !! 5
let res = mul() ?? 5

let a: ?int = null
print a.? // unsafe unwrap

if a :: val {
    // here val is an `int`
}

while getNext() :: next {
    // loop breaks the moment `getNext` returns `null`
}

```

### Extract operator

You can use the extract operator `::` to extract in several place

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
when err {
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

### Trait

- There is no OOP here, only composition done via trait system
- Traits can be generic
- Can have default methods implementation
- All non default implementation function **have** to be defined

```rust
trait Speak {
    // default implementation
    fn introduce(self) { print "I'm Tom" }
    // No implementation
    fn speakTo(self, to: str)
}
```

- Thanks to `when` working on types, it is possible to define triats on unions

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
Use of an uninit var is a runtime panic

### Pointers

- Take the address of a variable with `&`
- Dereference with the postfix operator `.*`
- Automatic dereference otherwise
- Returning a reference to a local variable is forbidden
- Returning a reference to a member is allowed

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
    number, indentifier,
}

fn main() {
    const tk: Token = .number;
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
    // Creates `lhs` in scope with value 1
    Node {lhs = 1, _} => print(lhs)
    Node {lhs = 1, _} :: n => print(n)
}

// Assignemnt, use l and r as local variables
let Node {lhs :: l, rhs :: r} = node
```

### Macro

Implement a macro system, one of the key args of the language. Macro use the same language
for it to be easy to write / read and manipulate the AST. Derive macro with '#'

```rust
macro constructor(s: struct) {
    if s.has_method("init") do compileErr("Already a constructor")

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

### Generics

- Generics are defined with `$` in function declarations
- The first named generic with `$` defines the type
- You can define trait constraints with `(Trait, ...)`

``` rust
fn add(v1: $T, v2: T) -> T { ... }
// same as
fn add(v1: T, v2: $T) -> T { ... }

// trait constraint
fn add(v1: $T[Add, Sub], v2: T) -> T { ... }
```

### Specialized else

*Maybe*: To work with nullable and error, we introduce `else?` and `else!`. It allow to define a fallback value
*It would be instead of `??` and `!!`.

```rust
fn get() -> ?int {}
fn get2() -> int ! Err {}

fn main() {
    let val = get() else? 5
    let val2 = get2() else! 5
}
```
