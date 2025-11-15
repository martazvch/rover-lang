# Ray documentation

## Variables

### Declaration

When declaring a variable, both it's type and initial value are optionals but at least one of the two must be provided.
- If only the type is provided, the variable will be in an *uninitialized* state until a value is provided. Using the variable before initialization will result in a compilation error.
- If only the value is provided, the type will be inferred from it.

In practise, you rarely need to specify types thanks to type inference.

Mutable variables (variables that can be modified at runtime) are defined with the keyword `var`.

```rust
var name: str = "Tom"
```

With type inference, you can simply write:

```rust
var name = "Tom"
```

Immutable variables (constant variable that can't change value) are defined with the keyword `let`.

```rust
let count = 5 // same as: let count: int = 5
```

### Multiple declarations

You can declare multiple variables at the same type in three ways:
- If you declare the same number of values as the number of variables, they will be assigned in order:
```rust
var a, b, c = 5, "ok", false
```

In this example, `a` is of type `int` and holds the value `5`, `b` is a `str` and holds `"ok"` and `c` is a `bool` and holds `false`.
This is equivalent to:
```rust
var a = 5
var b = "ok"
var c = false
```

- If you declare only one value, it will be assigned to all the variables:
```rust
var a, b, c = 5
```

This is equivalent to:
```rust
var a = 5
var b = 5
var c = 5
```

- If you provide no value, they will all be *uninit* and you **have to** specify their type:
```rust
var a, b, c: float
```

### Wildcard

You can use wildcard `_` as a variable's name during declaration. Doing so explicitly tells that you're discarding the value.
If not, the value will be discarded anyways but sometimes it's better to mark the intention so that later refactor/other developper will instantly see that it's suppose to return a value.

```rust
_ = "Tom"
```

This example is silly but it can actually be usefull in cases where you want to execute the right hand side expression without binding it to a variable like:

```rust
_ = update()
```

Here, the call to `update` will be executed and you explicitly ignore the return value.

## Operators

All the supported operators are listed below from highest to lowest [precedence](#Precedence) with their [associativity](#Associativity):

| Description | operators | Associativity |
| --------------- | --------------- | --------------- |
| unary postfix | `()` `[]` `.` `?` `!` | none |
| unary prefix | `-` `!` | none |
| multiplicative | `*` `/` `%` | left |
| additive | `+` `-` | left |
| shift | `<<` `>>` | left |
| bitwise AND | `&` | left |
| bitwise XOR | `^` | left |
| bitwise OR | `\|` | left |
| relational | `<` `<=` `>` `>=` | none |
| equality | `==` `!=` | none |
| logical AND | `&&` | left |
| logical OR | `\|\|` | left |
| null fallback | `??` | left |
| error fallback | `!!` | left |
| ternary | `expr1 ? expr2 : expr3` | right |
| assignment | `=` `*=` `/=` `+=` `-=` `&=` `^=` | right |
| expand | `..` | none |

### Precedence

Precedence expresses the *weight* of each operator used to determine the order of resolution.
For example `==` has a higher precedence than `&&` meaning that the two expressions below are equivalent:

```rust
if (a == 1 && b == 2) { ... }
if ((a == 1) && (b == 2)) { ... }
```

Both equality checks are gonna be resolved before the logical `&&`.

### Associativity

Associativity expresses the *direction* in which operations are resolved. A *left* associativity means that when chained, expressions
are going to be resolved from the left one to right one, for example the two following expressions are equivalent:

```rust
assert(5 * 6 / 3 == 10)
assert((5 * 6) / 3 == 10)
```

TODO: Explain operators

## Comments

Lines starting with `//` are comments. Multilines comments are surrounded by `/* comment */`:

```rust
// This a comment
/*
    This is a multiline comment
    that ends next line
*/
```

Documentation comments are defined with triple slash `///` and are used to generate code's documentation:

```rust
/// Player's data
struct Player {
    /// Life
    life: float,
}
```

## Types

Ray supports:
- [numbers](#Numbers): `int`, `uint`, `float`
- [bools](#Booleans): `bool`
- [strings](#Strings): `str`
- [null](#Null): `null`
- [references](#References)
- [arrays](#Arrays)
- [tuples](#Tuples)
- [maps](#Maps)
- [structures](#Structures)
- [enums](#Enumerations)
- [inlined enums](#Inlined%20enums)
- [error unions](#Error%20unions)
- [functions](#Functions)

### Numbers

There are three numeric types in Ray:

- Signed intergers `int`
64-bit integers which can take values from -2^63 to 2^{63-1}.

- Unsigned intergers `uint`
64-bit integers which can take values from 0 to 2^{64-1}.

- Floating point `float`
64-bit floatting point values defined by the IEEE 754 standard.

### Booleans

Booleans are a special type which can take only two values: `true` and `false`. They are used in logical expressions.

TODO: bool to int?

### Strings

Ray strings hold a sequence of `UTF-8` characters. They can be created either with single quote `'` or double quotes `"`:

```zig
var name = 'Tom'
var name = "Tom"
```

You can concatenate strings using `+` operator and repeat them with by multiplying them witn an unsigned integer `uint`:

```py
assert("Space " + "shuttle" == "Space shuttle")
assert("o" * 5 == "ooooo")
assert("o" * 5 + "k" == "oooook")
```

TODO: string interpolation
TODO: multiline strings

### Null

The `null` value can only be used with optional types. They are defined with a `?` preceding the type like `?int` or `?[]float`.
It means that the variable of type `?T` holds either `null` or a value of type `T` and is used for [null safety](#Null%20safety).
A variable of type `?T` will be initialized with `null` if no value is provided.

```zig
var id: ?int
assert(id == null)
```

As providing a value of type `T` will infer the variable's type to `T`, you have to explicitly annotate the type to declare an optional.

```zig
var id: ?int = 5
```

### References

In Ray, everything is by default passed by value, meaning that whenever you assign a value or pass a value to an expression (function call, structure literal, ...) you copy the data (not really true, see [clone on write](#Clone%20on%20write) section). So any changes on the value won't be reflected on the source of the value:

```zig
var count = 0
var other_count = count
count += 1
assert(count == 1)
assert(other_count == 0)
```

If you want to store a *reference* to the source of the value to reflect changes on it, you can get a reference with `&` operator then modifying its value 
with `.*` postfix syntax (known as *dereferencing*).

```zig
var count = 0
var ref_count = &count
ref_count.* += 1
assert(count == 1)
assert(ref_count.* == 1)
```

Ray offers auto-dereferencing when accessing members, meaning that the following syntaxes are equivalent:

```zig
struct User {
    id: int
}

fn foo(user: &User) {
    // Syntax 1
    var id = user.*.id
    // Syntax 2
    var id = user.id
}
```

In practise the syntax 2 is the preferred way.

### Arrays

An array is an mutable homogeneous sequence of data, meaning that all the values share the same type. To declare an array literal you must surround values with `[]` and seperate them with commas. Type will be infered from the values so you don't have to write it. If multiple types are found among the values, an [inlined enum](#Inlined%20enums).

```zig
var data = [1, 2, 3] // type is: [int]
var data = [
    1,
    "on",
    4,
] // type is: [int|str]
```

Arrays support indexing with `[<index>]` syntax noth for access and assignemnt:

```zig
var data = [3.14, 6.18]
assert(data[1] == 6.18)
data[0] = 1.41
assert(data[0] == 1.41)
```

Arrays' size are dynamic so you can add/remove elements at runtime. To get the length of the array you can use `len()` method. Add/removing is done with respectively methods `push()` and `pop()`.

```zig
var data = [1, 2, 3]
assert(data.len() == 3)
data.push(8)
assert(data.len() == 4)
data.pop()
assert(data.len() == 3)
```

### Tuples

A tuple is an immutable heterogeneous sequence of data where fields can be named. As it's immutable, neither its size or its fields can change after creation. To declare a tuple literal you must surround values with `()` and seperate them with commas.
Accessing tuples values is done in two ways:
- Using field's index like: `tuple.1`
- Using field's named if one: `tuple.a`

```zig
var tuple = (1, false, "ko") // type is: (int, bool, str)
assert(tuple.2 == "ko")

// With named fields
var tuple = (1, a: true, status: "ok") // type is: (int, a: bool, status: str)
assert(tuple.status == "ko")
```

Names fields are part of the type. It means two things:

- You have to explictly declare them if na value is provided at declaration site:

```zig
var tuple: (int, b: bool, status: str)
tuple = (1, b: true, status: "ok")
```

- Two tuples with same fields types aren't equivalent if their names mismatch:

```zig
var tuple1: (int, float) = (1, 2)
var tuple2: (int, float) = (3, 4)

// This is allowed
tuple1 = tuple2

var tuple1: (a: int, b: float) = (a: 1, b: 2)
var tuple2: (a: int, b: float) = (a: 3, b: 4)

// This is allowed
tuple1 = tuple2

var tuple1: (a: int, b: float) = (a: 1, b: 2)
var tuple2: (c: int, d: float) = (c: 3, d: 4)

// This is a compilation error
tuple1 = tuple2
```

### Maps

A map is a collection of key-value pairs. Each key can only occur once but a value can be associated with different keys.
To declare a map literal, you must surround key-values with `[]` and keys are separated from values with a colon `:`.
Keys must share the same type and value must share the same type but keys and values must not.

```zig
// type is: [int: str]
var map = [
    5: 'bed',
    70: 'chair',
]
```

You can retreive a value from a map by indexing it with a key:

```zig
var map = ["monday": 55, "tuesday": -4]
assert(map["monday"] == 55)
```

When assigning a value to a key, if the key already exists the value is updated otherwise the key-value is created:

```zig
var map = ["monday": 55, "tuesday": -4]
assert(map["monday"].len() == 2)
map["sunday"] = 12
assert(map["monday"].len() == 3)
```

### Functions

Functions in Ray are lightweight building blocks. They support default arguments, closures, anonymous form, error propagation, null chaining, and structured error handling via trap.

#### Declaration

A function is declared with `fn` followed by its name, arguemnts and optional return type after `->`:

```rust
fn add(a: int, b: int) -> int {
    return a + b
}
```

Arguments can share types:

```rust
fn add(a, b: int) -> int {
    return a + b
}
```

If the function returns nothing, you can emit the return type:

```rust
fn debug() { ... }
```

A function can return an error union alongside with a type:

```rust
fn parseInt(text: str) int!ParserErr {}
```

If the functions returns nothing or an error, the type becomes:

```rust
fn parseInt(text: str) !ParserErr {}
```

When returning an error, use `fail` instead of `return` to indicate that you're sending the value to the error channel:

```rust
fn parseInt(input: str) int!ParseErr {
    for c in input {
        if not c.isNumeric() do fail ParserErr.invalidInput
    }

    return int(input)
}
```

#### Named parameters

Each parameters can be invoked by their name. All positional arguments must come before named ones.

```rust
fn foo(a: int, b: float, c: bool, d: str) {}

// All positionals
foo(1, 2., false, "ok")

// All named in any order
foo(d: "ok", b: 2., c: false, a: 1)

// Positionals before named
foo(1, 2., d: "ok", c: false)
```

#### Default values

Parameters may have default values:

```rust
fn greet(name: str = "world") -> str {
    return "Hello {name}!"
}

greet() // "Hello world!"
greet("Ray") // "Hello Ray!"
```

Default values must be compile-time constants.

When providing a default value, you can omit the type as it is infered from the value

```rust
fn greet(name = "world") -> str {
    return "Hello {name}!"
}
```

#### First class objects

Functions are first class objects, meaning that you can pass them around as any other type.

```rust
fn greet(name = "world") {}

let g = greet
g("Tom") // "Hello Tom!"
```

Particulary useful when used as function's argument:

```rust
fn swap(x, y: int) {}
arr.map(swap)
```

#### Anonymous functions (lambdas)

Anonymous functions use the `fn` keyword without a name:

```rust
let double = fn(x: int) -> int { return x * 2 }
```

They can be used inline:

```rust
arr.map(fn(x) -> int { return x + 1 })
```

#### Closures

Closures capture surrounding variables by reference:

```rust
let factor = 3
let mul = fn(x: int) -> int { return x * factor }
mul(4) // 12
```

#### Null chaining

Calls returning nullable values can be chained with `?`. Then chain breaks at the first `null` encountered and resolves as `null`.

```rust
let name = getUser()?.getName()?
```

#### Error chaining

When chaining calls that return error unions, it behaves like [null chaining](#Null%20chaining), meaning that at the first error the chain breaks and resolves as the error encountered. 

In this exmaple, `parseInt` method on strings returns `int!ParseErr` and `toBase` on integers returns `int!IntErr`:

```rust
fn getInput() -> str!InputErr {}

fn compute() {
    let x: int!(InputErr & ParseErr & IntErr) = getInput()!.parseInt("12")!.toBase(2)
    ...
}
```

> [!NOTE]
> If the function does not declare an error-union return type, using `!` is a compile-time error.

#### Error propagation

Errors are propagated up the call stack using `!` at the **end of expression**:

```rust
fn compute() -> int!ParserErr {
    let x = parseInt("12")! // propagates error upward
    return x * 2
}
```

If we take the previous example:

```rust
fn compute() {
    // Here, x is of type: int!(InputErr & ParseErr & IntErr)
    let x = getInput()!.parseInt("12")!.toBase(2)
    // Here x is of type int but function `compute` now returns an error on fail
    let x = getInput()!.parseInt("12")!.toBase(2)!
}
```

#### Trap for error handling

The keyword `trap` can be used after function calls to interact with the error if the call resulted in an error. It can be used for:
- Providing a fallback value in case of error
- Interacting with the error before returning from the function

Fallback value example:

```rust
let i = parseInt("abc") trap 5
assert(i == 5)

let i = parseInt("abc") trap |e| {
    print("Error while parsing int")
    break 5
}
assert(i == 5)
```

Error interaction example:

```rust
fn createUser(input: str) User!UserErr {
    let id = parseInt(input)!.toBase(2) trap |e| {
        print("Either 'parseInt' or 'toBase' failed: " + e)
        // Return from function
        fail .invalidInput
    }
}
```

> [!NOTE]
> Using `trap` with an error propagation `!` at the end of expression results in a compilation error

#### Main function

All Ray programs are expected to have a `main` function that will automatically be called. This is the program's entry point.
It's signature defines:
- Only one argument `args`: array of strings containing the arguments 
- Return type `int!err`: union of an error and an integers

```zig
fn main(args: []str) {}
```

If you're not using the argument or the return type, you can omit them. All the following are valid syntaxes:
```zig
fn main() {}
fn main(args: []str) {}
fn main() -> int!Err {}
```

### Structures

#### Definition

A structure in Ray is a composite type that groups named fields and associated behavior. Structures are value types by default and support initialization, methods, trait implementations, and pattern matching.

Structures:
- Define typed fields
- May declare default values
- Support instance and static methods
- Resolve `Self` to the structure's own type
- Integrate into Ray's trait system
- Use dot‑syntax for field and method lookup

#### Syntax

A structure is introduced with the `struct` keyword. Fields are separated by commas and can share types.

```rust
struct Point {
    x: int,
    y: int,
}

// Is the same as
struct Point { x, y: int }
```

Fields can optionally have a default value. When provided, type can be infered and explicitly declaring it is optional

```rust
struct Config {
    port: int = 8080
    host: str = "localhost"
}
// Same as
struct Config {
    port = 8080
    host = "localhost"
}
```

If a field has a default value, it becomes optional during initialization.

#### initialization

Ray supports structure literals:

```rust
let p = Point { x: 3, y: 4 }
```

Fields with defaults may be omitted:

```rust
let c = Config { host: "127.0.0.1" }
```

Ray also supports redundant shorthand syntax, where field names match variable names:

```rust
let x = 10
let y = 20
let p = Point { x, y }
```

#### Methods

Instance methods are defined inside the structure and take `self` as the first parameter:

```rust
struct Point {
    x, y: int,

    fn magnitude(self) -> float {
        return sqrt(self.x*self.x + self.y*self.y)
    }
}
```

> [!NOTE]
> `self` is always passed as a reference to the instance. If you don't want to mutate the instance, first create a copy and the mutate it

#### Static functions

Static functions do not receive `self` and are called on the type directly:

```rust
struct Point {
    x, y: int

    fn origin() -> Point {
        return Point { x: 0, y: 0 }
    }
}

let o = Point.origin()
```

Static functions are ideal for constructors or utility functions.

#### The Self type

Self refers to the containing structure's type.

It is used for:
- Return types in factory or chaining methods
- Generic trait implementations
- Nested type references

```rust
struct Box {
    value: int,

    fn set(self, value: int) -> Self {
        return Self { value }
    }
}
```

#### Traits and structures

Structures can implement traits. `Self` is especially useful when traits define methods that return the implementing type.

```rust
trait Debug {
    fn debug(self) -> str
}

struct Point { x, y: int }

impl Debug for Point {
    fn debug(self) -> str {
        return "Point({self.x}, {self.y})"
    }
}
```

Traits can be used as bounds, enabling polymorphic behavior.

#### Dot syntax

When the type is know, you don't need to explicitly refer to the type, you can
use `.{}` syntax:

```rust
fn move(to: Point) { ... }

// Infered to Point
move(.{x: 5, y: 10})
// Equivalent
move(Point{x: 5, y: 10})
```

#### Private fields

Fields in Ray can be marked as `priv`, restricting access to within to the structure itself. This allows encapsulation while keeping most structure fields public by default.

```rust
struct User {
    name: str
    priv password: str

    priv fn hash(self, seed: int) {
        self.password = seed ^ 12345
    }
}
```

Attempting to access a private field outside the declaration is a compile-time error:

```rust
let u = User { name: "Ava", password: "secret" } // error: password is private
u.hash(5) // error: hash is private
```

Method Lookup Order
-------------------
- Method resolution follows three steps:
- Look for a field with the given name.
- Look for a method in the structure.
- Look for methods from implemented traits.
- Type‑Directed Access

Ray uses the type of the value to infer which method applies, ensuring static dispatch:

#### Pattern matching

Fields can be destructured directly:

```rust
match p {
    Point { x, y } => print(x + y)
}

```

Use `..` to ignore the rest:

```rust
match p {
    Point { x, .. } => print(xpos)
}
```

Match on specific values:

```rust
match p {
    Point { x: 0, y } => print("On Y-axis at {y}")
    Point { x, y: 0 } => print("On X-axis at {x}")
    _ => print("General point")
}
```

Use `when` for additional constraints:

```rust
match p {
    Point { x, y } when x == y => print("Diagonal")
    Point { x, y } => print("({x}, {y})")
}
```

### Enumerations

Enums in Ray represent tagged algebraic variants. They allow you to model structured alternatives, optionally with payloads, and integrate tightly with Ray’s pattern matching system. Enums can also define methods, nested types, and support dot-syntax construction.

#### Declaration

A basic enum lists variants without payloads:

```zig
enum Direction { north, east, west, south }
    ```

#### Constructing enum values

Full syntax:

```zig
var direction = Direction.west
```

Shorthand dot literal (when type is known), works in arrays, struct fields, function calls, etc.:

```zig
var dir: Direction = .south
assert(dir == .south)

// Here too
var path: [Direction] = [.west, .west, .north]

// Same for functions
fn go(dir: Direction) { ... }

go(.south)
```

#### Enums with payloads

Variants may carry data:

```rust
enum Token {
    identifier: str,
    number: int,
    dot,
    brace: Brace,

    enum Brace { left, right }
}
```

Constructing payloaded variants:

```rust
let t1 = Token.identifier("box")
let t2 = Token.number(42)
let t3 = Token.brace(.left)
```

#### Pattern matching

Simple variant matching:

```rust
match d {
    .north => print("N"),
    .south => print("S"),
    _ => print("other direction")
}
```

Extracting payloads:

```rust
match tok {
    Token.identifier(name) => print("id = {name}")
    Token.number(n) => print("num = {n}")
    Token.dot => print(".")
    Token.brace(b) => print("brace: {b}")
}
```

If a payload is a struct, Ray allows partial destructuring:

```rust
enum Event {
    click: Point,
    key: KeyEvent,

    struct Point { x, y: int }
    struct KeyEvent { code: int, alt, shift: bool }
}

match event {
    Event.click(.x, ..) => print("Clicked at x={x}")
    Event.key(.code, .alt = true, ..) => print("Alt+{code}")
    _ => {}
}
```

#### Functions in enums

Enums can define methods and static functions too:

```rust
enum Marker {
    circle,
    cross,

    fn invert(self) -> Self {
        return match self {
            .circle => .cross
            .cross  => .circle
        }
    }
}
```

#### Nested types and namespacing

As structures, enums can contain enums, structs, traits, and helper functions:

```rust
enum Shape {
    circle: float,
    rect: Rect,

    struct Rect { w, h: float }

    fn area(self) -> float {
        return match self {
            .circle(r) => 3.14159 * r * r
            .rect(r) => r.w * r.h
        }
    }
}
```

## Control flow

Ray provides a small, expressive set of control-flow constructs. Every block is an expression and must return a coherent value unless the flow exits (`return`, `break`, or error propagation).

When using `break` **without** a label, it exits the inner most [block](#Blocks), `return` always exits current function.

### Blocks

Blocks can be used in any expressions and are declared with `{}`. When openning a block, it creates a new lexical scope and when exiting it all local variables/declarations are gone.

A block can be:
- An expression if exited with `break <value>`
- Labeled with `:name`

```zig
let value = {
    // Local scope
    var tmp = 5
    tmp += 8
    break tmp
}
```

Naming blocks allow to nest them:

```zig
let value = v: {
    var tmp = 5
    tmp += {
        let count = getCounter()
        // Targets outter block
        if count == 10 do break :v count
        break count
    }
}
```

You can name any expression's block, for example:

```zig
let value = if true v: { break :v 5 } else 8
```

### If expressions

#### Syntax
If used as an expression, all branches must return a value. If `then` branch's body is a single statement, it can be written with any braces `{}` as long as it's after `do` keyword:

```zig
// do
if true do print("ok")

let x = if condition do 10 else 20
```

#### Chaining

You can chain `if`/`else`:

```zig
let r = if a > 0 do
    "positive"
else if a == 0 do
    "zero"
else
    "negative"

```

#### If-let / if-var

When used with nullable types, the `if let` or `if var` syntaxes can be used to bind to the value if not `null`:

```zig
var id: ?int = 8

if let i = id {
    // Here `i` is of type int
}
```

#### If-match

Can be used to test a pattern matching clause inside an `if` condition:

```rust
struct User { id: ?int, name: str }

fn verify(user: User) -> bool {
    if match user { id != null, name = "Tom" } {
        return true
    }
    ...

    return false
}
```

It allow powerful and short way to test otherwise pretty verbose condition. Without this, we would have written:

```rust
fn verify(user: User) -> bool {

}
```



## Error unions

## Patterns

## Null safety

## Clone on write
