# Various Notes

## Spec

### Expressions

- Variable should be declared with const if they aren't mutated
- Last expression of a block is an implicit return
- Assignment is not an expression, we can't chain. It's because we don't want that an assignment
at the end of a scope being the returned value
- Each control flow is an expression, all branches must return the same value:
   - If
   - While
   - Loop
   - Switch
- Regarding returned value:
    - If:
        - Each branch must return the same type unless exit scope
        - Each branch can exit the current scope with a ```return``` statement
    - While:
        - Can be exited with a ```break``` statement with a value after or not
    - Loop:
        - Same as while
    - Switch:
        - Each branch must return the same type

### Variables

Immutable variables are declared with "let" and mutables ones with "var"
Variable declared without value are declared as "uninit" automatically, no need 
to use manually the keyword

### Struct

Structures fields can be explicitly marked as private "priv"
Methods that uses the instance must reference it with the "self" as first arg
Methods that do not need "self" can be called on the type itself (allow defining
multiple constructor for example)
Struct can be constructed as:

```rust
struct Point {x, y: int}
let p = Point { x = 1, y = 2 }
```

They can have an "init" method thats called automatically on creation and allow
the structor name to be called as a constructor

```rust
struct Point {
    x, y: int

    fn init(self, x, y)
}
let p = Point(1, 2)
```

### Boilerplate reduction

- Allow rust-like structure creation redudant field name

```rust
struct Point { x: i64, y: i64 }
let x = 5;
let y = 10;
let p = Point { x, y }; // instead of Point { x = x, y = y };
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
    x, y: i64

    fn init(self, x, y)
}
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

### List comprehension

Language should support list comprehension because it's too powerfull

### Union type

Union type allow to use any type of the language together

```rust
type Animal = Dog | Cat
```

It can be mixed with errors

```rust
type ShelterRes = Dog | Cat ! NoPet | Burnt

fn fetch_pet(name: str) -> ShelterRes {}
```

### Error

Error propagation is done with "!" and throw up the call stack the error
For propagation, error types must match
Error unions can be merged as any union type:

```rust
error ShelterErr { NoPet } | HumanErr
```

Error can have a payload and implicitly derives #constructor

```rust
error ParserErr {
     InvalidToken: Token // Token is a struct defined elsewhere: struct Token { start: int, end: int }
     SyntaxeErr // implicit "void" payload equivalent to no payload
     UnclosedQuote: (int, str) // Tuple payload with no argument name
     InvalidType: { found: str, expect: str } // Anonymus struct payload for arg name
} | ShelterErr

fn parse(source: str) -> int ! ParserErr {
    if source[0] == "%" {
        ParserErr.InvalidToken(Token(source[0])
    } else if source[0] == "{" and not source[1] == "}" {
        ParserErr.InvalidType { found = "int", expect = "float"}
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

In case of use of *if* to check if a value is an error, it provides a special *else*
with the error:

```rust
let maybe_err: int!ParserErr = parse()

if maybe_err @ok_val {
    print("Ok: {}", ok_val)
} else @err {
    print("Got err {}", err)
}
```

### ifnull and iferr

Those keywords are meant to provide a fallback value in case of null/error value.
For erros, we can pattern match on it with *when*
Combination of *iferr* and *match* allow to write only once *err* (reduce boilerplate)

```rust
fn add() -> int!Err {}

let res = add() iferr 5 // fallback value

let res = add() iferr @err { // fallback value + error usage
    print(err)
    5
}

let res = add() iferr @err { // custom handling + error propagation
    print("you really messed up")
    return err
}

let res = add() iferr @err when err { // with when, we match the error type
    ParserErr.InvalidToken @tk => {
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

### Aliases

A key feature is that you can alias in various place with "@"
Invoking alias don't require using the "@" symbol, only for declaration

- Control flow:

```rust
match get_pet().name @n {
    "Rodrigo de la suerte" => print(fmt("I'm gonna kill you {}!", n))
    else => print("Who are you {}?", n)
}

when pet @p {
    Cat => print(fmt("I'm gonna kill you {}!", p.cat_method()))
    else => print("What type are you {}?", p)
}

// Alias branches (mostly useful for errors' payload)
when err { // with when, we match the error type
    ParserErr.InvalidToken @tk => {
        print("got {}", tk.lexeme)
        5
    }
    _ => return err
}

if nullable_res() @res { // custom alias for non-null value
    print(res)
}

if failable_fn() @val { // custom alias for non-err value
    print(val)
} else @err {
    print("Error: {}", err) 
}
```

- Loop aliases are before loop keyword because otherwise:

```rust
@outter while true {
    @inner loop {
        if today() == "monday" do break @outter
    }
}

let last = @outter loop {
    while get_token() @tk {
        if tk.kind == .Plus do break outter tk
    }
}
```

Can alias types while working with generics implementation (espacially with union type)

### Nullable

While loops should work with nullable to providing the value in an alias
if a non-null result is returned
Can unsafe collapse the nullable with: variable.?

```rust
fn next() -> ?Token {}

while next() @val {

}
```

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
var pet = Dog {}

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

impl Add<int|float|Vec2 @T, Vec2> for Vec2 {
    fn add(self, other: T) -> Vec2 when T {
        int|float => Vec2 { x = self.x + other.x, y = self.y + other.y }
        Vec2 => Vec2 { x = self.x + other.x, y = self.y + other.y }
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

```rust
struct Node {
    next: *Node
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

### Still to figure out

- Loop aliasing, maybe a bit cluttered for now, consider:
    - Optional "with" keyword:
    break outter with tk
    - Explicit dereferencing:
    break @outter tk

- String interpolation with a `fmt` function in the `std` or the `%` syntaxe
    -> `fmt` function

## Ideas

From Gleam:

- Pipe operator to avoid nested calls

```python
res = lstrip(rstrip(split("str")))
```

```gleam
let res = "str" |> string.strip() |> string.rstrip() |> string.lstrip()
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
