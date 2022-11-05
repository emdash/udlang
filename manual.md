# Programming In uDLang

A uDLang script is a kernel which operates on structured data. The
runtime accepts input from a variety of sources, then evaluates the
top-level expression to yield the output value.

If it helps, you can think of uDLang as a type-safe alternative to
`jq` or `awk`.

## Hello World

```
// hello_world.us

// every script must declare the language version it expects.
// this allows backward-compatibility as the langauge evolves.
version 0.2;

// every script must include a short description.
script "hello world example";

// Every script must declare the type of the data it consumes.
// This script consumes no input.
input  Void;

// Every script must declare the type of the data it produces.
// This script produces strings of plain text.
output Str;

// Every script must contain a top-level expression, which will be
// evaluated to produce its output.
//
// As this script consumes no input, its output must be a constant
// expression.
"Hello, World!\n"
```

# Language Model

A program in uDLang defines a *kernel* -- a piece of code executed on
a single input value to produce an output value. The runtime can
evaluate the kernel in a variety of ways.

uDLang kernels are *stateless* and, with one execption, without side
effects. The entire script can be thought of as a pure function, plus
a bit of metadata.

The lifecycle of a uDLang program is as follows:
- compile: the input source is parsed, type-checked, and converted an
  internal representation. This may take place at ahead of time, or
  immediately after startup.
- load: a compiled program is loaded into the memory of the runtime
  environment.
- input: an input value is constructed by the runtime environment.
- evaluation: the kernel is evaluated on the input value, returning a
  result value to the runtime.

The sequence of input and evaluate may repeats indefinitely. a uDLang
kernel has no control over its lifecycle.

The exact shape of the input accepted and the output produced are
specified by `input` and `output` delcarations within the script.

The typechecker is responsible for ensuring that a script's output
expression matches the declared output type. The runtime is
responsible for ensuring that a script is only evaluated on input
values of the correct type.

## Debugging, and Side Effects ##

uDLang has *one* effectful construct: The *debug operator*, `!`.

You can debug any subexpression by applying the debug operator to
it. The runtime offers various options for managing debug
output.

The behavior of the debug operator is only visible externally. Within
expressions, the `!` operator is equivalent to the identity function.

| Example Usage    | Debug output  |
|------------------|---------------|
| `func(!x * 2)`   | `x`           |
| `func(!(x * 2))` | `x * 2`       |
| `!func(x * 2)`   | `func(x * 2)` |
| `(!func)(x * 2)` | `func`        |

## Types and Expressions ##

The type system of uDLang is inspired primarily by Haskell and
Miranda, with a few ideas borrowed from other langauges such as
Erlang, and Python

### Primitive Types

| Type               | Description                             |
|--------------------|-----------------------------------------|
| Bool               | Single-bit value                        |
| I8, I16, I32, I64  | Fixed-width, signed, two's complement.  |
| U8, U16, U32, U64  | Fixed-width, unsigned two's complement. |
| Float              | 64-bit IEEE float                       |
| Nat                | Arbitrary length unsigned value.        |
| Int                | Arbitrary length signed value.          |
| BitStr             | Arbitrary length sequence of bits.      |
| ByteStr            | Arbitrary length sequence of U8.        |
| Char               | Unicode character.                      |
| Str                | Arbitrary length sequence of Char.      |

### Algebraic Types and Pattern Matching

```
type Bool {
    True,
    False
};
```

```
type List<A> {
    Nil,
    Cons(A, List<A>)
};
```
