# uDLang #

uDLang aims to be a small, statically typed, pure functional language
that reads like JavaScript or TypeScript. Syntax derives from C-family
languages, while semantics derive from ML-family languages.

Fundamentally impure concepts -- namely, mutable state and IO -- are
simply omitted from the language. The focus is on providing *pure
sublanguage* which may be easily embedded within an impure *host
environment*.

The reference implementation is in Rust, and aims to be a practical
runtime library and command-line interpreter. Design goals include:
low runtime overhead, fast startup time, and minimal dependencies.

## Some Examples

### Structured Data Example

Let's imagine that we have some temperature data in a file called
`temps.json`, captured in celcius.

We want to convert it to farenheit for presentation. The data is a
stream of JSON objects, separated by newlines:

```
{"time": 0, "temp": 4.5}
{"time": 1, "temp": 4.7}
{"time": 2, "temp": 6.2}
{"time": 3, "temp": 7.8}
{"time": 4, "temp": 9.5}
{"time": 5, "temp": 11.1}
```

Script:

```
// every script must begin with some metadata
version 0.2;
script  "Data processing example";

// SData is any structured data interconvertible with JSON.
input  SData;
output SData;

func convert(t: Float) -> Float = 9.0 * t / 5.0 + 32.0;

// convert samples which match the schema, leave others unchanged.
match (in) {
  case {time, temp}: {time, temp: convert(temp)};
  case $:            in;
}

```
Run this as follows:

```
udlang --mode streaming --input json:newline --output json < temps.json
```

...which yields the following output:

```
{"time": 0, "temp": 40.1}
{"time": 1, "temp": 40.46}
{"time": 2, "temp": 43.16}
{"time": 3, "temp": 46.04}
{"time": 4, "temp": 49.1}
{"time": 5, "temp": 51.98}
```

### Fibonacci, with error handling

Error handling is based on static exceptions. 

```
version 0.2;
script  "Calculate Nth Fibonacci Number";
input   Str;
output  Str;

// Addition of `U32` is `U32!`. Howver, Since the return type is also `U32!`, no `try` block is necessary.
func fib(n: U32) -> U32! = match (n) {
  // the plain value `1` may be used here, as U32 coerces to U32!
  case 0: 1;
  case 1: 1;
  case $: fib(n - 1) + fib(n - 2)
};

// The return type of `U32::parse()` is also `U32!`.
// Since set of handlers is exhaustive, the script is total and the output is
// type Str.
try {
  // within the `try`, fallible types appear infallible when handler matching the exceptions they throw.
  let result = fib(U32::parse(in));
  "${result}\n"
} catch (U32::ParseError) {
  "Error: Could not parse ${in} as an integer!"
} catch (U32::Overflow) {
  "Error: Result is larger than 32 bits!"
}
```

Run this as follows:

```
udlang --mode streaming --input raw:newline --output raw
```

With no input file given, `udlang` will use readline for input.


See [the manual](manual.md), and [examples directory][examples/] for
more examples for more.

## Why uDLang? ##

This is my attempt to design the pure functional language that I wish
had existed 10 years ago: ML-flavored semantics, JavaScript-flavored
syntax, and the flexibility of awk and Lua.

uDLang began as a DSL for
[uDash](https://github.com/emdash/udashboard), but has since become an
outlet for my interest in programming language design. This project
has also served as a vehicle for learning Rust.

My understanding of langauge design has evolved a great deal since I
began this project. Rust has also evolved a great deal. As a result,
progress has been halting.

Version 0.2 represents a fresh start after a prolonged hiatus. This is
a stripped-down, streamlined version of my original vision.

uDLang aims to be:

- completely pure.
- strongly, statically typed.
- accessible.

Values:

- safety and correctness
- "code you can reason about"
- separation of concerns
- small resource footprint
- ease of embedding
- accessiblity

Key Features:

- C-like syntax
- static typing with generics
- principled module system
- pattern matching and destructuring
- partial evaluation
- string interpolation
- extensible syntax, via *templates*.

Anticipated Uses:

- command-line batch data processing
- application configuration, scripting, and / or queries.
- a meta-language for custom DSLs.

## Purity and IO

How can a pure language allow side-effects and I/O? Well, it
*can't*. However, since uDLang is designed for embedding, this is not
really a problem: uDLang simply delegates I/O to the *host
environment*.

More generally, a core value of the uDLang project is "separation of
concerns". uDLang v0.2 is purely an *expression* language: it models
data, and operations on data. It makes no attempt to include anything
else.

### Input

Within uDLang, the `in` keyword referrs to the input value supplied by
the runtime. The type of this value must conform to the `input` type
declaration. The implementation must ensure that a script is never
supplied with invalid input.

### Output

Every script contains a top-level expression, called the *body* which
is evaluated to produce the output. The type of this expression must
conform to the `output` type declaration. The implementation must not
evaluate the script if there is a mismatch between the *body* and the
`output` declaration.

### Host Environments

#### Stand-Alone

The reference implementation supplies a command-line interpreter for
evaluating stand-alone scripts. This tool supports various modes of
operation, and a variety of common data formats, that may be flexibly
combined in various ways.

Not every combination of mode, value format, and framing format is
valid. Invalid combinations will be rejected at interpreter startup.

Modes of Operation:

- lift        (lift a constant expression to a stream)
- map         (stateless transformation of input stream)
- fold        (explicitly stateful transformation of input)
- interactive (closed loop that feeds output back to input)
- repl        (for tinkering)
- check       (for IDE support, CI, and other tool-chain uses)

Value Formats:

- structured
 - internal (exchange arbitrary data between scripts)
 - ASD (the subset of internal isomorphic to JSON)
- plain text
 - raw text
 - raw/S (raw text, excluding the listed characters.
 - tabular  (CSV and related formats)
 - JSON
- binary
 - fixed-length
 - bitstring
 - msgpack

Text Framing Formats:

- newline-delimited text
- null-delimited text
- form-feed delimited text

Binary Framing Formats:

- slurp
- fixed length packet
- length-prefixed packet (fixed max length)
- variable length packet, arbitrary length

#### Embedded

When uDLang is used as a library, with your application responsible
for all IO.

The embedding API is dead simple, because every udlang script is
essentially a pure function. You simply *call* your script on an input
value, and then inspect the result it returns. Functions are provided
for marshalling common formats, as well as for directly constructing
and inspecting uDLang values from the host language.

Memory management is similarly minimal: all memory is allocated within
a dedicated heap, which may be freed as soon as the script output is
no longer required. The embedding API provides convenience routines to
transfer values into, out of, and between heaps. Alternatively, a heap
can be recycled across invocations to take advantage of shared
computation. Garbage collection can be optionally performed, either
automatically according to a fixed policy, or explicitly at the
request of the host application.

## Examples

## uDLang and Koka

I became aware of Koka after starting uDLang. Koka is superficially
similar to uDLang, in that it's a pure functional language with curly
braces. I am not terribly familiar with Koka, but here's a summary of
the obvious differences:

- Koka supports algebraic effects, uDLang does not model effects.
- Koka supports IO via effects, uDLang delegates IO to the host environment.
- Koka supports error-handling via effects, uDLang via ADTs.
- Koka is general-purpose, while uDLang is domain-specfic.
- Both Koka and uDLang support "trailing lambdas" on function calls --
  but uDLang generalizes this to templates.
- Koka uses automatic semicolon insertion, uDLang requires semicolons.
- uDLang has special syntax for partial evaluation.
