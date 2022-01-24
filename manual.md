# Programming In uDLang

A uDLang script is a filter for structured data.

The script consumes binary records from the *input* channel,
processes that data, and then emits the result to the *output*.
 
If it helps, you can think of uDLang as a type-safe alternative to `jq` or `awk`.

The core uDLang interpreter consumes binary data in its native format. It is 
intendeted to work in conjunction with upstream and downstream processes which convert
data between this and a variety of formats.

# Language Model

A program in uDLang defines a *kernel* -- a piece of code executed on
a single input value to produce an output value.

The interpreter will execute the same kernel on each record it receives.

uDLang kernels are *stateless*. The entire script can be thought of as a pure
function from an input value to an output value.

A uDLang environment exposes *no* mutable state whatsoever.

The lifecycle of a uDLang program is as follows:
- compile: the input source is parsed, type-checked, and converted an
  optimized representation. This could take place at ahead of time, or
  immediately after startup.
- load: a compiled program is loaded into the memory of an embedding
  environment.
- init: one-time initialization may occur, using load-time values.
- read: a frame is consumed from an input file descriptor, and
  deserialized into memory as a record.
- exec: the kernel is allowed to run to completion with the record
  given as input.
- present: an `out` statement, or the end of the script has been reached.
  The final value, if any, is forwarded to the output.

The sequence of read, exec, present repeats indefinitely, until the
input is exhausted, or the environment terminates.

a uDLang kernel has no control over its runtime environment.

The exact shape of the input accepted and the output produced are
specified by `input` and `output` delcarations in the script. uDLang
compares its input shape against the shape expected by the script. The 
default behavior is for uDLang to abort as soon as it encounters an invalid input record.
Flags can be given to change this behavior to ignore, warn, or skip invalid records.

## Side Effects ##
 
uDLang provides exactly *two* side effect mechanisms: the `out` statement, 
and the `!` (debug) operator.

### Debug `! <Expr>`

The `!` operator binds the inner-most expression on its right hand side, 
printing a debug trace of the expression value to stderr
and otherwise returning the original expression unchanged. 

Parenthesis can be used to explicitly group debug values.

### Output `out <Expr>;`

When execution encounters an `out` statement, the value of the
expression will be captured and eventually serialized to the output
stream. The shape of `<Expr>` must match the shape declared by the 
`output` declaration at the script's top level.

A script must declare the output type exactly once in its lifetime,
and it must be declared before the first side effect statement
is reached.

`out` can be used inside functions or conditionals expressions. In all cases,
`out` acts like an "early return" for the script as a whole. 

When `out` occurs within a block, it must be the last line of the block.

## Types ##

The type system of uDLang is inspired primarily by FlowJS, with some
inspiration from Python and Rust.

- {I,U}{8,16,32,64}
- F{32,64}
- Nat: all unsigned integers
- Int: all integers, signed or otherwise
- Float: F32 | F64
- Number: Int | Float
- Str: Arbitrary utf-8 strings.
- Array: arbitrary-length sequence of uniform type
- Tuple: fixed-length sequence of arbitrary types.
- Map: arbitrary key/value pairs of uniform type.
- Record: fixed set of fields with explicit string keys and value types.
- Unions: closed set of types
- ValueType: lift aribitrary values to the type level.

All data in uDLang is immutable, with no exceptions.

uDlang distiguishes a tuple from a list. Though they may have the same
runtime representation, a List is a variable-length sequence of a
single type, while a Tuple is a fixed-length sequence of heterogenous
types. Both types are indexed numerically, with zero-based indexing.

Similarly, uDLang distinguishes between a *map*, which associates
arbitrary keys with values of a single type, and a *record*,
which has a fixed set of *fields*, each of which has its own type.

uDLang uses structural types.

*Unions*, aka "Sum Types" allow controlled polymorphism.

Access to unions is type-safe: only operations defined on the *intersection* 
of all the union variants are valid on a union type.

```
func add<T: Int>(x: T, y: T) -> T     {x + y}         // okay, types are the same.
func add(x: Int, y: Int)     -> Int   {x + y}         // okay, because of implicit widening conversions.
func add(x: Int, y: Float)   -> Float {x + y}         // nope, Int and floats aren't compatible.
func add(x: Int, y: Float)   -> Float {x as F32 + y}  // okay, explicit cast + implicit widening conversions.
```

A *match expression* can be used to extract data from the variants of interest.
```
...
match self.scale {
   case "default": (1.0, 1.0);
   case s:Float:   (s, s);
   case p:Point:   p;
}
...
```

## Syntax Reference (with Examples) ##

TBD
