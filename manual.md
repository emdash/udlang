# Programming In uDLang

A uDLang script is a *pure computation kernel* which operates on
structured data within some *host environment*.

The *host environment* is responsible for supplying the input and
interpreting the output. A script may written portably, such that it
can be used across any host environment.

This manual, and the language itself is a work in progress.

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
input Void;

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

# Execution Model

| Term                | Definition                                         |
|---------------------|----------------------------------------------------|
| Script              | A top-level uDLang source file                     |
| Kernel              | The complete compiled output of a script           |
| Runtime Environment | The code responsible for evaluation of the kernel  |
| Host Environment    | The top-level environment which embeds the runtime |

A script defines a *kernel* -- a piece of code executed on an input
value to produce an output value. A kernel is essentially a pure
function, plus associated metadata. uDLang kernels are *stateless*
and, with one minor exeception discussed below, without side effects.

The runtime environment is responsible for evaluating the kernel. The
runtime environment is embedded in a *host environment*, -- e.g, the
udlang command line tool, or your own applicaiton -- which can
evaluate the kernel in arbitrary ways.

The lifecycle of a version 0.2 script looks like this:

| compile   | Source is parsed, type-checked, and converted an internal representation. |
| load      | Compiled code  is loaded into the memory of the *runtime environment*.    |
| construct | an input datum is constructed by the *host environment*.                  |
| evaluate  | the runtime evaluates the kernel on the datum.                            |
| inspect   | the *host environment* inspects and processes the output datum.           |

The sequence of *construct*, *evaluate*, *inspect* may be performed
any number of times after the *compile*, and *load*. A kernel has no
control over its lifecycle.

## Debugging, and Side Effects ##

uDLang has *one* effectful construct: The *debug operator*, `!`, which
allows inspecting the result of arbitrary subexpressions within a
script or library.

| Example Usage    | Debug output  |
|------------------|---------------|
| `func(!x * 2)`   | `x`           |
| `func(!(x * 2))` | `x * 2`       |
| `!func(x * 2)`   | `func(x * 2)` |
| `(!func)(x * 2)` | `func`        |

The behavior of the debug operator is under the control of the *host
environment*.

## Types and Expressions ##

The type system of uDLang is similar to Haskell's, with a few ideas
borrowed from Python.

At the bottom, we have the following primitive types supplied by the
runtime.

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

User-defined types are introduced with the `type` keyword, followed by
a list of *constructors* in braces.

```
type Bool {
    True,
    False,
};
```

Constructors are scoped under the type name, as with Rust
enumerations. The constructors may appear in expressions, or as
patterns.

```
func not(x: Bool) -> Bool { match (x) {
  case Bool::True:  False,
  case Bool::False: True
} };
```

Type variants may cary data:

```
type Interval {
  Singleton(Int),
  LeftClosed(Int),
  RightClosed(Int),
  Closed(Int, Int)
};
```

Functions can be added to a type,

```
func clamp(self: Interval, x: Int) -> Int {
  match (self) {
    case Interval::Singleton(i):    i,
	case Interval::LeftClosed(lb):  max(lb, x),
	case Interval::RightClosed(ub): min(ub, x),
	case Interval::Closed(lb, ub):  min(ub, max(lb, x))
  }
};
```

Where appropriate, these can be called with method syntax:

```
let hours = Interval::Closed(0, 11);
hours.clamp(time.time() % 24)
```


Types may be generic:

```
type Option<A> {
	None,
	Some(A)
};
```

Types may be recursive. The special keyword Self can be used as a
shorthand for type under definition.

```
type Nat {
	Zero,
	Succ(Self)
};

func decrement(n: Nat) -> Nat {
  match (n) {
    case Nat::Zero:    Zero,
	case Nat::Succ(m): m
  }
};
```

### Structured Data, `SData`, and Syntactic Sugar

A major design goal is to support computations on JSON and related
data formats with a uniform and concise syntax. To that end, JSON-like
values sit at the core of the type system, with a dedicated syntax
that will hopefully seem familiar.

#### Lists

```
type List<A> {
	Nil,
	Cons(A, Self)
}
```

List shorthand looks like: `[x, y, z]`, which expands to
`List::Cons(x, List::Cons(y, List::Cons(z, List::Nil)))`

List types are written as `[A]` which expands to `List<A>`.

#### ALists

Associative lists are lists of key-value pairs:

```
type AList<A, B> {
	Nil,
	Cons(A, B, Self)
};
```

Associative lists are written using ES6-style object literal syntax,
which comes in two flavors:

For example, `{foo: 1, bar: 2}` expands to `AList::Cons("foo", 1,
AList::Cons("bar", 2, AList::Nil))`

While `{foo, bar}` expands to `AList::Cons("foo", foo,
AList::Cons("bar", bar, AList::Nil))`

AList types are written as `{K: V}`, which expands to `AList<K, V>`.

Note that other languages which use hashtables, ALists are a form of
linked list. This means that ordering and repetition of elements is
preserved, and patterns are sensitive to both order and repetition.

#### SData

This brings us to the SData type:

```
type SData {
	Null,
	True,
	False,
	Int(Int),
	Float(Float),
	String(String),
	Array([SData]),
	Object({Str: SData})
};
```

When no constructor is used to introduce an expression or pattern,
then extended desugaring is applied. These extra rules effecively make
`SData` the "default" data constructor, in a way designed to mimic the
syntax of ES6 and other mainstream languages. These same rules apply
uniformly in pattern and expression contexts, allowing for reasonably
concise patterns.

| Syntax Sugar    | Expansion                                  |
|-----------------|--------------------------------------------|
| `null`          | `SData::Null`                              |
| `123`, `4.0`    | `SData::Int(123)`, `SData::Float(4.0)`     |
| `true`, `false` | `SData::True`, `SData::False`              |
| `"foo"`         | `SData::String("foo")`                     |
| `[x, y]`        | `SData::Array([x, y])`                     |
| `{foo: bar}`    | `SData::Object({"foo": bar})`              |
| `{"foo": 123}`  | `SData::Object({"foo": SData::Int(123)})`  |
| `{foo: bar}`    | `SData::Object({"foo": bar})`              |
| `{foo, bar}`    | `SData::Object({"foo": foo, "bar": bar)})` |

### Error Handling

The type-based error handling of Rust and ML family languages is
certainly simple and effective. Exceptions, on the other hand, are
concise. uDLang offers the best of both worlds. In particular,
unhandled exceptions are detected statically.

One wrinkle is that, as with other control flow constructs, the `try
...` keyword introduces *an expression*, which must yield a
value. This means that handlers must either yield a value or throw.

As uDLang does not permit side effects, there is no need for
`finally`, which has no meaning in an expression context.

#### Falliblity

Operations can be fallible or infallible. Fallible operations either
yield a value, fail with an exception.

Infallible: boolean and, less than, bitwise operations on integers.
Fallible: fixed-integer addition, division, parsing.

For some type `A`, `A!` is the *fallible* version of that type, which
may yield a value or an arbitrary exceptions.

An `A!` cannot be used in place of an `A`, though an `A` can be used
in place of an `A!`.

The `try` ... `catch` expression works as follows: For each expression
in the try block, we identify the fallible subexpressions, and compute
the set of exception values they might raise. The union of the
expression sets of all the subexpressions is the set that must be
covered by the catch handlers. Each catch handler is an explicit
pattern.

The way to think of it is that every throwing operation has a type,
plus a set of exception values it might throw. Within a block,
exception values union onto the type of the block. This applies to the
`try` block, too.

`catch`, or handler clauses, on the other hand, *strip off
exceptions*. So the type of the try expression as a whole is the
underlying value type *minus* the exceptions stripped off by each
handler.

Another way to think of it is that within the uDLang type system,
every subexpression `e` of type `T` is implicitly of type
`Result<T,E>`, where `E` is an automatically inferred sum type
comprising every error that might be thrown from `e`. When `E` is
empty, we simply write the type as `T`. When `E` is nonempty, we write
it as `T!`.

It's important to understand that `try ... catch` is an expression. As
with pattern matching, the result type of each clause must agree.

```
func slope({x: Float, y: Float}) -> Float = try {
  let {x, y} = p;
  // the type is constrained to Float by the return type of `slope`.
  x / y 
} catch (Float::DivideByZero) {
  // type mismatch: Float / Int
  "Oops"
};
```

#### User-Defined Exceptions, and Explicit Throws

The `throw` keyword is a valid expression. It returns 

```
func octalDigitToInt(c: Char) -> Int! = match (c) {
  case '0': 0,
  case '1': 1,
  case '2': 2,
  case '3': 3,
  case '4': 4,
  case '5': 5,
  case '6': 6,
  case '7': 7,
  case c  : throw ValueError("Invalid digit ${c}"),
};
```

### Types as Namespaces

types should be convenient places to "hang" related information.


