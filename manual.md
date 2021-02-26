# Technical Details #

A program in uDLang defines a *kernel* -- a piece of code executed on
a single input *record* to produce arbitrary output. This kernel may
be executed once on a single record. Or, it may be executed repeatedly
to batch process an entire file. Or, it maybe executed continuously to
process a stream of realtime-data. Anything goes, so long as the data
can be segmented into records of a well-defined type.

A uDLang environment exposes *no* mutable global state to the running
script whatsoever. All state is contained entirely within the input
*record*.

The lifecycle of a uDLang program is as follows:
- compile: the input source is parsed, type-checked, and converted an
  optimized representation. This can take place at ahead of time, or
  immediately after startup.
- load: a compiled program is loaded into the memory of an embedding
  environment.
- init: one-time initialization may optionally take place, the result
  of which is re-used between exec phases.
- read: a frame is consumed from an input file descriptor, and
  deserialized into memory as a record.
- exec: the kernel is allowed to run to completion with the record
  given as input. the kernel may emit one or more "side effects" as
  output.
- present: the side effects are collected, and presented on the output
  file descriptor.

The sequence of read, exec, present repeats indefinitely, until the
runtime environment terminates. a uDLang kernel has no direct control
over its runtime environment, though in certain processing modes, the
runtime may decide to terminate in response to the behavior of a
uDLang kernel.

The exact shape of the input accepted by a uDLang kernel is specified
through the use of `param` definitions.

## Side Effects ##
 
Side effects are an unavoidable consequence of programming in the real
world. uDLang makes no attempt to hide them: instead, uDLang *models*
side-effects as data.

The output of a program is the merely concatenation of its
side-effects.

uDLang provides exactly *one* means of communicating with the outside
world: the `out` statement. 

The syntax is: `out <Expr>;`

When execution encounters an `out` statement, the value of the
expression will be captured and eventually serialized to the output
stream.

Side effects are ordered, and for the most part the sequence follows
the order in which `out` statements appear in the code, with the
notable exception of *subjunctives*, which will be discussed later.

Before producing side effects, a script must declare its output type,
for example: `output Str;` declares that the script produces plain
text, while `output [{name: Str, x: Int, y: Int}]` declares the
output type to be a list of named points (perhaps to be plotted).

A script must declare the output type exactly once in its lifetime,
and it must be declared before the first side effect statement
executes. In some cases, output is handled by library functions, in
which case the library should provide a type for you to use in this
declaration. By convention it is called `Output` in the library
namespace. If a script wishes to merge output produced by multiple
libraries, this can be accomplished with a union, like so:

```
import foo;
import bar;
output foo.Output | bar.Output;
```

Keep in mind that while uDLang itself is agnostic with respect to the
choice of encoding, clearly and precisely specifying the shape of the
output data has benefits:
- it potentially allows for runtime optimizations.
- it helps statically ensure the validity of the output produced.

You should always declare your output type as precisely as you can,
even if you know that a downstream process forces conversion to plain
text.

## Types ##

The type system of uDLang is inspired primarily by FlowJS, with some
inspiration from Python and Rust.

- Int: 64-bit signed integers.
- Float: 64-bit IEEE floating-point.
- Str: Arbitrary utf-8 strings.
- List: variable-length sequence of a single element type
- Tuple: fixed-sized sequence of heterogenous type
- Map: key-value pairs, with string keys.
- Record: fixed set of fields with static types.
- Unions: closed set of alternative types
- Filters: arbitrary subtyping using compile-time reflection.

All data in uDLang is immutable, with no exceptions.

uDlang distiguishes a tuple from a list. Though they may have the same
runtime representation, a List is a variable-length sequence of a
single type, while a Tuple is a fixed-length sequence of heterogenous
types. Both types are indexed numerically, with zero-based indexing.

Similarly, uDLang distinguishes between a *map*, which associates
arbitrary string keys with values of a single type, and a *record*,
which has a fixed set of *fields*, each of which has its own type.

*Records* have some *class-like* features borrowed from OO. In
addition to their fixed fields, records may define instance methods,
static values, static functions, and types. 

Records are structurally typed, rather than nominally
typed. Structural typing can be thought of as a static form of "duck
typing": Two records represent the same type if they define the same
members, regardless of what name we call them by.

*Unions* allow combining arbitary types. They allow lists and maps,
for example, to hold polymorphic data in a type-safe way. They consist
of two or more *variant* types. 

For example, in a configuration file,
a field `scale` might be be a string keyword, a scalar, or a vector
quantity. This can be expressed as follows:

```
record Style {
  // ... 
  field scale: "default" | Float | Point;
  // ...
};
```

Access to unions is type-safe: only operations defined on *all* the
variants are valid, unless a *refinement* is used to clarify which
variant is present:

```
...
match self.scale {
   case "default" -> (1.0, 1.0),
   case s:Float   -> (s, s),
   case p:Point   -> p,
}
...
```
Every union type `T` contains an associated type, `T.Tag` which holds merely the type of 
each alternative.

### Filter Types ###

This is an experimental feature, which has yet to be fully designed,
but the general idea is to provide a catch-all mechanism for
experimenting with extensions to the type system.

Different ways this could manifest. One idea might be a set-theoretic
approach, which might allow yout do type subsetting with boolean
predicates.

`type EvenInt: {x: Int | x % 2 == 0}`

In some ways this feature is inspired by C++ *concepts*, which are
basically form of compile-time programming.


## Templates ##

The template mechanism is a simple, elegant, language level solution
to the problem of factoring out *wrapping* or *decorating* patterns.

You want to avoid repeating yourself, but most languages make it hard
factor out code wich decorates objects with an aribtrary *prefix* and
*suffix* in a way that feels natural.

An imperative approach to tree traversal will often require an
intermediate *builder* object to track state, while a functional
approach requires constructing an intermediate data structure on the
call stack before serialization can proceed, polluting function
signatures in the process.

Somewhere in the middle are *macros*, a heavy-weight solution to what
should be a simple problem.

** TBD: summarize the template syntax, and how it de-sugars to a closure
argument.**

This allows function calls to nest in a way that more closely
resembles the shape of the data being produced.

For now, the template mechanism is restricted to *proceedures*
(functions with return-type void), but this may be relaxed in the
future if it doesn't lead to ambiguities in the grammar.

### Subjunctives ###

Subjunctives are a mechanism to ease the composition of templates by
simplifying the handling of a common edge-case: output that depends on
whether a tree node is empty.

More precisely, the subjunctive allows branching based on whether or
not a sub-expression produces output, which may or may not be
statically known, with subsequent *reordering* the delegate's output
to the correct position.

It's like a crystal ball that allows your code to "peer into the
future", so that it can make the right decision in the present.

There are four variants of the subjunctive:

`suppose (<expr>) <veridical:Block>`
`suppose (<expr>) <veridical:Block> otherwise <counterfactual:Block>`
`suppose <id> = (<expr>) <veridical:Block>`
`suppose <id> = (<expr>) <veridical:Block> otherwise <counterfactual:Block>`

The first two forms are shorthand for when the value of expr is not
needed in either branch body, while the latter forms bind the value of
expr inside either arm.

In all cases, the value of the subjunctive expression as a whole is
the value yielded by the whichever block is executed.

Within the *veridical* arm only, side-effects captured from `<expr>`
can be placed into the output stream with the `...` statement.

The `...` statement can appear arbitrarily many times, or not at all,
and in any statement context within the veridical arm. If it is
executed multiple times, then the side effects will be duplicated in
the output.

For example, the following snippet outputs "She loves you! Yeah! Yeah!
Yeah!".

```
output: Str;

proc she_may_love_you() {
  out "Yeah!";
}

suppose (she_may_love_you()) {
  out "She loves you!";
  repeat 3 {
    out " ";
	...;
  }
};
```
