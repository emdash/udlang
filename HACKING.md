# Notes for Hacking on uDLang

The following is a rough guide to hacking on uDLang. I can't give you
a more comprehensive overview of the design than this, because the
project is still in its infancy.

If you know anything about Rust, Type Theory, Functional Programming,
Programming Language Design, Unix Pipes, or working with structured
data, your contributions to the project would be especially valued.

Even if you know nothing of the above, as long as you are
enthusiastic, willing to learn, and willing to hack, you are welcome
here.

## Core Values

- keep uDLang small.
- keep uDLang static.
- keep uDLang fast.
- keep uDLang curly.
- keep uDLang pure.
- keep uDLang accessible.
- keep uDLang unix.

## Design

### Implementation

*keep uDLang small.*

Many popular languages -- both functional and imperative -- have a
very large footprint. uDLang aims to have a *small* footprint. This
means both a small footprint on disk for the environment and
libraries, as well as a small footprint in RAM for the interpreter.

The python interpreter binary is several megabytes in size. Just
starting the interpreter consumes 10 or 20MB, before any code has yet
run. uDLang should use far less.

### Types

*keep uDUlang static.*

uDLang is a statically-typed language. This is to help catch bugs,
first, and for performance second. Avoid proposing features for uDLang
which rely primarily on dynamic behavior to function. uDLang is about
static guarantees.

### Implementation

*keep uDLang fast.*

Avoid proposing features which require excessive runtime overhead, or
which might delay interpreter startup times. a uDLang interpeter
should have no perceptible startup time. It must be as fast, or
faster, than starting a python3 interpreter on the same system.

One caveat, however: at least for now, type checking is allowed to be
somewhat slow, so long as an optimized and byte-compiled uDLang
program can loaded quickly after type-checking succeeds.

### Syntax

*keep uDLang curly.*

uDLang aims to bring imperative programmers into *into the fold* (pun
intended).

uDLang embraces the curl-brace syntax of more recent algol-family
languages, among them C, C++, Java, and JavaScript. One of the goals
of uDLang is to be accessible to those unfamiliar with ML- or
LISP-family languages -- this is still many, many developers.

ML-style syntax is a barrier to understanding concepts in functional
programming.

If you're looking for a *syntax* model (not necessarily a semantic
model), Rust, Scala, and EcmaScript come the closest.

Where possible, try to avoid introducing syntax that makes C, C++, or
Java developers want to claw their eyes out.

### Semantics

*keep uDLang pure.*

uDLang is conceived as a *pure* functional language. It does this by
pushing state out to pre- and post-processors, and / or embedding
environments. Avoid proposing features which introduce mutable state
into the language.

A good rule of thumb is that a uDLang expression should always yield
the same result each time it is evaluated on the same input. If a
feature you're proposing violates this property, it probably isn't a
good fit for uDLang. Be prepared to justify it.

### Idioms, Patterns, and Libraries.

*keep uDLang accessible.*

Do you understand Monads? Good for you! Please don't propose that
uDLang place Monads front-and-center unless you're prepared to provide
a *one sentence* explanation of what a Monad *is* to a C, C++, or Java
developer.

Most programmers don't really learn a language by studying syntax and
library reference material. Some people can, but most study *working
examples*, and tweak them to suit their needs.

Through trial and error, with the compiler or interpreter's feedback,
a programmer eventually develops a repertoir of *idioms* and
*patterns*, which are the true building blocks of a language from the
human perspective.

These idioms can make or break a language. Witness C++ template
metaprogramming. uDLan should be easier to understand than C++
template metaprogramming.

If a proposed feature requires convoluted idioms or patterns in order
to be usefully applied, perhaps it is a mis-feature. Conversely, if we
find ourselves advocating for contorted idioms or patterns, perhaps it
is a clue that a crucial feature is missing from the language, or we
have uncovered a fundamental design problem within uDLang.

### Target Platform

*keep uDLang unix.*

One of the main use cases envisioned for uDLang is as a language for
pipeline filters. Whatever dreams we might have for uDLang, it should
remain an excellent tool for this purpose.

It's explicitly part of the uDLang programming model that data goes in
at one end, and comes out the other, moving in one direction. Avoid
proposing features which require bi-directional data-flow.

It's explicitly part of the uDLang programming model that it operate
within the minimal set of features exposed by a unix
pipe. Essentially, `read`, `write`, and `flush` are the only syscalls
that uDLang should require.

## Ways to get started

Start by reviewing the open issues in Github.

Some issues are mainly design notes, with open questions and some can
be considered "requests for comment." Feel free to opine. uDLang would
benefit greatly from early feedback.

Other issues are low-hanging fruit, and may be good ways to get a feel
for the code.

## Key Topics that will make your life easier.

### Parser

The parser is written in LALRPOP. It is similar to Yacc, but it is
better, and it works very nicely with Rust. For more, see the lalrpop
crate on crates.io.

### Type-checking

The type-cheker is a work-in-progress. The general idea is that it
will walk the AST produced by the parser, and try to determine the
type of every expression and sub-expression in the program, and then,
it tries to make sure that all the types in the program agree with
each other.

I have never written a type-checker before, so I would value any
assistance. I am still getting up to speed on type theory, so I am
bound to make some mistakes.

One thing I feel like I should point out is that I may not necessarily
make the same decisions as ML, Haskell, or other languages one might
be familiar with.

### Virtual machines, and FORTH

The plan for the moment is to write a naive tree interpreter just to
get uDLang off the ground. I expect that, with the choice of Rust, and
some experimentation with different memory management strategies, that
this will still perform "reasonably well enough" for an MVP. We shall
soon see.

I expect that eventually uDLang will target a custom stack-based
VM. It may help if you have some experience with FORTH, RPN, or JVM
bytecode, etc. Native compilation is a goal, but it is far down the
horizon.

Register machines are known to be more efficient in most cases. If you
have any experience with those, and with code gen targeting register
machines, I would love to pick your brain on the topic.

If you have experience with other models, and are prepared to advocate
for them, I'd love to hear about it. One hybdrid approach I find
intriguing uses a sliding "register window", effectively splitting the
difference between a register and a stack. It can avoid both the
complexity of register allocation, and the performance penalty of
stack-shuffling.

### IO handling

Unix pipes are not quite as simple as they first appear, or as I would
like. There are various flags and IOCTLs that one can set, in order to
influence how the kernel treats the data. There is also a lot of
legacy functionality devoted to the peculiarities of particular models
of teletypewriters and modems.

uDLang doesn't really care about most of this. It wants an input
stream from which it can read bytes, and it wants an output stream to
which it can write bytes. But it's good to have an understanding of it
anyway, because you may have to delve into it.

Things to be at least dimly aware of:
- file descriptors can be in a blocking or non-blocking modes, among
  others.
- what the flush operation does
- the various operations provided by termcap and termios
- IO buffering
- ulimit
- strace and what it's for

The one operation beyond reading and writing uDLang might expose to
the user is the *flush* operation. This tells the kernel to go ahead
and send whatever might be queued up in the write buffer for the file
descriptor in question. If you're not seeing output downstream when
you think you should, try following a call to `write()` with a call to
`flush()`. Most of the time, this gets the data flowing. Otherwise,
the kernel, or at least libc, is often waiting for the buffer to reach
a certain size before it sends it downstream.
