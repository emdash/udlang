# uDLang #

uDLang is a minimalist, pure functional programming language with
curly-brace syntax. 

As a command-line tool, uDLang targets operations on streams of JSON
or other structured data.  As an embedded languge, uDLang functions as
a principled scripting and configuration language.

### Hello World

```
// hello_world.ud

// uDLang is evolving, by mandating a version string, we guard
// against breaking changes to the language.
version 0.1-pre_mvp;

// uDLang requires a file be either a script or a library, and it
// requires a docstring describing the purpose of the file.
script  "hello world example";

// uDLang requires a script to declare the data types it handles at runtime.
input  Str;
output Str;

// uDLang has one mechanism for interacting with the world, the `out` statement. 
// The value given to `out` must be of the type declared by `output`.
out "Hello, World!\n";

```

To run:
`cargo run -- examples/hello.ud`


## Overview 

uDLang can be thought of as strongly-typed alternative to filters like
`jq`, `awk` or `sed`, for the increasingly common case that input data
is structured, like JSON, BSON, msgpack, yaml, and many others.

Alternatively, uDLang can be thought of as a pure, statically-typed
alternative to lua, python or EcmaScript.

uDLang strongly resembles [[Koka|]] in both syntax and semantics,
though this similarity is entirely coincidental.

uDLang is:
- record-oriented
- strongly, statically ytyped
- purely functional
- aims for safety, correctness, and readability over performance or
  brevity.

Feature list:
- type annotations, inference, and static type checking.
- all data is immutable.
- all side effects are explicit.
- pattern matching.
- rich, structural typing.
- partial application

uDLang models operations on structured data in the abstract,
independent of a specific wire format. Anything goes, so long as it
isomorphic to JSON.

uDLang extends primitive JSON types -- strings, numbers, lists, and
records with higher-level notions of "type membership", allowing for
static type safety.

For a more detaled discussion of uDlang's design, see `manual.md`.

**uDLang is a work in progress, not all features documented here are
implemented**.

## More Examples

### Input/Output

```
version 0.1-pre_mvp;
script "input example";
input  Str;
output Str;

// uDLang has one source for runtime values, the `in` expression.
out "Hello, " + in + "\n";

```

### Hello / Goodnight

```
version 0.1-pre_mvp;
script "pattern matching example";
input  Str;
output Str;

// uDLang supports pattern matching 
out match in {
  case "Hello":     "Hello, World!\n";
  case "Goodnight": "Goodnight, Moon!\n";
};

```

### JSON Processing

This example converts an array of point-like records to a record of
arrays.

```
version 0.1-pre_mvp;
script "json processing example";

// Records in uDLang are a special case of maps, and uDLang uses structural
// typing for records and maps.
//
// For records, fields required to be present unless explicitly declared optional.
input [ record {
  field  x: Int;
  field  y: Int;
  // This field is optional.
  field? z: Int;
} ];

output record {
  field x: [Float];
  field y: [Float];
  field z: [Float];
};

// records in uDLang are constructed like maps
out {
  x: in.map($.x as Float), 
  y: in.map($.y as Float),
  // a || b is sugar for `try {a} except (...) {b}`
  z: in.?.z as float || 0.0
};

```

## What uDLang is not

uDLang is not a systems language.

uDLang is not yet production-ready.

The goal for the MVP milestone is that uDLang should be minimally
useful for casual coding. 

Subsequent releases will introduce optimizations that benefit common
use-cases as they emerge.

## Why uDLang ##

uDLang started as a quick-and-dirty DSL for uDash, another project of
mine. In the course of this, I realized that a small, pure, functional
programming language with familar syntax might have broader appeal,
and should probably become a separate project.

uDLang has many sources of inspiration, in addition to those already listed:
- FlowJS, for the syntax and semantics of type annotations.
- OilShell, another project bringing a modern approach to a traditional domain.
- ML-family languages.
- JavaScript, and its data-oriented subset, JSON.
- JSON Schema
- CDL, the most minimal approach to language design I have yet
  encountered, and well worth pondering.
- The Elm project.
- Rust, the implementation language.
- Koka -- not a direct source of inspiration, but a surprisingly
  similar independent effort (that is farther along in most respects).

## uDLang and Koka

I've recently become aware of Koka, a language which shares many
superficial similarities with uDLang. There is a great deal of
overlap, and I'm very excited by the ideas presented in Koka. In the
future, I may evolve uDLang in ways that align it more closely with
Koka. As it stands right now, uDLang is part of the Rust ecosystem.

I am not terribly familiar with Koka myself, but here's a rough summary of the obvious differences:
- Koka's effect system is strictly more expressive than uDLang's
  - Koka's effects desugar to continuations.
  - Koka's effect system can express constructs like exceptions,
    whereas uDLang plans to implement exceptions as a language feature.
  - Koka appears to be a general-purpose applications or even systems
    language that targets native compilation, while uDLang aim to be a
    simple language that is easily embedded.
- Both Koka and uDLang support "trailing lambdas", but support for
  this in uDLang is limited to procedure calls due to limitations in
  the parser, while Koka supports them more generally.
- Koka defines method call expressions like `x.f(...)` 
  to mean: `f(x, ...)`,
  while udlang defines it to mean: `x[Atom("f")](x, ...)`
  - uDLang aims to guarantee that this statically optimize to a flat
    function call by default, but the semantic distinction influences
    how the method is resolved.
- uDLang programs are kernels operating on records, wheras Koka
  programs are strictly more general.
  - uDLang is record-oriented: a script body executes once for each 
    record supplied by the runtime.
  - the `in` keyword, which always binds to the current input record,
    is a construct unique to uDLang as far as I know.
- uDLang has an experimental construct `suppose` which is used for
  certain edge cases in tree-shaped data. Koka's effect system may or
  may not be powerful enough to express an equivalent notion.
- Koka uses automatic semicolon insertion similar to EcmaScript,
  whereas uDLang has no plans to implement this feature, or to
  otherwise eliminate semicolons from the grammar.

## Why the *name* uDLang?

The name is short for uDashboard Language. That's it. It's a working
title, and I'm open to changing it.

## An in-depth Example ##

Let's imagine that we are writing a todo-list web-application. As part
of this, we want to convert a JSON payload received from a web service
into legible HTML. Let's say our JSON payload looks like this:

```
{
  "name": "Brandon's Tasks"
  "items": [
    {"id": 0, "name": "Get prescriptions", "status": "complete"},
    {"id": 1, "name": "Schedule doctor's appointment", "status": "incomplete"},
    {"id": 2, "name": "Get gravel for driveway", "status": "blocked", "blocker": 3},
    {"id": 3, "name": "Get truck bed-liner installed"}
  ]
}
```

A uDLang filter to render the todo list into html might look like this:

```
// todo.ud

// uDLang is evolving, by mandating a version string, we guard our code
// against breaking changes to the language.
version 0.1-pre_mvp;
script "Todolist Example";

// Import some helper functions from our html companion library (see below), as well
// as the library itself.
import html.{_, html, head, title, body, h1, h2, ul, li, div, text};

// Define a type alias for a single todo-list item.
// Type names must start with an upper-case letter.
type TodoItem: record {
  field id: Int,
  field name: Str,
  // uDLang supports "string enums" similar to FLowJS.
  field status: "complete" |  "incomplete" | "blocked";
  field? blocker: Int;

  // records can define methods, which take an implicit self parameter.
  method format() {
    // uDLang supports string interpolation.
    li({class: "todo-item ${item.status}"}) {
      text(self.name);
      if (self.blocker != null) {
        let blocker = items[item.blocker];
        div({class: "alert"}) {
          text("Blocked on ${blocker.name}");
        };
	  }
    };
  };
};

// Declare the shape of the input.
input {field name: Str; field items: [TodoItem]};

// Declare the output type, which is defined by the HTML helper library.
output html.Output;

// Helper function to format single item in our list.
proc format_item(item: TodoItem) {
};

// Format the entire document. As with many scripting languages, there is no
// specific program entry point, such as a function named `main`.
//
// All allusions to HTML syntax here (`head`, `body`, etc) are uDLang
// functions living in the helper library.
//
// uDLang supports a "template" syntax for calling functions with trailing block, 
// a feature borrowed from Ruby.
html() {
   head() {
     title() {text(in.name);};
   };

   body() {
     h1() {text(in.name)};
     div({class: "todo-list"}) {
       ul() {
         for item in in.items {
           format_item(item);
         };
       };
     };
   };
};
```

Let's run this example:

 `$ udlang todo.md < example.json`

We obtain the following output:

**TBD**

Now let's try it again on some invalid input:

 **TBD**

## The HTML Library ##

We've seen that uDLang can serve as an HTML template engine, but it is
neither limited to, nor primarily intended for, this purpose. The
`html` module is a user library written in udlang. 

To get a feel for the power and simplicity of uDLang, let's explore
the `html` library:

```
// html.ud
version: 0.1-pre_mvp;
lib "Simple Html Formatting Library";

// Declare the output type for this library.
type Output: Str;

// Converts a string to an html-escaped format, implementation omitted for brevity.
func quote(text: Str) -> Str = { /* ... */ };

// A template allows flexible composition of functions that produce side-effects.
// Here, the `using` syntax is just sugar for binding `children` to a closure 
// argument. The `template` keyword is intended mainly as hint for linters.
template element(tag: Str, attrs: Map<Str> = {}) using content {
  // Begin by writing the opening tag.
  out "<${tag} ";
  
  // Append any attributes to the output.
  for (attr, value) in attrs {
    out " ${quote(attr)}=${quote(value)}";
  }
  
  // uDLang supports a back-tracking mechanism called a *subjunctive*.
  //
  // Here it is used to cleanly handle the syntax for empty HTML elements.
  // The side effects caused by evaluating `children()` will appear in the position of 
  // the elipsis in the final output.
  suppose (content()) {
    out ">";
    ...;
    out "</${tag}>";
  } otherwise {
    out "/>";
  };
};

// Make this template definition available to importing scripts.
export Output;
export element;

// With this general definition of an HTML element in hand, we can specialize it for
// standard tags. uDLang supports *partial application* via the wildcard operator.
export html = element("html", $, $);
export head = element("head", $, $);
export body = element("body", $, $);
export div  = element("div",  $, $);

// `br` cannot contain content. Here we supply an empty
// block to prevent callers from accidentally
// passing a closure
export br   = element("div", $) {};
// ... remaining elements elided for brevity.

```

## Reference Implementation Goals ##

Core

 - small, maintainable library written in entirely in safe Rust.
 - fast interpreter startup time.
 - limited optimizations:
   - inlining
   - const-folding
   - CSE
 - for now, focus on correctness over speed.
  - develop comprehensive test suite.
  
Ser
- pluggable encoding / decoding support for Core

Command-line tool:

 - targets uDLang as as a filter within unix pipelines
 - can flexibly switch between different encodings
 - native support for JSON, msgpack, plaint text, and various common
   "framing" schemes.
 - doubles as a command-line uDLang debugger.

## Project Status ##

The current version is `0.1-pre_mvp`.

 - Grammar: about 95% done.
 - Runtime: about 80% done.
 - Typechecker: very rough.
 - Serializtion / Deserialization: TBD
 - Exceptions: TBD

uDLang is in its infancy. If the ideas behind uDLang exite you,
contributions are welcome.  If you don't know where to start, consider
submitting test cases or example scripts, or implementing support for
your favorite encoding / framing format.

How might you use a tool like uDLang? I'd love to hear from you!
