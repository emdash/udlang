# uDLang #

uDLang is a minimalist, pure functional programming language with
curly-brace syntax. 

As a command-line tool, uDLang targets operations on streams of JSON
or other structured data.  As an embedded languge, uDLang functions as
a principled scripting and configuration language.

### Hello World

```
// hello_world.ud
version 0.1-pre_mvp;
script "hello world example";

input  Void;
output Str;

out "Hello, World!\n";
```

To run:
`cargo run -- examples/hello.ud`

## Why uDLang ##

uDLang started as a DSL for
[uDash](https://github.com/emdash/udashboard). Over time the language
became sufficiently general to warrant becoming a separate
project. Perhaps a simple functional language with mainstream
curly-brace syntax will have broader appeal.

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

### Why the *name* uDLang?

The name is short for uDashboard Language. It's a "working title", and
I'm open to changing it.

## More Examples

### Input / Output

```
version 0.1-pre_mvp;
script "input example";
input  Str;
output Str;

// uDLang has one source for runtime values, the `in` expression.
out "Hello, " + in + "\n";

```

### Pattern Matching

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
arrays, i.e this `[{x: 0, y: 1}, {x: -1, y: 7}]`, becomes `{x: [0.0,
-1.1], y: [1.0, 7.0], z:[0.0, 0.0]}`.

```
version 0.1-pre_mvp;
script "json processing example";

// Records in uDLang are a subtype of maps.
type Point: record {
  field  x: Int;
  field  y: Int;
  // This field is optional.
  field? z: Int;
};

// This script expects a list of Point records as input.
input [Point];

// The script will convert the input list to this output record.
output record {
  field x: [Float];
  field y: [Float];
  // z is not optional here.
  field z: [Float];
};

// Construct the output value and send it downstream.
out {
  // $.x is a partial function, short-hand for:
  //   `(i: Point) -> Float {i.x as Float}`
  x: in.map($.x as Float), 
  y: in.map($.y as Float),
  // since `in.z` may be null, we need to supply a default value.
  z: in.map($.z as float ? 0.0)
};

```

## Overview 

On the command line, uDLang is a strongly-typed alternative to stream
processing tools like `jq`, `awk` or `sed`.

As an embedded language, uDLang is a strongly-typed alternative to
lua, python or EcmaScript for scripting and configuration.

uDLang models operations on structured data in the abstract,
independent of a specific wire format. Data can be converted from
structured formats, like JSON, BSON, msgpack, yaml, or plain text, or
your own application-specific types.

uDLang extends primitive JSON types -- strings, numbers, lists, and
maps with higher-level notions for stronger static guarantees.

uDLang is:
- record-oriented.
- strongly, statically typed.
- purely functional.
- aims for safety, correctness, and readability over performance or
  brevity.

Features:
- static type checking.
- immutable data.
- all side effects are explicit.
- structural typing.
- pattern matching.
- destructuring assignments.
- partial evaluation.

For a more detaled discussion of uDlang's design, see `manual.md`.

uDLang resembles
[Koka](https://koka-lang.github.io/koka/doc/index.html) in both syntax
and conceptualy, though the two projects have no direct relationship.

**uDLang is a work in progress, not all features documented here are
implemented**.

### What uDLang is not

uDLang is not a systems language.

uDLang is not yet production-ready.

The goal for the MVP milestone is that uDLang should be minimally
useful for casual coding. 

Subsequent releases will introduce optimizations that benefit common
use-cases as they emerge.

### uDLang and Koka

I've recently become aware of Koka, a language which shares many
superficial similarities with uDLang. There is a great deal of
overlap, and I'm very excited by the ideas presented in Koka. In the
future, I may evolve uDLang in ways that align it more closely with
Koka. As it stands right now, uDLang is part of the Rust ecosystem.

I am not terribly familiar with Koka, but here's a summary of the
obvious differences:
- Koka's effect system is strictly more expressive than uDLang's
  - uDLang has one effect: the `out` statement.
  - uDLang effects cannot be "handled" within the uDLang script --
    they are values which are only externally visible.
  - Koka's effects are syntactic sugar around continuations, allowing
    for non-linear control flow.
  - Koka implements exceptions through its effect system, while uDLang
    will implement exceptions as a language feature.
  - Koka appears to be a general-purpose applications or even systems
    language that targets native compilation, while uDLang aims to be a
    small, embeddable language.
- Both Koka and uDLang support "trailing lambdas", but support for
  this in uDLang is limited to procedure calls due to limitations in
  the parser, while Koka supports them more generally.
- Koka defines method call expressions like `x.f(...)` 
  to mean: `f(x, ...)`,
  while udlang defines it to mean: `x[Atom("f")](x, ...)`
  - uDLang aims to guarantee that this will optimize to a direct
    function call, the distinction is mainly how each language decides
    which function to call.
  - Koka's notion allows defining "methods" on a type externally, whereas 
    uDLang does not allow for this.
  - In Koka, it's not clear you can refer to a method's function value
    directly (without calling it), while in uDLang you can.
- uDLang is record-oriented, while Koka has no such notion.
  - uDLang scripts are kernels operating on a stream of records.
  - the `in` keyword, which always binds to the current input record,
    is a construct unique to uDLang as far as I know.
	- as are the `input`, and `output` declarations.
- uDLang has an experimental construct `suppose` which is used for
  certain edge cases in tree-shaped data. It's not yet clear to me if
  Koka's effect system can express this construct.
- Koka uses automatic semicolon insertion similar to EcmaScript,
  whereas uDLang has no plans to implement this feature, or to
  otherwise eliminate semicolons from the grammar.
- Koka seems to be a more mature project.
- uDLang supports partial evaluation via the `$` operator, while it's
  not clear that Koka has any mechanism for partial evaluation.

## HTML Example ##

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

A uDLang filter to render the todo list into html might look like
this:

```
// todo.ud

// uDLang is evolving, by mandating a version string, we guard our code
// against breaking changes to the language.
version 0.1-pre_mvp;
script "Todolist Example";

// Import some helper functions from our html companion library (see below),
// including the library itself.
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
// uDLang supports a "template" syntax for calling functions with trailing 
// block, a feature borrowed from Ruby.
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

// Converts a string to an html-escaped format, implementation omitted for 
// brevity.
func quote(text: Str) -> Str = { /* ... */ };

// A template allows flexible composition of functions that produce 
// side-effects. Here, the `using` syntax is just sugar for binding `content` 
// to a closure.
template element(tag: Str, attrs: Map<Str> = {}) using content {
  // Begin by writing the opening tag.
  out "<${tag} ";
  
  // Append any attributes to the output.
  for (attr, value) in attrs {
    out " ${quote(attr)}=${quote(value)}";
  }
  
  // This back-tracking mechanism helps us cleanly handle the empty element
  // edge case.
  suppose (content()) {
    // Non-empty case
    out ">";
    ...;
    out "</${tag}>";
  } otherwise {
    // Empty case
    out "/>";
  };
};

// uDLang requires libraries to explicitly export definitions.
export Output;
export element;

// With this general definition of an HTML element in hand, we can specialize it
// for standard tags.
//
// The $ here is the "placeholder" operator, which triggers partial evaluation.
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
