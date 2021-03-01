# uDLang #

uDLang is a pure functional language with *practical* applications.

It supports creating efficient *filters*: small programs which operate
on structured data.

uDLang is both an embeddable DSL, as well as a stand-alone command-line
tool for stream processing.

In a nutshell, uDLang is an alternative to traditional pipeline
filters like `awk`, `sed`, and `perl` that is:
- record-oriented, like `jq`, rather than line-oriented, like `awk` or
  `sed`.
- clear and readable, rather than terse and cryptic.
- strongly, statically typed.
- purely functional.
- designed with security and performance in mind.

uDLang models operations on structured data in the abstract,
independent of a specific wire format. Anything goes, so long as it
isomorphic to JSON.

For a more detaled discussion of uDlang's design, see `manual.md`.

**uDLang is a work in progress, not all features documented here are
implemented**.

## Why uDLang ##

What started as an attempt to write a quick-and-dirty DSL for uDash,
another project of mine, snowballed into a deep dive into compilers,
type theory, and the Rust programming language.

My hope is that all this effort will prove generally useful to others.

uDLang has many sources of inspiration, in addition to those already listed:
- FlowJS, for the syntax and semantics of type annotations.
- OilShell, another project bringing a modern approach to a traditional domain.
- ML-family languages.
- JavaScript, and its data-oriented subset, JSON.
- JSON Schema
- CDL, the most minimal approach to language design I have yet encountered, and well worth pondering.
- The Elm project.

## A Simple Example ##

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
version: 0.1-pre_mvp;

// Import some helper functions from our html companion library (see below), as well
// as the library itself.
import html.{_, html, head, title, body, h1, h2, ul, li, div, text};

// Define the type of a single todo-list item.
// Type names must start with an upper-case letter.
type TodoItem: {
  field id: Int,
  field name: Str,
  field status: "complete" |  "incomplete" | "blocked";
  // Blocker is an optional field, as opposed to a field with an option value.
  field? blocker: Int;
};

// Declare the shape of the input.
input {field name: Str; field items: [TodoItem]};

// Declare the output type, which is defined by the HTML helper library.
output html.Output;

// Helper function to format single item in our list.
proc format_item(item: TodoItem) {
  // The item's status is appended directly to the element's class.
  li({class: "todo-item ${item.status}"}) {
    text(item.name);
    // ? is the *existential* operator, which returns true if an optional
    // field is present. An optional field *must* be tested for existence.
    if (item.blocker?) {
       let blocker = items[item.blocker];
       div({class: "alert"}) {
           text("Blocked on ${blocker.name}");
       };
    }
  };
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

## Going Deper ##

We've seen that uDLang can serve as an HTML template engine, but it is
neither limited to, nor primarily intended for, this purpose. The
`html` module is simply user library written in udlang itself. 

To get a feel for the power and simplicity of uDLang, let's explore
the `html` library:

```
// html.ud
version: 0.1-pre_mvp;

// Declare the output type for this library.
type Output: Str;

// Converts a string to an html-escaped format, implementation omitted for brevity.
func quote(text: Str) -> Str = { /* ... */ };

// A template allows flexible composition of functions that produce side-effects.
// Here, the `using` syntax is just sugar for binding `children` to a closure 
// argument. The `template` keyword is intended mainly as hint for linters.
template element(tag: Str, attrs: Map<Str> = {}) using children {
  // Begin by writing the opening tag.
  out "<${tag} ";
  
  // Append any attributes to the output.
  for attr, value in attrs {
    out " ${quote(attr)}=${quote(value)}";
  }
  
  // uDLang supports a back-tracking mechanism called a *subjunctive*.
  //
  // Here it is used to cleanly handle the syntax for empty HTML elements.
  // The side effects caused by evaluating `children()` will appear in the position of 
  // the elipsis in the final output.
  suppose (children()) {
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
// standard tags. uDLang supports *partial function application* via the wildcard operator.
export html = element("html", $, $);
export head = element("head", $, $);
export body = element("body", $, $);
export div = element("div", $, $);

// Here we supply an empty block to prevent callers from accidentally
// including children.
export br = element("div", $) {};
// ... remaining elements elided for brevity.

```

## Side Effects ##
 
At this point, you might be asking yourself: if uDLang is
*functional*, then why does this example feel so imperative? And
what's all this noise about *side-effects* in the comments?

The essence of so-called *functional* programming is less about the
use of functions, and more about the *absence of state* and the
careful management of *side-effects*. The design of uDLang simply does
not allow for mutable state to exist within the program.

Side effects are an unavoidable consequence of programming in the real
world. uDLang makes no attempt to hide them: instead, uDLang *models*
side-effects as *data*.  Since these side-effects are, in fact, just
data to be consumed downstream, they cannot influence the behavior of
the uDLang script itself, with one important exception.

This means that all functions in uDLang are pure functions, even those
evaluated purely for the output they produce.

The output of a uDLang program is merely the concatenation of the
side-effects it computes.

For more information, see `manual.md`.

## Language Design Goals ##

 - Make decades of academic research available for practical use.
 - Wrap functional semantics in a "mainstream" syntax.
 - Type-safe handling of strucured data.
 - Abstract away low-level details.
 - Allow for efficient execution via interpretation, ahead-of-time
   compilation, or JIT.
 
## Language Features (Proposed for MVP) ##

 - Curly-brace syntax friendly to a mainstream audience, and compatible with JSON.
 - Destructuring assignments.
 - Partial Evaluation.
 - Expressive type system that models real-world data.
    - Strong typing with generics.
    - HKT (Higher Kinded Types) *Targeted for 0.2 and later*
    - ADTs (via union types)
 - String interpolation.
 - Absolutely no mutable state allowed. Period.
 - Principled system for managing side-effects.
 - Templates.
 - Semantics allowing for deep analysis and optimization.

## Reference Implementation Goals ##

Core

 - small, maintainable library written in entirely in safe Rust.
 - easily embedded in an application context.
 - low interpreter overhead.
 - predictable resource usage.
 - fast interpreter startup time.
 - limited optimizations:
   - inlining
   - const-folding
   - CSE
 - for now, focus on correctness over speed.
  - develop comprehensive test suite.

Command-line tool:

 - targets for the most common use case for uDLang as a unix pipeline filter.
 - can flexibly switch between different encodings and memory management strategies.
 - native support for text formats like JSON, as well as binary formats msgpack.
 - optimized binary framing protocol for efficient operation in "release mode".
 - powerful debugging facilities.

## Project Status ##

The current version is `0.1-pre_mvp`.

uDLang is in its infancy, (i.e. vaporware). If the ideas behind uDLang exite you, contributions are welcome. 
If you don't know where to start, consider submitting test cases or example scripts. How might you use
a tool like uDLang? I'd love to hear from you!

 - Grammar: incomplete
 - Runtime: to be ported from uDashbord
 - Codegen: non-existent.
 - Typechecker: very rough
 - Command line tool: non-existent.
 
