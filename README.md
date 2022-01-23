# uDLang #

uDLang is a type-safe, pure-functional alternative to `awk`, which processes structured data on the command line over unix pipes.

The core uDLang language is a minimalist, pure language with JS-like syntax. 

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

On the command line, uDLang is a strongly-typed alternative to stream
processing tools like `jq`, `awk` or `sed`.

uDLang started as a DSL for [uDash](https://github.com/emdash/udashboard).

### Why the *name* uDLang?

The name is short for uDashboard Language. It's a "working title", and
I'm open to changing it. Or maybe it stands for "universal data language".

### Overview

uDLang is:
- a pipeline filter.
- record-oriented.
- purely functional.
- statically typed.

Planned Features:
- bidirectional type inference
- structural types
- pattern matching and destructuring
- partial evaluation.

uDLang operates on binary data.
- The input is a stream of binary records (in msgpack)
- uDLang defines a native protocol for record segmentation.
- uDLang values are a superset of msgpack values.
- udlang will ship with a suite of companion tools for working with this format.

uDLang's syntax resembles that of [Koka](https://koka-lang.github.io/koka/doc/index.html)

**uDLang is a work in progress, not all features documented here are
implemented**.

### What uDLang is not

- a general-purpose language.
- a systems language.
- fully implemented.

### Sources of Inspiration

- FlowJS, for the syntax of type annotations.
- OilShell, another project bringing a modern approach to a traditional domain.
- ML-family languages.
- JavaScript, and its data-oriented subset, JSON.
- JSON Schema
- CDL, the most minimal approach to language design I have yet
  encountered, and well worth pondering.
- The Elm project.
- Rust, the implementation language.
- [Koka](https://koka-lang.github.io/koka/doc/index.html) -- a case of
  convergent evolution. Koka syntax is similar to uDLang, despite
  being developed independently.

## Project Status and Roadmap ##

The current version is `0.1-pre_mvp`.

 - Grammar: about 95% done.
 - Runtime: about 80% done.
 - Typechecker: TBD.
 - msgpack support: TBD
 - Exceptions: TBD

The goal for the MVP milestone is that uDLang should be minimally
useful for casual coding.

Subsequent releases will introduce optimizations and convenience features.

uDLang is in its infancy. If the ideas behind uDLang exite you,
contributions are welcome.

How might you use a tool like uDLang? I'd love to hear from you!

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
input  "Hello" | "Goodnight";
output Str;

// uDLang supports pattern matching 
out match in {
  case "Hello":     "Hello, World!\n";
  case "Goodnight": "Goodnight, Moon!\n";
};

```

### Processing Structured Data

This example converts an array of point-like records to a record of
arrays, i.e this `[{x: 0, y: 1}, {x: -1, y: 7}]`, becomes `{x: [0.0,
-1.1], y: [1.0, 7.0], z:[0.0, 0.0]}`.

```
version 0.1-pre_mvp;
script "Convert a list of points to a record of parallel arrays";

// A point is a map with numeric keys
type Point: {
  x: I32 | F32;
  y: I32 | F32;
  z: I32 | F32;
};

// Input is a list of Points.
input [Point];

// Output is a record with parallel arrays.
output {
  x: [F32];
  y: [F32];
  z: [F32];
};

// Construct output record and write it to the output.
out {
  // $.x is a partial function, short-hand for: `(p) => {p.x}`
  x: in.map($.x as F32), 
  y: in.map($.y as F32),
  z: in.map($.z as F32)
};

```

### HTML Templating ###

Let's imagine that we are writing a todo-list web-application. As part
of this, we want to convert a JSON payload received from a web service
into legible HTML.

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

A uDLang script to to render the todo list might look like this:

```
// todo.ud

// uDLang is evolving, by mandating a version string, we guard our code
// against breaking changes to the language.
version 0.1-pre_mvp;
script "Todolist Example";

// Import some helper functions from our html companion library (see below),
// including the library itself.
import html.{_, html, head, title, body, h1, h2, ul, li, div, quote};

// Define a type alias for a single todo-list item.
// Type names must start with an upper-case letter.
type TodoItem: {
  id: U32,
  name: Str,
  // uDLang supports "string enums" similar to FLowJS.
  status: "complete" |  "incomplete" | "blocked";
  blocker: Int?;
};

let alert = div({class: "alert"}, content = "Blocked on: " + $);

// We can define methods on record types with `impl`.
impl TodoItem {
   func format() -> Str {
     // uDLang supports string interpolation.
     li({class: "todo-item ${self.status}"}) {
       quote(self.name) + "" + match (self.blocker) {
         case None:          "";
         case Some(blocker): alert(items[blocker].name)
        };
      }
    }
  }
};

// Declare the shape of the input.
input {field name: Str; field items: [TodoItem]};

// Declare the output type, which is defined by the HTML helper library.
output html.Output;

// Format the entire document. As with many scripting languages, there is no
// specific program entry point, such as a function named `main`.
//
// All allusions to HTML syntax here (`head`, `body`, etc) are uDLang
// functions living in the helper library.
//
// uDLang supports a "template" syntax for calling functions with trailing 
// block, a feature borrowed from Ruby.
out html() {
   head() {
     title() {in.name}
   } + body() {
     h1() {text(in.name)} +
     div({class: "todo-list"}) {
       ul() {
         in.items.map($.format()).join("")
       }
     }
   }
}
```

Let's run this example:

 `$ udlift --json --oneshot | udlang todo.md | udlower --text | tidy < example.json`
 
Where
- `udlift --json --oneshot` reads from stdin, outputs the contents as single record, and exits.
- `udlower --text` outputs a plain text string followed by a newline for each msgpack string it receives.

We obtain the following output:

**TBD**

Now let's try it again on some invalid input:


`bad.json`
```
{
  "name": "Brandon's Tasks"
  "items": [
    {"id": -1, "name": "I am malformed"}
  ]
}
```

The input record is invalid, because the id field is signed.

 **TBD**
 
The udlang interpreter rejects the input stream, because it has the wrong shape.

`udlift` attempts to deduce the shape of the input, but you can also specify an explicit schema.

#### The HTML Library ####

The `html` module is just a library written in udlang:

```
// html.ud
version 0.1-pre_mvp;
lib "Simple Html Formatting Library";

// Declare the output type for this library.
type Output: Str;

// Converts a string to an html-escaped format, implementation omitted for 
// brevity.
func quote(text: Str) -> Str = { /* ... */ };

// This is a generic function which formats a single element.
//
// Child elements are provided by the `content` function argument.
func element(
  tag: Str, 
  attrs: Map<Str> = {},
  allowChildren: Bool = true
  content: () -> Str = () => "",
) -> Output {
  // Begin by writing the opening tag.
  let attributes = attrs
     .map((k, v) => quote(k) + "=" + quote(v)})
     .join(", ");
  
  let opening = ["<", tag, " ", attrs, ">"].join("");
  let closing = ["</", tag, ">"].join("");
  
  if (allowChildren) {
    [opening, content(), close].join("")
  } else match (content()) {
    case "": opening,
    case x:  throw "Content must be empty, 
  }
};

// uDLang requires libraries to explicitly export definitions.
export Output;
export element;

// With this general definition of an HTML element in hand, we can specialize it
// for standard tags. `$` is the "placeholder" for partial application.
export html = element("html", $) $;
export head = element("head", $) $;
export body = element("body", $) $;
export div  = element("div",  $) $;

// These elements cannot contain content.
export br   = element("br", $, false);
export hr   = element("hr", $, false);
// ... remaining elements elided for brevity.
```

## uDLang and Koka

I became aware of Koka after starting uDLang. The a language is
superficially similar to uDLang. I am not terribly familiar with
Koka, but here's a summary of the obvious differences.

### Effects

Koka's effect system is more expressive than uDLang's
- uDLang has one effect: the `out` statement, which has a very specific meaning.
- uDLang effects cannot be "handled" within the uDLang script: script evaluation terminates when
  execution reaches an `out` statement.
- Koka implements exceptions through its effect system, while uDLang
  will implement exceptions as a core language feature.
- Koka appears to be a general-purpose applications or even systems
  language that targets native compilation, while uDLang is a pipeline filter.

### Trailing Lambdas

Both Koka and uDLang support "trailing lambdas" on function calls.
  
### Dot Notation

Koka defines method call expressions like `x.f(...)`  to mean: `f(x,
...)`, while udlang defines it to mean: `x['f](...)`.
  
uDLang aims to guarantee that this will optimize to a direct function
call at runtime.

In Koka, it's not clear you can refer to a method's function value
directly (without calling it), while in uDLang you can.

### Record Processing

- uDLang is record-oriented, while Koka has no such notion.
- uDLang scripts are kernels operating on a stream of records.
- the script runs to completion for each input record received
  - the `in` keyword, binds to the current input record
  - the `out` keyword yields the given expression and ends the cycle.

### Other features

Koka uses automatic semicolon insertion similar to EcmaScript,
whereas uDLang has no plans to implement this feature.
Explicit semicolons are considered a feature.

uDLang supports partial evaluation via the `$` operator.
