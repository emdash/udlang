# uDLang #

uDLang is a type-safe, pure-functional stream processor for structured binary data.

It's a bit like `awk`, but modern, type-safe, and pure.

Syntax is inspired by JS, Flow, and Rust.

## Example

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

uDLang model:
- The input is a stream of binary records (in msgpack)
- The script is a kernel which is run to completion on each record
- uDLang defines a native protocol for binary framing over pipes.
- uDLang values are a superset of msgpack values.

uDlang companion tools (planned):
- `udlift` and `udlower` for converting between native udlang and various other formats.
- `udmonad` driver for stateful udlang

uDLang's bears superficial resemblence to [Koka](https://koka-lang.github.io/koka/doc/index.html)

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

Subsequent releases will focus on ergonomics and optimizations.

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

### Normalizing Structured Data

This example converts an array of records to a record of arrays (normalizing the output as float):
 - `[{x: 0, y: 1, z: 2}, {x: -1, y: 7, z: 3}]`, becomes
 -  `{x: [0.0, -1.1], y: [1.0, 7.0], z:[2.0, 3.0]}`.

```
version 0.1-pre_mvp;
script "Normalize list points to parallel coordinate arrays";

// We can define type aliases, like so...
type Point: {
  x: I32 | F32;
  y: I32 | F32;
  z: I32 | F32;
};

// The input is an array of Point.
input [Point];

// The output is a record with parallel arrays.
output {
  x: [F32];
  y: [F32];
  z: [F32];
};

// Define a helper used below.
func helper(item) {item.z}

// Construct and emit the output.
out {
  // udlang relies on comprehensions and recursion.
  // The following array comprehensions are equivalent.
  x: [in | (item) => item.x as F32], // Explicit lambda
  y: [in | $.y as F32],              // Partial expression
  z: [in | helper],                  // Bound function value.
};

```

### HTML Templating ###

Let's imagine that we are writing a todo-list web-application. 
As part of this, we want to convert a JSON into legible HTML.

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
import html.{_, html, head, title, body, h1, h2, ul, li, div};

// Define a type alias for a single todo-list item.
// Type names must start with an upper-case letter.
type TodoItem: {
  id:       U32,
  name:     Str,
  // uDLang supports "string enums", like FlowJS.
  status:   "complete" |  "incomplete" | "blocked";
  // blocker is a field which, if present, is a U32.
  blocker?: U32;
  // If we wanted blocker to be nullable instead, we'd use
  // blocker: U32?;
  //
  // The distinction is that in the former, the field can be absent,
  // while in the latter, the slot must be present but the value can be `null`.
  //
  // Within the script, they both appear as Option<U32>
};

// type of `blocked` here is `(String) -> html.Element`. Note the wildcard.
let blocked = span({class: "alert"}, "Blocked on: " + items[$].name);

// A free function which formats a TodoItem as an html.Element.
func format(self: TodoItem) -> html.Element {
  let attrs = {class: ["todo-item", self.status].join(" ")};
  match (self.blocker) {
     case None:        li(attrs, self.name);
     case Some(index): li(attrs, self.name, blocked(index));
  }
}

// Declare the shape of the input.
input {
  name: Str, 
  items: [TodoItem]
};

// Declare that the output is plain text.
output String;

// Format the document and output it.
//
// All allusions to HTML syntax here (`head`, `body`, etc) are plain
// functions living in a helper library.
out html.format(html({},
   head({}, title({}, in.name)),
   body({},
     h1({}, in.name),
     div({class: "todo-list"},
       // uDLang supports JS-like spread syntax in function calls.
       ul({}, ...[in.items | format])
     )
   )
));
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

 **TBD**
 
In this example, the input record is invalid, because the id field is signed.  
uDlang refuses to process the input stream, because it has the wrong shape.
uDLang is type-safe by default. 

`udlift` attempts to deduce the shape of the input automatically. Use:
- `--schema <schema_file>` to specify an explicit schema
- `--reject` to cause `udlift` to abort when it receives invalid input
- `--ignore` to cause `udlift` to silently drop invalid input
- `--warn` to cause `udlift` to drop invalid input, but warn noisily about it on stderr.

#### The HTML Library ####

The `html` module is just a library written in udlang:

```
// html.ud
version 0.1-pre_mvp;
lib "Simple Html Formatting Library";

// Declare the output type for this library.
export type Output: Str;

// Define an HTML Element ADT
export type Element: {
   tag: String,
   attrs: {[String]: String},
   children: [Element | String],
   requiresClose: Bool
};

// Converts a string to an HTML-escaped string.
func escape(text: Str) -> Str = { /* ... */ };
// Converts a string to an html-quoted string, implementation omitted for 
// brevity.
func quote(text: Str) -> Str = { /* ... */ };
               
// Helper function for constructing elements.
export func element(
  tag: Str, 
  attrs: Map<Str>,
  allowChildren: Bool,
  // uDLang supports JS-like "rest" parameters.
  ...children: Element | String,
) -> Element ! Str {
  let requiresClose = if (allowChildren) {
    true
  } elif (content.length > 0) {
    throw tag + " tags should not contain children!"
  } else {
    false
  };

  {tag, attrs, children, requiresClose}
}

// Format the HTML Element tree
export func format(element: Element) -> String ! String {
  let {tag, attrs, children, requiresClose} = element;  
  let attributes = [attrs | escape($) + "=" + quote($)].join(" ")  
  let open_tag   = ["<", tag, " ", attrs, ">"].join("");
  if (requiresClose) {
    let close_tag = ["</", tag, ">"].join("");
    let content = [children | match ($) {
       case Element as e: format(e);
       case String  as s: escape(s);
    }];
    open_tag + content + close_tag
  } else match (content) {
    case None: open_tag;
    case _:    throw tag + " tags should not contain content!";
  }
};

// With the general definition of an HTML element in hand, we can specialize it
// for standard tags using `$`, the "placeholder" for partial application.
//
// each $ is a distinct argument.
// $... captures and spreads "rest" arguments. 
// it may only appear when the final argument is a "rest" parameter.
export html = element("html", $, true, $...));
export head = element("head", $, true, $...);
export body = element("body", $, true, $...);
export div  = element("div",  $, true, $...);
// ... etc

// These elements cannot contain content.
export br   = element("br", $, false);
export hr   = element("hr", $, false);
// ... etc

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
...)`.

uDLang currently defines it to mean: `x["f"](...)`, but this may be about to change.
  
Either way, uDLang aims to guarantee method syntax will be optimized as much as possible.

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
