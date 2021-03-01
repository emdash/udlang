# Roadmap #

By milestone.

The current project status is `0.1-pre_mvp`, meaning it has not yet reached MVP status.

Open to the possiblity of embracing either or both native or jit compilation. We'll have to see what
feels like the best fit given how the language ends up getting used.

Project is in its infancy so all of this is subject to change, depending on my time, resources, community involvment, etc.

## 0.1-mvp ##

The main goal here is to get a minimum viable product so that I can evaluate how well uDLang is likely to live up to its stated goals,
and make sure the fundamental design of the language is reasonable.

The implementation of the interpreter will kept intentionally simple,
so that it will be easy to modify as the language evolves. Benchmarks will be written, to establish a performance baseline to compare against
future versions.

- Feature complete enough for the example in the readme
- Naive proof-of-concept implementation.
- Static type checking can be incomplete. Fall back to runtime checks for anything tricky.
- HKTs not required.
- Support mainly JSON. 
- Stretch goal: also support msgpack with a simple binary framing protocol.

## 0.2 ##

The main goal is to get uDLang to a state where work on uDashboard can continue, while cleaning up any egregious mistakes in the language design
or implementation that emerge during development of uDashboard.

- Feature complete enough for uDashboard
- Potentially breaking changes to the language, based on experience using it within uDashboard.
- limited optimizations in the implementation, based on benchmarking real-world code.
- fully static type checking.
  - support real HM-style type inference
  - HKTs.
- comprehensive test suite
- official installation procedure, documented
- packaging for buildroot, NixOS, Debian

## 0.3 ##

By this release, it is hoped that uDLang has found a niche in the wider world outside of uDashboard. Lessons
learned from uDashboard can inform the design of the standard library. Perhaps there will be enough community
interest to justify time-consuming optimizations in the implementation.

- Potentially more breaking changes, based on real-world usage and user feedback
- more optimizations in the implementation
- introduce standard library

## 0.4 - 0.9 ##

If uDLang enjoys sustained interest following 0.3, then users can look forward to:

- stabilization of core language and standard library
- optional build-time support for additonal encodings and framing formats.
- comprehensive set of flags to control every aspect of the runtime.
- increasingly-aggressive optimizations in the implementation.

## 1.0 ##

- First stable release.
- Develop an official specification for the language.
- Experimental support for ahead-of-time compilation, targeting an intermediate format (likely webassembly or LLVM).

## 2.0 ##

- Second stable release.
- May offer a newer, incompatible edition spec for the language (opt-in).
- Stabilized ahead-of-time compilation.
- Experimental native / jit compilation.

## 3.0 ##

- Third stable release
- May offer a newer, incompatible, spec for the language (opt-in).
- May deprecate earlier editions.
- Stabilized native / jit compilation.

## 4.0 and beyond ##

- Maturity
- Long-term stability, performance, and security fixes.
