# Roadmap #

By milestone.

The current project status is `0.1-pre_mvp`, meaning it has not yet reached MVP status.

## 0.1-mvp ##

- Feature complete enough for the example in the readme
- Naive proof-of-concept implementation.
- Static type checking can be incomplete. Fall back to runtime checks for anything tricky.
- HKTs not required.
- Support mainly JSON. 
- Stretch goal: also support msgpack with a simple binary framing protocol.

## 0.2 ##

- Feature complete enough for uDashboard
- Potentially breaking changes to the language, based on experience using it within uDashboard.
- limited optimizations in the implementation, based on benchmarking real-world code.
- fully static type checking.
  - support real HM-style type inference
- comprehensive test suite
- official installation procedure, documented
- packaging for buildroot, NixOS, Debian

## 0.3 ##

- Potentially more breaking changes, based on real-world usage and user feedback
- more optimizations in the implementation
- introduce standard library

## 0.4 - 0.9 ##

- Begin stabilization of core language and standard library
- add optional build-time support for additonal encodings and framing formats.
- add comprehensive flags for controlling every aspect of the runtime.
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
