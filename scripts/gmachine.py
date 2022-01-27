#! python3

import json
import sys
import traceback

from collections import OrderedDict
from dataclasses import dataclass, asdict
from typing import Any, Callable, List, Tuple

## Toy implementation of a spineful, tagful "G-machine"
##
## References:
## - https://www.microsoft.com/en-us/research/wp-content/uploads/1992/01/student.pdf
## - https://danilafe.com/blog/05_compiler_execution/
## - Special mention to: https://amelia.how/posts/the-gmachine-in-detail.html
##   The clearest, most concise presentation of these ideas I could find.


@dataclass(frozen=True)
class Ins:
    """Pretty-printable instruction values."""
    name: str
    eval: callable
    meta: Tuple[Any]

    def __str__(self):
        if len(self.meta):
            return "%s(%s)" % (self.name, ", ".join(repr(r) for r in self.meta))
        else:
            return self.name

## Table of Instructions

# Name        immediates         debug name                   operation       debug data
push_value  = lambda v:      Ins("push_value",  lambda vm: vm.push_value(v),  (v,     ))
push_global = lambda g:      Ins("push_global", lambda vm: vm.push_global(g), (g,     ))
push_arg    = lambda n:      Ins("push_arg",    lambda vm: vm.push_arg(n),    (n,     ))
push_local  = lambda n:      Ins("push_local",  lambda vm: vm.push_local(n),  (n,     ))
mk_app      =                Ins("mk_app",      lambda vm: vm.mk_app(),       (      ))
unwind      =                Ins("unwind",      lambda vm: vm.unwind(),       (      ))
update      = lambda n:      Ins("update",      lambda vm: vm.update(n),      (n,    ))
pack        = lambda tag, n: Ins("pack",        lambda vm: vm.pack(tag, n),   (n, tag))
split       =                Ins("split",       lambda vm: vm.split(),        (      ))
match       = lambda cases:  Ins("match",       lambda vm: vm.match(cases),   (      ))
cond        = lambda t, e:   Ins("cond",        lambda vm: vm.cond(t, e),     (      ))
slide       = lambda n:      Ins("slide",       lambda vm: vm.slide(n),       (n,    ))
eval        =                Ins("eval",        lambda vm: vm.eval(),         (      ))
binop       = lambda n, f:   Ins("binop",       lambda vm: vm.binop(f),       (n,    ))
pop         = lambda n:      Ins("pop",         lambda vm: vm.pop(n),         (      ))

# primitive operations on values are a special case.
add = binop("+", lambda x, y: x + y)
sub = binop("-", lambda x, y: x - y)
mul = binop("*", lambda x, y: x * y)
div = binop("/", lambda x, y: x / y)
eq  = binop("=", lambda x, y: x == y)


# Graph ADT on which the GMachine operates
@dataclass(frozen=True)
class Node:
    pass
@dataclass(frozen=True)
class NVal(Node):
    value: Any
    def dump(self): return {"type": "value", "value": self.value}
@dataclass(frozen=True)
class NApp(Node):
    left: int
    right: int
    def dump(self): return {"type": "app", "left": self.left, "right": self.right}
@dataclass(frozen=True)
class NGlobal(Node):
    name: str
    arity: int
    code: Tuple[Ins]
    def dump(self): return {"type": "global", "name": self.name}
@dataclass(frozen=True)
class NInd(Node):
    next: int
    def dump(self): return {"type": "ind", "next": self.next}
@dataclass(frozen=True)
class NData(Node):
    tag: str
    data: Tuple[int]
    def dump(self): return {"type": "data", "tag": self.tag, "data": self.data}


class FakeHeap:

    """Simulates dynamic memory management.

    We don't need to manage memory in python, but the G-Machine is an
    intermediate representation intended for native compilation, and
    it explicitly requires a heap. In particular, the nodees in the
    ADT defined above actually store integer "pointers" to their child
    nodes, which values are in fact indices into this fake heap.

    We use id() to generate the initial "address" for a node: however,
    this is *not* an invariant!

    Instead, we simulate "in place" updates of memory cells.

    We collect data on how many updates are performed, and we can also
    easily tell which addresses have been updated, by whether or not
    the id of the value matches the address.

    """

    def __init__(self):
        self.forward = {}
        self.reverse = {}
        self.updates = 0

    def alloc(self, item):
        if item not in self.reverse:
            addr = id(item)
            self.forward[addr] = item
            self.reverse[item] = addr
        return self.reverse[item]

    def free(self, item):
        if item not in reverse:
            raise ValueError("Double Free")
        addr = self.reverse.pop(item)
        forward.pop(addr)

    def find(self, item):
        return self.reverse[item]

    def insert(self, item):
        assert isinstance(item, Node)
        if item not in self.reverse:
            return self.alloc(item)
        else:
            return self.find(item)

    def __contains__(self, item):
        return item in self.reverse

    def __setitem__(self, addr, value):
        if addr != id(value):
            self.updates += 1
        self.reverse.pop(value)
        self.reverse[value] = addr
        self.forward[addr] = value

    def __getitem__(self, addr):
        return self.forward[addr]

    def __iter__(self):
        return iter(self.forward.items())

    def updated(self):
        return {
            v for v in self.forward.iteritems()
            if k != id(v)
        }

    def dump(self):
        return {k: v.dump() for k, v in self}


class Halt(BaseException):
    pass


class GMachine:
    """Simulates the abstract machine"""

    def __init__(self, globals, main):
        # The instruction stream
        self.queue = list(main)
        self.stack = []

        # The "heap" is where our values live.
        #
        # Python has reference semantics, so at first I thought we didn't need
        # this. But it turns out that we need at least one level of indirection
        # to allow "updating" nodes.
        self.heap = FakeHeap()

        # This is the kindof like the call stack, but not exactly,
        # because the G-Machine is a lazy evaluator.
        self.dump = []

        # We need to "allocate" the globals on the heap (really we are
        # just assigning addresses to them).
        self.map = {
            g.name: self.heap.insert(g)
            for g in globals
        }

        # Some extra state for debug output
        self.exc = None
        self.instruction = None

    ## Machinery for the VM

    def debug_state(self, remark, **extra):
        """Return a json-serializable copy of state for debug output"""
        json.dump(OrderedDict((
            ("remark", remark),
            ("exception", traceback.format_exc() if self.exc else None),
            ("instruction",  str(self.instruction)),
            ("queue", list([str(i) for i in self.queue])),
            ("stack", self.stack),
            ("heap",  self.heap.dump()),
            ("map",   self.map),
            ("dump",  [(map(str, s), map(str, q)) for (s, q) in self.dump])
        )), sys.stdout)
        print()

    def run(self):
        """Run the program to completion"""
        try:
            self.debug_state("init")
            while self.queue:
                self.step()
        except Halt as e:
            self.debug_state("halt")
            return e.args[0]
        except BaseException as e:
            self.exc = e
            self.debug_state("exception")

    def step(self):
        """Run a single instruction"""
        self.instruction = self.queue.pop(0)
        self.instruction.eval(self)
        self.debug_state("step(%s)" % self.instruction)

    def call(self, instructions, args):
        self.dump.append((self.stack, self.queue))
        self.stack = args
        self.queue = list(instructions)
        self.debug_state("call")

    def ret(self):
        assert(self.dump)
        (self.stack, self.queue) = self.dump.pop()
        self.debug_state("ret")

    ## Plain stack operations

    def pop(self, n):
        ret = self.stack[:n]
        # keep the existing stack, otherwise I think it breaks the
        # dump?
        for i in range(n):
            self.stack.pop(0)
        return ret

    def push(self, *addrs):
        for a in reversed(addrs):
            self.stack.insert(0, a)

    ## Stack operations with heap indirection

    def peek(self, n):
        return self.heap[self.stack[n]]

    def poke(self, n, value):
        self.stack[n] = self.heap.insert(value)

    def top(self):
        return self.peek(0)

    ## Instruction Implementations

    def push_value(self, v):
        self.push(self.heap.insert(NVal(v)))

    def push_global(self, g):
        self.push(self.map[g])

    def push_local(self, n):
        self.push(self.stack[n])

    def push_arg(self, n):
        val = self.peek(n)
        assert(isinstance(val, NApp))
        self.push(val.right)

    def mk_app(self):
        [x, f] = self.stack[:2]
        self.push(self.heap.insert(NApp(f, x)))

    # This, and the following four methods, together implement unwind.
    # unwind is the top-level dispatch table, with a case for each of
    # the "unwindable" nodes.
    def unwind(self):
        top = self.top()
        if   isinstance(top, NVal):    self.unwind_val(top)
        elif isinstance(top, NApp):    self.unwind_app(top)
        elif isinstance(top, NGlobal): self.unwind_global(top)
        elif isinstance(top, NInd):    self.unwind_ind(top)
        else: raise ValueError("%r is not unwindable" % top)
    def unwind_app(self, top):
        self.push(top.left)
    def unwind_global(self, top):
        assert(top.arity >= 0)
        assert(len(self.stack) > top.arity)
        for i in range(1, top.arity + 1):
            self.stack[i] = self.peek(i).right
        self.queue = list(top.code)
    def unwind_val(self, top):
        if self.dump:
            self.ret()
            self.push(self.heap.insert(top))
        else:
            # unwind val with a non-empty dump means we have finished
            raise Halt(top)
    def unwind_ind(self, top):
        assert(isinstance(self.top(), NInd))
        # can't use poke here, top.next is an address
        self.stack[0] = self.top().next

    def update(self, n):
        # don't use peek here, n.next is an address
        self.poke(n, NInd(self.stack[n]))

    def pack(self, tag, n):
        self.push(NData(tag, self.stack.pop(n)))

    def split(self):
        top = self.heap[self.pop()]
        assert isinstance(NData, top)
        self.push(*top.data)

    def match(self, branches):
        top = self.heap[self.pop()]
        assert isinstance(top, NData)
        self.call(branches[top.tag], [top])

    def cond(self, then, else_):
        top = self.heap[self.pop()]
        assert isinstance(top, NValue)
        assert isinstance(top.value, bool)
        code = then if top.value else else_
        self.queue = list(code) + self.queue

    def slide(self, n):
        top = self.pop(1)
        self.pop(n)
        self.push(top)

    # this could be renamed "intrinsic", or "primative", if we want to
    # generalize beyond binary operations.
    def binop(self, op):
        a0 = self.stack.pop(0)
        a1 = self.stack.pop(0)
        self.stack.insert(0, NInt(op(a0.value, a1.value)))

    def eval(self):
        top = self.stack.pop(0)
        self.call([unwind], [top])


# Factorial(10)
GMachine([
    NGlobal("fac", 1, (
        push_arg(0),
        eval,
        push_local(0),
        push_value(0),
        eq,
        cond(
            (push_value(1), slide(3), unwind),
            ()
        ),
        push_global("fac"),
        push_local(1),
        push_value(1),
        sub,
        mk_app,
        push_local(1),
        mul,
        slide(2),
        unwind
    ))
], [
    push_global("fac"),
    push_value("10"),
    mk_app,
    slide(1),
    unwind,
]).run()