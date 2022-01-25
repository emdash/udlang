#! python3

import pprint
import sys

from collections import OrderedDict
from dataclasses import dataclass
from typing import Callable, List

## Toy implementation of a "G-machine", which is the core of most functional
## languages.
##
## Inspired from: https://danilafe.com/blog/05_compiler_execution/


# The G-Machine operates on graphs composed of the following nodes.
@dataclass
class Node:
    pass

@dataclass
class NInt(Node):
    value: int

@dataclass
class NApp(Node):
    f: Node
    x: Node

@dataclass
class NGlobal(Node):
    name: str
    eval: Callable

@dataclass
class NInd(Node):
    follow: Node

@dataclass
class NData(Node):
    constructor: str
    args: List[Node]


@dataclass
class Instruction:
    """In-Memory instruction format."""
    def eval(self, vm): raise NotImplemented


## Class names below below follow the mnemonics in the article.


@dataclass
class I(Instruction):
    n: int
    def eval(self, vm): vm.push_int(self.n)

@dataclass
class G(Instruction):
    f: str
    def eval(self, vm): vm.push_global(self.f)

@dataclass
class A(Instruction):
    def eval(self, vm): vm.mk_app()


builtins = {
    "double": None
}


class GMachine:
    """Implement the G-Machine"""

    def __init__(self, program):
        # The instruction queue.
        self.queue = list(program)

        # The stack is a stack values. In the literature, this is a stack of
        # addresses. In python, all values are references, so these just
        # contain values. We implicitly rely on python's internal heap and
        # reference semantics.
        self.stack = []
        self.map = dict(builtins)

        # Produce some nice debug output
        self.pp = pprint.PrettyPrinter(indent=2)

    def save(self):
        """Return a serializable copy of state for debug output"""
        return OrderedDict([
            ("queue", list(self.queue)),
            ("stack", list(self.stack)),
            ("map",   list(self.map))
        ])

    def run(self):
        """Run the program to completion"""
        while self.queue:
            self.step()

    def step(self):
        """Run a single instruction"""
        before = self.save()
        try:
            self.queue.pop(0).eval(self)
        finally:
            self.pp.pprint(OrderedDict([
                ("before", before),
                ("after", self.save())
            ]))

    def push_int(self, n):
        self.stack.append(NInt(n))

    def push_global(self, f):
        self.stack.append(self.map[f])

    # the literature calls this `Push` even though it feels more like
    # `over` or `dup` to me, coming from forth-like languages.
    def push(self, n):
        self.stack.append(self.stack[n])

    def mk_app(self):
        a0 = self.stack.pop()
        a1 = self.stack.pop()
        self.stack.append(NApp(a0, a1))


# Simple test
GMachine([I(326), G("double"), A()]).run()
