#! python3

## Python prototype of stack-folding optimizer for "self-optimizing" stack
## code.
##
## Defines a toy stack language, roughly isomorphic to uDLang, including:
##
## - toy instruction set
## - toy VM implementation
## - toy const-fold / inlining optimizer
##
## The goal here is to prove out the concept of optimizing stack-based IR
## through a custom VM.

# Define our instruction set.
#
# Each instruction either an `Instruction` instance, or a function returning an
# `Instruction` instance.
#
# The lambda given to `Instruction` computes the instruction result, where:
# - vm: is a vm instance
# -  v: is an argument vector of apropriate length
#
# The functions called on `vm` by this instruction set constitutes the VM
# interface, so different passes can process the VMs.
class Instruction:

    def __init__(self, name, args, func, *meta):
        self.name = name
        self.args = args
        self.func = func
        self.meta = meta

    def __repr__(self):
        if len(self.meta):
            return "%s(%s)" % (self.name, ", ".join(repr(r) for r in self.meta))
        else:
            return self.name

# Define a variable for each instruction.
const = lambda value: Instruction("const",      0, lambda vm, v: value,                  value)
load  = lambda id:    Instruction("load",       0, lambda vm, v: vm.load(id),               id)
store = lambda id:    Instruction("store",      1, lambda vm, v: vm.store(id, v[0]),        id)
call  = lambda n:     Instruction("call",   n + 1, lambda vm, v: vm.call_always(n, v),       n)
if_   = lambda n:     Instruction("if",     n + 2, lambda vm, v: vm.call_if(n, v),           n)
ife   = lambda n:     Instruction("ifelse", n + 3, lambda vm, v: vm.call_ifelse(n, v),       n)
# each= lambda n:     Instruction("each",   n + 1, lambda vm, v: vm.call_foreach(n, v),      n)
add   =               Instruction("add",        2, lambda vm, v: v[0] + v[1])
sub   =               Instruction("sub",        2, lambda vm, v: v[0] - v[1])
mul   =               Instruction("mul",        2, lambda vm, v: v[0] * v[1])
div   =               Instruction("div",        2, lambda vm, v: v[0] / v[1])
lt    =               Instruction("lt",         2, lambda vm, v: v[0] < v[1])
gte   =               Instruction("gte",        2, lambda vm, v: v[0] >= v[1])
out   =               Instruction("out",        1, lambda vm, v: vm.out(v[0]))
inp   =               Instruction("inp",        0, lambda vm, v: vm.input())


# Represents a function value in our stack language.
#
# - args: [str],         argument names
# - code: [Instruction], function body
# - name: str?,          optional name for recursive functions.
class Fun:
    def __init__(self, args, code, name=None):
        self.name = name
        self.args = args
        self.code = code

    # XXX: it would be better to do some minimal analysis to determine if we
    # are actually recursive.
    def is_recursive(self):
        return self.name is not None

    def __repr__(self):
        args = ", ".join(repr(r) for r in self.args) if self.args else ""
        code = " ".join(repr(r) for r in self.code)
        head = "fn %s" % self.name if self.is_recursive() else "fn"
        params = " (%s) " % args if args else " "
        return head + params + "[%s]" % code


# Recursive chain of local values.
#
# - parent: Scope?, optional parent scope.
class Scope:

    def __init__(self, parent=None):
        self.parent = parent
        self.locals = {}
        self.depth = parent.depth + 1 if parent else 0

    def load(self, name):
        if name in self.locals:
            return self.locals[name]
        else:
            if self.parent:
                return self.parent.load(name)
            else:
                raise ValueError("%s is undefined" % name)

    def store(self, name, value):
        self.locals[name] = value

    def __repr__(self):
        return repr(self.locals)


## Example programs using this instruction set.

# The usual recursive factorial. Demonstrates that recursion doesn't break the
# optimizer.
fact = [
    const(Fun(["n"], [
        load("n"),
        const(2),
        gte,
        const(Fun([], [
            load("n"),
            const(1),
            sub,
            load("fact"),
            call(1),
            load("n"),
            mul
        ])),
        const(Fun([], [const(1)])),
        ife(0)
    ], "fact")), # NOTE: function name provided here to enable recursion.
    # This store will be optimized out entirely.
    store('fact'),

    # This call should optimize out, since arg is const.
    const(10),
    load('fact'),
    call(1),
    out,

    # This call will not be optimized at all, because arg is non-const, and
    # `fact` is recursive.
    inp,
    load('fact'),
    call(1),
    out
]


# Here's example which will benefit from stack-folding.
#
# This sequence of calls is not recursive, so it should always optimize fully.
linear = [
    # define a general linear function
    const(Fun(["slope", "intercept", "x"], [
        load("x"),
        load("slope"),
        mul,
        load("intercept"),
        add
    ])),
    store("linear"),

    # convert celcius to farenheit, using linear
    const(Fun(["t"], [
        const(9.0),
        const(5.0),
        div,
        const(32.0),
        load("t"),
        load("linear"),
        call(3)
    ])),
    store("c2f"),

    # call c2f on a constant. Under optimization, call will be optimized out.
    const(100.0),
    load("c2f"),
    call(1),
    out,

    # call c2f on the input record. Under optimization, call will be inlined
    # here.
    inp,
    load("c2f"),
    call(1),
    out
]


# Here's an example which demonstrates optimized nesting closure calls.
#
# should print A(B(CD))
#
# tree("A", fn {tree("B", fn (){ out "C"; out "D"})})
#
# All of the calls should be optimized out, leading to a linear sequence of out
# instructions.
tree = [
    # define a tree template function
    const(Fun(["name", "contents"], [
        load("name"), out,
        const("("),   out,
        load("contents"), call(0),
        const(")"),   out
    ])),
    store("tree"),

    const("A"),
    const(Fun([], [
        const("B"),
        const(Fun([], [
            const("C"),
            out,
            const("D"),
            out
        ])),
        load("tree"),
        call(2)
    ])),
    load("tree"),
    call(2)
]


# Another tree example
#
# should print A(B(A(CD), B(CD)))
#
# func a (arg) {tree("A", arg)}
# func b (arg) {tree("B", arg)}
# func cd ()   {out "C"; out "D";}
# a(fn { b(cd); a(cd)})
          
tree2 = [
    # define a tree template function
    const(Fun(["name", "contents"], [
        load("name"), out,
        const("("),   out,
        load("contents"), call(0),
        const(")"),   out
    ])),

    store("tree"),

    const(Fun(["arg"], [const("A"), load("arg"), load("tree"), call(2)])),
    store("a"),
    
    const(Fun(["arg"], [const("B"), load("arg"), load("tree"), call(2)])),
    store("b"),

    const(Fun([], [const("C"), out, const("D"), out])),
    store("cd"),

    const(Fun([], [
        const(Fun([], [
            load("cd"),
            load("a"),
            call(1),

            load("cd"),
            load("b"),
            call(1)
        ])),
        load("b"),
        call(1)
    ])),
    load("a"),
    call(1)
]


# Naive stack VM for this instruction set.
#
# This is trivially correct, and used to validate that the optimizer works as
# expected. Also, it is used to execute optimized code.
class VM(object):

    def __init__(self, input_record, parent=None):
        self.locals = Scope(parent)
        self.stack = []
        self.input_record = input_record

    def debug_trace(self, i, code):
        return (
            ("locals", self.locals),
            ("stack", self.stack)
        )

    def trace_start(self, i, code):
        indent = "  " * self.locals.depth
        print(indent, "begin:", code[i])
        for (k, v) in self.debug_trace(i, code):
            print(indent, k + ":", v)

    def trace_end(self, i, code):
        indent = "  " * self.locals.depth
        print(indent, "end:", code[i])
        for (k, v) in self.debug_trace(i, code):
            print(indent, k + ":", v)

    def nested(self, frame):
        return VM(self.input_record, frame)

    def input(self):
        self.push(self.input_record)

    def push(self, value):
        self.stack.append(value)

    def pop(self):
        return self.stack.pop()

    def load(self, name):
        self.push(self.locals.load(name))

    def store(self, name, value, is_arg=False):
        self.locals.store(name, value)

    def get_operands(self, n):
        if n > 0:
            ret = self.stack[-n:]
            self.stack = self.stack[:-n]
            return ret
        else:
            return []

    def out(self, value):
        print("out: ", value)

    def eval(self, code):
        for (i, instruction) in enumerate(code):
            self.trace_start(i, code)
            self.eval_instruction(
                instruction,
                self.get_operands(instruction.args))
            self.trace_end(i, code)
            print()

    def eval_instruction(self, instruction, operands):
        result = instruction.func(self, operands)
        if result is not None:
            self.push(result)

    def call_always(self, n, args):
        self.call(n, args[:-1], args[-1])

    def call_if(self, n, args):
        func = args[-1]
        cond = args[-2]
        if cond:
            self.call(n, args[:-2], func)

    def call_ifelse(self, n, args):
        cond = args[-3]
        func = args[-2] if cond else args[-1]
        self.call(n, args[:-3], func)

    def call(self, n, args, func):
        assert(n == len(args) == len(func.args))
        args = zip(func.args, args)
        scope = Scope(self.locals)

        if func.is_recursive():
            self.store(func.name, func, is_arg=True)

        for (name, value) in args:
            self.store(name, value, is_arg=True)

        nested = self.nested(scope)
        nested.eval(func.code)

        if len(nested.stack) == 0:
            pass
        elif len(nested.stack) == 1:
            self.push(nested.stack[0])
        else:
            raise ValueError("Function returned multiple values")

        return nested


## Optimizer prototype


# A value representing a deferred instruction.
#
# A.k.a. a thunk.
#
# Operations on non-const return these.
class Defer:
    def __init__(self, instruction, operands):
        self.instruction = instruction
        self.operands = operands

    def __repr__(self):
        return "Def(%s, %s)" % (self.instruction, self.operands)


# The optimizer is a specialized stack machine that is aware of Defer values.
#
# Execution happens eagerly until an `inp` or `out` instruction is reached.
# `inp` places `Defer(inp, [])` onto the stack, causing subsequent instructions
# to fold into the Defer.
#
# `out` triggers code generation
# - if operand is a value, then it's lifted to a const() value
# - if operand is Defer, then it's flattened into the output.
#
# For this toy example, only code producing output will be present after
# optimization, and expression depending only on constant values will be
# const-folded.
#
# For the real interpreter, we would also want to emit code for exported
# functions when optimizing a library module. But this exploration is really
# just trying to define the essence of the stack-folding optimizer.
class Optimizer(VM):

    def __init__(self, scope=None):
        # create a vm, but give a thunk for the input record
        VM.__init__(self, Defer(inp, []), scope)
        self.output = []

    # override debug_trace: append our incremental generated code.
    def debug_trace(self, i, code):
        return VM.debug_trace(self, i, code) + (("output", self.output),)

    # override nested: return Optimizer instead of VM.
    def nested(self, scope):
        return Optimizer(scope)

    # override call_if: if cond is deferred, defer the conditional.
    def call_if(self, n, args):
        if isinstance(args[-2], Defer):
            self.push(Defer(if_(n), args))
        else:
            VM.call_if(self, n, args)

    # override call_ifelse: if cond is deferred, defer the conditional.
    def call_ifelse(self, n, args):
        if isinstance(args[-3], Defer):
            self.push(Defer(ife(n), args))
        else:
            VM.call_ifelse(self, n, args)

    # override call:
    # - if func is deferred, defer the call.
    # - if func is recursive, and any arguments are deferred, defer the call.
    def call(self, n, args, func):
        if isinstance(func, Defer):
            # if the function value is not static, we must push a
            # thunk representing the entire call.
            self.push(Defer(call(n), args + [func]))
        elif func.is_recursive() and any(isinstance(arg, Defer) for arg in args):
            self.push(Defer(call(n), args + [func]))
        else:
            # Otherwise we can try to call the function. If any arguments given
            # were thunks, then we'll get a thunk result on the stack when
            # eval_token tries to operate on it.
            nested = VM.call(self, n, args, func)
            self.output.extend(nested.output)

    # override out: generate optimized code to yield the output.
    def out(self, value):
        self.emit(value)
        self.emit(out)

    # override eval_instruction:
    # defer any arithmetic / logic operations on thunks.
    overrides = {"call", "out", "store", "ifelse", "if"}
    def eval_instruction(self, instruction, operands):
        if instruction.name in self.overrides:
            VM.eval_instruction(self, instruction, operands)
        elif any(isinstance(op, Defer) for op in operands):
            # we can't execute, so we push a thunk representing the computation.
            self.push(Defer(instruction, operands))
        else:
            VM.eval_instruction(self, instruction, operands)

    # place optimized instructions into the output.
    def emit(self, value):
        if isinstance(value, Defer):
            # recursively flatten thunks into the output.
            for operand in value.operands:
                self.emit(operand)
            self.output.append(value.instruction)
        elif isinstance(value, Instruction):
            # instructions pass through as is
            self.output.append(value)
        else:
            # Lift constant values to const instructions.
            self.output.append(const(value))


# should print 3628800
vm = VM(10)
vm.eval(fact)

# Prints: 212.0, 32.0
# So far so good.
vm = VM(0.0)
vm.eval(linear)

vm = VM(None)
vm.eval(tree)

opt = Optimizer()
opt.eval(tree)
print("Optimizer output")
for insn in opt.output:
    print(insn)
vm = VM(None)
vm.eval(opt.output)

opt = Optimizer()
opt.eval(tree2)
print("Optimizer output")
for insn in opt.output:
    print(insn)

opt = Optimizer()
opt.eval(linear)
print("Optimizer output")
for insn in opt.output:
    print(insn)
vm = VM(0.0)
vm.eval(opt.output)

opt = Optimizer()
opt.eval(fact)
print("Optimizer output")
for insn in opt.output:
    print(insn)
vm = VM(10)
vm.eval(opt.output)
