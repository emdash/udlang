// (C) 2021 Brandon Lewis

use crate::ast::{
    BinOp,
    UnOp,
};

use crate::ir::{
    Addr,
    Atom,
    // AList,
    CallType,
    Executable,
    IndexType,
    Instruction,
    IR,
    Block,
    // Map,
    Operations,
    Shared,
    Seq,
    TrapType,
    TypeTag,
    Value,
    compile,
};


use std::{
    collections::HashMap,
    fmt::Debug
};


/* Error Handling ************************************************************/


// Type for VM runtime errors.
//
// There might be a case for unifying some the compile / runtime
// errors, since it might impact stack-folding down the road.
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    StackUnderflow,
    StackOverflow,
    IllegalAddr(String),
    TypeError(String),
    NotImplemented(String),
    Trap(TrapType, Value),
    Undefined(String),
    Internal(String),
}


pub type Result<T> = std::result::Result<T, Error>;


// Provide some convenience functions returning error types under the
// Error namespace.
//
// In particular, these all return a Result<T>, which means they can
// be returned directly from `compile_*` methods in the VM.
//
// Note that even though these functions are generic over <T>, they
// never actually construct or return a T.
impl Error {
    pub fn stack_underflow<T>() -> Result<T> {
	Err(Self::StackUnderflow)
    }

    pub fn stack_overflow<T>() -> Result<T> {
	Err(Self::StackUnderflow)
    }

    pub fn illegal_addr<T>(addr: usize) -> Result<T> {
	Err(Self::IllegalAddr(format!("Illegal Address: {:?}", addr)))
    }

    pub fn not_callable<T>(value: &Value) -> Result<T> {
	Err(Self::TypeError(format!("{:?} is not callable", value)))
    }

    pub fn type_error<T: Debug, U: Debug, V>(expected: T, got: U) -> Result<V> {
	Err(Self::TypeError(format!("Expected {:?}, got {:?}", expected, got)))
    }

    pub fn not_implemented<T>(feature: &str) -> Result<T> {
	Err(Self::NotImplemented(format!("{:?} is not implemented", feature)))
    }

    pub fn undefined<T: Debug, U>(id: T) -> Result<U> {
	Err(Self::Undefined(format!("{:?} is not defined in the current scope", id)))
    }

    pub fn trap<T>(tt: TrapType, value: Value) -> Result<T> {
	Err(Self::Trap(tt, value))
    }

    pub fn internal<T>(mumbo_jumbo: &str) -> Result<T> {
	Err(Self::Internal(mumbo_jumbo.to_string()))
    }
}


/* Public API ****************************************************************/


// Conveneince function to compile and run the script at the given path.
pub fn run<S> (path: &str, input: S) -> Result<()>
where S: Iterator<Item = Value> {
    let program = match compile(path) {
	IR::Executable(e) => e,
	IR::Module(_) => panic!("You cannot execute a library as a script!")
    };

    VM::new(program).run(input)
}


/* Stack *********************************************************************/


// Handles the data required to execute a function.
#[derive(Clone, Debug)]
struct StackFrame {
    pub block: Shared<Block>,
    pub values: Seq<Value>,
    pub locals: HashMap<Atom, Value>,
    pub returns_value: bool
}


// Factor out the stack logic from the virtual machine.
//
// Stack becomes more complex because of the need to index function
// arguments, local variables, and captures.
//
// Within a block, values are processed on a stack. This stack lives
// inside a stack frame. When a function is called, we push a new
// stack frame, and when a function returns, we pop the stack frame.
//
// This is a pretty naive implementation, lots of room for improvement
// I'm sure.
#[derive(Clone, Debug)]
struct Stack {
    empty: Shared<Seq<Value>>,
    frames: Vec<StackFrame>
}


impl Stack {
    pub fn new() -> Stack {
	Stack {
	    empty: Shared::new(Vec::new()),
	    frames: Vec::new()
	}
    }

    pub fn debug_dump(&self) {
	eprintln!("Stack:");
	for (i, frame) in self.frames.iter().enumerate() {
	    eprintln!("-- Frame {} Values --\n", i);
	    for (i, val) in frame.values.iter().enumerate() {
		eprintln!("{} {:?}", i, val);
	    }
	    /*
	    eprintln!("-- Frame {} Captures --\n", i);
	    for (i, val) in frame.captures.iter().enumerate() {
		eprintln!("{} {:?}", i, val);
	    }*/
	}
    }

    pub fn push_entrypoint_frame(
	&mut self,
	block: Shared<Block>
    ) -> Result<()> {
	if block.rets != 0 {
	    Error::not_implemented("Return from top level")
	} else if block.args.len() != 0 {
	    Error::not_implemented("top-level block requiring arguments")
	} else {
	    self.frames.push(StackFrame {
		block: block,
		values: Vec::new(),
		locals: HashMap::new(),
		returns_value: false
	    });
	    Ok(())
	}
    }
	    
    
    // Push a new stack frame for the call that is about to occur.
    pub fn push_call_frame(
	&mut self,
	block: Shared<Block>
    ) -> Result<()> {
	if block.rets > 1 {
	    Error::not_implemented("Multiple Return Values")
	} else {
	    let values = Vec::new();
	    let returns_value = block.rets == 1;
	    let top = self.top_frame_mut()?;
	    let start = top.values.len() - block.args.len();
	    // Collect arguments off the current value stack frame,
	    // and place them into the locals of the callee's stack
	    // frame.
	    let locals = block.args
		.iter()
		.cloned()
		.zip(top.values.split_off(start))
		.collect();
	    // Push the new stack frame.
	    self.frames.push(StackFrame {block, values, locals, returns_value});
	    Ok(())
	}
    }

    pub fn pop_frame(&mut self) -> Result<Option<Value>> {
	if let Some(mut frame) = self.frames.pop() {
	    if frame.returns_value {
		Ok(Some(frame.values.pop().expect("Unpossible!")))
	    } else {
		Ok(None)
	    }
	} else {
	    Error::stack_underflow()
	}
    }

    pub fn top(&self) -> usize {
	self.frames.len() - 1
    }

    pub fn top_frame(&self) -> Result<&StackFrame> {
	let top = self.top();
	Ok(&self.frames[top])
    }

    pub fn top_frame_mut(&mut self) -> Result<&mut StackFrame> {
	let top = self.top();
	Ok(&mut self.frames[top])
    }

    pub fn push(&mut self, value: Value) -> Result<()> {
	let frame = self.top_frame_mut()?;
	frame.values.push(value);
	Ok(())
    }

    pub fn pop(&mut self) -> Result<Value> {
	let frame = self.top_frame_mut()?;
	if let Some(value) = frame.values.pop() {
	    Ok(value)
	} else {
	    Error::stack_underflow()
	}			
    }

    // Try to inspect the top of stack.
    fn peek(&self, rel: u8) -> Result<Value> {
	let frame = self.top_frame()?;
	let index = frame.values.len() - rel as usize - 1;
	if frame.values.len() > 0 {
	    Ok(frame.values[index].clone())
	} else {
	    Error::stack_underflow()
	}
    }

    // Load the given local onto the stack.
    pub fn load(&mut self, atom: &Atom) -> Result<()> {
	let value = self.get_local(atom)?;
	self.push(value)
    }

    // Find the given local somewhere in the call stack.
    fn get_local(&mut self, atom: &Atom) -> Result<Value> {
	for frame in self.frames.iter().rev() {
	    if let Some(value) = frame.locals.get(atom) {
		return Ok(value.clone());
	    }
	}
	Error::undefined(atom)
    }

    // Store the given local to the call stack.
    fn store(&mut self, atom: &Atom) -> Result<()> {
	let value = self.pop()?;
	let frame = self.top_frame_mut()?;
	frame.locals.insert(atom.clone(), value);
	Ok(())
    }
}


/* VM ************************************************************************/


// Specialize the Operations trait for our particular result type.
struct VMOps;
impl Operations for VMOps {
    type Result = Result<Value>;

    fn invalid_operand(value: &Value) -> Self::Result {
	Err(Error::TypeError(format!(
	    "Operation is not implemented for {:?}",
	    value,
	)))
    }

    fn invalid_operands(l: &Value, r: &Value) -> Self::Result {
	Err(Error::TypeError(format!(
	    "Operation is not implemented for {:?} and {:?}",
	    l,
	    r
	)))
    }

    fn invalid_cast(value: &Value, tt: TypeTag) -> Self::Result {
	Err(Error::TypeError(format!(
		"{:?} cannot be coerced into {:?}", value, tt
	)))
    }

    fn index_error(collection: &Value, index: &Value) -> Self::Result {
	Err(Error::TypeError(format!(
	    "{:?} has no element {:?}", collection, index
	)))
    }
    
    fn type_mismatch(value: &Value, tt: TypeTag) -> Self::Result {
	Err(Error::TypeError(format!(
	    "Expected {:?}, but got {:?}", value, tt
	)))
    }

    fn ok(value: Value) -> Self::Result {
	Ok(value)
    }

    fn new<T>(value: T) -> Shared<T> {
	Shared::new(value)
    }
}

    
// Holds the state we need to execute instructions sequentially.
pub struct VM {
    script: Executable,
    stack: Stack,
    input: Value,
}


// This is a naive stack-based interpreter which directly executes the
// IR in the `ir` module.
//
// This implemntation tries to KISS as far as possible.
impl VM {
    // Allocate and initialize a new virtual machine
    pub fn new(script: Executable) -> VM {
	let stack = Stack::new();
	let input = Value::None;
	VM {script, stack, input}
    }

    // Run the given script until the input iterator is exhausted.
    pub fn run<S: Iterator<Item = Value>>(
	&mut self,
	input: S,
    ) -> Result<()> {
	eprintln!("Run");
	self.init()?;
	Ok(for value in input {
	    self.main(value)?;
	})
    }

    // Execute the init block in the script.
    pub fn init(&mut self) -> Result<()> {
	eprintln!("Init");
	let block = Shared::new(self.script.code[0].clone());
	self.stack.push_entrypoint_frame(block.clone())?;
	self.exec_block(&block.code)?;
	// We DON'T want to pop this frame, as main executes as a
	// closure inside it.
	Ok(())
    }

    // Execute the main block in the script.
    pub fn main(&mut self, input: Value) -> Result<()> {
	eprintln!("Main");
	// XXX: It really bothers me that we can't just borrow this.
	let block = Shared::new(self.script.code[1].clone());
	self.stack.push_call_frame(block.clone())?;
	self.input = input;
	self.exec_block(&block.code)?;
	self.stack.pop_frame()?;
	Ok(())
    }

    // Execute each instruction in the given block.
    fn exec_block(&mut self, block: &[Instruction]) -> Result<()> {
	for insn in block {
	    self.exec_insn(insn)?;
	}
	Ok(())
    }
    
    // Dispatch over all instructions.
    fn exec_insn(&mut self, insn: &Instruction) -> Result<()> {
	use Instruction::*;
	eprintln!("\n\nExec: {:?}", insn);
	match insn {
	    Const(addr)       => self.load_const(*addr),
	    Load(atom)        => self.load_arg(atom),
	    Store(atom)       => self.store(atom),
	    Un(opcode)        => self.unop(*opcode),
	    Bin(opcode)       => self.binop(*opcode),
	    Call(ct)          => self.call(*ct),	    
	    In                => self.stack.push(self.input.clone()),

	    Out               => {eprintln!("{:?}", self.stack.pop()?); Ok(())},
	     /* XXX: should print to stderr */
	    Debug             => {eprintln!("{:#?}", self.stack.peek(0)?); Ok(())},
	    Drop(_)           => Error::not_implemented("Drop"),
	    Dup(_)            => Error::not_implemented("Dup"),
	    Swap(_, _)        => Error::not_implemented("Swap"),
	    Placeholder       => Error::not_implemented("Partial application"),
	    Index(t)          => self.index(*t),
	    Matches(t)        => self.matches(*t),
	    Coerce(t)         => self.coerce(*t),
	    TypeCheck(t)      => self.type_check(*t),
	    Trap(trap_type)   => Error::trap(*trap_type, self.stack.pop()?)
	}?;
	self.stack.debug_dump();
	Ok(())
    }

    // Load values from the data section of the script.
    //
    // Some values need to be translated to their runtime equivalent,
    // so this is not quite as trivial as it seems.
    fn load_const(&mut self, addr: Addr) -> Result<()> {
	let const_value = self.script.data[addr.0 as usize].clone();

	// XXX: what we need here is to translate lambda relative
	// capture paths in to absolute capture paths. this way we can
	// defer lookup, which is needed for recursive functions, but
	// we still get the right stack position. it's a lot more
	// complicated than I would like, and makes me wonder if this
	// scheme is worth it at this point. It'd be a lot easier to
	// work with a scope chain that simply took values by name.
	
	self.stack.push(const_value)
    }

    // Copy an argument or captured value to the top of stack.
    fn load_arg(&mut self, id: &Atom) -> Result<()> {
	self.stack.load(id)
    }

    // Shuffle an local variable to the correct stack slot position.
    fn store(&mut self, id: &Atom) -> Result<()> {
	self.stack.store(id)
    }

    // Dispatch over unary arithmetic and logic operators.
    fn unop(&mut self, op: UnOp) -> Result<()> {
        let value = self.stack.pop()?;
	self.stack.push(VMOps::unary(op, &value)?)
    }

    // Dispatch over binary arithmetic and logic operators.
    fn binop(&mut self, op: BinOp) -> Result<()> {
	// Remember, stack arguments pop in reverse.
	let rhs = self.stack.pop()?;
	let lhs = self.stack.pop()?;
	self.stack.push(VMOps::binary(op, &lhs, &rhs)?)
    }

    // Top level dispatch for function calls.
    fn call(&mut self, call_type: CallType) -> Result<()> {
	match call_type {
	    CallType::Always    => {
		let callable = self.stack.pop()?;
		self.exec_callable(callable)
	    },
	    CallType::If     => self.call_if(),
	    CallType::IfElse => self.call_if_else(),
	}	
    }

    // Conditional call, single branch.
    //
    // Corresponds to if ... elif.
    fn call_if(&mut self) -> Result<()> {
	let callable = self.stack.pop()?;
	let cond = self.stack.pop()?;
	match cond {
	    Value::Bool(false) => Ok(()),
	    Value::Bool(true)  => self.exec_callable(callable),
	    illegal => Error::type_error("Bool", &format!("{:#?}", illegal))
	}
    }

    // Conditional call, with default branch.
    //
    // Corresponds to if / elif ... else, and terminates an if-else
    // chain.
    fn call_if_else(&mut self) -> Result<()> {
	let if_false = self.stack.pop()?;
	let if_true = self.stack.pop()?;
	match self.stack.pop()? {
	    Value::Bool(true)  => self.exec_callable(if_true),
	    Value::Bool(false) => self.exec_callable(if_false),
	    illegal => Error::type_error("Bool", &format!("{:#?}", illegal))
	}
    }

    // Hand control to the given callable.
    //
    // Factors out managing the call-stack for all variants of the
    // call instruction.
    //
    // This works by recursively calling `exec_block()`, which in turn
    // makes this interpreter recursive. We could avoid this by just
    // keep track of the the current instruction, but for now I just
    // want to keep things simple.
    fn exec_callable(&mut self, callable: Value) -> Result<()> {
	let (block, _) = match callable {
	    Value::Lambda(b) => Ok((b, self.stack.empty.clone())),
	    Value::Closure(b, captures) => Ok((b, captures.clone())),
	    _ => Error::not_callable(&callable)
	}?;

	// Allocate the stack frame for this call.
	self.stack.push_call_frame(block.clone())?;

	// Hand control to the callable's code block
	self.exec_block(&block.code)?;

	// Call is done, remove the stack frame.
	if let Some(value) = self.stack.pop_frame()? {
	    self.stack.push(value)
	} else {
	    Ok(())
	}
    }

    // Try to index into the given collection.
    fn index(&mut self, t: IndexType) -> Result<()> {
	let index = self.stack.pop()?;
	let collection = self.stack.pop()?;
	match (&index, t) {
	    (Value::List(_), IndexType::List) => (),
	    (Value::Map(_), IndexType::Map) => (),
	    (Value::Record(_), IndexType::Record) => (),
	    (index, _) => {Error::type_error(index, t)?;}
	};
	self.stack.push(VMOps::index(&index, &collection)?)
    }

    // Check whether value matches a given type.
    fn matches(&mut self, tt: TypeTag) -> Result<()> {
	let value = self.stack.pop()?;
	self.stack.push(Value::Bool(VMOps::matches(&value, tt)))
    }    

    // Try to convert one value into another.
    fn coerce(&mut self, tt: TypeTag) -> Result<()> {
	let value = self.stack.pop()?;
	self.stack.push(VMOps::coerce(&value, tt)?)
    }

    // Error if value is not of expected type.
    //
    // Doesn't consume its argument if the check succeeds.
    fn type_check(&self, tt: TypeTag) -> Result<()> {
	let value = self.stack.peek(0)?;
	if VMOps::matches(&value, tt) {
	    Ok(())
	} else {
	    VMOps::type_mismatch(&value, tt)?;
	    Ok(())
	}
    }
}
