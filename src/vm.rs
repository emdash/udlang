use crate::ir::{
    Binding,
    CallType,
    CapturePath,
    Executable,
    Instruction,
    IR,
    Lambda,
    Value,
    compile,
};


use std::fmt::Debug;
use std::rc::Rc;


#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    StackUnderflow,
    StackOverflow,
    IllegalAddr(String),
    TypeError(String),
    NotImplemented(String),
    Internal(String),
}


pub type Result<T> = std::result::Result<T, Error>;


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

    pub fn type_error<T: Debug, U>(expected: T, got: T) -> Result<U> {
	Err(Self::TypeError(format!("Expected {:?}, got {:?}", expected, got)))
    }

    pub fn not_implemented<T>(feature: &str) -> Result<T> {
	Err(Self::NotImplemented(format!("{:?} is not implemented", feature)))
    }

    pub fn internal<T>(mumbo_jumbo: &str) -> Result<T> {
	Err(Self::Internal(mumbo_jumbo.to_string()))
    }
}


// Conveneince function to compile and run the script at the given path.
pub fn run<S> (path: &str, input: S) -> Result<()>
where S: Iterator<Item = Value> {
    let program = match compile(path) {
	IR::Executable(e) => e,
	IR::Module(_) => panic!("You cannot execute a library as a script!")
    };

    VM::new(program).run(input)
}


// Holds information needed to locate arguments on the stack.
struct StackFrame {
    pub locals: usize,
    pub args: usize,
    pub captures: Vec<Value>
}


impl StackFrame {
    // Make a new stack frame for this function call.
    pub fn new(
	ct: CallType,
	callable: &Lambda,
	val_stack: &[Value],
	call_stack: &[Self]
    ) -> Self {
	// Locals are referenced from the current stack depth
	let locals = val_stack.len();
	// Find the start of the arguments in the stack.
	let args = locals - callable.arity(ct) as usize;

	// Helper method to collect a value off the stack from a CapturePath.
	let capture_from_path = |path: &CapturePath| {
	    let frame = &call_stack[call_stack.len() - 1 - path.frame as usize];
	    let abs = frame.locals + path.index as usize;
	    &val_stack[abs]
	};

	// Collect the values we need to capture off the stack.
	let captures = callable.captures
	    .iter()
	    .map(capture_from_path)
	    .cloned()
	    .collect();

	StackFrame {locals, args, captures}
    }
}

    
// Holds the state we need to execute instructions sequentially.
pub struct VM {
    script: Executable,
    value_stack: Vec<Value>,
    call_stack: Vec<StackFrame>,
    input: Option<Value>,
}


// This is the high-level interface for our VM.
impl VM {
    // Allocate and initialize a new virtual machine
    pub fn new(script: Executable) -> VM {
	let value_stack = Vec::new();
	let call_stack = Vec::new();
	let input = None;
	VM {script, value_stack, call_stack, input}
    }

    // Run the given script until the input iterator is exhausted.
    pub fn run<S: Iterator<Item = Value>>(
	&mut self,
	input: S,
    ) -> Result<()> {
	self.init()?;
	Ok(for value in input {
	    self.main(value)?;
	})
    }

    // Execute the init block in the script.
    pub fn init(&mut self) -> Result<()> {
	// XXX: It really bothers me that we can't just borrow this.
	let block = self.script.code[0].clone();
	self.exec_block(&block)
    }

    // Execute the main block in the script.
    pub fn main(&mut self, input: Value) -> Result<()> {
	// XXX: It really bothers me that we can't just borrow this.
	let block = self.script.code[1].clone();
	self.input = Some(input);
	self.exec_block(&block)
    }

    // Execute each instruction in the given block.
    fn exec_block(&mut self, block: &[Instruction]) -> Result<()> {
	// Push an empty stack frame
	self.call_stack.push(StackFrame {
	    locals: 0,
	    args: 0xFF, /* XXX: do something useful here? */
	    captures: Vec::new()
	});

	for insn in block {
	    self.exec_insn(insn)?;
	}

	self.call_stack.pop();
	Ok(())
    }

    // Execute the given instruction.
    fn exec_insn(&mut self, insn: &Instruction) -> Result<()> {
	use Instruction::*;
	match insn {
	    Const(addr) => {
		let x = &self.script.data[*addr as usize].clone();
		self.push(&x)
	    },
	    Arg(binding, x) => self.arg(*binding, *x),
	    Def(_) => Ok(()) /* XXX: we don't need this instruction */,
	    Un(_) => Error::not_implemented("Unary operator"),
	    Bin(_) => Error::not_implemented("Binary operator"),
	    Call(_) => Error::not_implemented("Function call"),
	    In => self.push(&self.input.clone().expect("No input record")),
	    Out => {println!("{:?}", self.pop()?); Ok(())},
	     /* XXX: should be stderr */
	    Debug => {println!("{:?}", self.peek(0)?); Ok(())},
	    Drop(n) => self.drop(*n),
	    Dup(n) => self.dup(*n),
	    Swap(x, y) => self.swap(*x, *y),
	    Placeholder => Error::not_implemented("Partial application"),
	    Index(_) => Error::not_implemented("Indexing into collection"),
	    Matches => Error::not_implemented("Type query"),
	    Coerce => Error::not_implemented("Type cast")
	}
    }

    // Copy an argument or captured value to the top of stack.
    fn arg(&mut self, binding: Binding, pos: u8) -> Result<()> {
	let value = {
	    let top = self.call_stack.len() - 1;
	    let frame = &self.call_stack[top];
	    match binding {
		Binding::Local => self.abs(frame.locals + pos as usize),
		Binding::Arg => self.abs(frame.args + pos as usize),
		Binding::Capture => Ok(&frame.captures[pos as usize])
	    }
	}?.clone(); /* XXX UGH THIS IS FINE! I HATE THE BORROW CHECKER!!! */
	self.push(&value)
    }

    fn push(&mut self, value: &Value) -> Result<()> {
	self.value_stack.push(value.clone());
	Ok(())
    }

    fn pop(&mut self) -> Result<Value> {
	if let Some(value) = self.value_stack.pop() {
	    Ok(value)
	} else {
	    Error::stack_underflow()
	}
    }

    fn peek(&self, rel: u8) -> Result<&Value> {
	let len = self.value_stack.len();
	if len > 0 {
	    Ok(&self.value_stack[len - 1 - rel as usize])
	} else {
	    Error::stack_underflow()
	}
    }

    fn abs(&self, pos: usize) -> Result<&Value> {
	if pos < self.value_stack.len() {
	    Ok(&self.value_stack[pos])
	} else {
	    Error::stack_underflow()
	}
    }

    fn dup(&mut self, n: u8) -> Result<()> {
	let top = self.peek(0)?.clone();
	Ok(for _ in 0..n {
	    self.push(&top)?;
	})
    }

    fn drop(&mut self, n: u8) -> Result<()> {
	Ok(for _ in 0..n {
	    self.pop()?;
	})
    }

    fn swap(&mut self, _: u8, _: u8) -> Result<()> {
	Error::not_implemented("swap instruction")
    }
}
