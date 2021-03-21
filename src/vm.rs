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

    pub fn not_callable<T>(value: &Value) -> Result<T> {
	Err(Self::TypeError(format!("{:?} is not callable", value)))
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
	// Push an empty stack frame
	self.call_stack.push(StackFrame {
	    locals: 0,
	    args: 0xFF, /* XXX: do something useful here? */
	    captures: Vec::new()
	});
	self.exec_block(&block);
	self.call_stack.pop();
	Ok(())
    }

    // Execute the main block in the script.
    pub fn main(&mut self, input: Value) -> Result<()> {
	// XXX: It really bothers me that we can't just borrow this.
	let block = self.script.code[1].clone();
	// Push an empty stack frame
	self.call_stack.push(StackFrame {
	    locals: 0,
	    args: 0xFF, /* XXX: do something useful here? */
	    captures: Vec::new()
	});

	self.input = Some(input);
	self.exec_block(&block);
	self.call_stack.expect("Call stack underflow");
	Ok(())
    }

    // Execute each instruction in the given block.
    fn exec_block(&mut self, block: &[Instruction]) -> Result<()> {
	for insn in block {
	    self.exec_insn(insn)?;
	}

	self.call_stack.pop().expect("Call stack underflow");
	Ok(())
    }

    // Execute the given instruction.
    fn exec_insn(&mut self, insn: &Instruction) -> Result<()> {
	use Instruction::*;
	println!("\n\nExec: {:?}", insn);
	println!("Values: {:?}", self.value_stack);
	println!("Calls: {:?}", self.call_stack);
	match insn {
	    Const(addr) => {
		let x = &self.script.data[*addr as usize].clone();
		self.push(&x)
	    },
	    Arg(binding, x) => self.arg(*binding, *x),
	    Def(_) => Ok(()) /* XXX: we don't need this instruction */,
	    Un(_) => Error::not_implemented("Unary operator"),
	    Bin(_) => Error::not_implemented("Binary operator"),
	    Call(ct) => self.call(*ct),
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
	    println!("{:?} {:?} {:?}", top, frame, pos);
	    match binding {
		Binding::Local => self.abs(frame.locals + pos as usize),
		Binding::Arg => self.abs(frame.args + pos as usize),
		Binding::Capture => Ok(&frame.captures[pos as usize])
	    }
	}?.clone(); /* XXX UGH THIS IS FINE! I HATE THE BORROW CHECKER!!! */
	self.push(&value)
    }

    // Top level dispatch for function calls.
    fn call(&mut self, call_type: CallType) -> Result<()> {
	match call_type {
	    CallType::Always    => {
		let callable = self.pop()?;
		self.exec_callable(call_type, callable)
	    },
	    CallType::If(true)  => self.call_if_true(),
	    CallType::If(false) => self.call_if_false(),
	    CallType::IfElse    => self.call_if_else(),
	}	
    }

    // Conditional call, true case.
    fn call_if_true(&mut self) -> Result<()> {
	let callable = self.pop()?;

	match self.pop()? {
	    Value::Bool(true)  => self.exec_callable(CallType::If(true), callable),
	    Value::Bool(false) => Ok(()),
	    illegal => Error::type_error("Bool", &format!("{:?}", illegal))
	}
    }

    // Conditional call, false case.
    fn call_if_false(&mut self) -> Result<()> {
	let callable = self.pop()?;
	match self.pop()? {
	    Value::Bool(true)  => Ok(()),
	    Value::Bool(false) => self.exec_callable(CallType::If(true), callable),
	    illegal => Error::type_error("Bool", &format!("{:?}", illegal))
	}
    }

    // Conditional call, ternary case.
    fn call_if_else(&mut self) -> Result<()> {
	let if_false = self.pop()?;
	let if_true = self.pop()?;
	match self.pop()? {
	    Value::Bool(true)  => self.exec_callable(CallType::IfElse, if_true),
	    Value::Bool(false) => self.exec_callable(CallType::IfElse, if_false),
	    illegal => Error::type_error("Bool", &format!("{:?}", illegal))
	}
    }

    // Hand control to the given value if it is callable.
    //
    // This takes care of managing the call stack, so callers don't
    // have to.
    fn exec_callable(&mut self, ct: CallType, callable: Value) -> Result<()> {
	if let Value::Lambda(callable) = callable {
	    // Allocate the stack frame for this call, which may or may
	    // not occur.
	    self.call_stack.push(StackFrame::new(
		ct,
		&callable,
		&self.value_stack,
		&self.call_stack
	    ));

	    // Hand control to the callable's code block
	    self.exec_block(&callable.code)?;

	    // Call is done, remove the stack frame.
	    let frame = self.call_stack.pop().expect("Call stack underflow");

	    // Contract stack to contain only the return value.
	    let start = frame.locals;
	    let depth = start + callable.rets as usize;
	    while self.value_stack.len() > depth {
		self.value_stack.remove(start);
	    }
	    Ok(())
	} else {
	    Error::not_callable(&callable)
	}
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
