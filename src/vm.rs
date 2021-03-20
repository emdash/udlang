use crate::ir::{
    Executable,
    Instruction,
    IR,
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

    
// Holds the state we need to execute instructions sequentially.
pub struct VM {
    script: Executable,
    stack: Vec<Value>,
    input: Option<Value>,
}


// This is the high-level interface for our VM.
impl VM {
    // Allocate and initialize a new virtual machine
    pub fn new(script: Executable) -> VM {
	let stack = Vec::new();
	let input = None;
	VM {script, stack, input}
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
	Ok(for insn in block {
	    self.exec_insn(insn)?;
	})
    }

    // Execute the given instruction.
    fn exec_insn(&mut self, insn: &Instruction) -> Result<()> {
	use Instruction::*;
	match insn {
	    Const(addr) => {
		let x = &self.script.data[*addr as usize].clone();
		self.push(&x)
	    },
	    Arg(_, _) => Error::not_implemented("Arg instruction"),
	    Def(_) => Error::not_implemented("Local var"),
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

    fn push(&mut self, value: &Value) -> Result<()> {
	self.stack.push(value.clone());
	Ok(())
    }

    fn pop(&mut self) -> Result<Value> {
	if let Some(value) = self.stack.pop() {
	    Ok(value)
	} else {
	    Error::stack_underflow()
	}
    }

    fn peek(&self, rel: u8) -> Result<&Value> {
	let len = self.stack.len();
	if len > 0 {
	    Ok(&self.stack[len - 1 - rel as usize])
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
