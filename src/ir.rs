use crate::ast::{
    self,
    BinOp,
    UnOp, Seq,
    StmtNode,
    ExprNode,
    TypeNode,
    Statement,
    Program,
};

use crate::parser;

use std::collections::hash_map::{HashMap, Entry};
use std::ops::Deref;
use std::hash::Hash;
use std::fmt::Debug;


type Addr = u16;
type Boxed<T> = std::rc::Rc<T>;
type Float = eq_float::F64;


/* Errors occuring when lowering to IR ***************************************/


pub type Result<T> = std::result::Result<T, Error>;


#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NotCallable(String),
    Duplicate(String),
    NotFound(String),
    IllFormed(String),
    TypeError(String),
    TooManyArguments,
    NotImplemented(String),
    Internal(String),
}


impl Error {
    pub fn not_callable<T: Debug, U>(expr: T) -> Result<U> {
	Err(Self::NotCallable(format!("{:?} is not callable.", expr)))
    }

    pub fn duplicate<T: Debug, U>(entry: T) -> Result<U> {
	Err(Self::Duplicate(format!("Redefinition of {:?}.", entry)))
    }

    pub fn not_found<T: Debug, U>(entry: T) -> Result<U> {
	Err(Self::NotFound(format!("{:?} is undefined", entry)))
    }

    pub fn ill_formed<T: Debug, U>(entry: T, context: &str) -> Result<U> {
	Err(Self::IllFormed(format!("{:?} not allowed in {}", entry, context)))
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

    pub fn too_many_arguments<T>() -> Result<T> {
	Err(Self::TooManyArguments)
    }
}



/* Utility Classes (candidate for moving into lib.rs, or elsewhwere) ***********/


// A bi-directional vector which maps *from* and *to* numeric indices.
//
// The only insertion operation supported is `push`.
//
// Use `try_push` when inserting a duplicate should be considered an
// error.
pub struct BiVec<T: Hash> {
    to_index: HashMap<T, usize>,
    from_index: Vec<T>
}


impl<T: Hash + Eq + Clone + Debug> BiVec<T> {
    pub fn new() -> Self {
	Self {
	    to_index: HashMap::new(),
	    from_index: Vec::new()
	}
    }

    // Append a new value, if needed, and return its index.
    pub fn push(&mut self, value: T) -> usize {
	let count = self.from_index.len();
	match self.to_index.entry(value.clone()) {
	    Entry::Vacant(e) => {
		e.insert(count);
		self.from_index.push(value);
		count
	    },
	    Entry::Occupied(e) => *e.get()
	}
    }

    // Try to append a new value, but error if value is already present.
    pub fn try_push(&mut self, value: T) -> Result<usize> {
	let count = self.from_index.len();
	match self.to_index.entry(value.clone()) {
	    Entry::Vacant(e) => {
		e.insert(count);
		self.from_index.push(value);
		Ok(count)
	    },
	    _ => Error::duplicate(value)
	}
    }

    // Get the index of value.
    pub fn index(&self, value: &T) -> Result<usize> {
	if let Some(count) = self.to_index.get(value) {
	    Ok(*count)
	} else {
	    Error::not_found(value)
	}
    }

    // Return a copy of the inner vector.
    pub fn to_vec(&self) -> Vec<T> {
	self.from_index.clone()
    }

    // Convert ourselves into a vector.
    pub fn into_vec(self) -> Vec<T> {
	self.from_index
    }

    // Expose our size
    pub fn len(&self) -> usize {
	return self.from_index.len()
    }
}


// Handles logic around definitions / declarations.
pub struct ScopeChain<T> {
    scopes: Vec<(BiVec<String>, HashMap<String, T>)>
}


impl<T> ScopeChain<T> {
    fn new() -> ScopeChain<T> {
	ScopeChain {
	    scopes: Vec::new()
	}
    }

    fn top(&self) -> &(BiVec<String>, HashMap<String, T>) {
	&self.scopes[self.scopes.len() - 1]
    }

    fn top_mut(&mut self) -> &mut (BiVec<String>, HashMap<String, T>) {
	let len = self.scopes.len() - 1;
	&mut self.scopes[len]
    }

    fn push(&mut self) -> Result<()> {
	if self.scopes.len() < 256 {
	    self.scopes.push((BiVec::new(), HashMap::new()));
	    Ok(())
	} else {
	    Error::too_many_arguments()
	}
    }

    fn pop(&mut self) -> Result<(BiVec<String>, HashMap<String, T>)> {
	if let Some(scope) = self.scopes.pop() {
	    Ok(scope)
	} else {
	    Error::internal("Scope chain underflow.")
	}
    }

    fn define(&mut self, id: &str, value: T) -> Result<()> {
	let (indices, values) = self.top_mut();

	if indices.len() <= 256 {
	    indices.try_push(id.to_string())?;
	    values.insert(id.to_string(), value);
	    Ok(())
	} else {
	    Error::too_many_arguments()
	}
    }

    fn index(&self, id: &str) -> Result<(u8, u8)> {
	// XXX: slightly annoying to have to make a copy for a look-up
	// it's due to BiVec being generic over T, and not specialized
	// for `String` / borrowing.
	let id = id.to_string();

	for (scope, (indices, values)) in self.scopes.iter().rev().enumerate() {
	    if values.contains_key(&id) {
		return Ok((scope as u8, indices.index(&id.to_string())? as u8))
	    }
	}

	Error::not_found(id)
    }
}



/* IR ************************************************************************/


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CallType {
    Always,
    If(bool)
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum IndexType {
    List,
    Map,
    Record
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum Instruction {
    Const(Addr),
    Arg(u8, u8),
    Def(u8),
    Un(UnOp),
    Bin(BinOp),
    Call(CallType),
    In,
    Out,
    Debug,
    Drop(u8),
    Dup(u8),
    Swap(u8, u8),
    Placeholder,
    Index(IndexType),
    Matches,
    Coerce,
}


type Block = Vec<Instruction>;


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(String),
    Type(Boxed<TypeTag>),
    Point(Float, Float),
    Lambda {
	code: Block,
	args: u8,
	rets: u8,
	captures: Vec<Value>
    },
    List(Vec<Value>),
    Map(Vec<(String, Value)>),
    Record(Boxed<TypeTag>, Vec<Value>)
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
// XXX: placeholder type for now. ast::TypeTag isn't hashable. It may
// also be too heavyweight. We need a FrozenMap from an external crate
// for some variants.
pub enum TypeTag {
    Int,
    Float,
    Str,
    Type,
    Lambda(u8, u8),
    List,
    Map,
    Record
}

    
// A source file compiles to a executable or a module.
#[derive(Clone, Debug, PartialEq)]
pub enum IR {
    Executable(Executable),
    Module(Module)
}


// The complete output of compiled program, including all dependencies.
#[derive(Clone, Debug, PartialEq)]
pub struct Executable {
    pub desc: String,
    pub input: TypeNode,
    pub output: TypeNode,
    pub data: Vec<Value>,
    pub code: Vec<Block>,
}


// A source file becomes
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub desc: String,
    pub types: HashMap<String, TypeNode>,
    pub values: HashMap<String, Value>,
}


// Bookeeping enumeration for lexical bindings
#[derive(Clone, Debug, PartialEq)]
pub enum Binding {
    Argument(u8, TypeNode),
    Local(u8, TypeNode),
    Capture(u8, TypeNode)
}


// Holds state required to process an AST into IR.
pub struct Compiler {
    data: BiVec<Value>,
    blocks: Vec<Block>,
    scopes: ScopeChain<Binding>,
    // TBD: scope chain / stack
    // TBD: modules.
}


/* IR Impls ******************************************************************/


impl Compiler {
    pub fn new() -> Self {
	Self {
	    data: BiVec::new(),
	    blocks: Vec::new(),
	    scopes: ScopeChain::new(),
	}
    }

    // Try to convert the AST to IR
    pub fn compile_program(&mut self, prog: Program) -> Result<IR> {
	Ok(match prog {
	    Program::Script {desc, decls, input, output, body}
	    => IR::Executable(self.compile_script(desc, decls, input, output, body)?),
	    Program::Library {desc, decls}
	    => IR::Module(self.compile_library(desc, decls)?)
	})
    }


    // Try to compile a script to an IR executable.
    pub fn compile_script(
	&mut self,
	desc: String,
	decls: Seq<Statement>,
	input: TypeNode,
	output: TypeNode,
	body: Seq<Statement>
    ) -> Result<Executable> {
	self.scopes.push()?;

	self.blocks.push(Block::new());
	for d in decls {
	    self.compile_statement(&d)?;
	}

	self.blocks.push(Block::new());
	for statement in body {
	    self.compile_statement(&statement)?;
	}

	self.scopes.pop()?;

	Ok(Executable {
	    desc,
	    input: input,
	    output: output,
	    data: self.data.to_vec(),
	    code: self.blocks.clone()
	})
    }

    // Try to compile a libray to an IR module.
    pub fn compile_library(
	&mut self,
	desc: String,
	decls: Vec<StmtNode>
    ) -> Result<Module> {
	self.scopes.push()?;

	for d in decls {
	    self.compile_statement(&d)?;
	}

	self.scopes.pop()?;

	// TBD: extract top-level scope decls to hashmap.

	Error::not_implemented("Libraries")
    }

    // Dispatch to each instruction variant
    pub fn compile_statement(&mut self, statement: &StmtNode) -> Result<()> {
	use ast::Statement::*;

	// Do NOT include a wildcard match in this block, it breaks
	// exhaustivity analysis.
	match statement.deref() {
	   Import(_)           => Error::not_implemented("module imports")?,
	   Export(_)           => Error::not_implemented("module exports")?,
	   ExprForEffect(expr) => self.compile_expr(expr)?,
	   Emit(expr)          => self.compile_emit(expr)?,
	   Def(id, val)        => self.compile_def(id, val)?,
	   TypeDef(id, t)      => self.compile_typedef(id, t)?,
	   ListIter(_, _, _)   => Error::not_implemented("list iteration")?,
	   MapIter(_, _, _, _) => Error::not_implemented("map iteration")?,
	   While(_, _)         => Error::not_implemented("while loops")?,
	   Suppose(_, _, _)    => Error::not_implemented("subjunctives")?,
	   EffectCapture       => Error::not_implemented("effect captures")?
	};

	Ok(())
    }

    pub fn compile_emit(&mut self, expr: &ExprNode) -> Result<()> {
	self.compile_expr(expr)?;
	self.emit(Instruction::Out)
    }

    pub fn compile_def(&mut self, id: &str, expr: &ExprNode) -> Result<()> {
	Error::not_implemented("local bindings")
    }

    pub fn compile_typedef(&mut self, id: &str, expr: &TypeNode) -> Result<()> {
	Error::not_implemented("local typedefs")
    }

    // Dispatch to each expression variant.
    pub fn compile_expr(&mut self, expr: &ExprNode) -> Result<()> {
	use ast::Expr::*;

	// Do NOT include a wildcard match in this block, it breaks
	// exhaustivity analysis.
	match expr.deref() {
	    Void                    => Ok(()),
	    Bool(b)                 => self.compile_const(Value::Bool(*b)),
	    Int(i)                  => self.compile_const(Value::Int(*i)),
	    Float(f)                => self.compile_const(Value::Float(*f)),
	    Str(s)                  => self.compile_const(Value::Str(s.clone())),
	    Point(x, y)             => self.compile_const(Value::Point(*x, *y)),
	    This                    => Error::not_implemented("self"),
	    In                      => self.emit(Instruction::In),
	    Partial                 => self.emit(Instruction::Placeholder),
	    List(_)                 => Error::not_implemented("list literal"),
	    Map(_)                  => Error::not_implemented("map literal"),
	    Id(_)                   => Error::not_implemented("variable lookup"),
	    Dot(_, _)               => Error::not_implemented("fixed index"),
	    Has(_, _)               => Error::not_implemented("member test"),
	    Index(_, _)             => Error::not_implemented("computed index"),
	    Cond(_, _)              => Error::not_implemented("conditional"),
	    Block(stmts, ret)       => self.compile_block(stmts, ret),
	    BinOp(op, l, r)         => self.compile_bin(*op, l, r),
	    UnOp(op, operand)       => self.compile_un(*op, operand),
	    Call(func, args)        => self.compile_call(func, args),
	    Lambda(args, ret, body) => Error::not_implemented("lambda expressions")
	}?;

	Ok(())
    }

    // A constant value will be replaced with a Const instruction in
    // the output.
    //
    // We need to look up the value in our table to get its index. If
    // this value has already been seen, it will be re-used.
    pub fn compile_const(&mut self, val: Value) -> Result<()> {
	// XXX: handle address overflow.
	let addr = self.data.push(val) as u16;
	self.emit(Instruction::Const(addr))
    }

    // Iterate over the statements in a block, and compile each.
    pub fn compile_block(&mut self, stmts: &[StmtNode], ret: &ExprNode) -> Result<()> {
	self.scopes.push()?;

	for statement in stmts {
	    self.compile_statement(statement)?;
	}

	self.compile_expr(&ret)?;
	self.scopes.pop()?;
	
	Ok(())
    }

    // Try to compile a binary operator
    pub fn compile_bin(&mut self, op: BinOp, l: &ExprNode, r: &ExprNode) -> Result<()> {
	self.compile_expr(l)?;
	self.compile_expr(r)?;
	self.emit(Instruction::Bin(op))
    }

    // Try to compile a unary operator
    pub fn compile_un(&mut self, op: UnOp, operand: &ExprNode) -> Result<()> {
	self.compile_expr(operand)?;
	self.emit(Instruction::Un(op))
    }

    // Try to compile a function call
    //
    // Args are placed on the stack in order.
    //
    // foo(1, 2, 3) => [1 2 3 <foo> call]
    //
    // Inside the call of foo, arg(0) is 1.
    pub fn compile_call(&mut self, func: &ExprNode, args: &[ExprNode]) -> Result<()> {
	self.compile_expr(func)?;
	for arg in args {
	    self.compile_expr(arg)?;
	}
	self.emit(Instruction::Call(CallType::Always))
    }

    // Emit an instruction to the output.
    //
    // Since this is a post-order notation, it is convenient for this
    // function to return Ok(()), since most compile_* functions will
    // end with this call.
    pub fn emit(&mut self, inst: Instruction) -> Result<()>{
	let top = self.blocks.len() - 1;
	self.blocks[top].push(inst);
	Ok(())
    }
}


impl Instruction {
    // The argument arity of an instruction may depend on the top-most
    // stack value.
    pub fn arity(self, top: &Value) -> Result<(u8, u8)> {
	match self {
	    Instruction::Const(_)      => Ok((0, 1)),
	    Instruction::Arg(_, _ )    => Ok((0, 1)),
	    Instruction::Def(_)        => Ok((1, 0)),
	    Instruction::Un(_)         => Ok((1, 1)),
	    Instruction::Bin(_)        => Ok((2, 1)),
	    Instruction::In            => Ok((0, 1)),
	    Instruction::Out           => Ok((1, 0)),
	    Instruction::Debug         => Ok((0, 0)),
	    Instruction::Drop(n)       => Ok((n, 0)),
	    Instruction::Swap(_, _)    => Ok((0, 0)),
	    Instruction::Dup(n)        => Ok((0, n)),
	    Instruction::Placeholder   => Ok((0, 1)),
	    Instruction::Index(_)      => Ok((2, 1)),
	    Instruction::Coerce        => Ok((2, 1)),
	    Instruction::Matches       => Ok((2, 1)),
	    Instruction::Call(variant) => self.call_arity(variant, top)
	}
    }

    pub fn call_arity(self, ct: CallType, top: &Value) -> Result<(u8, u8)> {
	if let &Value::Lambda {code: _, args, rets, captures: _} = top {
	    match ct {
		CallType::Always => Ok((1 + args, rets)),
		CallType::If(_)  => Ok((2 + args, rets))
	    }
	} else {
	    Error::not_callable(top)
	}
    }
}


// Compile the given path to IR
pub fn compile(path: &str) -> IR {
    let mut compiler = Compiler::new();
    compiler.compile_program(parser::parse(path)).expect("Compilation Error")
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn assert_file(path: &str, expected: IR) {
	assert_eq!(compile(path), expected)
    }
    
    #[test]
    pub fn test_hello() {
	let ast = ast::Builder::new();
	use Instruction::*;
	use BinOp::*;
	
	assert_file(
	    "examples/hello.us",
	    IR::Executable(Executable {
		desc: "Hello world, in uDLang".to_string(),
		input: ast.t_str.clone(),
		output: ast.t_str.clone(),
		data: vec![Value::Str("Hello, ".to_string())],
		code: vec![vec![], vec![Const(0), In, Bin(Add), Out]]
	    })
	);
    }
}
