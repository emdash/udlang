use crate::ast::{
    self,
    Node,
    BinOp,
    UnOp, Seq,
    AList,
    StmtNode,
    ExprNode,
    TypeNode,
    Statement,
    Program,
    Expr
};

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
    Int(u64),
    Float(Float),
    Str(String),
    Type(Boxed<TypeTag>),
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
    pub input: ast::TypeTag,
    pub output: ast::TypeTag,
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
    output: Vec<Instruction>,
    scopes: ScopeChain<Binding>,
    // TBD: scope chain / stack
    // TBD: modules.
}


/* IR Impls ******************************************************************/


impl Compiler {
    pub fn new() -> Self {
	Self {
	    data: BiVec::new(),
	    output: Vec::new(),
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
	self.scopes.push();

	for d in decls {
	    self.compile_statement(&d)?;
	}

	for statement in body {
	    self.compile_statement(&statement)?;
	}

	self.scopes.pop()?;

	Ok(Executable {
	    desc,
	    input: Node::try_unwrap(input).unwrap(),
	    output: Node::try_unwrap(output).unwrap(),
	    data: self.data.to_vec(),
	    code: vec![self.output.clone()]
	})
    }

    // Try to compile a libray to an IR module.
    pub fn compile_library(
	&mut self,
	desc: String,
	decls: Vec<StmtNode>
    ) -> Result<Module> {
	self.scopes.push();

	for d in decls {
	    self.compile_statement(&d)?;
	}

	self.scopes.pop()?;

	// TBD: extract top-level scope decls to hashmap.

	Error::not_implemented("Libraries")
    }

    // Try to compile a statement to instructions.
    //
    // The instructions will be placed in the output block.
    pub fn compile_statement(&mut self, statement: &StmtNode) -> Result<()> {
	Error::not_implemented("Anything at all")
    }

    // Try to compile a block to instructions.
    //
    // The instructions will be placed in the output block.
    pub fn compile_block(&mut self, stmts: &[StmtNode], ret: ExprNode) -> Result<()> {
	self.scopes.push();

	for statement in stmts {
	    self.compile_statement(statement)?;
	}

	self.compile_expr(&ret)?;
	self.scopes.pop()?;
	
	Ok(())
    }

    // Try to compile an expression to instructions.
    //
    // The instructions will be placed in the output block.
    pub fn compile_expr(&mut self, expr: &ExprNode) -> Result<()> {
	Error::not_implemented("Expressions")
    }

    // A constant value will be replaced with a Const instruction in
    // the output.
    //
    // We need to look up the value in our table to get its index. If
    // this value has already been seen, it will be re-used.
    pub fn compile_const(&mut self, val: Value) -> Result<()> {
	// XXX: handle address overflow.
	let addr = self.data.push(val) as u16;
	self.emit(Instruction::Const(addr));
	Ok(())
    }

    // Emit an instruction to the output.
    pub fn emit(&mut self, inst: Instruction) {
	self.output.push(inst)
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
	    Instruction::Swap(x, y)    => Ok((0, 0)),
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


#[cfg(test)]
mod tests {
    use crate::grammar;
    use crate::ast::*;
    use super::*;

    #[test]
    pub fn test_simple() {
    }
}
