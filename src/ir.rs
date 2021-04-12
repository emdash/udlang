use crate::ast::{
    self,
    BinOp,
    Expr,
    ExprNode,
    Float,
    Node,
    Program,
    Statement,
    StmtNode,
    TypeNode,
    UnOp,
};

use crate::parser;

use std::collections::hash_map::{HashMap, Entry};
use std::collections::hash_set::HashSet;
use std::ops::Deref;
use std::hash::Hash;
use std::fmt::Debug;

use ordered_float::OrderedFloat;


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
pub struct BiVec<T: Hash + Clone + Debug> {
    to_index: HashMap<T, usize>,
    from_index: Vec<T>
}


impl<T: Hash + Clone + Debug> std::fmt::Debug for BiVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	f.debug_list().entries(self.from_index.iter()).finish()
    }
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


/* IR Instruction Set ********************************************************/


// One small concession to optimization: identifers are automatically
// interned.
//
// In theory, by placing these behind Shared<>, and keeping them in a
// HashSet, we get pointer equality for free, which should speed up
// local variable lookup.
pub type Atom = Shared<str>;


// This represents the entire instruction set.
//
// You can think of this IR as a minimal instruction set over rich
// types.
//
// No effort is made here at optimization. The IR is intended to be
// easy to reason about for subsequent optimization passes, and easy
// to process.
//
// It is essentially just a stack-based representation of the AST,
// with a few simplifications.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Instruction {
    Const(Value),       // Place a constant value on the stack.
    Lcons(u16),         // Construct a list literal with top n stack items.
    Mcons(u16),         // Construct a list literal with top n stack items.
    Load(Atom),         // Load a local variable by atom id.
    Store(Atom),        // Store a value as a local variable.
    // XXX: Arg(u8)     // Load the corresponding argument by its position.
    // XXX: Capture(u8) // Load the corresponding capture by its index.
    // XXX: Drop,       // Discard from stack
    Un(UnOp),           // Wraps all unary arithmetic and logic operations.
    Bin(BinOp),         // Wraps all binary arithmetic and logic operations.
    Call(CallType),     // Unconditional, conditional, and iterative.
    In,                 // Place input record on stack.
    Out,                // Send top of stack to output.
    Capture(CaptureOp), // Instructions for the subjunctive mechanism.
    Debug,              // Inspect top of stack without altering it.
    Placeholder,        // This value is used in a partial application.
    Index(IndexType),   // Index into a collection.
    Matches(TypeTag),   // True if top of stack matches given type.
    Coerce(TypeTag),    // Try to cast from one type to another.
    TypeCheck(TypeTag), // Error if top of stack doesn't match expected type.
    Trap(TrapType)      // Signal an error condition from user code.
}


// The only mechanism for control flow is the Call instruction.
//
// This enum specifies the variants of the call instruction, including
// conditional and iterative forms.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CallType {
    Always,  // Plain, vanilla function call.
    If,      // Conditional call.
    IfElse,  // Ternary expression.
    ForEach, // Iterate over over a collection for side-effects.
    // XXX: Reduce // Fold over a collection to produce a scalar
    // XXX: Map    // Isomorphism: iterate over a collection to produce a new collection.
}


// There is a single index instruction, which is statically typed.
//
// The collection and key type provided to an Index instruction must
// agree with the variant here.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum IndexType {
    List,
    Map,
    Record,
    Module
}


// Variant of capture instruction for output captures.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CaptureOp {
    Push,  // Push a new output capture
    Pop,   // Pop the output capture
    Test,  // Check if current output capture is non-empty.
    Send   // Copy top-most capture downstream.
}


// We can trap in two ways: recoverable, and fatal.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TrapType {
    Exception,
    Fatal
}


/* IR Values *****************************************************************/


// Abstract over memory management.
pub type Shared<T> = std::rc::Rc<T>;
pub type Seq<T> = Vec<T>;
pub type AList<T> = Vec<(String, T)>;
// XXX: use a real, hashable map type here at some point.
pub type Map<T> = AList<T>;
    

// Holds any representable value.
//
// It should be cheap to clone this type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    // These values are unboxed.
    None,
    Atom(Atom),
    Type(TypeTag),
    Bool(bool),
    Int(i64),
    Float(Float),
    // XXX: box strings, but not right this second.
    Str(String),
    // Values below are heavy-weight enough to be boxed.
    Point(Shared<Point>),
    Lambda(Shared<Block>),
    Closure(Shared<Block>, Shared<Seq<Value>>),
    Tuple(Shared<Seq<Value>>),
    List(Shared<Seq<Value>>),
    Map(Shared<Map<Value>>),
    Record(Shared<Record>),
    Module(Shared<DummyModule>),
}


// Placeholder to get this to compile until I can make module
// hashable
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct DummyModule;


// Represents the discriminant of a Value.
//
// This is the light-weight type representation used as an immediate
// in the Instruction enum, so it must be repr(u8).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TypeTag {
    None,
    Atom,
    Type,
    Bool,
    Int,
    Float,
    Str,
    Point,
    Lambda,
    Closure,
    Tuple,
    List,
    Map,
    RecordDef,
    RecordValue,
    Module,
}


// A record value.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Record {
    vtable: Shared<Map<Member>>,
    values: Value
}


// An individual field in a record vtable.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Member {
    Field(Shared<TypeTag>),
    Method(Shared<Block>),
    StaticValue(Value),
    StaticMethod(Shared<Block>)
}


// A 2d vector / point value.
//
// XXX: implement me optimized
//
// I anticipate that this type will be come central to udashboard, if
// not udlang in general. But we could take it further with 3d, or n-d
// vectors.
//
// Also, it should go without saying that a point is not a vector, and
// the operations on them are distinct. If we want to be truly
// type-safe, we'd need both types.
//
// For now this is just a pair of floats, and we just leave it here as
// a placeholder value.
pub type Point = (Float, Float);


// A function value.
//
// Since, the only form of control flow is the `Call` instruction, in
// all its variations, function values are central to this IR.
//
// Simply put, a Lambda value is a block, plus the meta-data required
// to call it. This includes the airty of the function and return
// values, and the *capture* list, which may be empty, in case the
// function happens to be a closure.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block {
    pub code: Vec<Instruction>,
    pub args: Vec<Atom>,
    pub rets: u8,
}


// Represents the output of compilation, either a script or a module.
#[derive(Clone, Debug, PartialEq)]
pub enum IR {
    Executable(Executable),
    Module(Module)
}


// A udling script source file compiles to an exectuable.
#[derive(Clone, Debug, PartialEq)]
pub struct Executable {
    pub desc: String,
    pub input: TypeNode,
    pub output: TypeNode,
    // All constants live in this table, including function values.
    // References point either back into this table, or into runtime
    // storage.
    pub code: Vec<Block>,
}


// A udlang library source file becomes a module object after lowering
// to IR.
#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub desc: String,
    // XXX: consider implementing hash to just call hash on `path`.
    // This would possibly make it easier to cache module imports.
    // pub path: String,
    pub exports: AList<Value>,
}


/* Arithmetic and Logic ******************************************************/


// This macro factors out operator implementation boilerplate.
//
// There are two variants: binary and unary.
//
// They both the take:
// - the name of the method to define,
// - a list of <pattern> => <expr>,
//
// Basically the patterns express the variants for which a given
// operator actually yields a value. If no patern matches, the
// result is a type error.
macro_rules! operator {

    // Template for a unary operator
    (unary $name:ident { $( $p:pat => $e:expr ),+ } ) => {
        #[allow(unreachable_patterns)]
	fn $name (value: &Value) -> Self::Result {
            // Bringing Value into scope saves us some characters in
            // the invocation.
            use Value::*;
            match value {
                $($p => Self::ok($e)),+ ,
                value => Self::invalid_operand(value)
            }
        }
    };

    // Template for a binary operator
    (binary $name:ident { $( $p:pat => $e:expr ),+ } ) => {
	#[allow(unreachable_patterns)]
        fn $name (a: &Value, b: &Value) -> Self::Result {
            // Bringing Value into scope saves us some characters the
            // invocation.
            use Value::*;
            match (a, b) {
                $($p => Self::ok($e)),+ ,
                (a, b) => Self::invalid_operands(a, b)
            }
        }
    };
}


// Defines behavior of arithmetic and logic operations on values.
//
// Factors out a bunch of tedious and repetitive logic we'd otherwise
// need to implement in slightly different ways.
//
// There could be a parent trait which defines everything down to
// `binary`, leaving out the particular operator implementations.
// That would allow even greater flexibility.
pub trait Operations {
    // An opaque result type, probably a specialization of std::Result.
    type Result;

    // Default implementations call these methods to handle the result
    // of operations.
    fn invalid_operand(t: &Value) -> Self::Result;
    fn invalid_operands(left: &Value, right: &Value) -> Self::Result;
    fn invalid_cast(value: &Value, tt: TypeTag) -> Self::Result;
    fn index_error(collection: &Value, index: &Value) -> Self::Result;
    fn type_mismatch(value: &Value, tt: TypeTag) -> Self::Result;
    fn ok(value: Value) -> Self::Result;

    // Operations on compound types may need to construct new shared
    // values.
    fn new<T: Clone + Debug>(value: T) -> Shared<T>;

    // Get the type of a value.
    fn get_type(value: &Value) -> TypeTag {
	use Value as V;
	use TypeTag as TT;
        match value {
	    V::None          => TT::None,
	    V::Atom(_)       => TT::Atom, // we might want to make each atom a distinct type          
	    V::Type(_)	     => TT::Type,
	    V::Bool(_)	     => TT::Bool,
	    V::Int(_)	     => TT::Int,
	    V::Float(_)	     => TT::Float,
	    V::Str(_)	     => TT::Str,
	    V::Point(_)	     => TT::Point,
	    V::Lambda(_)     => TT::Lambda,
	    V::Closure(_, _) => TT::Closure,
	    V::Tuple(_)	     => TT::Tuple,
	    V::List(_)	     => TT::List,
	    V::Map(_)	     => TT::Map,
	    V::Record(_)     => TT::RecordValue,
	    V::Module(_)     => TT::Module
	}
    }

    // Describes conversions between variants of Value.
    fn coerce(value: &Value, tt: TypeTag) -> Self::Result {
	use Value as V;
	use TypeTag as TT;
        match (value, tt) {
            (V::Bool(_),  TT::Bool)  => Self::ok(value.clone()),
            (V::Bool(v),  TT::Int)   => Self::ok(V::Int(if *v {0} else {1})),
            (V::Int(v),   TT::Bool)  => Self::ok(V::Bool(*v != 0)),
            (V::Int(_),   TT::Int)   => Self::ok(value.clone()),
            (V::Int(v),   TT::Float) => Self::ok(V::Float(OrderedFloat(*v as f64))),
            (V::Float(v), TT::Int)   => Self::ok(V::Int(**v as i64)),
            (V::Float(_), TT::Float) => Self::ok(value.clone()),
            (V::Str(v),   TT::Bool)  => Self::ok(V::Bool(!v.is_empty())),
            (V::List(v),  TT::Bool)  => Self::ok(V::Bool(!v.is_empty())),
            (V::Map(v),   TT::Bool)  => Self::ok(V::Bool(!v.is_empty())),
	    // XXX: * => string coercion
	    // XXX: record <=> map coercion.
	    // XXX: list <=> tuple coercion.
            (a,           b)         => Self::invalid_cast(a, b)
        }
    }

    // True if a value matches the given type tag.
    fn matches(value: &Value, tt: TypeTag) -> bool {
	Self::get_type(value) == tt
    }

    // Dispatch from opcode over all unary operations.
    fn unary(opcode: UnOp, value: &Value) -> Self::Result {
	match opcode {
            UnOp::Not  => Self::not(value),
            UnOp::Neg  => Self::neg(value),
            UnOp::Abs  => Self::abs(value)
	}
    }

    // Try to index into the given object.
    fn index(collection: &Value, index: &Value) -> Self::Result {
	// use Value::*;
	// TBD: implement me
	Self::index_error(collection, index)
    }

    // Dispatch from opcode over all binary operations.
    fn binary(opcode: BinOp, a: &Value, b: &Value) -> Self::Result {
	match opcode {
            BinOp::Add  => Self::add(a, b),
            BinOp::Sub  => Self::sub(a, b),
            BinOp::Mul  => Self::mul(a, b),
            BinOp::Div  => Self::div(a, b),
	    BinOp::Mod  => Self::modulo(a, b),
            BinOp::Pow  => Self::pow(a, b),
            BinOp::And  => Self::bitand(a, b),
            BinOp::Or   => Self::bitor(a, b),
            BinOp::Xor  => Self::bitxor(a, b),
            BinOp::Lt   => Self::lt(a, b),
            BinOp::Gt   => Self::gt(a, b),
            BinOp::Lte  => Self::lte(a, b),
            BinOp::Gte  => Self::gte(a, b),
            BinOp::Eq   => Self::eq(a, b),
            BinOp::Shl  => Self::shl(a, b),
            BinOp::Shr  => Self::shr(a, b),
            BinOp::Min  => Self::min(a, b),
            BinOp::Max  => Self::max(a, b)
	}
    }

    operator! { unary abs {
        Int(value)   => Int(value.abs()),
        Float(value) => Float(ordered_float::OrderedFloat(value.abs()))
    } }

    operator! { unary not {
	Bool(value) => Bool(!value),
	Int(value) => Int(!value)
    } }

    operator! { unary neg {
        Int(value)   => Int(-value),
        Float(value) => Float(-*value)
    } }

    operator! { binary pow {
	// XXX* silent coercion of b to u32, since pow is not implemented
        (Int(a),   Int(b))   => Int(a.pow(*b as u32)),
        (Float(a), Float(b)) => Float(a.powf(**b).into())
    } }

    operator! { binary min {
        (Int(a),   Int(b))   => Int(*a.min(b)),
        (Float(a), Float(b)) => Float(*a.min(b))
    } }

    operator! { binary max {
        (Int(a),   Int(b))   => Int(*a.max(b)),
        (Float(a), Float(b)) => Float(*a.max(b))
    } }

    operator! { binary add {
        (Int(a),   Int(b))   => Int(*a + *b),
        (Float(a), Float(b)) => Float(*a + *b)
    } }

    operator! { binary sub {
        (Int(a),   Int(b))   => Int(*a - *b),
        (Float(a), Float(b)) => Float(*a - *b)
    } }

    operator! { binary mul {
        (Int(a),   Int(b))   => Int(a * b),
        (Float(a), Float(b)) => Float((*a) * (*b))
    } }

    operator! { binary div {
        (Int(a),   Int(b))   => Int(a / b),
        (Float(a), Float(b)) => Float(*a / *b)
    } }

    operator! { binary modulo {
        (Int(a),   Int(b))   => Int(a % b),
        (Float(a), Float(b)) => Float(*a % *b)
    } }

    operator! { binary bitand {
        (Bool(a), Bool(b)) => Bool(a & b),
        (Int(a),  Int(b))  => Int(a & b)
    } }

    operator! { binary bitor {
        (Bool(a), Bool(b)) => Bool(a | b),
        (Int(a),  Int(b))  => Int(a | b)
    } }

    operator! { binary bitxor {
        (Bool(a), Bool(b)) => Bool(a ^ b),
        (Int(a),  Int(b))  => Int(a ^ b)
    } }

    operator! { binary shl {
	(Int(a), Int(b)) => Int(a << b)
    } }

    operator! { binary shr {
	(Int(a), Int(b)) => Int(a >> b)
    } }

    operator! { binary lt {
        (Int(a),   Int(b))   => Bool(a < b),
        (Float(a), Float(b)) => Bool(a < b),
        (Str(a),   Str(b))   => Bool(a < b)
    } }

    operator! { binary gt {
        (Int(a),   Int(b))   => Bool(a > b),
        (Float(a), Float(b)) => Bool(a > b),
        (Str(a),   Str(b))   => Bool(a > b)
    } }

    operator! { binary lte {
        (Int(a),   Int(b))   => Bool(a <= b),
        (Float(a), Float(b)) => Bool(a <= b),
        (Str(a),   Str(b))   => Bool(a <= b)
    } }

    operator! { binary gte {
        (Int(a),   Int(b))   => Bool(a >= b),
        (Float(a), Float(b)) => Bool(a >= b),
        (Str(a),   Str(b))   => Bool(a >= b)
    } }

    operator! { binary eq {
        (None,           None)           => Bool(true),
        (Atom(a),        Atom(b))        => Bool(a == b),
        (Type(a),        Type(b))        => Bool(a == b),
        (Bool(a),        Bool(b))        => Bool(a == b),
        (Int(a),         Int(b))         => Bool(a == b),
        (Float(a),       Float(b))       => Bool(a == b),
        (Str(a),         Str(b))         => Bool(a == b),
        (Point(a),       Point(b))       => Bool(a == b),
        (Lambda(a),      Lambda(b))      => Bool(a == b),
        (Tuple(a),       Tuple(b))       => Bool(a == b),
        (List(a),        List(b))        => Bool(a == b),
        (Map(a),         Map(b))         => Bool(a == b),
        (Record(a),      Record(b))      => Bool(a == b),
        (Module(a),      Module(b))      => Bool(a == b),
        _                                => Bool(false)
    } }
}


/* Compiler ******************************************************************/


// Compiles an AST to IR.
//
// This uses the recursive visitor pattern to construct output
// incrementally.
pub struct Compiler {
    atoms: HashSet<Shared<str>>,
    blocks: Vec<Vec<Instruction>>,
}


impl Compiler {
    // Create a new Compiler.
    pub fn new() -> Self {
	Self {
	    atoms: HashSet::new(),
	    blocks: Vec::new(),
	}
    }

    // Try to convert the AST to IR.
    pub fn compile_program(&mut self, prog: Program) -> Result<IR> {
	
	eprintln!("compile program");

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
	decls: Seq<Shared<Statement>>,
	input: TypeNode,
	output: TypeNode,
	body: Seq<Shared<Statement>>
    ) -> Result<Executable> {
	
	eprintln!("compile script");

	self.blocks.push(Vec::new());
	for d in decls {
	    self.compile_statement(&d)?;
	}

	self.blocks.push(Vec::new());
	for statement in body {
	    self.compile_statement(&statement)?;
	}

	let block1 = Block {
	    code: self.blocks[1].clone(),
	    args: Vec::new(),
	    rets: 0
	};

	let block0 = Block {
	    code: self.blocks[0].clone(),
	    args: Vec::new(),
	    rets: 0
	};

	Ok(Executable {
	    desc,
	    input: input,
	    output: output,
	    code: vec![block0, block1]
	})
    }

    // Try to compile a libray to an IR module.
    pub fn compile_library(
	&mut self,
	desc: String,
	_decls: Vec<StmtNode>
    ) -> Result<Module> {
	
	eprintln!("compile library");

	/* 
	self.scopes.push_scope()?;
	for d in decls {
	    self.compile_statement(&d)?;
	}
	let _ = self.scopes.pop_capture_list()?;
	 */
	Ok(Module {desc, exports: Vec::new()})
    }

    // Try to compile a statement into IR.
    pub fn compile_statement(&mut self, statement: &StmtNode) -> Result<()> {
	use ast::Statement::*;
		
	eprintln!("compile statement: {:?}", statement);

	match statement.deref() {
	   Import(_)           => Error::not_implemented("module imports"),
	   Export(_)           => Error::not_implemented("module exports"),
	   ExprForEffect(expr) => self.compile_expr_for_effect(expr),
	   Out(expr)           => self.compile_out(expr),
	   Def(id, val)        => self.compile_def(id, val),
	   TypeDef(id, t)      => self.compile_typedef(id, t),
	   ListIter(i, l, b)   => self.compile_iter(&[i], l, b),
	   MapIter(k, v, m, b) => self.compile_iter(&[k, v], m, b),
	   Suppose(c, b, l)    => self.compile_subjunctive(c, b, l),
	   EffectCapture       => self.emit(Instruction::Capture(CaptureOp::Send))
	}
    }

    // Compile a statement which just invokes an expression for its effects.
    pub fn compile_expr_for_effect(&mut self, expr: &ExprNode) -> Result<()> {
	eprintln!("compile expr_for_effect");
	self.compile_expr(expr)
    }

    // Try to compile an ouptut instruction.
    pub fn compile_out(&mut self, expr: &ExprNode) -> Result<()> {
	eprintln!("compile out {:?}", expr);
	self.compile_expr(expr)?;
	self.emit(Instruction::Out)
    }

    // Try to compile a lexical binding
    //
    // AKA "let", "func", "proc". There are several syntactic forms
    // which result in a Def() node being inserted.
    pub fn compile_def(&mut self, id: &str, expr: &ExprNode) -> Result<()> {
	let atom = self.compile_atom(id);

	eprintln!("compile def {:?} {:?}", id, atom);

	// We need to place the expression onto the stack.
	self.compile_expr(expr)?;

	// Store it in the current scope under the appropriate atom.
	// XXX: handle 16-bit atom id overflow
	self.emit(Instruction::Store(atom))
    }

    // Similar to above, but we expect a type node instead.
    pub fn compile_typedef(&mut self, _id: &str, _expr: &TypeNode) -> Result<()> {
	Error::not_implemented("local typedefs")
    }

    // Try to compile a for loop over a collection
    pub fn compile_iter(
	&mut self,
	itervars: &[&str],
	collection: &ExprNode,
	body: &ExprNode
    ) -> Result<()> {
	// First, compile the collection expression
	self.compile_expr(collection)?;

	// Add type information to the plain values. We just treat
	// these as Any for now.
	//
	// XXX: this should be derived from the underlying collection
	// somehow, or else explicitly included in the grammar.
	let any = Shared::new(ast::TypeTag::Any);
	let args = itervars
	    .iter()
	    .cloned()
	    .map(|iv| (iv.to_string(), any.clone()))
	    .collect::<Vec<(String, TypeNode)>>();
	
	self.compile_lambda(
	    // The given iterator ids are converted to the lambda args
	    //
	    // XXX: the type should be the element type of the
	    // collection, whatever that ends up being.
	    args.as_slice(),
	    // XXX: For now we only support the statement form of iteration.
	    // But we have to eventually support the expression form.
	    &Shared::new(ast::TypeTag::Void),
	    // Body is just passed as given.
	    body
	)?;
	self.emit(Instruction::Call(CallType::ForEach))
	    
    }

    // Compile the capture expression of a subjunctive statement.
    //
    // This wraps the expression in capture-stack instructions to
    // expose the effects as data.
    pub fn compile_capture_expr(&mut self, expr: &ExprNode) -> Result<()> {
	self.emit(Instruction::Capture(CaptureOp::Push))?;
	// XXX: Assumption here: expr has type void
	self.compile_expr(expr)?;
	self.emit(Instruction::Capture(CaptureOp::Pop))
    }

    // Try to copile a subjunctive
    pub fn compile_subjunctive(
	&mut self,
	cond: &ExprNode,
	branch: &ExprNode,
	leaf: &ExprNode
    ) -> Result<()> {
	self.compile_capture_expr(cond)?;
	let elipsis = self.compile_atom("...");
	// XXX: This will store into the current scope, which means at most
	// one subjunctive per scope, or we'll get redefinition errors
	// at runtime..
	//
	// This really should be compiled like a block, i.e. as an
	// IIFE, so that the ... is bound in a sub-scope.
	self.emit(
	    /* XXX: assumption: ...  is invalid identifier, therefore,
	    will never collide with a valid identifier. */
	    Instruction::Store(elipsis)
	)?;
	self.compile_variable_reference("...")?;
	self.emit(Instruction::Capture(CaptureOp::Test))?;
	self.compile_basic_block(branch)?;
	self.compile_basic_block(leaf)?;
	self.emit(Instruction::Call(CallType::IfElse))
    }

    // Dispatch to each expression variant.
    //
    // XXX: everything in this class could take nodes by move, the
    // trouble is that we can't move out of Rc, and the AST used RC
    // everywhere. IF there is a way to consume an Rc to get its inner
    // value, I would love to know. Then we could avoid incrementing
    // the ref count. We would consume the AST to produce the IR,
    // avoiding allocations and refcounts. That feels like a win.
    pub fn compile_expr(&mut self, expr: &ExprNode) -> Result<()> {
	use ast::Expr::*;

	eprintln!("compile expr {:?}", expr);

	// Do NOT include a wildcard match in this block, it breaks
	// exhaustivity analysis.
	match expr.deref() {
	    Void                    => Ok(()),
	    Bool(b)                 => self.compile_const(Value::Bool(*b)),
	    Int(i)                  => self.compile_const(Value::Int(*i)),
	    Float(f)                => self.compile_const(Value::Float(*f)),
	    Str(s)                  => self.compile_const(Value::Str(s.clone())),
	    Point(x, y)             => self.compile_const(Value::Point(Shared::new((*x, *y)))),
	    This                    => Error::not_implemented("self"),
	    In                      => self.emit(Instruction::In),
	    Partial                 => self.emit(Instruction::Placeholder),
	    List(items)             => self.compile_list(items),
	    Map(items)              => self.compile_map(items),
	    Id(id)                  => self.compile_variable_reference(id),
	    Dot(_, _)               => Error::not_implemented("fixed index"),
	    Has(_, _)               => Error::not_implemented("member test"),
	    Index(_, _)             => Error::not_implemented("computed index"),
	    Cond(conds, default)    => self.compile_conds(conds, default),
	    Block(_, _)             => self.compile_block_expr(expr),
	    BinOp(op, l, r)         => self.compile_bin(*op, l, r),
	    UnOp(op, operand)       => self.compile_un(*op, operand),
	    Call(func, args)        => self.compile_call(func, args),
	    Lambda(args, ret, body) => self.compile_lambda(args, ret, body)
	}?;

	Ok(())
    }

    // Find or create an atom for the given &str.
    pub fn compile_atom(&mut self, val: &str) -> Atom {
	if let Some(atom) = self.atoms.get(val) {
	    atom.clone()
	} else {
	    let ret = Atom::from(val);
	    self.atoms.insert(ret.clone());
	    ret
	}
    }

    // Try to compile a literal / constant value.
    //
    // The value will be added to the data section if not already
    // present, and a `Const` instruction placed in the output.
    pub fn compile_const(&mut self, val: Value) -> Result<()> {
	self.emit(Instruction::Const(val))
    }

    // Compile a list literal.
    //
    //
    // Somewhat obnoxiously, these have to be constructed on the
    // stack, since they are allowed to contain arbitrary expressions,
    // and they can't be mutated.
    //
    // The hope is that subsequent optimization passes will handle
    // collecting these into constant values -- but if the value of
    // any element depends on the input record, we still have to fall
    // Somewhat obnoxiously, these have to be constructed on the
    // stack, since they are allowed to contain arbitrary expressions,
    // and they can't be mutated.
    pub fn compile_list(&mut self, items: &[ExprNode]) -> Result<()> {
	for item in items {
	    self.compile_expr(item)?;
	}

	if items.len() < 0xFFFF {
	    self.emit(Instruction::Lcons(items.len() as u16))
	} else {
	    Error::not_implemented("List literal with more than 2^16 items")
	}
    }    
    
    // Compile a map literal.
    //
    // Somewhat obnoxiously, these have to be constructed on the
    // stack, since they are allowed to contain arbitrary expressions,
    // and they can't be mutated.
    //
    // The hope is that subsequent optimization passes will handle
    // collecting these into constant values -- but if the value of
    // any element depends on the input record, we still have to fall
    // back to stack construction.
    pub fn compile_map(&mut self, items: &HashMap<String, ExprNode>) -> Result<()> {
	for (key, value) in items {
	    self.compile_const(Value::Str(key.clone()))?;
	    self.compile_expr(value)?;
	}

	if items.len() < 0xFFFF {
	    self.emit(Instruction::Mcons(items.len() as u16))
	} else {
	    Error::not_implemented("Map literal with > than 2^16 items")
	}
    }

    // Try to compile an if-else chain.
    //
    // The conditions are compiled directly to the output stream, the
    // control flow blocks become lambda values invoked by conditional
    // calls.
    pub fn compile_conds(
	&mut self,
	conds: &[(ExprNode, ExprNode)],
	default: &ExprNode
    ) -> Result<()> {

	eprintln!("compile conds");

	// Compile the if, elif blocks as (cond) (block) Call(If)
	let last = conds.len() - 1;
	for (cond, action) in conds[..last].iter() {

	    eprintln!("compile if/elif: {:?}, {:?}", cond, action);

	    self.compile_expr(cond)?;
	    self.compile_basic_block(action)?;
	    self.emit(Instruction::Call(CallType::If))?;
	}

	// Compile the Last item as (Cond) (block) (block) Call(IfElse)
	let (cond, action) = &conds[last];

	eprintln!("compile else: {:?}, {:?}", cond, action);

	self.compile_expr(cond)?;
	self.compile_basic_block(action)?;
	self.compile_basic_block(default)?;
	self.emit(Instruction::Call(CallType::IfElse))
    }

    // Try to compile a binary operator.
    pub fn compile_bin(
	&mut self,
	op: BinOp,
	l: &ExprNode,
	r: &ExprNode
    ) -> Result<()> {

	eprintln!("compile bin: {:?} {:?}, {:?}", op, l, r);

	self.compile_expr(l)?;
	self.compile_expr(r)?;
	self.emit(Instruction::Bin(op))
    }

    // Try to compile a unary operator.
    pub fn compile_un(&mut self, op: UnOp, operand: &ExprNode) -> Result<()> {

	eprintln!("compile bin: {:?}, {:?}", op, operand);

	self.compile_expr(operand)?;
	self.emit(Instruction::Un(op))
    }

    // Try to compile a function call
    //
    // Args are placed on the stack in order.
    //
    // foo(1, 2, 3) => [1 2 3 <foo> call]
    pub fn compile_call(
	&mut self,
	func: &ExprNode,
	args: &[ExprNode]
    ) -> Result<()> {

	eprintln!("compile call: {:?}, {:?}", func, args);

	for arg in args {
	    self.compile_expr(arg)?;
	}
	self.compile_expr(func)?;
	self.emit(Instruction::Call(CallType::Always))
    }

    // Compile a nested expression block.
    //
    // This gets wrapped into a lambda which is immediately called, in
    // order for lexical scoping to work correctly.
    //
    // If you know javascript, this is equivalent to an IIFE.
    pub fn compile_block_expr(
	&mut self,
	expr: &ExprNode
    ) -> Result<()> {

	eprintln!("compile block expr: {:?}", expr);

	self.compile_basic_block(expr)?;
	self.emit(Instruction::Call(CallType::Always))
    }

    // Compile an arbitrary expression to a lambda taking no arguments.
    //
    // These are used as the targets of control flow instructions.
    pub fn compile_basic_block(&mut self, body: &ExprNode) -> Result<()> {
	eprintln!("compile basic block: {:#?}", body);
	self.compile_lambda(&[], /* XXX */ &Node::new(ast::TypeTag::Void), body)
    }

    // Compile a function value.
    pub fn compile_lambda(
	&mut self,
	args: &[(String, TypeNode)],
	ret: &TypeNode,
	body: &ExprNode
    ) -> Result<()> {
	eprintln!("compile lambda block: {:?}, {:?}, {:?}", args, ret, body);

	let args = args
	    .iter()
	    .map(|(name, _)| self.compile_atom(name))
	    .collect();

	// Create a code block to hold the lambda's code.
	self.blocks.push(Vec::new());

	// Compile the body.
	//
	// The special case here is if expr is a block, then we don't
	// want this to recurse into compile_block_expr, which would
	// ultimately call compile_lambda in an infinite loop.
	//
	// We just want to compile the statements and return
	// expression directly into the lambda block.
	match body.deref() {
	    Expr::Block(stmts, ret) => {
		for statement in stmts {
		    self.compile_statement(statement)?;
		}
		self.compile_expr(&ret)?;
	    },
	    // But for any other expression variant, we *do* just
	    // recurse into compile_expr.
	    _ => self.compile_expr(body)?,
	};

	// Pop our code and capture list off their respective stacks.
	let code = self.blocks.pop().expect("Block stack underflow");
	let rets = match ret.deref() {
	    ast::TypeTag::Void => 0,
	    // XXX: may need to multiple return values at some point,
	    // for example, to support destructuring assignments. For
	    // now this is either going to be zero or one.
	    _ => 1
	};

	// Now this is key: the lambda is a *literal value*, so it
	// actually becomes a `Const(x)` in the output instruction
	// stream.
	//
	// The actual lambda value we just constructed gets stored in
	// the data section.
	self.compile_const(Value::Lambda(Shared::new(Block {
	    code, args, rets
	})))
    }

    // Compile a variable reference to a load instruction.
    pub fn compile_variable_reference(&mut self, id: &str) -> Result<()> {
	// XXX: keep a stack of hash-set of argument and local
	// definitions for closure capture analysis.
	//
	// If we know which ids are captured, then we can wrap them
	// up along with the closure if it escapes.
	let atom = self.compile_atom(id);
	self.emit(Instruction::Load(atom))
    }

    // Emit an instruction to the output.
    //
    // This never fails, but, since this is a post-order notation, it
    // is convenient for this method to return a result. This saves us
    // having to end most methods and blocks above with Ok(()).
    pub fn emit(&mut self, inst: Instruction) -> Result<()>{
	let top = self.blocks.len() - 1;
	self.blocks[top].push(inst);
	Ok(())
    }
}


/* Public API ****************************************************************/


// Convenience function to compile file at `path` to IR.
pub fn compile(path: &str) -> IR {
    let mut compiler = Compiler::new();
    compiler.compile_program(parser::parse(path)).expect("Compilation Error")
}


#[cfg(test)]
mod tests {
    use super::*;

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
		code: vec![
		    Block {
			code: vec![],
			args: vec![],
			rets: 0,
		    }, Block {
			code: vec![Const(Value::Str("Hello, ".to_string())), In, Bin(Add), Out],
			args: vec![],
			rets: 0,
		    }
		]
	    })
	);
    }
}
