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


/* ScopeChain datastructure **************************************************/


// Provides a one-way association between a name and arbitrary value,
// two-way association between names and numerical indices, and
// facilities for collecting the results.
//
// The AST uses strings as symbolic identifiers for variable
// references. This IR is stack-based, and uses numerical
// indexes.
//
// This data-structure helps perform this transformation by letting us
// associate variable names to both an arbtirary value (type
// information) *and* its numerical index in the output code.
//
// It models a "stack of stacks", where the inner stack is referred to
// as a "scope". The interface presented is itself stack-based.


// A location on the stack for captures
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct CapturePath {
    pub frame: u8,
    pub index: u8,
}

// XXX:
//
// ScopeChain needs to be a stack of Scope, Where Scope is
//
// struct Scope {
//   arguments: BiVec<String>
//   locals: BiVec<String>
//   values: HashMap<String, T>
// }
//
// ArgType is just Local or Capture.
// Arg and Locals share same index space.
//
// We need to eliminate the "double scope push" in the compiler, since our
// scope indices will be off.
//
// 

pub struct ScopeChain<T> {
    scopes: Vec<(BiVec<String>, HashMap<String, T>)>
}

impl<T: Clone + Debug> ScopeChain<T> {
    // Create a new scope chain.
    pub fn new() -> ScopeChain<T> {
	ScopeChain {
	    scopes: Vec::new()
	}
    }

    // Return a reference to the current scope.
    pub fn top(&self) -> &(BiVec<String>, HashMap<String, T>) {
	&self.scopes[self.scopes.len() - 1]
    }

    // Return a mutable reference to the current scope.
    pub fn top_mut(&mut self) -> &mut (BiVec<String>, HashMap<String, T>) {
	let len = self.scopes.len() - 1;
	&mut self.scopes[len]
    }

    // Allocate a new scope.
    pub fn push(&mut self) -> Result<()> {
	if self.scopes.len() < 256 {
	    self.scopes.push((BiVec::new(), HashMap::new()));
	    Ok(())
	} else {
	    Error::too_many_arguments()
	}
    }

    // Remove and return the current scope.
    pub fn pop(&mut self) -> Result<(BiVec<String>, HashMap<String, T>)> {
	if let Some(scope) = self.scopes.pop() {
	    Ok(scope)
	} else {
	    Error::internal("Scope chain underflow.")
	}
    }

    // Pop the current scope into an ordered list of values.
    pub fn pop_values(&mut self) -> Result<Vec<T>> {
	let (names, mut values) = self.pop()?;
	Ok(
	    names
		.into_vec()
		.into_iter()
		.map(|name| values.remove(&name).expect("corrupted scope chain"))
		.collect()
	)
    }

    // Pop the current scope into a hashmap from name => value.
    pub fn pop_value_map(&mut self) -> Result<Vec<(String, T)>> {
	let (_, values) = self.pop()?;
	Ok(values.into_iter().collect())
    }
    

    // Add an item, if doesn't already exist, and return the index in
    // the current scope.
    //
    // This item will always be inserted into the top scope, shadowing
    // any with the same name in parent scopes.
    //
    // This can fail if the index overflows the 8-bit argument limit.
    // XXX: argument indices should probably be at least a type alias,
    // in case we decide we need more than 256 arguments.
    pub fn insert(&mut self, id: &str, value: T) -> Result<u8> {
	// XXX: This could probably be optimized. For now it doesn't
	// matter.
	match self.try_insert(id, value) {
	    Ok(index) => Ok(index),
	    Err(Error::Duplicate(_)) => Ok(self.get_path(id)?.index),
	    err => err
	}
    }

    // Like insert, but will error if the key already exists.
    pub fn try_insert(&mut self, id: &str, value: T) -> Result<u8> {
	let (indices, values) = self.top_mut();

	println!("{:?} {:?}", id, value);

	if indices.len() <= 256 {
	    let index = indices.try_push(id.to_string())?;
	    values.insert(id.to_string(), value);
	    Ok(index as u8)
	} else {
	    Error::too_many_arguments()
	}
    }

    // Get the path of `id`, wherever it happens to be in the scope chain.
    pub fn get_path(&self, id: &str) -> Result<CapturePath> {
	// XXX: slightly annoying to have to make a copy for a look-up
	// it's due to BiVec being generic over T, and not specialized
	// for `String` / borrowing.
	let id = id.to_string();

	for (scope, (indices, values)) in self.scopes.iter().rev().enumerate() {
	    if values.contains_key(&id) {
		return Ok(CapturePath {
		    frame: scope as u8,
		    index: indices.index(&id.to_string())? as u8,
		})
	    }
	}

	Error::not_found(id)
    }

    // Try to find the value associated with `id` in this scope chain.
    pub fn get(&self, id: &str) -> Result<T> {
	// XXX: slightly annoying to have to make a copy for a look-up
	// it's due to BiVec being generic over T, and not specialized
	// for `String` / borrowing.
	let id = id.to_string();

	for (_, values) in self.scopes.iter().rev() {
	    if let Some(value) = values.get(&id) {
		return Ok(value.clone());
	    }
	}

	Error::not_found(id)
    }

    // Get the current length of the current scope
    pub fn len(&self) -> Result<u8> {
	// XXX: top and top_mut() should take return a result.
	Ok(self.top().0.len() as u8)
    }
}


/* IR Instruction Set ********************************************************/


// Abstract address of a value in memory somewhere. Keeping small
// because it's used as an instruction immediate.
pub type Addr = u16;


// This represents the entire instruction set.
//
// You can think of this IR as a minimal instruction set over rich
// types.
//
// This is flatter than I would like, but the goal is to avoid the
// enum bloat that can happen with deeply-nested enumerations in
// Rust.
//
// Since we directly execute vectors of these instructions, it is
// important to keep the size of this value as small as practical. We
// do this by making sure that any inner enumeration is `[repr(u8)]`
// here.
//
// One way the above manifests is the decision to separate Const from
// Load.
//
// XXX: Somewhere we should be able to static assert that
// sizeof(Instruction) <= 64-bits.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Instruction {
    Const(Addr),        // Load a value from the const table.
    Load(ArgType, u8),  // Load a local variable, argument, or catpure.
    Store(u8),          // Store a value as a local variable.
    Un(UnOp),           // Wraps all unary arithmetic and logic operations.
    Bin(BinOp),         // Wraps all binary arithmetic and logic operations.
    Call(CallType),     // Unconditional, conditional, and iterative.
    In,                 // Place input record on stack.
    Out,                // Send top of stack to output.
    Debug,              // Inspect top of stack without altering it.
    Drop(u8),           // Discard values.
    Dup(u8),            // Copy values.
    Swap(u8, u8),       // Exchange two stack indices.
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
//
// The number of 
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CallType {
    Always,
    If,
    IfElse,
    // XXX: TBD
    // Iterative forms.
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


// There is a single Load instruction for loading data onto the stack.
// This enum specifies the different address spaces from which we can load
// values.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ArgType {
    Local,
    Arg,
    Capture
}


// We can trap in two ways: recoverable, and fatal.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TrapType {
    Exception,
    Fatal
}


// A linear sequence of instructions.
//
// The block is the basic unit of control flow.
pub type Block = Vec<Instruction>;


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
    Type(TypeTag),
    Bool(bool),
    Int(i64),
    Float(Float),
    // XXX: box strings, but not right this second.
    Str(String),
    // Values below are heavy-weight enough to be boxed.
    Point(Shared<Point>),
    Lambda(Shared<Lambda>),
    Closure(Shared<Lambda>, Shared<Seq<Value>>),
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
    Method(Shared<Lambda>),
    StaticValue(Value),
    StaticMethod(Shared<Lambda>)
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
pub struct Lambda {
    pub code: Block,
    pub args: u8,
    pub locals: u8,
    pub rets: u8,
    pub captures: Vec<CapturePath>
}


impl Lambda {
    // Return the arity of the call instruction according to its type.
    pub fn arity(&self, ct: CallType) -> u8 {
	match ct {
	    CallType::Always => 1 + self.args,
	    CallType::If     => 2 + self.args,
	    CallType::IfElse => 3 + self.args,
	}
    }

    // Perhaps surprisingly, some operations are supported on
    // functions.
    //
    // In particular functions can be concatenated with each other,
    // and with instructions. This is used to implement partial
    // evaluation.
    //
    // XXX: TBD thunk folding, see issue #37
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
    pub data: Seq<Value>,
    // Code blocks with special meaning. For now this is limited to
    // the library / script entry points.
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
    data: BiVec<Value>,
    blocks: Vec<Block>,
    scopes: ScopeChain<ArgType>,
    captures: ScopeChain<CapturePath>,
}


impl Compiler {
    // Create a new Compiler.
    pub fn new() -> Self {
	Self {
	    data: BiVec::new(),
	    blocks: Vec::new(),
	    scopes: ScopeChain::new(),
	    captures: ScopeChain::new()
	}
    }

    // Try to convert the AST to IR.
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
	decls: Seq<Shared<Statement>>,
	input: TypeNode,
	output: TypeNode,
	body: Seq<Shared<Statement>>
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
	let _ = self.scopes.pop_value_map()?;
	Ok(Module {desc, exports: Vec::new()})
    }

    // Try to compile a statement into IR.
    pub fn compile_statement(&mut self, statement: &StmtNode) -> Result<()> {
	use ast::Statement::*;
	match statement.deref() {
	   Import(_)           => Error::not_implemented("module imports"),
	   Export(_)           => Error::not_implemented("module exports"),
	   ExprForEffect(expr) => self.compile_expr_for_effect(expr),
	   Out(expr)           => self.compile_out(expr),
	   Def(id, val)        => self.compile_def(id, val),
	   TypeDef(id, t)      => self.compile_typedef(id, t),
	   ListIter(_, _, _)   => Error::not_implemented("list iteration"),
	   MapIter(_, _, _, _) => Error::not_implemented("map iteration"),
	   While(_, _)         => Error::not_implemented("while loops"),
	   Suppose(_, _, _)    => Error::not_implemented("subjunctives"),
	   EffectCapture       => Error::not_implemented("effect captures")
	}
    }

    // Compile a statement which just invokes an expression for its effects.
    pub fn compile_expr_for_effect(&mut self, expr: &ExprNode) -> Result<()> {
	self.compile_expr(expr)
    }

    // Try to compile an ouptut instruction.
    pub fn compile_out(&mut self, expr: &ExprNode) -> Result<()> {
	self.compile_expr(expr)?;
	self.emit(Instruction::Out)
    }

    // Try to compile a lexical binding
    //
    // AKA "let", "func", "proc". There are several syntactic forms
    // which result in a Def() node being inserted.
    pub fn compile_def(&mut self, id: &str, expr: &ExprNode) -> Result<()> {
	// We need to convert the local name to an argument index.
	//
	// Do this first so that recursive function calls can at least
	// find their own name on the stack.
	//
	// Duplicate definitions are an error, so we use try_insert
	// rather than insert.
	let index = self.scopes.try_insert(id, ArgType::Local)?;

	// We need to place the expression representing the value of
	// the binding onto the stack.
	self.compile_expr(expr)?;

	// This instruction shuffles the top of stack into the correct
	// place in the stack frame. This is needed because a let
	// binding can occur anywhere in the body.
	self.emit(Instruction::Store(index))
    }

    // Similar to above, but we expect a type node instead.
    pub fn compile_typedef(&mut self, _id: &str, _expr: &TypeNode) -> Result<()> {
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
	    Point(x, y)             => self.compile_const(Value::Point(Shared::new((*x, *y)))),
	    This                    => Error::not_implemented("self"),
	    In                      => self.emit(Instruction::In),
	    Partial                 => self.emit(Instruction::Placeholder),
	    List(_)                 => Error::not_implemented("list literal"),
	    Map(_)                  => Error::not_implemented("map literal"),
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

    // Try to compile a literal / constant value.
    //
    // The value will be added to the data section if not already
    // present, and a `Const` instruction placed in the output.
    pub fn compile_const(&mut self, val: Value) -> Result<()> {
	// XXX: handle address overflow.
	let addr = self.data.push(val) as Addr;
	self.emit(Instruction::Const(addr))
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
	// Compile the if, elif blocks as (cond) (block) Call(If)
	let last = conds.len() - 1;
	for (cond, action) in conds[..last].iter() {
	    self.compile_expr(cond)?;
	    self.compile_basic_block(action)?;
	    self.emit(Instruction::Call(CallType::If))?;
	}

	// Compile the Last item as (Cond) (block) (block) Call(IfElse)
	let (cond, action) = &conds[last];
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
	self.compile_expr(l)?;
	self.compile_expr(r)?;
	self.emit(Instruction::Bin(op))
    }

    // Try to compile a unary operator.
    pub fn compile_un(&mut self, op: UnOp, operand: &ExprNode) -> Result<()> {
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
	self.compile_basic_block(expr)?;
	self.emit(Instruction::Call(CallType::Always))
    }

    // Compile an arbitrary expression to a lambda taking no arguments.
    //
    // These are used as the targets of control flow instructions.
    pub fn compile_basic_block(&mut self, body: &ExprNode) -> Result<()> {
	match body.deref() {
	    Expr::Block(_, _) => Ok(()),
	    _ => Error::internal("not a block!")
	}?;
	self.compile_lambda(&[], /* XXX */ &Node::new(ast::TypeTag::Any), body)
    }

    // Compile a function value.
    pub fn compile_lambda(
	&mut self,
	args: &[(String, TypeNode)],
	ret: &TypeNode,
	body: &ExprNode
    ) -> Result<()> {
	// First we put the formal parameters, if any, into scope.
	self.scopes.push()?;
	for (id, _) in args {
	    // Duplicate parameter names would be an error, hence
	    // try_insert.
	    self.scopes.try_insert(id, ArgType::Arg)?;
	}

	// Push the capture scope.
	self.captures.push()?;

	// Create a code block to hold the lambda's code.
	self.blocks.push(Block::new());

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
	let captures = self.captures.pop_values()?;
	let locals = self.scopes.pop()?.0.len() as u8;
	let args = args.len() as u8;

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
	self.compile_const(Value::Lambda(Shared::new(Lambda {
	    code, args, locals, rets, captures
	})))
    }

    // Compile a variable reference to a load instruction.
    pub fn compile_variable_reference(&mut self, id: &str) -> Result<()> {
	let path = self.scopes.get_path(id)?;

	match (path.frame, path.index) {
	    (0, index) => self.emit(Instruction::Load(ArgType::Local, index)),
	    (1, index) => self.emit(Instruction::Load(ArgType::Arg, index)),
	    _ => {
		// Captures need special handling.
		//
		// The positional index of the argument at runtime is
		// its index in the capture list, not its path in the
		// lexical scope chain.
		//
		// The paths are used at runtime when the lambda is
		// placed on the stack.
		//
		// The capture may or may not already be defined,
		// which is fine -- captures appear more than once --
		// so we use insert, rather than try_insert.
		let position = self.captures.insert(id, path)?;
		self.emit(Instruction::Load(ArgType::Capture, position))
	    }
	}
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
		data: vec![Value::Str("Hello, ".to_string())],
		code: vec![vec![], vec![Const(0), In, Bin(Add), Out]]
	    })
	);
    }
}
