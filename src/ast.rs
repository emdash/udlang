// (C) 2021 Brandon Lewis

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
//use std::ops::Deref;
use std::hash::Hash;


// Associative list macro for slices of named pairs with python-like
// syntax.
//
// Basically converts this:
//
// alist!{"foo" => bar, "baz" => quux}
//
// To this:
//
// [("foo", bar), ("baz", bar)]
//
#[macro_export]
macro_rules! alist(
    { $($key:expr => $value:expr),* } => {
        [ $( ($key, $value)),* ]
    }
);


// Like the above, but implicitly uses String instead of &str, and
// creates a collection instead of a slice.
//
// Basically converts this:
//
// map!{"foo" => bar, "baz" => quux}
//
// To this:
//
// [("foo".to_string(), bar), ("baz".to_string(), bar)]
//
// *and* places the result is placed into a collection.
#[macro_export]
macro_rules! map(
    { $($key:expr => $value:expr),* } => {
        [ $( ($key.to_string(), $value)),* ]
	    .iter()
	    .cloned()
	    .collect()
    }
);


// Collect a slice of pairs into an AList.
//
// Basically, wouldn't be needed but for the distinction between []
// and Vec, &str and string.
//
// Converts this:
//
// &[("foo", bar), ("baz", quux)]
//
// Into this:
//
// vec![("foo".to_string(), bar.clone()), ("baz".to_tring(), quux.clone())]
//
pub fn alist<T: Clone>(items: &[(&str, T)]) -> Vec<(String, T)> {
    items
	.iter()
	.map(|i| (i.0.to_string(), i.1.clone()))
	.collect()
}


// Convenience Type Aliases
//
// Rust allows us to define the AST as an algebraic data type (ADT)
// which is (roughly) isomorphic to the grammar. This means that, the
// underlying data types are enumerations, or lists thereof, which are
// composed to form the syntax tree.
//
// We can let exhaustivity analysis help us keep the catch mistakes as
// the language evolves, and keep the code from descending into a mess
// of type mismatches that has ultimately overtaken similar efforts
// begun in other languages. Frankly, this is one of the main reasons
// for chosing Rust for this project.
//
// Having said all that, I find it's actually a bit tedious to work
// directly with the enumerations. Calls to `Node::new()`,
// `.to_string()`, 'map`, and `clone()` proliferate everywhere, and
// the code ends up looking far more intense than it really needs
// to. It also becomes a pain to switch from, say, Box, to Rc, or some
// *third* kind of allocation strategy.
//
// To abstract over various memory management strategies, and to help
// cut down on syntactic noise and keep the code-base tight, I added
// these type aliases.
//
// Simplest solution to issue #7: we define a Node type, derive a
// bunch of related containers from it. This allows us to change
// strategies, though a lot of code will break if Node doesn't look
// sufficiently like `std::Box` or `sd::Rc`.
//
// I think perhaps factoring this into a trait will make it easier to
// implement bookeeping of source location info (for error reporting).
pub type Node<T> = Rc<T>;
pub type Seq<T> = Vec<Node<T>>;
pub type PairSeq<T> = Vec<(Node<T>, Node<T>)>;
pub type AList<T> = Vec<(String, Node<T>)>;
pub type Map<T> = HashMap<String, Node<T>>;
pub type ExprNode = Node<Expr>;
pub type TypeNode = Node<TypeTag>;
pub type StmtNode = Node<Statement>;
pub type Float = ordered_float::OrderedFloat<f64>;

// *** ADT ********************************************************************

// These are the underlying types which roughly correspond to the
// productions in grammar.lalrpop.
//
// Don't use the enumerations directly to build an AST. Use the
// Builder API in this file instead.
//
// When writing a visitor, it is okay to use the raw enums in match
// patterns, but avoid any `_` default match arms so that exhaustivity
// analysis can play its proper role in catching bugs.
//
// Do use the Builder API for transformations on the syntax tree,
// since this will allow preserving source location information across
// transformations.

// Arithmetic and logic operations
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Xor,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Shl,
    Shr,
    Min,
    Max
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum UnOp {
    Not,
    Neg,
    Abs,
}


// ADT for types
#[derive(Clone, Debug, PartialEq)]
pub enum TypeTag {
    // The bottom type
    Void,
    // The unit type
    None,
    // Primitive Types
    Bool,
    Int,
    Float,
    Str,
    Point,
    // The top type.
    Any,
    // A bound type name.
    TypeName(String),
    // A type which may be None.
    Option(TypeNode),
    // Fixed-sized list of heterogenous types.
    Tuple(Seq<TypeTag>),
    // Variable-length list of single type.
    List(TypeNode),
    // Maps string keys to values of a single type.
    Map(TypeNode),
    // Internal type for holding a map literal, which converts to map
    // or record, depending on context.
    Lambda(Seq<TypeTag>, TypeNode),
    // Application of a type constructor.
    TypeCons(TypeNode, Seq<TypeTag>),
    // Definition of a type constructor.
    TypeFunc(Seq<String>, TypeNode)
}


// ADT for values
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Void,
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(String),
    In,              // The `in` keyword.
    Partial,         // The `$` placeholder.
    List(Seq<Expr>),
    Map(Map<Expr>),
    Id(String),
    Dot(ExprNode, String),
    Index(ExprNode, ExprNode),
    Block(Seq<Statement>, ExprNode),
    BinOp(BinOp, ExprNode, ExprNode),
    UnOp(UnOp, ExprNode),
    Call(ExprNode, Seq<Expr>),
    Lambda(AList<TypeTag>, TypeNode, ExprNode)
}


// ADT for effects and control-flow.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Import(Import),
    Export(Export),
    Def(String, ExprNode),
    TypeDef(String, TypeNode),
}


// Just extract the list of exports here.
#[derive(Clone, Debug, PartialEq)]
pub enum Export {
    Name(String),
    Decl(StmtNode)
}


// An import must either name an entire module or select specific
// items from a module.
#[derive(Clone, Debug, PartialEq)]
pub struct Import {
    root: String,
    selection: Option<ImportSelector>
}


// These are the different ways to select items from a module.
#[derive(Clone, Debug, PartialEq)]
pub enum ImportSelector {
    Itself,
    All,
    Item(String),
    Alias(String, String),
    Group(Vec<ImportSelector>),
    Nested(String, Node<ImportSelector>)
}


// Represents a single source file.
//
// A file has to decide whether it is a script or a library. It cannot
// be both.
#[derive(Clone, Debug, PartialEq)]
pub enum Program {
    Script {
	desc: String,
	decls: Seq<Statement>,
	input: TypeNode,
	output: TypeNode,
	body: ExprNode
    },
    Library {
	desc: String,
	decls: Seq<Statement>,
    }
}


/**** AST Builder API ********************************************************/


// This type is not strictly necessary, but it does cut down on the
// syntactic noise and verbosity when constructing ASTs. It also helps
// with separating implementation concerns (like source location info
// and node sharing to cut down memory usage) from the underlying
// structure of the ADT.
pub struct Builder {
    // Singleton values that can be used directly
    pub void:    ExprNode,
    pub in_:     ExprNode,
    pub partial: ExprNode,
    pub t_void:  TypeNode,
    pub t_none:  TypeNode,
    pub t_bool:  TypeNode,
    pub t_int:   TypeNode,
    pub t_float: TypeNode,
    pub t_point: TypeNode,
    pub t_str:   TypeNode,
    pub t_any:   TypeNode,
    pub exports: RefCell<Vec<Export>>,
    // TBD: hashtables to cache constants, strings, exprs, and
    // statements.
}


// Use this structure to build ASTs.
//
// This api abstracts over memory management and other implementation
// concerns. Roughly speaking, there's a method here for every
// production in the grammar, though it's not perfectly 1-to-1.
//
// It does this by:
// - consistently consuming and returning the wrapped
//   Node<E>, where E is the underlying enumeration.
// - consistently consuming &[Node<E>], where collections are needed.
// - consistently consuming &str, where string values are needed.
//
// Keep this API in sync with the grammar, and the underlying
// enumerations.
impl Builder {
    // This constructor allocates a new Builder, and initializes some
    // leaf nodes that can be trivially shared.
    pub fn new() -> Self {
	Self {
	    void    : Node::new(Expr::Void),
	    in_     : Node::new(Expr::In),
	    partial : Node::new(Expr::Partial),
	    t_void  : Node::new(TypeTag::Void),
	    t_none  : Node::new(TypeTag::None),
	    t_bool  : Node::new(TypeTag::Bool),
	    t_int   : Node::new(TypeTag::Int),
	    t_float : Node::new(TypeTag::Float),
	    t_point : Node::new(TypeTag::Point),
	    t_str   : Node::new(TypeTag::Str),
	    t_any   : Node::new(TypeTag::Any),
	    exports : RefCell::new(Vec::new()),
	}
    }

    // Should return a memoized Rc to a subexpression, but can't
    // because Expr isn't hashable.
    //
    // XXX: Fix this when issue #5 is fixed.
    fn subexpr(&self, expr: Expr) -> ExprNode {
	Node::new(expr)
    }

    // Like above, but for types
    //
    // XXX: same fixme.
    fn type_(&self, t: TypeTag) -> TypeNode {
	Node::new(t)
    }

    // Like above but for statements
    //
    // XXX: same fixme
    pub fn statement(&self, s: Statement) -> StmtNode {
	Node::new(s)
    }

    // ** Every variant in Statement, Expr, etc gets a short-hand
    // method in this section **
    //
    // In general, these should return a value wrapped in Node<> which
    // has been memoized where possible.
    //
    // *** There should be no usage of Node::new() below this line! ***

    pub fn b(&self, value: bool) -> ExprNode {
	self.subexpr(Expr::Bool(value))
    }

    pub fn i(&self, value: i64) -> ExprNode {
	self.subexpr(Expr::Int(value))
    }

    pub fn f(&self, value: f64) -> ExprNode {
	self.subexpr(Expr::Float(value.into()))
    }

    pub fn s(&self, value: &str) -> ExprNode {
	self.subexpr(Expr::Str(String::from(value)))
    }

    pub fn list(&self, value: &[ExprNode]) -> ExprNode {
	self.subexpr(Expr::List(value.iter().cloned().collect()))
    }

    pub fn map(&self, value: &[(&str, ExprNode)]) -> ExprNode {
	let value = value
	    .iter()
	    .map(|v| (String::from(v.0), v.1.clone()))
	    .collect();

	self.subexpr(Expr::Map(value))
    }

    pub fn id(&self, value: &str) -> ExprNode {
	self.subexpr(Expr::Id(value.to_string()))
    }

    pub fn dot(&self, lhs: ExprNode, field: &str) -> ExprNode {
	self.subexpr(Expr::Dot(lhs.clone(), field.to_string()))
    }

    pub fn index(&self, lhs: ExprNode, rhs: ExprNode) -> ExprNode {
	self.subexpr(Expr::Index(lhs.clone(), rhs))
    }

    pub fn block(
	&self,
	statements: &[StmtNode],
	retval: ExprNode
    ) -> ExprNode {
	let statements = statements.iter().cloned().collect();
	self.subexpr(Expr::Block(statements, retval))
    }

    pub fn bin(&self, op: BinOp, lhs: ExprNode, rhs: ExprNode) -> ExprNode {
	self.subexpr(Expr::BinOp(op, lhs, rhs))
    }

    pub fn un(&self, op: UnOp, operand: ExprNode) -> ExprNode {
	self.subexpr(Expr::UnOp(op, operand))
    }

    pub fn call(&self, callee: ExprNode, args: &[ExprNode]) -> ExprNode {
	self.subexpr(Expr::Call(callee, args.iter().cloned().collect()))
    }
    
    pub fn template_call(
	&self,
	func: ExprNode,
	args: &[ExprNode],
	delegate: ExprNode
    ) -> ExprNode {
	let mut args = args.to_vec();
	// Construct a closure using block and append to arglist.
	args.push(self.lambda(&[], self.t_void.clone(), delegate));
	self.call(func, args.as_slice())
    }

    pub fn template(
	&self,
	args: &[(&str, TypeNode)],
	delegate: &str,
	body: ExprNode
    ) -> ExprNode {
	let mut args = args.to_vec();
	// Append the delegate closure argument to args.
	args.push((delegate, self.t_lambda(&[], self.t_void.clone())));
	self.lambda(args.as_slice(), self.t_void.clone(), body)
    }

    pub fn lambda(
	&self,
	args: &[(&str, TypeNode)],
	ret: TypeNode,
	body: ExprNode
    ) -> ExprNode {
	let args = args
	    .iter()
	    .map(|arg| {
		let (name, t) = arg;
		(name.to_string(), t.clone())
	    })
	    .collect();
	self.subexpr(Expr::Lambda(args, ret, body))
    }

    // Every variant in TypeTag gets a method here

    pub fn type_name(&self, name: &str) -> TypeNode {
	self.type_(TypeTag::TypeName(name.to_string()))
    }

    pub fn option(&self, t: TypeNode) -> TypeNode {
	self.type_(TypeTag::Option(t))
    }

    pub fn tuple(&self, tys: &[TypeNode]) -> TypeNode {
	self.type_(TypeTag::Tuple(tys.iter().cloned().collect()))
    }

    pub fn t_list(&self, t: TypeNode) -> TypeNode {
	self.type_(TypeTag::List(t))
    }

    pub fn t_map(&self, value_type: TypeNode) -> TypeNode {
	self.type_(TypeTag::Map(value_type))
    }

    pub fn t_lambda(&self, fields: &[TypeNode], ret: TypeNode) -> TypeNode {
	self.type_(TypeTag::Lambda(fields.iter().cloned().collect(), ret))
    }

    // TBD: TypeCons, TypeFunc... these will probably change from
    // their current form.

    // ** Statements here ***

    pub fn def(&self, name: &str, expr: ExprNode) -> StmtNode {
	self.statement(Statement::Def(name.to_string(), expr))
    }

    pub fn typedef(&self, name: &str, t: TypeNode) -> StmtNode {
	self.statement(Statement::TypeDef(name.to_string(), t))
    }

    // Export an identifier or type name.
    pub fn export(&self, name: &str) -> StmtNode {
	self.statement(Statement::Export(Export::Name(name.to_string())))
    }


    // Wrap the underlying decl as an export.
    pub fn export_decl(&self, decl: StmtNode) -> StmtNode {
	match *decl {
	    Statement::Def(_, _) => (),
	    Statement::TypeDef(_, _) => (),
	    _ => panic!("Unreachable.")
	}
	self.statement(Statement::Export(Export::Decl(decl)))
    }

    pub fn import(
	&self,
	root: &str,
	selection: Option<ImportSelector>
    ) -> StmtNode {
	let root = root.to_string();
	self.statement(Statement::Import(Import {root, selection}))
    }

    pub fn import_item(&self, name: &str) -> ImportSelector {
	ImportSelector::Item(name.to_string())
    }

    pub fn import_alias(&self, name: &str, alias: &str) -> ImportSelector {
	ImportSelector::Alias(name.to_string(), alias.to_string())
    }

    pub fn import_nested(
	&self,
	name: &str,
	sel: ImportSelector
    ) -> ImportSelector {
	// XXX: This use of Node::new() is allowable for now.
	ImportSelector::Nested(name.to_string(), Node::new(sel))
    }

    pub fn import_group(&self, items: &[ImportSelector]) -> ImportSelector {
	ImportSelector::Group(items.to_vec())
    }

    pub fn script(
	&self,
	desc: &str,
	decls: &[StmtNode],
	input: TypeNode,
	output:TypeNode,
	body: ExprNode,
    ) -> Program {
	Program::Script {
	    desc: desc.to_string(),
	    decls: decls.to_vec(),
	    input,
	    output,
	    body: body
	}
    }

    pub fn library(
	&self,
	desc: &str,
	decls: &[StmtNode]
    ) -> Program {
	Program::Library {
	    desc: desc.to_string(),
	    decls: decls.to_vec(),
	}
    }
}
