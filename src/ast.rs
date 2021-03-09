use std::collections::HashMap;
//use std::collections::HashSet;
use std::rc::Rc;
use std::ops::Deref;


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
// Associative list macro for slices of named pairs with python-like
// syntax.
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

// Convenience Type Aliases
//
// It's actually a bit tedious to work directly with the
// enumerations. Calls to `Node::new()`, `.to_string()`, and `clone()`
// proliferate everywhere, the enum variants can't live in the
// top-level namespace, and etc.
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
//
// Don't use the enumerations directly to build an AST. Use the
// Builder API in this file to build an AST. When writing a visitor,
// it is okay to use the raw enums in match patterns.
pub type Node<T> = Rc<T>;
pub type Seq<T> = Vec<Node<T>>;
pub type PairSeq<T> = Vec<(Node<T>, Node<T>)>;
pub type AList<T> = Vec<(String, Node<T>)>;
pub type Map<T> = HashMap<String, Node<T>>;
pub type ExprNode = Node<Expr>;
pub type TypeNode = Node<TypeTag>;
pub type StmtNode = Node<Statement>;


// Arithmetic and logic operations
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    // The `Self` keyword, but we can't call it that
    This,
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
    MapExpr(Map<TypeTag>),
    // Same runtime representation as map, but with named fields and
    // method syntax.
    Record(AList<Member>),
    // Anonymous function expressoin.
    Lambda(Seq<TypeTag>, TypeNode),
    // A union of one or more types.
    Union(Seq<TypeTag>),
    // Application of a type constructor.
    TypeCons(TypeNode, Seq<TypeTag>),
    // Definition of a type constructor.
    TypeFunc(Seq<String>, TypeNode)
}


// ADT For record fields
#[derive(Clone, Debug, PartialEq)]
pub enum Member {
    Field(TypeNode),
    Method(AList<TypeTag>, TypeNode, ExprNode),
    StaticValue(TypeNode, ExprNode),
    StaticMethod(AList<TypeTag>, TypeNode, ExprNode)
}


// ADT for values
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Void,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Point(f64, f64),
    This, // the lowercase `self` keyword, but we can't call it that.
    List(Seq<Expr>),
    Map(Map<Expr>),
    Id(String),
    Dot(ExprNode, String),
    Index(ExprNode, ExprNode),
    Cond(PairSeq<Expr>, ExprNode),
    Block(Seq<Statement>, ExprNode),
    BinOp(BinOp, ExprNode, ExprNode),
    UnOp(UnOp, ExprNode),
    Call(ExprNode, Seq<Expr>),
    Lambda(AList<TypeTag>, TypeNode, ExprNode)
}


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


// ADT for effects and structure
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    ExprForEffect(ExprNode),
    Emit(String, Seq<Expr>),
    Def(String, ExprNode),
    TypeDef(String, TypeNode),
    ListIter(String, ExprNode, StmtNode),
    MapIter(String, String, ExprNode, StmtNode),
    While(ExprNode, StmtNode),
}


// ADT for programs
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub description: String,
    pub params: HashMap<String, (TypeTag, String)>,
    pub code: Seq<Statement>
}


// This type would be unnecessary but for the need to abstract over
// memory management patterns.
//
// Concretely, this struct is used for ast node re-use.
pub struct Builder {
    // Singleton values that can be used directly
    pub void:  ExprNode,
    pub this:  ExprNode,
    pub t_void:  TypeNode,
    pub t_none:  TypeNode,
    pub t_bool:  TypeNode,
    pub t_int:   TypeNode,
    pub t_float: TypeNode,
    pub t_point: TypeNode,
    pub t_str:   TypeNode,
    pub t_any:   TypeNode,
    pub t_this:  TypeNode,
}


// This api abstracts over memory management for the underlying
// enumerations.
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
	    this    : Node::new(Expr::This),
	    t_void  : Node::new(TypeTag::Void),
	    t_none  : Node::new(TypeTag::None),
	    t_bool  : Node::new(TypeTag::Bool),
	    t_int   : Node::new(TypeTag::Int),
	    t_float : Node::new(TypeTag::Float),
	    t_point : Node::new(TypeTag::Point),
	    t_str   : Node::new(TypeTag::Str),
	    t_any   : Node::new(TypeTag::Any),
	    t_this  : Node::new(TypeTag::This)
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
	self.subexpr(Expr::Float(value))
    }

    pub fn s(&self, value: &str) -> ExprNode {
	self.subexpr(Expr::Str(String::from(value)))
    }

    pub fn point(&self, x: f64, y: f64) -> ExprNode {
	self.subexpr(Expr::Point(x, y))
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

    pub fn cond(
	&self,
	conds: &[(ExprNode, ExprNode)],
	// XXX: Maybe use Option<ExprNode> here
	else_: ExprNode
    ) -> ExprNode {
	let conds = conds.iter().cloned().collect();
	self.subexpr(Expr::Cond(conds, else_))
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
    ) -> StmtNode {
	let mut args = args.to_vec();
	// Construct a closure using block and append to arglist.
	args.push(self.lambda(&[], self.t_void.clone(), delegate));
	self.expr_for_effect(self.call(func, args.as_slice()))
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

    pub fn map_expr(&self, fields: &[(&str, TypeNode)]) -> TypeNode {
	let fields = fields
	    .iter()
	    .map(|field| (field.0.to_string(), field.1.clone()))
	    .collect();
	self.type_(TypeTag::MapExpr(fields))
    }

    pub fn record(&self, members: &[(&str, Member)]) -> TypeNode {
	let members = members
	    .iter()
	    // XXX: This useage of Node::new is acceptable for now.
	    .map(|m| (m.0.to_string(), Node::new(m.1.clone())))
	    .collect();
	self.type_(TypeTag::Record(members))
    }

    pub fn field<'a>(&self, name: &'a str, t: TypeNode) -> (&'a str, Member) {
	(name, Member::Field(t))
    }

    pub fn method<'a, 'b>(
	&self,
	name: &'a str,
	args: &'b [(&'b str, TypeNode)],
	ret: TypeNode,
	body: ExprNode
    ) -> (&'a str, Member) {
	(name, Member::Method(alist(args), ret, body))
    }
    
    pub fn static_val<'a>(
	&self,
	name: &'a str,
	t: TypeNode,
	v: ExprNode
    ) -> (&'a str, Member) {
	(name, Member::StaticValue(t, v))
    }

    pub fn static_method<'a, 'b>(
	&self,
	name: &'a str,
	args: &'b [(&'b str, TypeNode)],
	ret: TypeNode,
	body: ExprNode
    ) -> (&'a str, Member) {
	(name, Member::StaticMethod(alist(args), ret, body))
    }

    pub fn t_lambda(&self, fields: &[TypeNode], ret: TypeNode) -> TypeNode {
	self.type_(TypeTag::Lambda(fields.iter().cloned().collect(), ret))
    }

    pub fn union(&self, ts: &[TypeNode]) -> TypeNode {
	self.type_(TypeTag::Union(ts.iter().cloned().collect()))
    }

    // TBD: TypeCons, TypeFunc... these will probably change from
    // their current form.

    // ** Statements here ***

    pub fn expr_for_effect(&self, expr: ExprNode) -> StmtNode {
	match expr.deref() {
            Expr::Block(stmts, node) => match node.deref() {
		Expr::Void => if stmts.len() == 1 {
                    stmts[0].clone()
		} else {
                    Node::new(Statement::ExprForEffect(expr))
		},
		_ => Node::new(Statement::ExprForEffect(expr))
            },
            _ => Node::new(Statement::ExprForEffect(expr))
	}
    }

    pub fn emit(&self, name: &str, exprs: &[ExprNode]) -> StmtNode {
	let exprs = exprs
	    .iter()
	    .cloned()
	    .collect();
	Node::new(Statement::Emit(name.to_string(), exprs))
    }

    pub fn def(&self, name: &str, expr: ExprNode) -> StmtNode {
	Node::new(Statement::Def(name.to_string(), expr))
    }

    pub fn typedef(&self, name: &str, t: TypeNode) -> StmtNode {
	Node::new(Statement::TypeDef(name.to_string(), t))
    }

    // XXX: So far as I can tell this only gets used in the tests in parser.rs.
    //
    // It is really just a short-hand around cond clauses. I believe
    // the statement variants of cond were actually factored out of
    // grammar.lalrpop.
    pub fn guard(
	&self,
	clauses: &[(ExprNode, ExprNode)],
	default: Option<StmtNode>
    ) -> StmtNode {
	let default = if let Some(default) = default {
            self.block(&[default], self.void.clone())
	} else {
            self.void.clone()
	};

	self.expr_for_effect(self.cond(clauses, default))
    }

    pub fn list_iter(
	&self,
	name: &str, list: ExprNode,
	body: StmtNode
    ) -> StmtNode {
	Node::new(
	    Statement::ListIter(
		name.to_string(),
		list,
		body
	    )
	)
    }
    
    pub fn map_iter(
	&self,
	key: &str,
	value: &str,
	map: ExprNode,
	body: StmtNode
    ) -> StmtNode {
	Node::new(Statement::MapIter(
            String::from(key),
            String::from(value),
            map,
            body
	))
    }
}
