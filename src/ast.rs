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


// Abstract over various memory management strategies.
//
// Simplest solution to issue #7: we define a Node type and related
// containers. This allows us to change strategies, though a lot of
// code will probably break if the API doesn't look like `std::Box` or
// `sd::Rc`.
pub type Node<T> = Rc<T>;
pub type Seq<T> = Vec<Node<T>>;
pub type PairSeq<T> = Vec<(Node<T>, Node<T>)>;
pub type AList<T> = Vec<(String, Node<T>)>;
pub type Map<T> = HashMap<String, Node<T>>;


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
    Option(Node<TypeTag>),
    // Fixed-sized list of heterogenous types.
    Tuple(Seq<TypeTag>),
    // Variable-length list of single type.
    List(Node<TypeTag>),
    // Maps string keys to values of a single type.
    Map(Node<TypeTag>),
    // Internal type for holding a map literal, which converts to map
    // or record, depending on context.
    MapExpr(Map<TypeTag>),
    // Same runtime representation as map, but with named fields and
    // method syntax.
    Record(AList<Member>),
    // Anonymous function expressoin.
    Lambda(Seq<TypeTag>, Node<TypeTag>),
    // A union of one or more types.
    Union(Seq<TypeTag>),
    // Application of a type constructor.
    TypeCons(Node<TypeTag>, Seq<TypeTag>),
    // Definition of a type constructor.
    TypeFunc(Seq<String>, Node<TypeTag>)
}


// ADT For record fields
#[derive(Clone, Debug, PartialEq)]
pub enum Member {
    Field(Node<TypeTag>),
    Method(AList<TypeTag>, Node<TypeTag>, Node<Expr>),
    StaticValue(Node<TypeTag>, Node<Expr>),
    StaticMethod(AList<TypeTag>, Node<TypeTag>, Node<Expr>)
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
    Dot(Node<Expr>, String),
    Index(Node<Expr>, Node<Expr>),
    Cond(PairSeq<Expr>, Node<Expr>),
    Block(Seq<Statement>, Node<Expr>),
    BinOp(BinOp, Node<Expr>, Node<Expr>),
    UnOp(UnOp, Node<Expr>),
    Call(Node<Expr>, Seq<Expr>),
    Lambda(AList<TypeTag>, Node<TypeTag>, Node<Expr>)
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
    ExprForEffect(Node<Expr>),
    Emit(String, Seq<Expr>),
    Def(String, Node<Expr>),
    TypeDef(String, Node<TypeTag>),
    ListIter(String, Node<Expr>, Node<Statement>),
    MapIter(String, String, Node<Expr>, Node<Statement>),
    While(Node<Expr>, Node<Statement>),
}


// XXX: Should there be a a wrapped type equivalent for the underlying
// enums? Or is it better not to hide the Node<> wrapping?
pub type SubExpr = Node<Expr>;
type TypeExpr = Node<TypeTag>;


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
    pub void:  SubExpr,
    pub this:  SubExpr,
    pub t_void:  TypeExpr,
    pub t_none:  TypeExpr,
    pub t_bool:  TypeExpr,
    pub t_int:   TypeExpr,
    pub t_float: TypeExpr,
    pub t_point: TypeExpr,
    pub t_str:   TypeExpr,
    pub t_any:   TypeExpr,
    pub t_this:  TypeExpr,
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
    fn subexpr(&self, expr: Expr) -> SubExpr {
	Node::new(expr)
    }

    // Like above, but for types
    //
    // XXX: same fixme.
    fn type_(&self, t: TypeTag) -> Node<TypeTag> {
	Node::new(t)
    }

    // Like above but for statements
    //
    // XXX: same fixme
    pub fn statement(&self, s: Statement) -> Node<Statement> {
	Node::new(s)
    }

    // ** Every variant in Statement, Expr, etc gets a short-hand
    // method in this section **
    //
    // In general, these should return a value wrapped in Node<> which
    // has been memoized where possible.
    //
    // *** There should be no usage of Node::new() below this line! ***

    pub fn b(&self, value: bool) -> SubExpr {
	self.subexpr(Expr::Bool(value))
    }

    pub fn i(&self, value: i64) -> SubExpr {
	self.subexpr(Expr::Int(value))
    }

    pub fn f(&self, value: f64) -> SubExpr {
	self.subexpr(Expr::Float(value))
    }

    pub fn s(&self, value: &str) -> SubExpr {
	self.subexpr(Expr::Str(String::from(value)))
    }

    pub fn point(&self, x: f64, y: f64) -> SubExpr {
	self.subexpr(Expr::Point(x, y))
    }

    pub fn list(&self, value: &[SubExpr]) -> SubExpr {
	self.subexpr(Expr::List(value.iter().cloned().collect()))
    }

    pub fn map(&self, value: &[(&str, SubExpr)]) -> SubExpr {
	let value = value
	    .iter()
	    .map(|v| (String::from(v.0), v.1.clone()))
	    .collect();

	self.subexpr(Expr::Map(value))
    }

    pub fn id(&self, value: &str) -> SubExpr {
	self.subexpr(Expr::Id(value.to_string()))
    }

    pub fn dot(&self, lhs: SubExpr, field: &str) -> SubExpr {
	self.subexpr(Expr::Dot(lhs.clone(), field.to_string()))
    }

    pub fn index(&self, lhs: SubExpr, rhs: SubExpr) -> SubExpr {
	self.subexpr(Expr::Index(lhs.clone(), rhs))
    }

    pub fn cond(
	&self,
	conds: &[(SubExpr, SubExpr)],
	// XXX: Maybe use Option<SubExpr> here
	else_: SubExpr
    ) -> SubExpr {
	let conds = conds.iter().cloned().collect();
	self.subexpr(Expr::Cond(conds, else_))
    }

    pub fn block(
	&self,
	statements: &[Node<Statement>],
	retval: SubExpr
    ) -> SubExpr {
	let statements = statements.iter().cloned().collect();
	self.subexpr(Expr::Block(statements, retval))
    }

    pub fn bin(&self, op: BinOp, lhs: SubExpr, rhs: SubExpr) -> SubExpr {
	self.subexpr(Expr::BinOp(op, lhs, rhs))
    }

    pub fn un(&self, op: UnOp, operand: SubExpr) -> SubExpr {
	self.subexpr(Expr::UnOp(op, operand))
    }

    pub fn call(&self, callee: SubExpr, args: &[SubExpr]) -> SubExpr {
	self.subexpr(Expr::Call(callee, args.iter().cloned().collect()))
    }
    
    pub fn template_call(
	&self,
	func: SubExpr,
	args: &[SubExpr],
	delegate: SubExpr
    ) -> Node<Statement> {
	let mut args = args.to_vec();
	// Construct a closure using block and append to arglist.
	args.push(self.lambda(&[], self.t_void.clone(), delegate));
	self.expr_for_effect(self.call(func, args.as_slice()))
    }

    pub fn template(
	&self,
	args: &[(&str, Node<TypeTag>)],
	delegate: &str,
	body: SubExpr
    ) -> SubExpr {
	let mut args = args.to_vec();
	// Append the delegate closure argument to args.
	args.push((delegate, self.t_lambda(&[], self.t_void.clone())));
	self.lambda(args.as_slice(), self.t_void.clone(), body)
    }

    pub fn lambda(
	&self,
	args: &[(&str, Node<TypeTag>)],
	ret: Node<TypeTag>,
	body: SubExpr
    ) -> SubExpr {
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

    pub fn type_name(&self, name: &str) -> TypeExpr {
	self.type_(TypeTag::TypeName(name.to_string()))
    }

    pub fn option(&self, t: TypeExpr) -> TypeExpr {
	self.type_(TypeTag::Option(t))
    }

    pub fn tuple(&self, tys: &[TypeExpr]) -> TypeExpr {
	self.type_(TypeTag::Tuple(tys.iter().cloned().collect()))
    }

    pub fn t_list(&self, t: TypeExpr) -> TypeExpr {
	self.type_(TypeTag::List(t))
    }

    pub fn t_map(&self, value_type: TypeExpr) -> TypeExpr {
	self.type_(TypeTag::Map(value_type))
    }

    pub fn map_expr(&self, fields: &[(&str, TypeExpr)]) -> TypeExpr {
	let fields = fields
	    .iter()
	    .map(|field| (field.0.to_string(), field.1.clone()))
	    .collect();
	self.type_(TypeTag::MapExpr(fields))
    }

    pub fn record(&self, members: &[(&str, Member)]) -> TypeExpr {
	let members = members
	    .iter()
	    // XXX: This useage of Node::new is acceptable for now.
	    .map(|m| (m.0.to_string(), Node::new(m.1.clone())))
	    .collect();
	self.type_(TypeTag::Record(members))
    }

    pub fn field<'a>(&self, name: &'a str, t: TypeExpr) -> (&'a str, Member) {
	(name, Member::Field(t))
    }

    pub fn method<'a, 'b>(
	&self,
	name: &'a str,
	args: &'b [(&'b str, TypeExpr)],
	ret: TypeExpr,
	body: SubExpr
    ) -> (&'a str, Member) {
	(name, Member::Method(alist(args), ret, body))
    }
    
    pub fn static_val<'a>(
	&self,
	name: &'a str,
	t: TypeExpr,
	v: SubExpr
    ) -> (&'a str, Member) {
	(name, Member::StaticValue(t, v))
    }

    pub fn static_method<'a, 'b>(
	&self,
	name: &'a str,
	args: &'b [(&'b str, TypeExpr)],
	ret: TypeExpr,
	body: SubExpr
    ) -> (&'a str, Member) {
	(name, Member::StaticMethod(alist(args), ret, body))
    }

    pub fn t_lambda(&self, fields: &[TypeExpr], ret: TypeExpr) -> TypeExpr {
	self.type_(TypeTag::Lambda(fields.iter().cloned().collect(), ret))
    }

    pub fn union(&self, ts: &[TypeExpr]) -> TypeExpr {
	self.type_(TypeTag::Union(ts.iter().cloned().collect()))
    }

    // TBD: TypeCons, TypeFunc... these will probably change from
    // their current form.

    // ** Statements here ***

    pub fn expr_for_effect(&self, expr: SubExpr) -> Node<Statement> {
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

    pub fn emit(&self, name: &str, exprs: &[SubExpr]) -> Node<Statement> {
	let exprs = exprs
	    .iter()
	    .cloned()
	    .collect();
	Node::new(Statement::Emit(name.to_string(), exprs))
    }

    pub fn def(&self, name: &str, expr: SubExpr) -> Node<Statement> {
	Node::new(Statement::Def(name.to_string(), expr))
    }

    pub fn typedef(&self, name: &str, t: TypeExpr) -> Node<Statement> {
	Node::new(Statement::TypeDef(name.to_string(), t))
    }

    // XXX: So far as I can tell this only gets used in the tests in parser.rs.
    //
    // It is really just a short-hand around cond clauses. I believe
    // the statement variants of cond were actually factored out of
    // grammar.lalrpop.
    pub fn guard(
	&self,
	clauses: &[(SubExpr, SubExpr)],
	default: Option<Node<Statement>>
    ) -> Node<Statement> {
	let default = if let Some(default) = default {
            self.block(&[default], self.void.clone())
	} else {
            self.void.clone()
	};

	self.expr_for_effect(self.cond(clauses, default))
    }

    pub fn list_iter(
	&self,
	name: &str, list: SubExpr,
	body: Node<Statement>
    ) -> Node<Statement> {
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
	map: SubExpr,
	body: Node<Statement>
    ) -> Node<Statement> {
	Node::new(Statement::MapIter(
            String::from(key),
            String::from(value),
            map,
            body
	))
    }
}
