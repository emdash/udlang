use crate::ast::*;
use crate::env::*;
use std::ops::Deref;


#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    Mismatch(TypeNode, TypeNode),
    NotAList(TypeNode),
    NotAMap(TypeNode),
    NotARecord(TypeNode),
    Undefined(String),
    ListIndexMustBeInt(TypeNode),
    KeyError(Map<TypeTag>, String),
    FieldError(AList<Member>, String),
    NotOneOf(Seq<TypeTag>),
    NotIterable(TypeNode),
    NotCallable(TypeNode),
    ArgError(Seq<TypeTag>, Seq<TypeTag>),
    NotImplemented
}


use TypeError::*;


pub type TypeExpr = core::result::Result<TypeNode, TypeError>;
pub type TypeCheck = core::result::Result<(), TypeError>;


pub struct TypeChecker {
    types: Node<Env<TypeTag>>,
}


// Encapsulates all the state required to type-check a program.
//
// Collects type information to be used in subsequent passes.
impl TypeChecker {
    pub fn new(env: Env<TypeTag>) -> TypeChecker {
        TypeChecker { types: Node::new(env) }
    }

    // Return the narrowest representation of the given set of types.
    //
    // If the sequence is empty, reduces to Void.
    // If the sequence contains exactly one type, returns that type.
    // If the sequence contains multiple types, returns a Union with de-duped     //
    // XXX: see github issue #9
    pub fn narrow(mut types: Seq<TypeTag>) -> TypeNode {
        types.dedup();
        match types.len() {
            0 => Node::new(TypeTag::Void),
            1 => types.pop().unwrap(),
            _ => Node::new(TypeTag::Union(types))
        }
    }

    // Helper function to lookup a type in the given map.
    pub fn map_lookup(fields: &Map<TypeTag>, name: &String) -> TypeExpr {
        if let Some(t) = fields.get(name) {
            Ok(t.clone())
        } else {
            Err(KeyError(fields.clone(), name.clone()))
        }
    }

    // Helper function to lookup a type in a record
    //
    // XXX: see github issue #10
    pub fn record_lookup(&self, members: &AList<Member>, name: &String) -> TypeExpr {
	if let Some(member) = members.iter().find(|m| m.0 == *name) {
	    match &*member.1 {
		Member::Field(tt) => Ok(tt.clone()),
		Member::Method(args, ret, body) => self.eval_lambda(args, ret, body),
		Member::StaticValue(t, _) => Ok(t.clone()),
		Member::StaticMethod(args, ret, body) => self.eval_lambda(args, ret, body)
	    }
	} else {
	    Err(TypeError::FieldError(members.clone(), name.clone()))
	}
    }

    pub fn eval_expr(&self, expr: &Expr) -> TypeExpr {
        match expr {
            Expr::Void               => Ok(Node::new(TypeTag::Void)),
            Expr::Bool(_)            => Ok(Node::new(TypeTag::Bool)),
            Expr::Int(_)             => Ok(Node::new(TypeTag::Int)),
            Expr::Float(_)           => Ok(Node::new(TypeTag::Float)),
            Expr::Str(_)             => Ok(Node::new(TypeTag::Str)),
            Expr::Point(_, _)        => Ok(Node::new(TypeTag::Point)),
	    Expr::This               => self.eval_this(),
            Expr::List(items)        => self.eval_list(items),
            Expr::Map(items)         => self.eval_map(items),
            Expr::Id(name)           => self.eval_id(name),
            Expr::Dot(obj, key)      => self.eval_dot(obj, key),
            Expr::Index(lst, i)      => self.eval_index(lst, i),
            Expr::Cond(cases, def)   => self.eval_cond(cases, def),
            Expr::Block(stmts, ret)  => self.eval_block(stmts, ret),
            Expr::BinOp(op, l, r)    => self.eval_binop(*op, l, r),
            Expr::UnOp(op, operand)  => self.eval_unop(*op, operand),
            Expr::Call(func, args)   => self.eval_call(func, args),
            Expr::Lambda(args, ret, body) => self.eval_lambda(args, ret, body)
        }
    }

    pub fn eval_this(&self) -> TypeExpr {
	Err(TypeError::NotImplemented)
    }

    pub fn eval_list(&self, items: &Seq<Expr>) -> TypeExpr {
        let items: Result<Seq<TypeTag>, TypeError> = items
            .iter()
            .map(|v| self.eval_expr(v))
            .collect();
        Ok(Node::new(TypeTag::List(Self::narrow(items?))))
    }

    pub fn eval_map(&self, fields: &Map<Expr>) -> TypeExpr {
        let fields: Result<Map<TypeTag>, TypeError> = fields
            .iter()
            .map(|(k, v)| Ok((k.clone(), self.eval_expr(v)?)))
            .collect();
        Ok(Node::new(TypeTag::MapExpr(fields?)))
    }

    pub fn eval_id(&self, name: &String) -> TypeExpr {
        let value = self.types.get(name);
        if let Some(type_) = value {
            Ok(type_.clone())
        } else {
            Err(Undefined(name.clone()))
        }
    }

    pub fn eval_dot(&self, obj: &ExprNode, field: &String) -> TypeExpr {
        let obj = self.eval_expr(obj)?;
        match obj.deref() {
	    TypeTag::Map(t) => Ok(t.clone()),
            TypeTag::MapExpr(items) => Self::map_lookup(items, field),
	    TypeTag::Record(members) => self.record_lookup(members, field),
            _ => Err(NotAMap(obj.clone()))
        }
    }

    pub fn eval_block(
        &self,
        stmts: &Seq<Statement>,
        ret: &ExprNode
    ) -> TypeExpr {
        let env = Env::chain(&self.types);
        let sub = TypeChecker::new(env);
        for stmt in stmts {
            sub.check_statement(stmt)?
        }
        sub.eval_expr(ret)
    }

    pub fn eval_index(&self, lst: &ExprNode, index: &ExprNode) -> TypeExpr {
        let lst = self.eval_expr(lst)?;
        let index = self.eval_expr(index)?;

        if index.deref() == &TypeTag::Int {
            match lst.deref() {
                TypeTag::List(item) => Ok(item.clone()),
                _ => Err(NotAList(lst.clone()))
            }
        } else {
            Err(ListIndexMustBeInt(index))
        }
    }

    pub fn eval_cond(
        &self,
        cases: &PairSeq<Expr>,
        default: &ExprNode
    ) -> TypeExpr {
        let conds: Result<Seq<TypeTag>, TypeError> = cases
            .iter()
            .map(|case| Ok(self.eval_expr(&case.0)?.clone()))
            .collect();

        let conds = conds?
            .iter()
            .cloned()
            .find(|type_| type_.deref() != &TypeTag::Bool);

        let exprs: Result<Seq<TypeTag>, TypeError> = cases
            .iter()
            .map(|case| Ok(self.eval_expr(&case.1)?.clone()))
            .collect();

        let mut exprs = exprs?;
        exprs.push(self.eval_expr(&default)?);

        match conds {
            None => Ok(Self::narrow(exprs)),
            Some(wrong_type) => Err(
                Mismatch(wrong_type, Node::new(TypeTag::Bool))
            )
        }
    }

    pub fn eval_binop(
        &self,
        op: BinOp,
        l: &ExprNode,
        r: &ExprNode
    ) -> TypeExpr {
        use TypeTag as TT;
        let l = self.eval_expr(l)?;
        let r = self.eval_expr(r)?;
        match (op, l.deref(), r.deref()) {
            (BinOp::Eq, a, b) if a == b => Ok(Node::new(a.clone())),
            (_, TT::Bool, TT::Bool)   => Ok(Node::new(TT::Bool)),
            (_, TT::Int, TT::Int)     => Ok(Node::new(TT::Int)),
            (_, TT::Float, TT::Float) => Ok(Node::new(TT::Float)),
            (_, TT::Str, TT::Str)     => Ok(Node::new(TT::Float)),
            _                         => Err(Mismatch(l, r))
        }
    }

    pub fn eval_unop(&self, op: UnOp, operand: &ExprNode) -> TypeExpr {
        use TypeTag as TT;
        let type_ = self.eval_expr(operand)?;
        let numeric = Node::new(TT::Union(vec! {
            Node::new(TT::Int),
            Node::new(TT::Float)
        }));
        match (op, type_.deref()) {
            (UnOp::Not, TT::Bool)  => Ok(Node::new(TT::Bool)),
            (UnOp::Not, _)         => Err(Mismatch(type_, Node::new(TT::Bool))),
            (UnOp::Neg, TT::Int)   => Ok(Node::new(TT::Int)),
            (UnOp::Neg, TT::Float) => Ok(Node::new(TT::Float)),
            (UnOp::Neg, _)         => Err(Mismatch(type_, numeric)),
            (UnOp::Abs, TT::Int)   => Ok(Node::new(TT::Int)),
            (UnOp::Abs, TT::Float) => Ok(Node::new(TT::Float)),
            (UnOp::Abs, _)         => Err(Mismatch(type_, numeric))
        }
    }

    fn eval_call(&self, func: &ExprNode, args: &Seq<Expr>) -> TypeExpr {
        let func = self.eval_expr(func)?;
        let args: Result<Seq<TypeTag>, TypeError> = args
            .iter()
            .map(|arg| Ok(self.eval_expr(arg)?))
            .collect();
        let args = args?;

        if let TypeTag::Lambda(aargs, ret) = func.deref() {
            if args == args {
                Ok(ret.clone())
            } else {
                Err(ArgError(args, aargs.clone()))
            }
        } else {
            Err(NotCallable(func))
        }
    }

    pub fn eval_lambda(
        &self,
        args: &AList<TypeTag>,
        ret: &TypeNode,
        body: &ExprNode
    ) -> TypeExpr {
        let env = Env::chain(&self.types);
        env.import(args);
        let sub = TypeChecker::new(env);
        let body_type = sub.eval_expr(body)?;
        if body_type.deref() == ret.deref() {
            Ok(Node::new(TypeTag::Lambda(
                args.iter().map(|arg| arg.1.clone()).collect(),
                ret.clone()
            )))
        } else {
            Err(Mismatch(ret.clone(), body_type))
        }
    }

    // Check whether expr is a list, and return the item type.
    pub fn is_list(&self, expr: &ExprNode) -> TypeExpr {
        let result = self.eval_expr(expr)?;
        match result.deref() {
            TypeTag::List(item_type) => Ok(item_type.clone()),
            _ => Err(NotIterable(result))
        }
    }

    // Check whether expr is convertible to a map, and return the item type.
    pub fn is_map(&self, expr: &ExprNode) -> TypeExpr {
        let result = self.eval_expr(expr)?;
        match result.deref() {
            TypeTag::Map(item_type) => Ok(item_type.clone()),
	    TypeTag::MapExpr(items) => Ok(Self::narrow(
		items.values().cloned().collect())
	    ),
	    _                       => Err(NotIterable(result))
        }
    }

    // Check whether expr is a bool.
    pub fn is_bool(&self, expr: &ExprNode) -> TypeCheck {
        let result = self.eval_expr(expr)?;
        match result.deref() {
            TypeTag::Bool => Ok(()),
            _ => Err(Mismatch(result, Node::new(TypeTag::Bool)))
        }
    }

    // Check whether expr is void.
    pub fn is_void(&self, expr: &ExprNode) -> TypeCheck {
        let result = self.eval_expr(expr)?;
        match result.deref() {
            TypeTag::Void => Ok(()),
            _ => Err(Mismatch(result, Node::new(TypeTag::Void)))
        }
    }

    // Type-checking works at the statement level.
    pub fn check_statement(
        &self,
        stmt: &StmtNode
    ) -> TypeCheck {
	// We check each statement by computing its type. If this
	// succeeds, depedning on the type of statement, we check it
	// against what we expect.
        match stmt.deref() {
            Statement::ExprForEffect(body) => {
                self.is_void(body)?;
            },
            Statement::Emit(_op, exprs) => {
                for expr in exprs {
                    self.eval_expr(expr)?;
                }
            },
            Statement::Def(name, val) => {
                self.types.define(name, &self.eval_expr(val)?);
            },
	    Statement::TypeDef(name, t) => {
		self.types.define(name, &t);
	    },
            Statement::ListIter(iter, lst, body) => {
                let item = self.is_list(lst)?;
                let env = Env::chain(&self.types);
                let sub = TypeChecker::new(env);
                sub.types.define(iter, &item);
                sub.check_statement(body)?;
            },
            Statement::MapIter(k, v, map, body) => {
                // TODO: raise proper error, rather than crashing.
                assert!(k != v, "cannot be the same");
                let item = self.is_map(map)?;
                let env = Env::chain(&self.types);
                let sub = TypeChecker::new(env);
                sub.types.define(k, &Node::new(TypeTag::Str));
                sub.types.define(v, &item);
                sub.check_statement(body)?;
            },
            Statement::While(cond, body) => {
                self.is_bool(cond)?;
                self.check_statement(body)?;
            },
        };
        Ok(())
    }

    // Entry point to the type checker.
    //
    // It will return only the first type error, rather than all of them.
    //
    // Ideally this would be the only public method, and ideally it
    // would return a list of all the type errors in the program. But
    // the current implementation is fail-fast, because it's easier.
    //
    // If type-checking succeeds, then for now the result is just
    // Ok(()).  Eventually, this should return some useful summary of
    // the type information we collected to be used for code-gen.
    pub fn check_program(&self, prog: Program) -> TypeCheck {
        for stmt in prog.code {
            self.check_statement(&stmt)?;
        }
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[allow(unused_imports)]
    #[macro_use]
    use crate::ast;
    #[allow(unused_imports)]
    #[macro_use]
    use crate::ast::*;

    // Assert that expr evaluated in env has type t.
    fn assert_types_to(env: Env<TypeTag>, expr: &Expr, t: &TypeExpr) {
	let tc = TypeChecker::new(env);
	assert_eq!(tc.eval_expr(expr), *t);
    }

    // Works like map!, but constructs an Env instead.
    //
    // Env has a mutable API, so this can't really share code with
    // map.  This could be a function, but I don't mind having this
    // extra little bit of sugar.
    macro_rules! env (
        ( $( $id:expr => $v:expr),* ) => { {
            let env = Env::root();
            $( env.define(&$id.to_string(), &($v)); )*
	    env
	} }
    );

    #[test]
    fn test_simple() {
	let ast = Builder::new();

	let expr = ast.map(&[
            ("foo", ast.i(42)),
            ("bar", ast.s("baz")),
            ("quux", ast.list(&[ast.i(1), ast.i(2), ast.i(3)]))
        ]);

	let t = ast.map_expr(&[
            ("foo", ast.t_int.clone()),
            ("bar", ast.t_str.clone()),
            ("quux", ast.t_list(ast.t_int.clone()))
	]);
    
        assert_types_to(Env::root(), &expr, &Ok(t));
    }

    #[test]
    fn test_list() {
	let ast = Builder::new();

        assert_types_to(
            Env::root(),
            &ast.list(&[ast.i(42), ast.i(3), ast.i(4)]),
            &Ok(ast.t_list(ast.t_int.clone()))
        );

	assert_types_to(
	    Env::root(),
	    &ast.list(&[ast.f(42.0), ast.f(3.0), ast.f(4.0)]),
	    &Ok(ast.t_list(ast.t_float.clone()))
	);

	assert_types_to(
	    Env::root(),
	    &ast.list(&[ast.i(42), ast.f(2.0), ast.s("foo")]),
	    &Ok(ast.t_list(
		ast.union(&[
		    ast.t_int.clone(),
		    ast.t_float.clone(),
		    ast.t_str.clone()]))
	    )
	);
    }

    #[test]
    fn test_id() {
	let ast = Builder::new();

	assert_types_to(
	    env! {"foo" => ast.t_int.clone()},
	    &ast.id("foo"),
	    &Ok(ast.t_int.clone())
	);

	assert_types_to(
	    env! {"foo" => ast.t_int.clone()},
	    &ast.id("bar"),
	    &Err(Undefined("bar".to_string()))
	);
    }

    #[test]
    fn test_dot() {
	let ast = Builder::new();

        assert_types_to(
            env! {"x" => ast.map_expr(&alist! {"foo" => ast.t_str.clone()})},
            &ast.dot(ast.id("x"), "foo"),
            &Ok(ast.t_str.clone())
        );

	assert_types_to(
            env! {"x" => ast.map_expr(&alist! {"foo" => ast.t_str.clone()})},
            &ast.dot(ast.id("x"), "bar"),
            &Err(KeyError(map! {"foo" => ast.t_str.clone()}, "bar".to_string()))
        );
	
	assert_types_to(
            env! {"x" => ast.map_expr(&alist! {"foo" => ast.t_str.clone()})},
            &ast.dot(ast.i(42), "bar"),
            &Err(NotAMap(ast.t_int.clone()))
        );

	assert_types_to(
            env! {"x" => ast.map_expr(&alist! {
		"foo" => ast.map_expr(&alist! {
		    "bar" => ast.t_int.clone()
		})
	    })},
	    &ast.dot(ast.dot(ast.id("x"), "foo"), "bar"),
	    &Ok(ast.t_int.clone())
	);

	assert_types_to(
            env! {"x" => ast.map_expr(&alist! {
		"foo" => ast.map_expr(&alist! {
		    "bar" => ast.t_int.clone()
		})
	    })},
	    &ast.dot(ast.dot(ast.id("x"), "foo"), "baz"),
	    &Err(
		KeyError(
		    map! {"bar" => ast.t_int.clone()},
		    "baz".to_string()
		)
	    )
	);
    }


    #[test]
    fn test_list_iter() {
	let ast = Builder::new();

        let tc = TypeChecker::new(
            env!{"x" => ast.t_list(ast.t_str.clone())}
        );

        let statement = Node::new(ast.list_iter(
            "i",
            ast.id("x"),
            ast.emit("show_text", &[ast.id("i")])
        ));

        assert_eq!(tc.check_statement(&statement), Ok(()));

        let statement = Node::new(ast.list_iter(
            "i",
            ast.id("x"),
            ast.expr_for_effect(ast.block(&[], ast.id("i")))
        ));

        assert_eq!(
            tc.check_statement(&statement),
            Err(Mismatch(ast.t_str.clone(), ast.t_void.clone()))
        );
    }


    #[test]
    fn test_map_iter() {
	let ast = Builder::new();
        let tc = TypeChecker::new(
            env!{"x" => ast.map_expr(
		&(alist!{"x" => ast.t_str.clone()})
	    )}
        );

        let statement = Node::new(ast.map_iter(
            "k",
            "v",
            ast.id("x"),
            ast.emit("show_text", &[ast.id("v")])
        ));

        assert_eq!(tc.check_statement(&statement), Ok(()));

        let statement = Node::new(ast.map_iter(
            "k",
            "v",
            ast.id("x"),
            ast.expr_for_effect(ast.block(&[], ast.id("v")))
        ));

        assert_eq!(
            tc.check_statement(&statement),
            Err(Mismatch(ast.t_str.clone(), ast.t_void.clone()))
        );
    }


    #[test]
    fn test_lambda() {
        use crate::ast::BinOp::*;
	let ast = Builder::new();
	
        assert_types_to(
            Env::root(),
            &ast.lambda(
		&[("x", ast.t_int.clone())],
		ast.t_int.clone(),
                ast.bin(Add, ast.id("x"), ast.i(4))
            ),
            &Ok(ast.t_lambda(&[ast.t_int.clone()], ast.t_int.clone()))
        );
    }
}
