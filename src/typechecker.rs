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
	    _ => panic!("unpossible")
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

    pub fn eval_expr(&self, expr: &Expr) -> TypeExpr {
        match expr {
            Expr::Void               => Ok(Node::new(TypeTag::Void)),
            Expr::Bool(_)            => Ok(Node::new(TypeTag::Bool)),
            Expr::Int(_)             => Ok(Node::new(TypeTag::Int)),
            Expr::Float(_)           => Ok(Node::new(TypeTag::Float)),
            Expr::Str(_)             => Ok(Node::new(TypeTag::Str)),
	    Expr::In                 => self.eval_in(),
	    Expr::Partial            => Err(TypeError::NotImplemented),
            Expr::List(items)        => self.eval_list(items),
            Expr::Map(items)         => self.eval_map(items),
            Expr::Id(name)           => self.eval_id(name),
            Expr::Dot(obj, key)      => self.eval_dot(obj, key),
            Expr::Index(lst, i)      => self.eval_index(lst, i),
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

    pub fn eval_in(&self) -> TypeExpr {
	Err(TypeError::NotImplemented)
    }

    pub fn eval_list(&self, items: &Seq<Expr>) -> TypeExpr {
        let items: Result<Seq<TypeTag>, TypeError> = items
            .iter()
            .map(|v| self.eval_expr(v))
            .collect();
        Ok(Node::new(TypeTag::List(Self::narrow(items?))))
    }

    pub fn eval_map(&self, _fields: &Map<Expr>) -> TypeExpr {
	Err(TypeError::NotImplemented)
    }

    pub fn eval_id(&self, name: &String) -> TypeExpr {
        let value = self.types.get(name);
        if let Some(type_) = value {
            Ok(type_.clone())
        } else {
            Err(Undefined(name.clone()))
        }
    }

    pub fn eval_dot(&self, obj: &ExprNode, _field: &String) -> TypeExpr {
        let obj = self.eval_expr(obj)?;
        match obj.deref() {
	    TypeTag::Map(t) => Ok(t.clone()),
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
	// XXX: this is wrong, should be union of int and float, but
	// types are being redefined.
        let numeric = Node::new(TT::Int);
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
	    Statement::Import(_) => Err(TypeError::NotImplemented)?,
	    Statement::Export(exp) => match exp {
		Export::Decl(decl) => self.check_statement(decl),
		Export::Name(_) => Err(TypeError::NotImplemented)?
	    }?,
            Statement::Def(name, val) => {
                self.types.define(name, &self.eval_expr(val)?);
            },
	    Statement::TypeDef(name, t) => {
		self.types.define(name, &t);
	    },
        };
        Ok(())
    }

    fn check_script_body(&self, body: &ExprNode, output: &TypeNode) -> TypeCheck {
	let result_type = self.eval_expr(body)?;
	if result_type == *output {
	    Ok(())
	} else {
	    Err(Mismatch(output.clone(), result_type))
	}
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
	match prog {
	    Program::Script { desc: _, decls, input: _, output, body } => {
		for statement in decls {
		    self.check_statement(&statement)?;
		}
		self.check_script_body(&body, &output)?;
	    },
	    Program::Library { desc: _, decls } => {
		for statement in decls {
		    self.check_statement(&statement)?;
		}
	    }
	};
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[allow(unused_imports)]
    use crate::ast;
    #[allow(unused_imports)]
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
	//let ast = Builder::new();
	// XXX: tbd
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
