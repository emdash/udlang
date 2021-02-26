// Mostly just a place to hold the unit tests for the grammar. The
// actual parser is just a wrapper around what LALRPOP has generated for us.
//
// XXX: Currently incomplete.
// 
// The eventual goal here would be to have exhaustive coverage of the
// entire grammar: i.e. each variant of each production, with overlap
// from higher levels to make sure that even odd usages parse as expected.
//
// LALRPOP lets us test each production in isolation, as well as for
// the grammar as a whole.


#[cfg(test)]
mod tests {
    use crate::grammar;
    use crate::ast;
    use crate::ast::*;
    use Expr::*;
    use ast::BinOp::*;
    use ast::UnOp::*;

    // Couple rules of thumb: just because it's ill-formed doesn't
    // mean it isn't a valid test case.
    //
    // Just because it looks weird doesn't mean it's ill-formed.
    //
    // Just because it wouldn't type-check doesn't mean it isn't a
    // valid test case.
    //
    // Just because it doesn't make sense doesn't mean it's not a
    // valid test case.
    
    // Helper functions for test cases.

    fn assert_expr(text: &'static str, ast: Expr) {
        assert_eq!(
            grammar::ExprParser::new().parse(text).unwrap(),
            ast
        );
    }

    fn assert_statement(text: &'static str, ast: Statement) {
        assert_eq!(
            grammar::StatementParser::new().parse(text).unwrap(),
            ast
        );
    }

    fn assert_type(text: &'static str, ast: TypeTag) {
        assert_eq!(
	    grammar::TypeParser::new().parse(text).unwrap(),
            ast
        );
    }

    // Test Parsing of Expressions

    #[test]
    fn test_basic() {
        assert_expr("3 + 4 * 5", bin(
            Add,
            Int(3),
            bin(Mul, Int(4), Int(5))
        ));
        assert_expr("a + 3", bin(Add, Id(s("a")), Int(3)));
        assert_expr("3 * a", bin(Mul, Int(3), id("a")));
        assert_expr("\"foo\"", string("foo"));

    }

    #[test]
    fn test_list() {
        assert_expr("[]", list(vec!{}));
        assert_expr("[3]", list(vec!{Int(3)}));
        assert_expr("[3, 4, 5]", list(vec!{Int(3), Int(4), Int(5)}));
        assert_expr(
            "[3 + 4, 5]",
            list(vec!{bin(Add, Int(3), Int(4)), Int(5)})
        );
    }

    #[test]
    fn test_map() {
        assert_expr("{}", map(vec!{}));
        assert_expr(
            r#"{"foo": 1}"#,
            map(vec!{(s("foo"), Int(1))})
        );

        assert_expr(
            r#"{"foo": 1, "bar": 2}"#,
            map(vec!{
                (s("foo"), Int(1)),
                (s("bar"), Int(2))
            })
        );
    }

    #[test]
    fn test_terms() {
        assert_expr("42", Int(42));
        assert_expr("42.0", Float(42.0));
        assert_expr("(42)", Int(42));
        assert_expr("foo", id("foo"));
        assert_expr("-42", Int(-42));
        assert_expr("-42.0", Float(-42.0));
        assert_expr("-x", un(Neg, id("x")));
        assert_expr("- 42", un(Neg, Int(42)));
        assert_expr("- 42.0", un(Neg, Float(42.0)));
        assert_expr("-(42)", un(Neg, Int(42)));
    }

    #[test]
    fn test_relational() {
        assert_expr("3 + 4 < 3 * 4", bin(
            Lt,
            bin(Add, Int(3), Int(4)),
            bin(Mul, Int(3), Int(4))
        ));

        assert_expr("3 + 4 > 3 * 4", bin(
            Gt,
            bin(Add, Int(3), Int(4)),
            bin(Mul, Int(3), Int(4))
        ));

        assert_expr("3 + 4 <= 3 * 4", bin(
            Lte,
            bin(Add, Int(3), Int(4)),
            bin(Mul, Int(3), Int(4))
        ));

        assert_expr("3 + 4 >= 3 * 4", bin(
            Gte,
            bin(Add, Int(3), Int(4)),
            bin(Mul, Int(3), Int(4))
        ));

        assert_expr("3 + 4 == 3 * 4", bin(
            Eq,
            bin(Add, Int(3), Int(4)),
            bin(Mul, Int(3), Int(4))
        ));
    }

    #[test]
    fn test_logic() {
        assert_expr(
            "x >= lower and x <= upper",
            bin(And,
                bin(Gte, id("x"), id("lower")),
                bin(Lte, id("x"), id("upper"))));

        assert_expr(
            "x >= 3 or x > 4 and x > 5",
            bin(And,
                bin(Or,
                    bin(Gte, id("x"), Int(3)),
                    bin(Gt, id("x"), Int(4))),
                bin(Gt, id("x"), Int(5))));

        assert_expr(
            "x >= 3 or (x > 4 and x > 5)",
            bin(Or,
                bin(Gte, id("x"), Int(3)),
                bin(And,
                    bin(Gt, id("x"), Int(4)),
                    bin(Gt, id("x"), Int(5)))));

        assert_expr(
            "(x >= 3) or (x > 4 and x > 5)",
            bin(Or,
                bin(Gte, id("x"), Int(3)),
                bin(And,
                    bin(Gt, id("x"), Int(4)),
                    bin(Gt, id("x"), Int(5)))));
    }

    #[test]
    fn test_call() {
        assert_expr(
            "foo(a + 3, a and b)",
            call(
                id("foo"),
                vec! { bin(Add, id("a"), Int(3)), bin(And, id("a"), id("b")) }
            )
        );

        assert_expr(
            "x(a or b, y <= 3, y(-g(a * 7)))",
            call(
                id("x"),
                vec! {
                    bin(Or, id("a"), id("b")),
                    bin(Lte, id("y"), Int(3)),
                    call(
                        id("y"),
                        vec!{
                            un(
                                Neg,
                                call(
                                    id("g"),
                                    vec!{ bin(Mul, id("a"), Int(7)) }
                                )
                            )
                        }
                    )
                }
            )
        );
    }

    #[test]
    fn test_dot() {
        assert_expr(
            "foo.bar",
            dot(id("foo"), "bar")
        );

        assert_expr(
            "foo.bar.baz",
            dot(dot(id("foo"), "bar"), "baz")
        );

        assert_expr(
            "foo.bar()",
            call(dot(id("foo"), "bar"), vec! {})
        );
    }

    #[test]
    fn test_index() {
        assert_expr(
            "foo[0]",
            index(id("foo"), Int(0))
        );

        assert_expr(
            "foo[bar]",
            index(id("foo"), id("bar"))
        );

        assert_expr(
            "foo[3 + 10 * 5]",
            index(id("foo"), bin(Add, Int(3), bin(Mul, Int(10), Int(5))))
        );

        assert_expr(
            "foo[3][4]",
            index(index(id("foo"), Int(3)), Int(4))
        );

        assert_expr(
            "foo[3].bar.baz[5][6]",
            index(
                index(
                    dot(
                        dot(
                            index(id("foo"), Int(3)),
                            "bar"
                        ),
                        "baz"
                    ),
                    Int(5)
                ),
                Int(6)
            )
        );

        // XXX: allow parsing foo()[3] identically. For now, language
        // won't support returning function values, only passing them,
        // so this is okay.
        assert_expr(
            "(foo())[3]",
            index(call(id("foo"), vec!{}), Int(3))
        );

        // XXX: see above.
        assert_expr(
            "(foo()).bar",
            dot(call(id("foo"), vec!{}), "bar")
        );
    }

    #[test]
    fn test_simple_statement() {
        assert_statement("out fill;", emit("fill", vec!{}));
        assert_statement(
            "out moveto x, y;",
            emit("moveto", vec!{id("x"), id("y")})
        );

        assert_statement(
            "let y = x * 3 + 4;",
            def("y", bin(Add, bin(Mul, id("x"), Int(3)), Int(4)))
        );
    }

    #[test]
    fn test_expr_block() {
        assert_expr("{let x = y; yield 4}", expr_block(
            vec!{def("x", id("y"))},
            Int(4))
        );

        assert_expr("{let x = frob(y); yield y * 4}", expr_block(
            vec!{def("x", call(id("frob"), vec!{id("y")}))},
            bin(Mul, id("y"), Int(4))
        ));

        assert_expr("{out debug x; yield x}", expr_block(
            vec!{emit("debug", vec!{id("x")})},
            id("x")
        ));

        assert_expr(
            "{let x = {let y = 2; yield y * 3}; yield x}",
            expr_block(
                vec!{
                    def("x",
                        expr_block(
                            vec!{def("y", Int(2))},
                            bin(Mul, id("y"), Int(3))
                        )
                    )
                },
                id("x")
            )
        );
    }

    #[test]
    fn test_list_iter() {
        let test = r#"
              for p in points {
                  out moveto p;
                  out circle 50.0;
              }
        "#;

        assert_statement(test, list_iter("p", id("points"), statement_block(vec!{
            emit("moveto", vec!{id("p")}),
            emit("circle", vec!{Float(50.0)})
        })));
    }


    #[test]
    fn test_map_iter() {
        let test = r#"
              for (k, v) in x {
                  out moveto v, v;
                  out text k;
              }
        "#;

        assert_statement(test, map_iter("k", "v", id("x"), statement_block(vec!{
            emit("moveto", vec!{id("v"), id("v")}),
            emit("text", vec!{id("k")})
        })));
    }

    #[test]
    fn test_if() {
        assert_statement(
            "if (a) { out text b; }",
            guard(
                vec!{(id("a"), emit("text", vec!{id("b")}))},
                None
            )
        );
    }

    #[test]
    fn test_if_else() {
        assert_statement(
            r#"if (a) { out text b; } else { out text "error"; }"#,
            guard(
                vec!{(id("a"), emit("text", vec!{id("b")}))},
                Some(emit("text", vec!{string("error")}))
            )
        );
    }

    #[test]
    fn test_if_elif_else() {
        assert_statement(
            r#"if (a) {
               out text "a";
            } elif (b) {
               out text "b";
            } else {
               out text "error";
            }"#,
            guard(
                vec!{
                    (id("a"), emit("text", vec!{string("a")})),
                    (id("b"), emit("text", vec!{string("b")})),
                },
                Some(emit("text", vec!{string("error")}))
            )
        );

        assert_statement(
            r#"if (a) {
               out text "a";
            } elif (b) {
               out text "b";
            } elif (c) {
               out text "c";
            } else {
               out text "error";
            }"#,
            guard(
                vec!{
                    (id("a"), emit("text", vec!{string("a")})),
                    (id("b"), emit("text", vec!{string("b")})),
                    (id("c"), emit("text", vec!{string("c")})),
                },
                Some(emit("text", vec!{string("error")}))
            )
        );
    }

    #[test]
    fn test_if_elif() {
        assert_statement(
            r#"if (a) {
               out text "a";
            } elif (b) {
               out text "b";
            }"#,
            guard(
                vec!{
                    (id("a"), emit("text", vec!{string("a")})),
                    (id("b"), emit("text", vec!{string("b")})),
                },
                None
            )
        );

        assert_statement(
            r#"if (a) {
               out text "a";
            } elif (b) {
               out text "b";
            } elif (c) {
               out text "c";
            }"#,
            guard(
                vec!{
                    (id("a"), emit("text", vec!{string("a")})),
                    (id("b"), emit("text", vec!{string("b")})),
                    (id("c"), emit("text", vec!{string("c")})),
                },
                None
            )
        );
    }

    fn anon(statements: Vec<Statement>) -> Expr {
        lambda(vec!{}, TypeTag::Void, expr_block(statements, Expr::Void))
    }

    fn tree(
        func: Expr,
        args: Vec<Expr>, trailing: Vec<Statement>
    ) -> Statement {
        let mut args = args;
        args.push(anon(trailing));
        expr_for_effect(call(func, args))
    }

    #[test]
    fn test_tree_expr() {
        assert_statement("foo();", expr_for_effect(
            call(id("foo"), vec!{})
        ));

        assert_statement(
            "foo() { out paint;}",
            tree(id("foo"), vec!{}, vec!{emit("paint", vec!{})})
        );

        assert_statement(
            r#"
            foo(x) {
                bar(y, z) {
                   gronk();
                   frobulate();
                }
                frobulate();
            }
            "#,
            tree(id("foo"), vec!{id("x")}, vec!{
                tree(
                    id("bar"),
                    vec!{id("y"), id("z")},
                    vec!{
                        expr_for_effect(call(id("gronk"), vec!{})),
                        expr_for_effect(call(id("frobulate"), vec!{}))
                    }
                ),
                expr_for_effect(call(id("frobulate"), vec!{}))
            })
        );
    }

    #[test]
    fn test_lambda_expr() {
        assert_expr(
            "() = {}",
            lambda(vec!{}, TypeTag::Void, map(vec!{}))
        );

        assert_expr(
            "() = 4",
            lambda(vec!{}, TypeTag::Void, Int(4))
        );


        assert_expr(
            "() -> Int = 4",
            lambda(vec!{}, TypeTag::Int, Int(4))
        );

        assert_expr(
            "() = {let x = 4; yield x}",
            lambda(
                vec!{},
                TypeTag::Void,
                expr_block(
                    vec!{def("x", Int(4))},
                    id("x")
                )
            )
        );

        assert_expr(
            "(x: Int, y: Int) -> Int = x * y",
            lambda(
                to_alist(vec!{
		    (s("x"), TypeTag::Int),
		    (s("y"), TypeTag::Int)
		}),
                TypeTag::Int,
                bin(Mul, id("x"), id("y"))
            )
        );

        assert_expr(
            "(x: Int) -> Int = 4",
            lambda(
                to_alist(vec!{(s("x"), TypeTag::Int)}),
                TypeTag::Int,
                Int(4)
            )
        );
    }

    // Test parsing of Types and Type Expressions
    
    #[test]
    fn test_type_record_empty() {
	assert_type(
	    r#"{}"#,
	    TypeTag::Record(vec!{})
	);
    }

    #[test]
    fn test_type_record_simple() {
	assert_type(
	    r#"{field x: Int}"#,
	    TypeTag::Record(
		to_alist(vec!{(s("x"), Member::Field(Node::new(TypeTag::Int)))})
	    )
	);
    }

    #[test]
    fn test_type_record_complex() {
	let dist = Member::Method(
	    to_alist(vec!{(s("other"), TypeTag::This)}),
	    Node::new(TypeTag::Float),
	    Node::new(expr_block(
		vec!{
		    def("dx", bin(Sub, dot(This, "x"), dot(id("other"), "x"))),
		    def("dy", bin(Sub, dot(This, "y"), dot(id("other"), "y"))),
		
		},
		call(
		    id("sqrt"),
		    vec!{
			bin(Add,
			    bin(Mul, id("dx"), id("dx")),
			    bin(Mul, id("dy"), id("dy"))
			)
		    }
		)
	    ))
	);
    
	let from_polar = Member::StaticMethod(
	    to_alist(vec!{
		(s("r"), TypeTag::Float),
		(s("theta"), TypeTag::Float)}
	    ),
	    Node::new(TypeTag::This),
	    Node::new(map(vec!{
		(s("x"), bin(Mul, id("r"), call(id("cos"), vec! {id("theta")}))),
		(s("y"), bin(Mul, id("r"), call(id("sin"), vec! {id("theta")})))
	    }))
	);

	let origin = Member::StaticValue(
	    Node::new(TypeTag::This),
	    Node::new(map(vec!{
		(s("x"), Float(0.0)),
		(s("y"), Float(0.0))
	    }))
	);

	// A plausible point or vector type.
	assert_type(
	    r#"{
               field x: Float;
               field y: Float;

               method dist(other: Self) -> Float = {
                  let dx = self.x - other.x;
                  let dy = self.y - other.y;
                  yield sqrt(dx * dx + dy * dy)
               };

               static fromPolar(r: Float, theta: Float) -> Self = {
                     x: r * cos(theta),
                     y: r * sin(theta),
               };
 
               const origin: Self = {x: 0.0, y: 0.0};
            }"#,
	    TypeTag::Record(
		to_alist(vec!(
		    (s("x"), Member::Field(Node::new(TypeTag::Float))),
		    (s("y"), Member::Field(Node::new(TypeTag::Float))),
		    (s("dist"), dist),
		    (s("fromPolar"), from_polar),
		    (s("origin"), origin)
		))
	    )
	);
    }

    // Test parsing of Statements.

    #[test]
    fn test_statement_function_def() {
        assert_statement(
            r#"func foo(y: Int) -> Int {yield 4 * y}"#,
            def(
                "foo",
                lambda(
		    to_alist(vec!{(s("y"), TypeTag::Int)}),
                    TypeTag::Int,
                    bin(Mul, Int(4), id("y"))
		)
            )
        );

        assert_statement(
            r#"proc foo(y: Int) {
               out paint;
            }"#,
            def(
                "foo",
                lambda(
                    to_alist(vec!{(s("y"), TypeTag::Int)}),
                    TypeTag::Void,
                    expr_block(vec!{emit("paint", vec!{})}, Expr::Void)
                )
            )
        );
    }

    #[test]
    fn test_statement_type_def() {
	assert_statement(
	    r#"type Foo: Int;"#,
	    typedef("Foo", TypeTag::Int)
	);
    }

    #[test]
    fn test_statement_let() {
	assert_statement(
	    r#"let dx = a.x - b.x;"#,
	    ast::def("dx", bin(Sub, dot(id("a"), "x"), dot(id("b"), "x")))
	);
    }

    // Tests for edge cases and regressions
    
    // XXX: So far there are none.
}

