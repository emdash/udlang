// Mostly just a place to hold the unit tests for the grammar. The
// actual parser is just a wrapper around what LALRPOP has generated for us.
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

    fn assert_expr(text: &'static str, ast: ast::SubExpr) {
        assert_eq!(
            Node::new(grammar::ExprParser::new().parse(text).unwrap()),
            ast
        );
    }

    fn assert_statement(text: &'static str, ast: Node<Statement>) {
        assert_eq!(
            Node::new(grammar::StatementParser::new().parse(text).unwrap()),
            ast
        );
    }

    fn assert_type(text: &'static str, ast: ast::Node<TypeTag>) {
        assert_eq!(
	    Node::new(grammar::TypeParser::new().parse(text).unwrap()),
            ast
        );
    }

    // Test Parsing of Expressions

    #[test]
    fn test_basic() {
	let ast = Builder::new();
        assert_expr("3 + 4 * 5", ast.bin(
            Add,
            ast.i(3),
            ast.bin(Mul, ast.i(4), ast.i(5))
        ));
        assert_expr("a + 3", ast.bin(Add, ast.id("a"), ast.i(3)));
        assert_expr("3 * a", ast.bin(Mul, ast.i(3), ast.id("a")));
        assert_expr("\"foo\"", ast.s("foo"));

    }

    #[test]
    fn test_list() {
	let ast = Builder::new();
        assert_expr("[]", ast.list(&[]));
        assert_expr("[3]", ast.list(&[ast.i(3)]));
        assert_expr("[3, 4, 5]", ast.list(&[ast.i(3), ast.i(4), ast.i(5)]));
        assert_expr(
            "[3 + 4, 5]",
            ast.list(&[ast.bin(Add, ast.i(3), ast.i(4)), ast.i(5)])
        );
    }

    #[test]
    fn test_map() {
	let ast = Builder::new();
        assert_expr("{}", ast.map(&[]));
        assert_expr(
            r#"{"foo": 1}"#,
            ast.map(&alist!{"foo" => ast.i(1)})
        );

        assert_expr(
            r#"{"foo": 1, "bar": 2}"#,
            ast.map(&alist!{
                "foo" => ast.i(1),
                "bar" => ast.i(2)
            })
        );
    }

    #[test]
    fn test_terms() {
	let ast = Builder::new();
        assert_expr("42", ast.i(42));
        assert_expr("42.0", ast.f(42.0));
        assert_expr("(42)", ast.i(42));
        assert_expr("foo", ast.id("foo"));
        assert_expr("-42", ast.i(-42));
        assert_expr("-42.0", ast.f(-42.0));
        assert_expr("-x", ast.un(Neg, ast.id("x")));
        assert_expr("- 42", ast.un(Neg, ast.i(42)));
        assert_expr("- 42.0", ast.un(Neg, ast.f(42.0)));
        assert_expr("-(42)", ast.un(Neg, ast.i(42)));
    }

    #[test]
    fn test_relational() {
	let ast = Builder::new();
        assert_expr("3 + 4 < 3 * 4", ast.bin(
            Lt,
            ast.bin(Add, ast.i(3), ast.i(4)),
            ast.bin(Mul, ast.i(3), ast.i(4))
        ));

        assert_expr("3 + 4 > 3 * 4", ast.bin(
            Gt,
            ast.bin(Add, ast.i(3), ast.i(4)),
            ast.bin(Mul, ast.i(3), ast.i(4))
        ));

        assert_expr("3 + 4 <= 3 * 4", ast.bin(
            Lte,
            ast.bin(Add, ast.i(3), ast.i(4)),
            ast.bin(Mul, ast.i(3), ast.i(4))
        ));

        assert_expr("3 + 4 >= 3 * 4", ast.bin(
            Gte,
            ast.bin(Add, ast.i(3), ast.i(4)),
            ast.bin(Mul, ast.i(3), ast.i(4))
        ));

        assert_expr("3 + 4 == 3 * 4", ast.bin(
            Eq,
            ast.bin(Add, ast.i(3), ast.i(4)),
            ast.bin(Mul, ast.i(3), ast.i(4))
        ));
    }

    #[test]
    fn test_logic() {
	let ast = Builder::new();
        assert_expr(
            "x >= lower and x <= upper",
            ast.bin(And,
                ast.bin(Gte, ast.id("x"), ast.id("lower")),
                ast.bin(Lte, ast.id("x"), ast.id("upper"))));

        assert_expr(
            "x >= 3 or x > 4 and x > 5",
            ast.bin(And,
                ast.bin(Or,
                    ast.bin(Gte, ast.id("x"), ast.i(3)),
                    ast.bin(Gt, ast.id("x"), ast.i(4))),
                ast.bin(Gt, ast.id("x"), ast.i(5))));

        assert_expr(
            "x >= 3 or (x > 4 and x > 5)",
            ast.bin(Or,
                ast.bin(Gte, ast.id("x"), ast.i(3)),
                ast.bin(And,
                    ast.bin(Gt, ast.id("x"), ast.i(4)),
                    ast.bin(Gt, ast.id("x"), ast.i(5)))));

        assert_expr(
            "(x >= 3) or (x > 4 and x > 5)",
            ast.bin(Or,
                ast.bin(Gte, ast.id("x"), ast.i(3)),
                ast.bin(And,
                    ast.bin(Gt, ast.id("x"), ast.i(4)),
                    ast.bin(Gt, ast.id("x"), ast.i(5)))));
    }

    #[test]
    fn test_call() {
	let ast = Builder::new();

        assert_expr(
            "foo(a + 3, a and b)",
            ast.call(
                ast.id("foo"),
                &[
		    ast.bin(Add, ast.id("a"), ast.i(3)),
		    ast.bin(And, ast.id("a"), ast.id("b"))
		]
            )
        );

        assert_expr(
            "x(a or b, y <= 3, y(-g(a * 7)))",
            ast.call(
                ast.id("x"),
                &[
                    ast.bin(Or, ast.id("a"), ast.id("b")),
                    ast.bin(Lte, ast.id("y"), ast.i(3)),
                    ast.call(
                        ast.id("y"),
                        &[
                            ast.un(
                                Neg,
                                ast.call(
                                    ast.id("g"),
                                    &[ast.bin(Mul, ast.id("a"), ast.i(7))]
                                )
                            )
                        ]
                    )
                ]
            )
        );
    }

    #[test]
    fn test_dot() {
	let ast = Builder::new();
        assert_expr(
            "foo.bar",
            ast.dot(ast.id("foo"), "bar")
        );

        assert_expr(
            "foo.bar.baz",
            ast.dot(ast.dot(ast.id("foo"), "bar"), "baz")
        );

        assert_expr(
            "foo.bar()",
            ast.call(ast.dot(ast.id("foo"), "bar"), &[])
        );
    }

    #[test]
    fn test_index() {
	let ast = Builder::new();
        assert_expr(
            "foo[0]",
            ast.index(ast.id("foo"), ast.i(0))
        );

        assert_expr(
            "foo[bar]",
            ast.index(ast.id("foo"), ast.id("bar"))
        );

        assert_expr(
            "foo[3 + 10 * 5]",
            ast.index(ast.id("foo"), ast.bin(Add, ast.i(3), ast.bin(Mul, ast.i(10), ast.i(5))))
        );

        assert_expr(
            "foo[3][4]",
            ast.index(ast.index(ast.id("foo"), ast.i(3)), ast.i(4))
        );

        assert_expr(
            "foo[3].bar.baz[5][6]",
            ast.index(
                ast.index(
                    ast.dot(
                        ast.dot(
                            ast.index(ast.id("foo"), ast.i(3)),
                            "bar"
                        ),
                        "baz"
                    ),
                    ast.i(5)
                ),
                ast.i(6)
            )
        );

        // XXX: see issue #8 in github
	// assert_expr("foo()[3]", index(call(id("foo"), vec!{}), Int(3)));
	// assert_expr("foo()bar", index(dot(id("foo"), vec!{}), "bar"));

        assert_expr(
            "(foo())[3]",
            ast.index(ast.call(ast.id("foo"), &[]), ast.i(3))
        );

        assert_expr(
            "(foo()).bar",
            ast.dot(ast.call(ast.id("foo"), &[]), "bar")
        );
    }

    #[test]
    fn test_simple_statement() {
	let ast = Builder::new();
        assert_statement("out fill;", ast.emit("fill", &[]));
        assert_statement(
            "out moveto x, y;",
            ast.emit("moveto", &[ast.id("x"), ast.id("y")])
        );

        assert_statement(
            "let y = x * 3 + 4;",
            ast.def("y", ast.bin(Add, ast.bin(Mul, ast.id("x"), ast.i(3)), ast.i(4)))
        );
    }

    #[test]
    fn test_expr_block() {
	let ast = Builder::new();
        assert_expr("{let x = y; yield 4}", ast.block(
            &[ast.def("x", ast.id("y"))],
            ast.i(4))
        );

        assert_expr("{let x = frob(y); yield y * 4}", ast.block(
            &[ast.def("x", ast.call(ast.id("frob"), &[ast.id("y")]))],
            ast.bin(Mul, ast.id("y"), ast.i(4))
        ));

        assert_expr("{out debug x; yield x}", ast.block(
            &[ast.emit("debug", &[ast.id("x")])],
            ast.id("x")
        ));

        assert_expr(
            "{let x = {let y = 2; yield y * 3}; yield x}",
            ast.block(
		&[
                    ast.def("x",
			    ast.block(
				&[ast.def("y", ast.i(2))], 
				ast.bin(Mul, ast.id("y"), ast.i(3))
			    )
                    )
		],
		ast.id("x")
	    )
	);
    }

    #[test]
    fn test_list_iter() {
	let ast = Builder::new();
        let test = r#"
              for p in points {
                  out moveto p;
                  out circle 50.0;
              }
        "#;

	let parse = ast.list_iter(
	    "p",
	    ast.id("points"),
	    ast.expr_for_effect(ast.block(
		&[
		    ast.emit("moveto", &[ast.id("p")]),
		    ast.emit("circle", &[ast.f(50.0)])
		],
		ast.void.clone()
	    ))
        );

        assert_statement(test, parse);
    }


    #[test]
    fn test_map_iter() {
	let ast = Builder::new();
        let test = r#"
              for (k, v) in x {
                  out moveto v, v;
                  out text k;
              }
        "#;

	let parse = ast.map_iter(
	    "k", "v",
	    ast.id("x"),
	    ast.expr_for_effect(ast.block(
		&[
		    ast.emit("moveto", &[ast.id("v"), ast.id("v")]),
		    ast.emit("text", &[ast.id("k")])
		],
		ast.void.clone()
	    ))
	);

        assert_statement(test, parse);
    }

    #[test]
    fn test_if() {
	let ast = Builder::new();
        assert_statement(
            "if (a) { out text b; }",
            ast.guard(
                &[(ast.id("a"),
		   ast.block(
		       &[
			   ast.emit("text", &[ast.id("b")])
		       ],
		       ast.void.clone()
		   ))
		],
                None
            )
        );
    }

    #[test]
    fn test_if_else() {
	let ast = Builder::new();
        assert_statement(
            r#"if (a) { out text b; } else { out text "error"; }"#,
            ast.guard(
                &[(ast.id("a"),
		   ast.block(
		       &[ast.emit("text", &[ast.id("b")])],
		       ast.void.clone()
		   ))
		],
                Some(ast.emit("text", &[ast.s("error")]))
            )
        );
    }

    #[test]
    fn test_if_elif_else() {
	let ast = Builder::new();
        assert_statement(
            r#"if (a) {
               out text "a";
            } elif (b) {
               out text "b";
            } else {
               out text "error";
            }"#,
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit("text", &[ast.s("a")])], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit("text", &[ast.s("b")])], ast.void.clone())),
                ],
                Some(ast.emit("text", &[ast.s("error")]))
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
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit("text", &[ast.s("a")])], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit("text", &[ast.s("b")])], ast.void.clone())),
                    (ast.id("c"), ast.block(&[ast.emit("text", &[ast.s("c")])], ast.void.clone())),
                ],
                Some(ast.emit("text", &[ast.s("error")]))
            )
        );
    }

    #[test]
    fn test_if_elif() {
	let ast = Builder::new();
        assert_statement(
            r#"if (a) {
               out text "a";
            } elif (b) {
               out text "b";
            }"#,
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit("text", &[ast.s("a")])], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit("text", &[ast.s("b")])], ast.void.clone())),
                ],
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
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit("text", &[ast.s("a")])], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit("text", &[ast.s("b")])], ast.void.clone())),
                    (ast.id("c"), ast.block(&[ast.emit("text", &[ast.s("c")])], ast.void.clone())),
                ],
                None
            )
        );
    }

    #[test]
    fn test_tree_expr() {
	let ast = Builder::new();
        assert_statement("foo();", ast.expr_for_effect(
            ast.call(ast.id("foo"), &[])
        ));

        assert_statement(
            "foo() { out paint;}",
            ast.template_call(ast.id("foo"), &[], ast.block(
		&[ast.emit("paint", &[])],
		ast.void.clone()
	    ))
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
            ast.template_call(
		ast.id("foo"),
		&[ast.id("x")],
		ast.block(
		    &[
			ast.template_call(
			    ast.id("bar"),
			    &[ast.id("y"), ast.id("z")],
			    ast.block(
				&[
				    ast.expr_for_effect(ast.call(ast.id("gronk"), &[])),
				    ast.expr_for_effect(ast.call(ast.id("frobulate"), &[]))
				],
				ast.void.clone()
			    )
			),
			ast.expr_for_effect(ast.call(ast.id("frobulate"), &[]))
		    ],
		    ast.void.clone()
		),
	    )
        );
    }

    #[test]
    fn test_lambda_expr() {
	let ast = Builder::new();
        assert_expr(
            "() = {}",
            ast.lambda(&[], ast.t_void.clone(), ast.map(&[]))
        );

        assert_expr(
            "() = 4",
            ast.lambda(&[], ast.t_void.clone(), ast.i(4))
        );


        assert_expr(
            "() -> Int = 4",
            ast.lambda(&[], ast.t_int.clone(), ast.i(4))
        );

        assert_expr(
            "() = {let x = 4; yield x}",
            ast.lambda(
		&[],
                ast.t_void.clone(),
                ast.block(
                    &[ast.def("x", ast.i(4))],
                    ast.id("x")
                )
            )
        );

        assert_expr(
            "(x: Int, y: Int) -> Int = x * y",
            ast.lambda(
		&alist!{
		    "x" => ast.t_int.clone(),
		    "y" => ast.t_int.clone()
		},
                ast.t_int.clone(),
                ast.bin(Mul, ast.id("x"), ast.id("y"))
	    )
        );

        assert_expr(
            "(x: Int) -> Int = 4",
            ast.lambda(
                &alist!{
		    "x" => ast.t_int.clone()
		},
                ast.t_int.clone(),
                ast.i(4)
	    )
        );
    }

    // Test parsing of Types and Type Expressions
    
    #[test]
    fn test_type_record_empty() {
	let ast = Builder::new();
	assert_type(
	    r#"{}"#,
	    ast.record(&[])
	);
    }

    #[test]
    fn test_type_record_simple() {
	let ast = Builder::new();
	assert_type(
	    r#"{field x: Int}"#,
	    ast.record(&alist!{
		"x" => Member::Field(ast.t_int.clone())
	    })
	);
    }

    #[test]
    fn test_type_record_complex() {
	let ast = Builder::new();

	let dist = Member::Method(
	    alist(&alist!{"other" => ast.t_this.clone()}.to_vec()),
	    ast.t_float.clone(),
	    ast.block(
		&[
		    ast.def("dx", ast.bin(Sub, ast.dot(ast.this.clone(), "x"), ast.dot(ast.id("other"), "x"))),
		    ast.def("dy", ast.bin(Sub, ast.dot(ast.this.clone(), "y"), ast.dot(ast.id("other"), "y"))),
		],
		ast.call(
		    ast.id("sqrt"),
		    &[
			ast.bin(Add,
			    ast.bin(Mul, ast.id("dx"), ast.id("dx")),
			    ast.bin(Mul, ast.id("dy"), ast.id("dy"))
			)
		    ]
		)
	    )
	);
    
	let from_polar = Member::StaticMethod(
	    alist(&alist!{
		"r" => ast.t_float.clone(),
		"theta" => ast.t_float.clone()
	    }),
	    ast.t_this.clone(),
	    ast.map(&alist!{
		"x" => ast.bin(Mul, ast.id("r"), ast.call(ast.id("cos"), &[ast.id("theta")])),
		"y" => ast.bin(Mul, ast.id("r"), ast.call(ast.id("sin"), &[ast.id("theta")]))
	    })
	);

	let origin = Member::StaticValue(
	    ast.t_this.clone(),
	    ast.map(&alist!{
		"x" => ast.f(0.0),
		"y" => ast.f(0.0)
	    })
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
	    ast.record(&alist!{
		"x"         => Member::Field(ast.t_float.clone()),
		"y"         => Member::Field(ast.t_float.clone()),
		"dist"      => dist,
		"fromPolar" => from_polar,
		"origin"    => origin
	    })
	);
    }

    // Test parsing of Statements.

    #[test]
    fn test_statement_function_def() {
	let ast = Builder::new();
        assert_statement(
            r#"func foo(y: Int) -> Int {yield 4 * y}"#,
            ast.def(
                "foo",
                ast.lambda(
		    &alist!{"y" => ast.t_int.clone()},
                    ast.t_int.clone(),
                    ast.bin(Mul, ast.i(4), ast.id("y"))
		)
            )
        );

        assert_statement(
            r#"proc foo(y: Int) {
               out paint;
            }"#,
            ast.def(
                "foo",
                ast.lambda(
                    &alist!{"y" => ast.t_int.clone()},
                    ast.t_void.clone(),
                    ast.block(&[ast.emit("paint", &[])], ast.void.clone())
                )
            )
        );
    }

    #[test]
    fn test_statement_type_def() {
	let ast = Builder::new();
	assert_statement(
	    r#"type Foo: Int;"#,
	    ast.typedef("Foo", ast.t_int.clone())
	);
    }

    #[test]
    fn test_statement_let() {
	let ast = Builder::new();
	assert_statement(
	    r#"let dx = a.x - b.x;"#,
	    ast.def(
		"dx",
		ast.bin(
		    Sub,
		    ast.dot(ast.id("a"), "x"),
		    ast.dot(ast.id("b"), "x")
		)
	    )
	);
    }

    // Tests for edge cases and regressions
    
    // So far there are none.
}

