// Mostly just a place to hold the unit tests for the grammar. The
// actual parser is just a wrapper around what LALRPOP has generated for us.
//
// The eventual goal here would be to have exhaustive coverage of the
// entire grammar: i.e. each variant of each production, with overlap
// from higher levels to make sure that even odd usages parse as expected.
//
// LALRPOP lets us test each production in isolation, as well as for
// the grammar as a whole.
use std::fs;

use crate::grammar;
use crate::ast;
use crate::ast::*;


// Parse the given path to an AST
pub fn parse(path: &str) -> ast::Program {
    let builder = Builder::new();

    let contents = fs::read_to_string(path)
	.expect(&format!("Couldn't read file: {}", path));

    grammar::ProgramParser::new()
	.parse(&builder, &contents)
	.expect("Syntax Error")
}


#[cfg(test)]
mod tests {
    use super::*;
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

    fn assert_expr(text: &'static str, ast: ast::ExprNode) {
	let builder = ast::Builder::new();
        assert_eq!(
            grammar::ExprParser::new().parse(&builder, text).unwrap(),
            ast
        );
    }

    fn assert_statement(text: &'static str, ast: StmtNode) {
	let builder = ast::Builder::new();
        assert_eq!(
            grammar::StatementParser::new().parse(&builder, text).unwrap(),
            ast
        );
    }

    fn assert_type(text: &'static str, ast: ast::TypeNode) {
	let builder = ast::Builder::new();
        assert_eq!(
	    grammar::TypeParser::new().parse(&builder, text).unwrap(),
            ast
        );
    }


    fn assert_prog(text: &'static str, ast: ast::Program) {
	let builder = ast::Builder::new();
        assert_eq!(
	    grammar::ProgramParser::new().parse(&builder, text).unwrap(),
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

	assert_expr(
	    "foo($, 1, \"baz\")",
	    ast.call(
		ast.id("foo"),
		&[ast.partial.clone(),
		  ast.i(1),
		  ast.s("baz")
		]
	    )
	);

	assert_statement(
	    "let y = foo($, 1, \"baz\");",
	    ast.def(
		"y",
		ast.call(
		    ast.id("foo"),
		    &[ast.partial.clone(),
		      ast.i(1),
		      ast.s("baz")
		    ]
		)
	    )
	);

	assert_expr(
	    "foo()()",
	    ast.call(ast.call(ast.id("foo"), &[]), &[])
	);

	assert_expr(
	    "foo()()()",
	    ast.call(ast.call(ast.call(ast.id("foo"), &[]), &[]), &[])
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
    fn test_has() {
	let ast = Builder::new();
        assert_expr(
            "foo.?.bar",
            ast.has(ast.id("foo"), "bar")
        );

        assert_expr(
            "foo.bar.?.baz",
            ast.has(ast.dot(ast.id("foo"), "bar"), "baz")
        );

        assert_expr(
            "foo.?.bar()",
            ast.call(ast.has(ast.id("foo"), "bar"), &[])
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
    }

    #[test]
    fn test_mixed_selections() {
	let ast = Builder::new();
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

	assert_expr(
            "foo[3].bar.baz[5][6].?.baz",
	    ast.has(
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
		),
		"baz"
	    )
        );
    }

    #[test]
    fn test_call_chaining() {
	let ast = Builder::new();

	assert_expr(
	    "foo().bar()",
	    ast.call(ast.dot(ast.call(ast.id("foo"), &[]), "bar"), &[])
	);

	assert_expr(
	    "foo()[3]",
	    ast.index(ast.call(ast.id("foo"), &[]), ast.i(3))
	);

	assert_expr(
	    "foo().bar",
	    ast.dot(ast.call(ast.id("foo"), &[]), "bar")
	);

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
        assert_statement("out \"fill\";", ast.emit(ast.s("fill")));
        assert_statement(
            "out moveto(x, y);",
            ast.emit(ast.call(ast.id("moveto"), &[ast.id("x"), ast.id("y")]))
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

        assert_expr("{out debug(x); yield x}", ast.block(
            &[ast.emit(ast.call(ast.id("debug"), &[ast.id("x")]))],
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
                  out ["moveto", p];
                  out ["circle", 50.0];
              }
        "#;

	let parse = ast.list_iter(
	    "p",
	    ast.id("points"),
	    ast.expr_for_effect(ast.block(
		&[
		    ast.emit(ast.list(&[ast.s("moveto"), ast.id("p")])),
		    ast.emit(ast.list(&[ast.s("circle"), ast.f(50.0)]))
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
              for (k, p) in x {
                  out ["moveto", [p.x, p.y]];
                  out ["text", k];
              }
        "#;

	let parse = ast.map_iter(
	    "k", "p",
	    ast.id("x"),
	    ast.expr_for_effect(ast.block(
		&[
		    ast.emit(ast.list(&[
			ast.s("moveto"),
			ast.list(&[ast.dot(ast.id("p"), "x"),
				   ast.dot(ast.id("p"), "y")])
		    ])),
		    ast.emit(ast.list(&[ast.s("text"), ast.id("k")]))
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
            "if (a) { out [\"text\", b]; }",
            ast.guard(
                &[(ast.id("a"),
		   ast.block(
		       &[ast.emit(ast.list(&[ast.s("text"), ast.id("b")]))],
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
            r#"if (a) { out a; } else { out "error"; }"#,
            ast.guard(
                &[(ast.id("a"),
		   ast.block(
		       &[ast.emit(ast.id("a"))],
		       ast.void.clone()
		   ))
		],
                Some(ast.emit(ast.s("error")))
            )
        );
    }

    #[test]
    fn test_if_elif_else() {
	let ast = Builder::new();
        assert_statement(
            r#"if (a) {
               out "a";
            } elif (b) {
               out "b";
            } else {
               out "error";
            }"#,
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit(ast.s("a"))], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit(ast.s("b"))], ast.void.clone())),
                ],
                Some(ast.emit(ast.s("error")))
            )
        );

        assert_statement(
            r#"if (a) {
               out "a";
            } elif (b) {
               out "b";
            } elif (c) {
               out "c";
            } else {
               out "error";
            }"#,
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit(ast.s("a"))], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit(ast.s("b"))], ast.void.clone())),
                    (ast.id("c"), ast.block(&[ast.emit(ast.s("c"))], ast.void.clone())),
                ],
                Some(ast.emit(ast.s("error")))
            )
        );
    }

    #[test]
    fn test_if_elif() {
	let ast = Builder::new();
        assert_statement(
            r#"if (a) {
               out "a";
            } elif (b) {
               out "b";
            }"#,
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit(ast.s("a"))], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit(ast.s("b"))], ast.void.clone())),
                ],
                None
            )
        );

        assert_statement(
            r#"if (a) {
               out "a";
            } elif (b) {
               out "b";
            } elif (c) {
               out "c";
            }"#,
            ast.guard(
                &[
                    (ast.id("a"), ast.block(&[ast.emit(ast.s("a"))], ast.void.clone())),
                    (ast.id("b"), ast.block(&[ast.emit(ast.s("b"))], ast.void.clone())),
                    (ast.id("c"), ast.block(&[ast.emit(ast.s("c"))], ast.void.clone())),
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
            "foo() { out \"paint\";}",
            ast.template_call(ast.id("foo"), &[], ast.block(
		&[ast.emit(ast.s("paint"))],
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

    #[test]
    fn test_in_expr() {
	let ast = Builder::new();
	assert_expr("in", ast.in_.clone());
	assert_expr("in.foo", ast.dot(ast.in_.clone(), "foo"));
	assert_expr("in[0]", ast.index(ast.in_.clone(), ast.i(0)));
	assert_statement("let y = in;", ast.def("y", ast.in_.clone()));

	assert_statement(
	    r#"
            for i in in { out i; }
            "#,
	    ast.list_iter(
		"i",
		ast.in_.clone(),
		ast.expr_for_effect(
		    ast.block(&[ast.emit(ast.id("i"))], ast.void.clone())
		)
	    )
	);

	assert_statement(
	    r#"
            for i in in.items { out i; }
            "#,
	    ast.list_iter(
		"i",
		ast.dot(ast.in_.clone(), "items"),
		ast.expr_for_effect(
		    ast.block(&[ast.emit(ast.id("i"))], ast.void.clone())
		)
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

	assert_type(
	    r#"{field? x: Int}"#,
	    ast.record(&alist!{
		"x" => Member::OptionField(ast.t_int.clone())
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


    #[test]
    fn test_type_union() {
	let ast = Builder::new();

	assert_type(
	    r#"Int | Float"#,
	    ast.union(&[ast.t_int.clone(), ast.t_float.clone()])
	);

	assert_type(
	    r#"| Int"#,
	    ast.t_int.clone()
	);

	assert_type(
	    r#"| Int | Float"#,
	    ast.union(&[ast.t_int.clone(), ast.t_float.clone()])
	);

	assert_type(
	    r#"Int | Float | Str"#,
	    ast.union(&[ast.t_int.clone(), ast.union(&[
		ast.t_float.clone(),
		ast.t_str.clone()
	    ])])
	);

	assert_type(
	    r#"| Int | Float | Str"#,
	    ast.union(&[ast.t_int.clone(), ast.union(&[
		ast.t_float.clone(),
		ast.t_str.clone()
	    ])])
	);

	assert_type(
	    r#"| Int | [Int] | Str"#,
	    ast.union(&[ast.t_int.clone(), ast.union(&[
		ast.t_list(ast.t_int.clone()),
		ast.t_str.clone()
	    ])])
	); 

	assert_type(
	    r#"[Int | [Int] | Str]"#,
	    ast.t_list(
		ast.union(&[ast.t_int.clone(), ast.union(&[
		    ast.t_list(ast.t_int.clone()),
		    ast.t_str.clone()
		])])
	    )
	);

	assert_type(
	    r#"[| Int | [Int] | Str]"#,
	    ast.t_list(
		ast.union(&[ast.t_int.clone(), ast.union(&[
		    ast.t_list(ast.t_int.clone()),
		    ast.t_str.clone()
		])])
	    )
	);

	assert_type(
	    r#"| [| Int | [Int] | Str]"#,
	    ast.t_list(
		ast.union(&[ast.t_int.clone(), ast.union(&[
		    ast.t_list(ast.t_int.clone()),
		    ast.t_str.clone()
		])])
	    )
	);

	assert_type(
	    r#"| [| Int | [Int] | Str] | Bool"#,
	    ast.union(&[
		ast.t_list(ast.union(&[
		    ast.t_int.clone(),
		    ast.union(&[
			ast.t_list(ast.t_int.clone()),
			ast.t_str.clone()
		    ])])
		),
		ast.t_bool.clone()
	    ])
	);

	assert_type(
	    r#"| Int | [Int] | Str | Bool"#,
	    ast.union(&[
		ast.t_int.clone(),
		ast.union(&[
		    ast.t_list(ast.t_int.clone()),
		    ast.union(&[
			ast.t_str.clone(),
			ast.t_bool.clone()
		    ])
		])
	    ])
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
               out "paint";
            }"#,
            ast.def(
                "foo",
                ast.lambda(
                    &alist!{"y" => ast.t_int.clone()},
                    ast.t_void.clone(),
                    ast.block(&[ast.emit(ast.s("paint"))], ast.void.clone())
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

    #[test]
    fn test_import() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.1-pre_mvp;
            script "Minimal Import Test";
            import foo;
            input Str;
            output Str;
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", None)],
		ast.t_str.clone(),
		ast.t_str.clone(),
		&[]
	    )
	);
    }

    #[test]
    fn test_import_self() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.1-pre_mvp;
            script "Minimal Import Test";
            import foo._;
            input Str;
            output Str;
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast::ImportSelector::Itself))],
		ast.t_str.clone(),
		ast.t_str.clone(),
		&[]
	    )
	);
    }

    #[test]
    fn test_import_all() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.1-pre_mvp;
            script "Minimal Import Test";
            import foo.*;
            input Str;
            output Str;
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast::ImportSelector::All))],
		ast.t_str.clone(),
		ast.t_str.clone(),
		&[]
	    )
	);
    }

    #[test]
    fn test_import_item() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.1-pre_mvp;
            script "Minimal Import Test";
            import foo.bar;
            input Str;
            output Str;
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast.import_item("bar")))],
		ast.t_str.clone(),
		ast.t_str.clone(),
		&[]
	    )
	);
    }

    #[test]
    fn test_import_alias() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.1-pre_mvp;
            script "Minimal Import Test";
            import foo.bar as baz;
            input Str;
            output [Str | Float];
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast.import_alias("bar", "baz")))],
		ast.t_str.clone(),
		ast.t_list(ast.union(&[ast.t_str.clone(), ast.t_float.clone()])),
		&[]
	    )
	);
    }

    #[test]
    fn test_import_multi() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.1-pre_mvp;
            script "Minimal Import Test";
            import bands.{
              _,
              beatles.{george, paul, john, ringo},
              stones.mcjagger,
              greatful_dead.*
            };
            input Str;
            output Str;
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import(
		    "bands",
		    Some(ast.import_group(&[
			ast::ImportSelector::Itself,
			ast.import_nested("beatles", ast.import_group(&[
			    ast.import_item("george"),
			    ast.import_item("paul"),
			    ast.import_item("john"),
			    ast.import_item("ringo")
			])),
			ast.import_nested("stones", ast.import_item("mcjagger")),
			ast.import_nested("greatful_dead", ast::ImportSelector::All)
		    ]))
		)],
		ast.t_str.clone(),
		ast.t_str.clone(),
		&[]
	    )
	);
    }

    #[test]
    fn test_shebang() {
	let ast = Builder::new();
	assert_prog(
	    r#"#! foo bar baz"
            version 0.1-pre_mvp;
            script "Test Shebang";
            input Str;
            output [Str | Float];
            "#,
	    ast.script(
		"Test Shebang",
		&[],
		ast.t_str.clone(),
		ast.t_list(ast.union(&[ast.t_str.clone(), ast.t_float.clone()])),
		&[]
	    )
	);
    }

    #[test]
    fn test_comments() {
	let ast = Builder::new();
	assert_prog(
	    r#"#! foo bar baz"
            version 0.1-pre_mvp;
            script "Test Comments";
            // Here's a short comment.
            input Str;
            /* Here's a lengthy comment.
             * It spans multiple
             * lines. But it ends here. */
            output [Str | Float];
            "#,
	    ast.script(
		"Test Comments",
		&[],
		ast.t_str.clone(),
		ast.t_list(ast.union(&[ast.t_str.clone(), ast.t_float.clone()])),
		&[]
	    )
	);
    }
    
    #[test]
    fn test_suppose() {
	let ast = Builder::new();
	assert_statement(
	    r#"
            suppose(she_may_love_you()) {
               out "She loves you,";
               ...;
               ...;
               ...;
            } else {
               out "Yesterdayyyyyy.....";
            }
            "#,
	    ast.suppose(
		ast.call(ast.id("she_may_love_you"), &[]),
		ast.expr_for_effect(ast.block(&[
		    ast.emit(ast.s("She loves you,")),
		    ast.effect_capture.clone(),
		    ast.effect_capture.clone(),
		    ast.effect_capture.clone()
		], ast.void.clone())),
		ast.expr_for_effect(ast.block(&[
		    ast.emit(ast.s("Yesterdayyyyyy....."))
		], ast.void.clone()))
	    )
	);
    }

    // Tests for edge cases and regressions
    
    // So far there are none.
}

