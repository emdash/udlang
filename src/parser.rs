// (C) 2021 Brandon Lewis
//
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
    fn assert_expr_error(text: &'static str) {
	let builder = ast::Builder::new();
	match grammar::ExprParser::new().parse(&builder, text) {
	    Ok(_)  => panic!("Expected error"),
	    Err(_) => ()
	}

    }

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
            "foo[3].bar.baz[5][6].baz",
	    ast.dot(
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
        assert_statement(
            "let y = x * 3 + 4;",
            ast.def("y", ast.bin(Add, ast.bin(Mul, ast.id("x"), ast.i(3)), ast.i(4)))
        );
    }

    #[test]
    fn test_block_expr() {
	let ast = Builder::new();
        assert_expr("{let x = y; 4}", ast.block(
            &[ast.def("x", ast.id("y"))],
            ast.i(4))
        );

        assert_expr("{let x = frob(y); y * 4}", ast.block(
            &[ast.def("x", ast.call(ast.id("frob"), &[ast.id("y")]))],
            ast.bin(Mul, ast.id("y"), ast.i(4))
        ));

        assert_expr(
            "{let x = {let y = 2; y * 3}; x}",
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
    fn test_lambda_expr() {
	let ast = Builder::new();
        assert_expr(
            "() = {}",
            ast.lambda(&[], ast.t_any.clone(), ast.map(&[]))
        );

	let ast = Builder::new();
        assert_expr(
            "() -> {Any} {{}}",
            ast.lambda(&[], ast.t_map(ast.t_any.clone()), ast.block(&[], ast.map(&[])))
        );

        assert_expr_error("() {}");

	assert_expr(
            "() -> Int {4}",
            ast.lambda(&[], ast.t_int.clone(), ast.block(&[], ast.i(4)))
        );

	
	assert_expr(
            "() -> Void {done}",
            ast.lambda(&[], ast.t_void.clone(), ast.block(&[], ast.id("done")))
        );


        assert_expr(
            "() = 4",
            ast.lambda(&[], ast.t_any.clone(), ast.i(4))
        );


        assert_expr(
            "() -> Int = 4",
            ast.lambda(&[], ast.t_int.clone(), ast.i(4))
        );

        assert_expr(
            "() -> Int {let x = 4; x}",
            ast.lambda(
		&[],
                ast.t_int.clone(),
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
    }

    // Test parsing of Types and Type Expressions
    #[test]
    fn test_statement_function_def() {
	let ast = Builder::new();
        assert_statement(
            r#"func foo(y: Int) -> Int {4 * y}"#,
            ast.def(
                "foo",
                ast.lambda(
		    &alist!{"y" => ast.t_int.clone()},
                    ast.t_int.clone(),
                    ast.block(&[], ast.bin(Mul, ast.i(4), ast.id("y")))
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
            version 0.2;
            script "Minimal Import Test";
            import foo;
            input Str;
            output Str;
            "hello, world"
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", None)],
		ast.t_str.clone(),
		ast.t_str.clone(),
		ast.s("hello, world")
	    )
	);
    }

    #[test]
    fn test_import_self() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.2;
            script "Minimal Import Test";
            import foo._;
            input Str;
            output Str;
            "foo"
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast::ImportSelector::Itself))],
		ast.t_str.clone(),
		ast.t_str.clone(),
		ast.s("foo")
	    )
	);
    }

    #[test]
    fn test_import_all() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.2;
            script "Minimal Import Test";
            import foo.*;
            input Str;
            output Str;
            "foo"
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast::ImportSelector::All))],
		ast.t_str.clone(),
		ast.t_str.clone(),
		ast.s("foo")
	    )
	);
    }

    #[test]
    fn test_import_item() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.2;
            script "Minimal Import Test";
            import foo.bar;
            input Str;
            output Str;
            "foo"
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast.import_item("bar")))],
		ast.t_str.clone(),
		ast.t_str.clone(),
		ast.s("foo")
	    )
	);
    }

    #[test]
    fn test_import_alias() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.2;
            script "Minimal Import Test";
            import foo.bar as baz;
            input Str;
            output Float;
            3.14
            "#,
	    ast.script(
		"Minimal Import Test",
		&[ast.import("foo", Some(ast.import_alias("bar", "baz")))],
		ast.t_str.clone(),
		ast.t_float.clone(),
		ast.f(3.14)
	    )
	);
    }

    #[test]
    fn test_import_multi() {
	let ast = Builder::new();
	assert_prog(
	    r#"
            version 0.2;
            script "Minimal Import Test";
            import bands.{
              _,
              beatles.{george, paul, john, ringo},
              stones.mcjagger,
              greatful_dead.*
            };
            input Str;
            output Str;
            "foo"
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
		ast.s("foo")
	    )
	);
    }

    #[test]
    fn test_shebang() {
	let ast = Builder::new();
	assert_prog(
	    r#"#! foo bar baz"
            version 0.2;
            script "Test Shebang";
            input Str;
            output [Float];
            [3.14, 2.71]
            "#,
	    ast.script(
		"Test Shebang",
		&[],
		ast.t_str.clone(),
		ast.t_list(ast.t_float.clone()),
		ast.list(&[ast.f(3.14), ast.f(2.71)])
	    )
	);
    }

    #[test]
    fn test_comments() {
	let ast = Builder::new();
	assert_prog(
	    r#"#! foo bar baz"
            version 0.2;
            script "Test Comments";
            // Here's a short comment.
            input Str;
            /* Here's a lengthy comment.
             * It spans multiple
             * lines. But it ends here. */
            output [Str];
            "foo"
            "#,
	    ast.script(
		"Test Comments",
		&[],
		ast.t_str.clone(),
		ast.t_list(ast.t_str.clone()),
		ast.s("foo")
	    )
	);
    }

    // Tests for edge cases and regressions
    
    // So far there are none.
}

