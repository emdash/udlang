// (C) 2021 Brandon Lewis

use std::env;

use udlang::grammar;
use udlang::parser::parse;
use udlang::ast::{Builder};
use udlang::ir::{compile, Value};
use udlang::vm::run;

use std::io::{
    BufRead,
    stdin
};


// Dump expressions in a REPL
fn dump_expr() {
    let builder = Builder::new();
    for line in stdin().lock().lines() {
	println!(
	    "{:?}",
	    grammar::ExprParser::new().parse(&builder, &line.unwrap())
	);
    }
}


// Try to parse `path` and print the resulting AST.
fn dump_ast(path: &str) {
    println!("{:#?}", parse(path));
}


// Try to compile `path` and print the resulting IR.
fn dump_ir(path: &str) {
    println!("{:#?}", compile(path))
}


// Try to decode an ir::Value from a string
fn decode(_input: std::io::Result<String>) -> Value {
    // XXX: really implement me
    Value::Int(4)
}


fn main() {
    let args: Vec<String> = env::args().into_iter().collect();
    let strargs: Vec<&str> = args.iter().map(|i| i.as_str()).collect();
    
    match strargs.as_slice() {
	[_, "--dump-expr"] => dump_expr(),
	[_, "--dump-ast", path] => dump_ast(path),
	[_, "--compile", path] => dump_ir(path),
	[_, path] => run(path, stdin().lock().lines().map(decode)).expect("runtime error"),
	_ => println!("Invalid args. Usage string TBD.")
    };
}
