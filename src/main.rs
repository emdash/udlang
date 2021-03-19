use std::env;
use std::fs;

use udlang::grammar;
use udlang::ast::{self, Builder};
use udlang::ir::Compiler;

use std::io::{
    BufReader,
    BufRead,
    stdin
};


// Parse the given path to an AST
fn parse(path: &str) -> ast::Program {
    let builder = Builder::new();
    let contents = fs::read_to_string(path).expect(&format!("Couldn't read file: {}", path));
    grammar::ProgramParser::new().parse(&builder, &contents).expect("Parse error")
}


// Compile the given path to IR
fn compile(path: &str) {
    let mut compiler = Compiler::new();
    println!("{:?}", compiler.compile_program(parse(path)))
}


// Dump expressions in a REPL
fn dump_expr() {
    let builder = Builder::new();
    loop {
	let mut reader = BufReader::new(stdin());
	let mut line = String::new();
	reader.read_line(&mut line).unwrap();
	println!("{:?}", grammar::ExprParser::new().parse(&builder, &line));
    }
}


// Try to parse `path` and print the resulting AST.
fn dump_ast(path: &str) {
    println!("{:?}", parse(path));
}


// Try to compile `path` and print the resulting IR.
fn dump_ir(path: &str) {
    println!("{:?}", compile(path))
}


fn main() {
    let args: Vec<String> = env::args().into_iter().collect();
    let strargs: Vec<&str> = args.iter().map(|i| i.as_str()).collect();
    
    match strargs.as_slice() {
	[_, "--dump-expr"] => dump_expr(),
	[_, "--dump-ast", path] => dump_ast(path),
	[_, "--compile", path] => dump_ir(path),
	_ => println!("Invalid args. Usage string TBD.")
    };
}
