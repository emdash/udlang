use udlang::grammar;
use udlang::ast::Builder;

use std::io::{
    BufReader,
    BufRead,
    Read,
    stdin
};

fn dump_expr(text: &str) {
    let builder = Builder::new();
    println!("{:?}", grammar::ExprParser::new().parse(&builder, text));
}

fn main() {
    loop {
	let mut reader = BufReader::new(stdin());
	let mut line = String::new();
	reader.read_line(&mut line);
	dump_expr(&line);
    }
}
