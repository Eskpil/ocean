mod ast;
mod ir;
mod lexer;
mod parser;
mod backend;

use ast::statements::Statement;
use ir::generator::Generator;
use backend::nasm::{NasmBackend};
use std::env;

use parser::Parser;

fn main() {
    let mut args = env::args();

    if args.len() < 2 {
        eprintln!("Error: Please provide file name");
        std::process::exit(1);
    }

    let file_path = args.nth(1).unwrap();

    let mut parser = Parser::new(std::fs::read_to_string(file_path).unwrap());

    let program = parser.parse();

    let mut generator = Generator::new();

    program.print(0);
    program.generate(&mut generator);

    let ops = generator.eject();

    println!("Ops:");

    for op in ops.iter() {
        println!("  {:?}", op); 
    }

    let mut backend = NasmBackend::new("test.asm".to_string());
    backend.generate_ops(ops);
    backend.finish();
}
