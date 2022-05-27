mod ast;
mod ir;
mod lexer;
mod parser;
mod backend;
mod errors;
mod util;

use ast::statements::Statement;
use errors::syntax::SyntaxError;
use ir::generator::Generator;
use backend::nasm::{NasmBackend};
use std::env;
use std::process;

use parser::Parser;

fn main() {
    let mut args = env::args();

    if args.len() < 2 {
        eprintln!("Error: Please provide file name");
        std::process::exit(1);
    }

    let file_path = args.nth(1).unwrap();

    let mut parser = Parser::new(std::fs::read_to_string(file_path).unwrap());

    let mut children = Vec::<Statement>::new();

    loop {
        match parser.parse_statement() {
            Ok(statement) => children.push(statement),
            Err(err) => {
                if let SyntaxError::End = err {
                    break;
                } else {
                    panic!("Encountered unexepected error: {:?}", err);
                    process::exit(1);
                } 
            }
        }
    }

    let program = Statement::Program(children);

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

    util::run_cmd_echoed("nasm -felf64 test.asm".to_string());
    util::run_cmd_echoed("gcc -o test ./test.o".to_string());
}
