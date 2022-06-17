mod ast;
mod ir;
mod lexer;
mod parser;
mod backend;
mod errors;
mod util;
mod unescape;
mod types;
mod compiler;

use types::project::Project;
use compiler::Compiler;
use std::env;

use parser::Parser;

fn main() {
    let mut args = env::args();

    if args.len() < 2 {
        eprintln!("Error: Please provide file name");
        std::process::exit(1);
    }

    let file_path = args.nth(1).unwrap();

    let mut compiler = Compiler::new("program".into());

    compiler.compile_file(file_path);
    compiler.generate_backend();
    compiler.link_binary();
}
