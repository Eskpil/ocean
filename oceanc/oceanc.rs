mod ast;
mod ir;
mod lexer;
mod parser;
mod backend;
mod errors;
mod util;
mod unescape;
mod types;

use ast::statements::Statement;
use errors::syntax::SyntaxError;
use ir::generator::Generator;
use ir::project::generate_project;
use backend::nasm::{NasmBackend};
use types::project::Project;
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

    let mut project = Project::new(); 

    match project.typecheck_program(&program) {
        Ok(_) => println!("Typechecked."),
        Err(err) => {
            eprintln!("Typechecking return error: {:?}", err);
            process::exit(1);
        }
    }

    generate_project(&project, &mut generator);

    // program.generate(&mut generator);

    let ops = generator.eject();

    let mut backend = NasmBackend::new("test.asm".to_string());
    backend.generate_ops(ops);
    backend.finish();

    // This is very bad. Runtime should definitily be install globally on the system
    // and be versioned. For now this is fine since we are just developing the language
    // and not actually using it.
    let pwd = env::current_dir().unwrap().into_os_string().into_string().unwrap();
    let runtime_link = format!("{pwd}/bld/runtime/runtime.so");

    util::run_cmd_echoed("nasm -felf64 test.asm".to_string());
    util::run_cmd_echoed(format!("clang -no-pie -o test {runtime_link} ./test.o"));
}
