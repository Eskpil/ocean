use crate::types::project::Project;
use crate::ir::generator::Generator;
use crate::parser::Parser;
use crate::ast::statements::Statement;
use crate::errors::syntax::SyntaxError;
use crate::backend::nasm::NasmBackend;
use crate::ir::project::generate_project;
use crate::util;

use std::process;
use std::fs;
use std::env;

pub struct Compiler {
    project: Project,
    generator: Generator,
    output: String,
}

impl Compiler {
    pub fn new(
        output_file: String,
    ) -> Self {
        Self {
            project: Project::new(),
            generator: Generator::new(),
            output: output_file,
        }
    }

    pub fn compile_file(
        &mut self,
        input_file: String,
    ) {
        let mut parser = Parser::new(fs::read_to_string(input_file).unwrap()); 

        let mut children = Vec::<Statement>::new();

        loop {
            match parser.parse_statement() {
                Ok(statement) => children.push(statement),
                Err(err) => {
                    if let SyntaxError::End = err {
                        break;
                    } else {
                        panic!("Encountered unexepected error: {:?}", err);
                    } 
                }
            }
        }

        let program = Statement::Program(children);

        self.project.typecheck_program(&program).unwrap();
    }

    pub fn generate_backend(
        &mut self,
    ) {
        generate_project(&self.project, &mut self.generator);
        let ops = self.generator.eject();

        let mut backend = NasmBackend::new(
            format!("{}.asm", self.output).to_string()
        );

        backend.generate_ops(ops);
        backend.finish();
    }

    pub fn link_binary(
        &mut self,
    ) {
        // This is very bad. Runtime should definitily be install globally on the system
        // and be versioned. For now this is fine since we are just developing the language
        // and not actually using it.
        let pwd = env::current_dir().unwrap().into_os_string().into_string().unwrap();
        let runtime_link = format!("{pwd}/bld/runtime/runtime.so");
        let object_file = format!("{}.o", self.output);

        util::run_cmd_echoed(format!("nasm -felf64 {}.asm", self.output));
        util::run_cmd_echoed(format!("clang -no-pie -o {} {runtime_link} ./{object_file}", self.output));
    }
}
