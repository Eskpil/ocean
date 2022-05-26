use super::scope::{BackendScope};
use crate::ir::{op::{Op, OpKind}};
use crate::ast::BinaryOp;
use std::io::Write;
use std::fs::File;
use std::collections::HashMap;

#[derive(Debug)]
pub struct NasmBackend {
    scopes: Vec<BackendScope>,    
    current: BackendScope,
    output: File,
}

impl NasmBackend {
    pub fn new(output_path: String) -> Self {
        let mut file = File::create(output_path).unwrap(); 
    
        write!(file, "BITS 64\n");
        write!(file, "segment .text\n");
        write!(file, "    global main\n");
        write!(file, "    main:\n");
        write!(file, "    push rbp\n");
        write!(file, "    mov rbp, rsp\n");
        write!(file, "    mov rax, 0\n");
        write!(file, "    call program\n");
        write!(file, "    mov rax, 0\n");
        write!(file, "    pop rbp\n");
        write!(file, "    ret\n");

        Self {
            scopes: vec![],
            output: file,
            current: BackendScope::new("main".to_string()),
        }
    }    

    pub fn new_scope(&mut self, name: String) {
        self.scopes.push(self.current.clone());
        let scope = BackendScope::from(name, self.current.clone());
        self.current = scope;
    }

    pub fn generate_ops(&mut self, ops: Vec<Op>) {
        for op in ops.iter() {
            match op.kind() {
                OpKind::Block => {
                    let symbol = op.operands()[0].as_symbol(); 
                    self.new_scope(symbol.clone());
                    write!(self.output, "    {}:\n", symbol);
                } 
                OpKind::Proc => {
                    let symbol = op.operands()[0].as_symbol();
                    self.new_scope(symbol.clone());
                    write!(self.output, "    {}:\n", symbol);
                    write!(self.output, "    push rbp\n");
                    write!(self.output, "    mov rbp, rsp\n");
                }
                OpKind::End => {
                    // Clear our stack garbage.
                    for i in 0..self.current.count {
                        write!(self.output, "    pop rax\n");
                    }

                    write!(self.output, "    pop rbp\n");
                    write!(self.output, "    ret\n");
                }
                OpKind::Push => {
                    let value = op.operands()[0].as_uint();
                    write!(self.output, "    mov rax, {}\n", value);
                    write!(self.output, "    push rax\n");
                    // We have pushed a value onto the stack.
                    self.current.count += 1;
                }
                OpKind::Intrinsic => {
                    let binary_op = op.operands()[0].as_op();
                    match binary_op {
                        BinaryOp::Add => {
                            write!(self.output, "    pop rax\n");
                            write!(self.output, "    pop rbx\n");
                            write!(self.output, "    add rax, rbx\n");
                            write!(self.output, "    push rax\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.count -= 1;
                        }
                        BinaryOp::Mul => {
                            write!(self.output, "    pop rax\n");
                            write!(self.output, "    pop rbx\n");
                            write!(self.output, "    imul rax, rbx\n");
                            write!(self.output, "    push rax\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.count -= 1;                           
                        }
                        BinaryOp::Div => {
                            write!(self.output, "   pop rax\n"); // What we are dividing by.
                            write!(self.output, "   pop rbx\n"); // What we are dividing.
                            write!(self.output, "   mov rbx, rdx\n");
                            write!(self.output, "   cqo\n");
                            write!(self.output, "   div rax\n");
                            write!(self.output, "   push rbx\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.count -= 1;                           
                        }
                        BinaryOp::Sub => {
                            write!(self.output, "   pop rax\n"); // What we are subtracting with.
                            write!(self.output, "   pop rbx\n"); // What we are subtracting.
                            write!(self.output, "   sub rax, rbx\n");
                            write!(self.output, "   push rax\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.count -= 1;                           
                        }
                        BinaryOp::Greater => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setg al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.count -= 1;
                        }
                        BinaryOp::GreaterEquals => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setge al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");  
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.count -= 1;
                        }
                        BinaryOp::Less => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setl al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.count -= 1;
                        }
                        BinaryOp::LessEquals => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setle al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.count -= 1;

                        }
                        o => todo!("Implement instrinsic: {:?}", o),
                    }
                }
                OpKind::NewVariable => {
                    let symbol = op.operands()[0].as_symbol();
                    let var_name = self.current.append(symbol);
                    write!(self.output, "    pop rax\n");
                    write!(self.output, "    mov [rel {}], rax\n", var_name);
                    // We have taken one value of the stack and moved it into a variable.
                    if self.current.count >= 1 { self.current.count -= 1; }
                }
                OpKind::ResolveVariable => {
                    let symbol = op.operands()[0].as_symbol();
                    let var_name = self.current.find(symbol.clone());
                    write!(self.output, "    lea rax, [rel {}]\n", var_name);
                    write!(self.output, "    push rax\n");
                    // We have resolved a value from a variables onto the stack.
                    self.current.count += 1;
                }
                OpKind::Call => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    call {}\n", symbol);
                }
                OpKind::Jump => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    jmp {}\n", symbol);
                }
                OpKind::JumpUnless => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    pop rax\n");
                    write!(self.output, "    cmp rax, 0\n");
                    write!(self.output, "    je {}\n", symbol);
                }
                other => unimplemented!("Generating for: {:?} not implemented yet", other) 
            }
        }  
    }

    pub fn finish(&mut self) {
        write!(self.output, "segment .bss\n");
        self.scopes.push(self.current.clone());
        let mut variables = HashMap::<String, String>::new();
        for scope in self.scopes.iter() {
            for variable in scope.eject_variables() {
                variables.insert(variable.0, variable.1);
            }
        }

        for variable in variables {
            write!(self.output, "    {}: resb 8\n", variable.1);
        }
    }
}
