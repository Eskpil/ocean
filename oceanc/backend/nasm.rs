use super::scope::{BackendScope};
use crate::ir::{op::{Op, OpKind}};
use std::io::Write;
use std::fs::File;

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
        write!(file, "    global _start\n");
        write!(file, "    _start:\n");

        Self {
            scopes: vec![],
            output: file,
            current: BackendScope::new("_start".to_string()),
        }
    }    

    pub fn new_scope(&mut self, name: String) {
        self.scopes.push(self.current.clone());
        let scope = BackendScope::new(name);
        self.current = scope;
}

    pub fn generate_ops(&mut self, ops: Vec<Op>) {
        for op in ops.iter() {
            match op.kind() {
                OpKind::Label => {
                    let symbol = op.operands()[0].as_symbol(); 
                    self.new_scope(symbol.clone());
                    write!(self.output, "    {}:\n", symbol);
                } 
                OpKind::Push => {
                    let value = op.operands()[0].as_float();
                    write!(self.output, "    sub rsp, 8\n");
                    write!(self.output, "    mov qword [rsp], __float32__({}.0)\n", value);
                }
                OpKind::Add => {
                    write!(self.output, "    cvtsi2sd xmm0, [rsp]\n");
                    write!(self.output, "    add rsp, 8\n");
                    write!(self.output, "    cvtsi2sd xmm1, [rsp]\n");
                    write!(self.output, "    add rsp, 8\n");
                    write!(self.output, "    addsd xmm0, xmm1\n");
                    write!(self.output, "    sub rsp, 8\n");
                    write!(self.output, "    movsd qword [rsp], xmm0\n");
                }
                other => unimplemented!("Generating for: {:?} not implemented yet", other) 
            }
        }  
    }

    pub fn finish(&mut self) {
        write!(self.output, "    exit:\n"); 
        write!(self.output, "    mov rax, 60\n");
        write!(self.output, "    mov rdi, 0\n");
        write!(self.output, "    syscall\n");
    }
}
