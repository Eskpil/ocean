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
        write!(file, "    global main\n");
        write!(file, "    main:\n");
        write!(file, "    call program\n");

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
                OpKind::LabelEnd => {
                    write!(self.output, "    ret\n");
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
                OpKind::NewVariable => {
                    let symbol = op.operands()[0].as_symbol();
                    let var_name = self.current.append(symbol);
                    write!(self.output, "    mov qword [{}], rsp\n", var_name);
                    write!(self.output, "    add rsp, 8\n");
                }
                OpKind::ResolveVariable => {
                    let symbol = op.operands()[0].as_symbol();
                    let var_name = self.current.find(symbol.clone());
                    write!(self.output, "    sub rsp, 8\n");
                    write!(self.output, "    lea rsp, [rel {}]\n", var_name);
                }
                OpKind::Call => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    call {}\n", symbol);
                }
                other => unimplemented!("Generating for: {:?} not implemented yet", other) 
            }
        }  
    }

    pub fn finish(&mut self) {
        write!(self.output, "segment .bss\n");
        self.scopes.push(self.current.clone());
        for scope in self.scopes.iter() {
            for variable in scope.eject_variables() {
                write!(self.output, "     {}: resb 8\n", variable.1); 
            }
        }
    }
}
