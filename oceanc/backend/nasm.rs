use super::scope::{BackendScope};
use crate::ir::{op::{Op, OpKind, Type}};
use crate::ast::BinaryOp;
use std::io::Write;
use std::fs::File;
use std::collections::HashMap;

#[derive(Debug)]
pub struct NasmBackend {
    scopes: Vec<BackendScope>,    
    current: BackendScope,
    output: File,

    convention: HashMap<u64, String>,
}

impl NasmBackend {
    pub fn new(output_path: String) -> Self {
        let mut file = File::create(output_path).unwrap(); 
        let mut convention = HashMap::<u64, String>::new();

        // TODO: Implement the right-left stack convention.
        convention.insert(0, "rdi".into());
        convention.insert(1, "rsi".into());
        convention.insert(2, "rdx".into());
        convention.insert(3, "rcx".into());
        convention.insert(4, "r8".into());
        convention.insert(5, "r9".into());
    
        write!(file, "BITS 64\n");
        write!(file, "extern printf\n");
        write!(file, "extern gpa_allocate_sized\n");
        write!(file, "extern gpa_memory_free\n");
        write!(file, "extern gpa_memory_ref_inc\n");
        write!(file, "extern gpa_memory_ref_dec\n");
        write!(file, "extern gpa_allocate_counted\n");
        write!(file, "extern gpa_memory_set_object_field\n");
        write!(file, "extern gpa_memory_set_num_field\n");
        write!(file, "extern gpa_memory_set_ptr_field\n");
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
            convention,
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
                    write!(self.output, "    ; -- Block --\n");
                    for i in 0..self.current.gc_count {
                        write!(self.output, "    pop rax\n");
                    }
                    self.new_scope(symbol.clone());
                    write!(self.output, "    {}:\n", symbol);
                } 
                OpKind::Proc => {
                    let symbol = op.operands()[0].as_symbol();
                    let parameters_size = op.operands()[1].as_uint();
                    write!(self.output, "    ; -- Proc --\n");
                    for i in 0..self.current.gc_count {
                        write!(self.output, "    pop rax\n");
                    }
                    self.new_scope(symbol.clone());
                    write!(self.output, "    {}:\n", symbol);
                    write!(self.output, "    push rbp\n");
                    write!(self.output, "    mov rbp, rsp\n");

                    for i in 0..parameters_size {
                        let reg = self.convention.get(&i).unwrap();
                        write!(self.output, "    push {}\n", reg);
                        self.current.gc_count += 1;
                    }
                }
                OpKind::End => {
                    write!(self.output, "    ; -- End --\n");

                    // Clear our stack garbage.
                    for i in 0..self.current.gc_count {
                        write!(self.output, "    pop rax\n");
                    }

                    write!(self.output, "    pop rbp\n");
                    write!(self.output, "    ret\n");
                }
                OpKind::Push => {
                    let value = op.operands()[0].as_uint();
                    write!(self.output, "    ; -- Push --\n");
                    write!(self.output, "    mov rax, {}\n", value);
                    write!(self.output, "    push rax\n");
                    // We have pushed a value onto the stack.
                    self.current.gc_count += 1;
                }
                OpKind::Intrinsic => {
                    let binary_op = op.operands()[0].as_op();
                    write!(self.output, "    ; -- Intrinsic({:?}) --\n", binary_op);
                    match binary_op {
                        BinaryOp::Add => {
                            write!(self.output, "    pop rax\n");
                            write!(self.output, "    pop rbx\n");
                            write!(self.output, "    add rax, rbx\n");
                            write!(self.output, "    push rax\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.gc_count -= 1;
                        }
                        BinaryOp::Mul => {
                            write!(self.output, "    pop rax\n");
                            write!(self.output, "    pop rbx\n");
                            write!(self.output, "    imul rax, rbx\n");
                            write!(self.output, "    push rax\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.gc_count -= 1;                           
                        }
                        BinaryOp::Div => {
                            write!(self.output, "    pop rax\n"); // What we are dividing by.
                            write!(self.output, "    pop rbx\n"); // What we are dividing.
                            write!(self.output, "    mov rbx, rdx\n");
                            write!(self.output, "    cqo\n");
                            write!(self.output, "    div rax\n");
                            write!(self.output, "    push rbx\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.gc_count -= 1;                           
                        }
                        BinaryOp::Sub => {
                            write!(self.output, "   pop rax\n"); // What we are subtracting with.
                            write!(self.output, "   pop rbx\n"); // What we are subtracting.
                            write!(self.output, "   sub rax, rbx\n");
                            write!(self.output, "   push rax\n");
                            // 2 - 1 = 1. We have taken one element of the stack.
                            self.current.gc_count -= 1;                           
                        }
                        BinaryOp::Greater => {
                           write!(self.output, "    mov rcx, 0\n"); 
                           write!(self.output, "    mov rdx, 1\n");
                           write!(self.output, "    pop rbx\n");
                           write!(self.output, "    pop rax\n");
                           write!(self.output, "    cmp rax, rbx\n");
                           write!(self.output, "    cmovg rcx, rdx\n");
                           write!(self.output, "    push rcx\n");

                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.gc_count -= 1;
                        }
                        BinaryOp::GreaterEquals => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setge al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");  
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.gc_count -= 1;
                        }
                        BinaryOp::Less => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setl al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.gc_count -= 1;
                        }
                        BinaryOp::LessEquals => {
                           write!(self.output, "    pop rax\n"); // 1 > 0 in this case we have 0. 
                           write!(self.output, "    pop rbx\n"); // 1 > 0 in this case we have 1.
                           write!(self.output, "    cmp rbx, rax\n");
                           write!(self.output, "    setle al\n");
                           write!(self.output, "    movzx rax, al\n");
                           write!(self.output, "    push rax\n");
                           // 2 - 1 = 1. We have taken one element of the stack.
                           self.current.gc_count -= 1;

                        }
                        o => todo!("Implement instrinsic: {:?}", o),
                    }
                }
                OpKind::NewVariable => {
                    let offset = op.operands()[0].as_uint();
                    let typ = op.operands()[1].as_type();
                    write!(self.output, "    ; -- NewVariable -- \n");
                    write!(self.output, "    pop rax\n");
                    write!(self.output, "    mov [rbp-{:x}], rax\n", offset);

                    if typ == Type::Reference {
                        write!(self.output, "    mov {}, rax\n", self.convention.get(&0).unwrap());
                        write!(self.output, "    call gpa_memory_ref_inc\n"); 
                    }

                    // We have taken one value of the stack and moved it into a variable.
                    self.current.gc_count -= 1;
                }
                OpKind::ResolveVariable => {
                    let offset = op.operands()[0].as_uint();
                    write!(self.output, "    ; -- ResolveVariable --\n");
                    write!(self.output, "    mov rax, [rbp-{:x}]\n", offset);
                    write!(self.output, "    push rax\n");
                    // We have resolved a value from a variables onto the stack.
                    self.current.gc_count += 1;
                }
                OpKind::NewString => {
                    let data = op.operands()[0].as_data(); 
                    let scoped_name = self.current.append_data(data.clone());

                    write!(self.output, "    ; -- NewString --\n");
                    write!(self.output, "    mov rax,{}\n", scoped_name);
                    write!(self.output, "    push rax\n");

                    self.current.gc_count += 1;
                }
                OpKind::Call => {
                    let symbol = op.operands()[0].as_symbol();
                    let arguments_size = op.operands()[1].as_uint();

                    write!(self.output, "    ; -- Call --\n");

                    for i in 0..arguments_size {
                        let reg = self.convention.get(&i).unwrap();
                        write!(self.output, "    pop {}\n", reg);
                        self.current.gc_count -= 1;
                    }

                    write!(self.output, "    xor eax, eax\n");
                    write!(self.output, "    call {}\n", symbol);

                    if op.operands()[2].as_bool() {
                        write!(self.output, "    push rax\n");
                        self.current.gc_count += 1;
                    }
                }
                OpKind::Jump => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    ; -- Jump --\n");
                    for i in 0..self.current.gc_count {
                        write!(self.output, "    pop rax\n");
                    }
                    write!(self.output, "    jmp {}\n", symbol);
                }
                OpKind::JumpUnless => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    ; -- JumpUnless --\n");
                    write!(self.output, "    pop rax\n");
                    write!(self.output, "    test rax, rax\n");
                    write!(self.output, "    jz {}\n", symbol);
                    self.current.gc_count -= 1;
                }
                OpKind::Return => {
                    write!(self.output, "    ; -- Return --\n");
                    write!(self.output, "    pop rax\n");
                    write!(self.output, "    pop rbp\n");
                    write!(self.output, "    ret\n");
                }
                OpKind::SetField => {
                    write!(self.output, "    ; -- SetField --\n");
                    let offset = op.operands()[0].as_uint();  
                    let typ = op.operands()[1].as_type();

                    // write!(self.output, "    pop {}\n", self.convention.get(&2).unwrap());
                    // write!(self.output, "    pop rax\n");
                    // write!(self.output, "    mov rax, {}\n", self.convention.get(&0).unwrap());
                    // write!(self.output, "    mov {}, {}\n", self.convention.get(&1).unwrap(), offset);
                    
                    // Data
                    write!(self.output, "    pop rax\n");

                    // Memory
                    write!(self.output, "    pop r12\n");

                    write!(self.output, "    mov {}, r12\n", self.convention.get(&0).unwrap());
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&1).unwrap(), offset);
                    write!(self.output, "    mov {}, rax\n", self.convention.get(&2).unwrap());

                    match typ {
                        Type::Num => {
                            write!(self.output, "    call gpa_memory_set_num_field\n");
                        }                        
                        Type::Ptr => {
                            write!(self.output, "    call gpa_memory_set_ptr_field\n");
                        }
                        Type::Reference |
                        Type::Object => {
                            write!(self.output, "    call gpa_memory_set_object_field\n");
                        }
                    }

                    write!(self.output, "    push r12\n");

                    self.current.gc_count -= 1;
                }
                OpKind::NewStruct => {
                    let size = op.operands()[0].as_uint();
                    write!(self.output, "    ; -- NewStruct --\n");
                    write!(self.output, "    mov rdi, {}\n", size);
                    write!(self.output, "    call gpa_allocate_counted\n");
                    write!(self.output, "    push rax\n");
                    self.current.gc_count += 1;
                }
                OpKind::Deref => {
                    let offset = op.operands()[0].as_uint();
                    write!(self.output, "    ; -- Deref --\n");
                    write!(self.output, "    mov rax, [rbp-{:x}]\n", offset);
                    write!(
                        self.output, 
                        "    mov {}, rax\n", 
                        self.convention.get(&0).unwrap(), 
                    );

                    write!(self.output, "    call gpa_memory_ref_dec\n");
                }
                other => unimplemented!("Generating for: {:?} not implemented yet", other) 
            }
        }  
    }

    pub fn finish(&mut self) {
        self.scopes.push(self.current.clone());
        let mut bss = HashMap::<String, u64>::new();
        let mut data = HashMap::<String, String>::new();
        for scope in self.scopes.iter() {
            for variable in scope.eject_variables() {
                bss.insert(variable.1.scoped_name, variable.1.size);
            }
            
            for d in scope.eject_data() {
                data.insert(d.0, d.1);
            }
        }

        write!(self.output, "segment .data\n");
        for value in data {
            write!(self.output, "    {}: db ", value.0); 
            for byte in value.1.as_bytes() {
                write!(self.output, "{}, ", byte);
            }
            write!(self.output, "00");
            write!(self.output, "\n");
        }
        write!(self.output, "segment .bss\n");
        for value in bss {
            write!(self.output, "    {}: resb {}\n", value.0, value.1); 
        }    
    }
}
