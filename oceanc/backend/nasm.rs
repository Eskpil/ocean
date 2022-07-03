use super::scope::{BackendScope};
use crate::ir::{op::{Op, OpKind, Type}, register::Register};
use crate::ast::BinaryOp;
use std::io::Write;
use std::fs::File;
use std::collections::HashMap;

#[derive(Debug)]
pub struct NasmBackend {
    scopes: Vec<BackendScope>,    
    current: BackendScope,
    output: File,

    externals: Vec<String>,

    convention: HashMap<u64, String>,
}

pub fn native_reg(reg: &Register) -> String {
    let res = match reg.clone() {
        Register::R1 => "rax",
        Register::R2 => "rcx",
        Register::R3 => "rdx",
        Register::R4 => "rbx",
        Register::R5 => "rsi",
        Register::R6 => "rdi",
        Register::R7 => "r8",
        Register::R8 => "r9",
        Register::R9 => "r10",
        Register::R10 => "r11",
        Register::R11 => "r12",
        Register::R12 => "r13",
        Register::R13 => "r14",
        Register::R14 => "r15",
    };

    res.to_string()
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

        let mut backend = Self {
            scopes: vec![],
            externals: vec![],
            output: file,
            convention,
            current: BackendScope::new("main".to_string()),
        };

        backend
    }    

    pub fn generate_header(&mut self, externals: Vec<String>) {
        write!(self.output, "BITS 64\n");

        for external in externals.iter() {
            write!(self.output, "extern {external}\n");
        }

        write!(self.output, "extern gpa_allocate_sized\n");
        write!(self.output, "extern gpa_allocate_counted\n");

        write!(self.output, "extern gpa_memory_free\n");
        write!(self.output, "extern gpa_memory_ref_inc\n");
        write!(self.output, "extern gpa_memory_ref_dec\n");

        write!(self.output, "extern gpa_memory_set_object_field\n");
        write!(self.output, "extern gpa_memory_set_num_field\n");
        write!(self.output, "extern gpa_memory_set_ptr_field\n");

        write!(self.output, "extern gpa_memory_get_object_field\n");
        write!(self.output, "extern gpa_memory_get_num_field\n");
        write!(self.output, "extern gpa_memory_get_ptr_field\n");

        write!(self.output, "extern runtime_allocate_array\n");

        write!(self.output, "extern runtime_array_at_object\n");
        write!(self.output, "extern runtime_array_at_ptr\n");
        write!(self.output, "extern runtime_array_at_num\n");

        write!(self.output, "extern runtime_array_append_object\n");
        write!(self.output, "extern runtime_array_append_ptr\n");
        write!(self.output, "extern runtime_array_append_num\n");

        write!(self.output, "segment .text\n");
        write!(self.output, "    global main\n");
        write!(self.output, "    main:\n");
        write!(self.output, "    push rbp\n");
        write!(self.output, "    mov rbp, rsp\n");
        write!(self.output, "    mov rax, 0\n");
        write!(self.output, "    call program\n");
        write!(self.output, "    mov rax, 0\n");
        write!(self.output, "    pop rbp\n");
        write!(self.output, "    ret\n");
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
                    self.new_scope(symbol.clone());
                    write!(self.output, "    {}:\n", symbol);
                } 
                OpKind::Proc => {
                    let symbol = op.operands()[0].as_symbol();
                    let parameters_size = op.operands()[1].as_uint();
                    write!(self.output, "    ; -- Proc --\n");

                    self.new_scope(symbol.clone());

                    write!(self.output, "    {}:\n", symbol);
                    write!(self.output, "    push rbp\n");
                    write!(self.output, "    mov rbp, rsp\n");

                    for i in 0..parameters_size {
                        let reg = self.convention.get(&i).unwrap();
                        write!(self.output, "    push {}\n", reg);
                    }
                }
                OpKind::NewParameter => {
                    let src = op.operands()[0].as_uint();
                    let offset = op.operands()[1].as_uint();
                    let typ = op.operands()[2].as_type();
                    write!(self.output, "    ; -- NewParameter -- \n");
                    write!(self.output, "    mov [rbp-{:x}], {}\n", offset, self.convention.get(&src).unwrap());

                    if typ == Type::Reference {
                        write!(self.output, "    mov {}, {}\n", self.convention.get(&0).unwrap(), self.convention.get(&src).unwrap());
                        write!(self.output, "    call gpa_memory_ref_inc\n"); 
                    }
                }
                OpKind::End => {
                    write!(self.output, "    ; -- End --\n");

                    write!(self.output, "    pop rbp\n");
                    write!(self.output, "    ret\n");
                }
                OpKind::Load => {
                    let dst = op.operands()[0].as_reg();
                    let value = op.operands()[1].as_uint();

                    write!(self.output, "    ; -- Load --\n");
                    write!(self.output, "    mov {}, {}\n", native_reg(&dst), value);
                }
                OpKind::Intrinsic => {
                    let binary_op = op.operands()[0].as_op();

                    let dst = op.operands()[1].as_reg();
                    let lhs = op.operands()[2].as_reg();
                    let rhs = op.operands()[3].as_reg();

                    write!(self.output, "    ; -- Intrinsic({:?}) --\n", binary_op);
                    match binary_op {
                        BinaryOp::Add => {
                            write!(self.output, "    add {}, {}\n", native_reg(&lhs), native_reg(&rhs));
                            write!(self.output, "    mov {}, {}\n", native_reg(&dst), native_reg(&lhs));
                        }
                        BinaryOp::Mul => {
                            write!(self.output, "    imul {}, {}\n", native_reg(&lhs), native_reg(&rhs));
                            write!(self.output, "    mov {}, {}\n", native_reg(&dst), native_reg(&lhs));
                        }
                        BinaryOp::Div => {
                            write!(self.output, "    mov {}, rdx\n", native_reg(&lhs));
                            write!(self.output, "    cqo\n");
                            write!(self.output, "    div {}\n", native_reg(&rhs));
                            write!(self.output, "    mov {}, rbx\n", native_reg(&dst));

                        }
                        BinaryOp::Sub => {
                            write!(self.output, "   sub {}, {}\n", native_reg(&lhs), native_reg(&rhs));
                            write!(self.output, "   mov {}, {}\n", native_reg(&dst), native_reg(&lhs));
                        }
                        BinaryOp::Greater => {
                           write!(self.output, "    cmp {}, {}\n", native_reg(&lhs), native_reg(&rhs));
                           write!(self.output, "    setg al\n");
                           write!(self.output, "    movzx {}, al\n", native_reg(&dst));
                        }
                        BinaryOp::GreaterEquals => {
                           write!(self.output, "    cmp {}, {}\n", native_reg(&lhs), native_reg(&rhs));
                           write!(self.output, "    setge al\n");
                           write!(self.output, "    movzx {}, al\n", native_reg(&dst));
                        }
                        BinaryOp::Less => {
                           write!(self.output, "    cmp {}, {}\n", native_reg(&lhs), native_reg(&rhs));
                           write!(self.output, "    setl al\n");
                           write!(self.output, "    movzx {}, al\n", native_reg(&dst));
                        }
                        BinaryOp::LessEquals => {
                           write!(self.output, "    cmp {}, {}\n", native_reg(&lhs), native_reg(&lhs));
                           write!(self.output, "    setle al\n");
                           write!(self.output, "    movzx {}, al\n", native_reg(&dst));

                        }
                        o => todo!("Implement instrinsic: {:?}", o),
                    }
                }
                OpKind::NewVariable => {
                    let src = op.operands()[0].as_reg();
                    let offset = op.operands()[1].as_uint();
                    let typ = op.operands()[2].as_type();
                    write!(self.output, "    ; -- NewVariable -- \n");
                    write!(self.output, "    mov [rbp-{:x}], {}\n", offset, native_reg(&src));

                    if typ == Type::Reference {
                        write!(self.output, "    mov {}, {}\n", self.convention.get(&0).unwrap(), native_reg(&src));
                        write!(self.output, "    call gpa_memory_ref_inc\n"); 
                    }
                }
                OpKind::ResolveVariable => {
                    let dst = op.operands()[0].as_reg();
                    let offset = op.operands()[1].as_uint();
                    write!(self.output, "    ; -- ResolveVariable --\n");
                    write!(self.output, "    mov {}, [rbp-{:x}]\n", native_reg(&dst), offset);
                }
                OpKind::NewString => {
                    let dst = op.operands()[0].as_reg();
                    let data = op.operands()[1].as_data(); 

                    let scoped_name = self.current.append_data(data.clone());

                    write!(self.output, "    ; -- NewString --\n");
                    write!(self.output, "    mov {},{}\n", native_reg(&dst), scoped_name);
                }
                OpKind::Call => {
                    let symbol = op.operands()[0].as_symbol();
                    let args = op.operands()[1].as_regs();
                    let dst = op.operands()[2].as_reg();

                    write!(self.output, "    ; -- Call --\n");

                    for arg in args.iter() {
                        write!(self.output, "    push {}\n", native_reg(&arg));
                    }

                    let mut i = (args.len()) as u64;

                    while i > 0 {
                        let reg = self.convention.get(&(i - 1)).unwrap();
                        write!(self.output, "    pop {}\n", reg);
                        i -= 1; 
                    }

                    write!(self.output, "    xor eax, eax\n");
                    write!(self.output, "    call {}\n", symbol);
                    write!(self.output, "    mov {}, rax\n", native_reg(&dst));
                }
                OpKind::Jump => {
                    let symbol = op.operands()[0].as_symbol();
                    write!(self.output, "    ; -- Jump --\n");
                    write!(self.output, "    jmp {}\n", symbol);
                }
                OpKind::JumpUnless => {
                    let guard = op.operands()[0].as_reg();
                    let symbol = op.operands()[1].as_symbol();
                    write!(self.output, "    ; -- JumpUnless --\n");
                    write!(self.output, "    test {}, {}\n", native_reg(&guard), native_reg(&guard));
                    write!(self.output, "    jz {}\n", symbol);
                }
                OpKind::Return => {
                    let reg = op.operands()[0].as_reg();

                    write!(self.output, "    ; -- Return --\n");
                    write!(self.output, "    mov rax, {}\n", native_reg(&reg));

                    write!(self.output, "    pop rbp\n");
                    write!(self.output, "    ret\n");
                }
                OpKind::SetField => {
                    write!(self.output, "    ; -- SetField --\n");
                    let value = op.operands()[0].as_reg();
                    let structure = op.operands()[1].as_reg();
                    let offset = op.operands()[2].as_uint();  
                    let typ = op.operands()[3].as_type();

                    write!(self.output, "    mov {}, {}\n", self.convention.get(&0).unwrap(), native_reg(&structure));
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&1).unwrap(), offset);
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&2).unwrap(), native_reg(&value));
                    write!(self.output, "    xor eax, eax\n");

                    match typ {
                        Type::Reference |
                        Type::Object => {
                            write!(self.output, "    call gpa_memory_set_object_field\n");                                      
                        }
                        Type::Num => {
                            write!(self.output, "    call gpa_memory_set_num_field\n");
                        }
                        Type::Ptr => {
                            write!(self.output, "    call gpa_memory_set_ptr_field\n");
                        }
                    }
                }
                OpKind::ResolveField => {
                    write!(self.output, "    ; -- ResolveField --\n");
                    let structure = op.operands()[0].as_reg();
                    let reg = op.operands()[1].as_reg();
                    let offset = op.operands()[2].as_uint();
                    let typ = op.operands()[3].as_type();

                    write!(self.output, "    mov {}, {}\n", self.convention.get(&0).unwrap(), native_reg(&structure));
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&1).unwrap(), offset);
                    write!(self.output, "    xor eax, eax\n");

                    match typ {
                        Type::Reference |
                        Type::Object => {
                            write!(self.output, "    call gpa_memory_get_object_field\n");                                      
                        }
                        Type::Num => {
                            write!(self.output, "    call gpa_memory_get_num_field\n");
                        }
                        Type::Ptr => {
                            write!(self.output, "    call gpa_memory_get_ptr_field\n");
                        }
                    }

                    write!(self.output, "    mov {}, rax\n", native_reg(&reg));
                }
                OpKind::NewStruct => {
                    let dst = op.operands()[0].as_reg();
                    let size = op.operands()[1].as_uint();
                    write!(self.output, "    ; -- NewStruct --\n");
                    write!(self.output, "    mov rdi, {}\n", size);
                    write!(self.output, "    call gpa_allocate_counted\n");
                    write!(self.output, "    mov {}, rax\n", native_reg(&dst));
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
                OpKind::ArrayInit => {
                    let dst = op.operands()[0].as_reg();
                    let elem_size = op.operands()[1].as_uint(); 
                    let array_size = op.operands()[2].as_uint();

                    write!(self.output, "    ; -- ArrayInit --\n");
                    write!(self.output, "    mov {}, {elem_size}\n", self.convention.get(&0).unwrap());
                    write!(self.output, "    mov {}, {array_size}\n", self.convention.get(&1).unwrap());
                    write!(self.output, "    xor eax, eax\n");
                    write!(self.output, "    call runtime_allocate_array\n");
                    write!(self.output, "    mov {}, rax\n", native_reg(&dst));
                }
                OpKind::ArrayAppend => {
                    let val = op.operands()[0].as_reg();
                    let array = op.operands()[1].as_reg();
                    let typ = op.operands()[2].as_type();

                    write!(self.output, "    ; -- ArrayAppend --\n");
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&1).unwrap(), native_reg(&array));
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&0).unwrap(), native_reg(&val));
                    write!(self.output, "    xor eax, eax\n");

                    match typ {
                        Type::Reference |
                        Type::Object => {
                            write!(self.output, "    call runtime_array_append_object\n");                                      
                        }
                        Type::Num => {
                            write!(self.output, "    call runtime_array_append_num\n");
                        }
                        Type::Ptr => {
                            write!(self.output, "    call runtime_array_append_ptr\n");
                        }
                    }

                    write!(self.output, "    push rax\n");;
                }
                OpKind::ArrayIndex => {
                    let array = op.operands()[0].as_reg();
                    let dst = op.operands()[1].as_reg();
                    let index = op.operands()[2].as_uint();
                    let typ = op.operands()[3].as_type();

                    write!(self.output, "    ; -- ArrayIndex --\n"); 
                    write!(self.output, "    mov {}, {}\n", self.convention.get(&0).unwrap(), native_reg(&array));
                    write!(self.output, "    mov {}, {index}\n", self.convention.get(&1).unwrap());

                    match typ {
                        Type::Reference |
                        Type::Object => {
                            write!(self.output, "    call runtime_array_at_object\n");                                      
                        }
                        Type::Num => {
                            write!(self.output, "    call runtime_array_at_num\n");
                        }
                        Type::Ptr => {
                            write!(self.output, "    call runtime_array_at_ptr\n");
                        }
                    }

                    write!(self.output, "    mov {}, rax\n", native_reg(&dst));
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
