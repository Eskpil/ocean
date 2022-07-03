use super::op::{Op};
use super::register::{Register};

use std::collections::HashMap;

pub struct Generator {
    ops: Vec<Op>,
    label_count: usize,
    externals: Vec<String>,

    registers: HashMap<Register, bool>
}

impl Generator {
    pub fn new() -> Self {
        let ops = Vec::new();
        let mut registers = HashMap::new();

        registers.insert(Register::R1, true);
        registers.insert(Register::R2, true);
        registers.insert(Register::R3, true);
        registers.insert(Register::R4, true);
        registers.insert(Register::R5, true);
        registers.insert(Register::R6, true);
        registers.insert(Register::R7, true);
        registers.insert(Register::R8, true);
        registers.insert(Register::R9, true);
        registers.insert(Register::R10, true);
        registers.insert(Register::R11, true);
        registers.insert(Register::R12, true);
        registers.insert(Register::R13, true);
        registers.insert(Register::R14, true);

        Self {
            ops,
            externals: vec![],
            label_count: 0,

            registers,
        }
    }

    pub fn dump_registers(&self) {
        for (reg, released) in self.registers.iter() {
            println!("{:?}:{released}", reg);
        }
    }

    pub fn allocate_reg(&mut self) -> Register {
        for (reg, released) in self.registers.iter_mut() {
            if *released {
                *released = false;
                return *reg;
            }
        }

        panic!("Unable to allocate register.");
    }

    pub fn release_reg(&mut self, reg: Register) {
        self.registers.insert(reg, true); 
    }

    pub fn add_external(&mut self, name: String) {
        self.externals.push(name);
    }

    pub fn allocate_label(&mut self) -> String {
        let name = format!("block_{}", self.label_count);
        self.label_count += 1;
        name
    }

    pub fn append(&mut self, op: Op) {
        self.ops.push(op);
    }

    pub fn eject_ops(&mut self) -> Vec<Op> {
        self.ops.clone()
    }

    pub fn eject_externals(&mut self) -> Vec<String> {
        self.externals.clone()
    }
}

