use super::op::{Op, OpKind, Operand};

pub struct Generator {
    ops: Vec<Op>,
    label_count: usize,
}

impl Generator {
    pub fn new() -> Self {
        let ops = Vec::new();

        Self {
            ops,
            label_count: 0,
        }
    }

    pub fn label(&mut self, name: String) {
        let op = Op::single(OpKind::Label, Operand::Symbol(name));

        self.ops.push(op);
    }

    pub fn allocate_label(&mut self) -> String {
        let name = format!("block_{}", self.label_count);
        self.label_count += 1;
        name
    }

    pub fn label_auto(&mut self) {
        let op = Op::single(OpKind::Label, Operand::Symbol(self.allocate_label()));

        self.ops.push(op);
    }

    pub fn append(&mut self, op: Op) {
        self.ops.push(op);
    }

    pub fn eject(&mut self) -> Vec<Op> {
        self.ops.clone()
    }
}
