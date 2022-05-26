use super::op::{Op};

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

    pub fn allocate_label(&mut self) -> String {
        let name = format!("block_{}", self.label_count);
        self.label_count += 1;
        name
    }

    pub fn append(&mut self, op: Op) {
        self.ops.push(op);
    }

    pub fn eject(&mut self) -> Vec<Op> {
        self.ops.clone()
    }
}
