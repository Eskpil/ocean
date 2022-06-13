use super::op::{Op};

pub struct Generator {
    ops: Vec<Op>,
    label_count: usize,
    externals: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        let ops = Vec::new();

        Self {
            ops,
            externals: vec![],
            label_count: 0,
        }
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

