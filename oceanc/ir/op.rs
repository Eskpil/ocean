#[derive(Clone, Debug)]
pub enum OpKind {
    // Label is a special operation handeled differently that all the others.
    Label,
    Call,

    ResolveVariable,
    NewVariable,

    Push,
    Add,
    Mul,
    Sub,
    Div,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Float(f64),
    Symbol(String),
}

#[derive(Clone, Debug)]
pub struct Op {
    kind: OpKind,
    operands: Vec<Operand>,
}

impl Op {
    pub fn new(kind: OpKind, operands: Vec<Operand>) -> Self {
        Self { kind, operands }
    }

    pub fn none(kind: OpKind) -> Self {
        let operands = Vec::new();

        Op::new(kind, operands)
    }

    pub fn single(kind: OpKind, operand: Operand) -> Self {
        let mut operands = Vec::new();
        operands.push(operand);

        Op::new(kind, operands)
    }

    pub fn double(kind: OpKind, one: Operand, two: Operand) -> Self {
        let mut operands = Vec::new();
        operands.push(one);
        operands.push(two);

        Op::new(kind, operands)
    }
}
