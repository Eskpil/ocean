#[derive(Clone, Debug)]
pub enum OpKind {
    Block,
    Proc,
    End,
    Call,

    ResolveVariable,
    NewVariable,

    Push,
    Add,
    Mul,
    Sub,
    Div,

    Jump,
    JumpUnless,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Uint(u64),
    Symbol(String),
}

#[derive(Clone, Debug)]
pub struct Op {
    kind: OpKind,
    operands: Vec<Operand>,
}

impl Operand {
    pub fn as_symbol(&self) -> String {
        match self {
            Operand::Symbol(s) => s.clone(),
            _ => unreachable!("As symbol not symbol")
        } 
    }
    
    pub fn as_uint(&self) -> u64 {
        match self {
            Operand::Uint(v) => v.clone(),
            o => unreachable!("Expected TokenKind::Float found TokenKind: {:?}", o)
        }
    }
}

impl Op {
    pub fn new(kind: OpKind, operands: Vec<Operand>) -> Self {
        Self { kind, operands }
    }

    pub fn kind(&self) -> OpKind {
        self.kind.clone()
    }

    pub fn operands(&self) -> Vec<Operand> {
        self.operands.clone()
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
