use crate::ast::{BinaryOp};

use super::register::Register;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    Ptr,
    Num,
    Reference,
    Object,
}

#[derive(Clone, Debug)]
pub enum OpKind {
    Load,

    Block,
    Proc,
    End,
    Call,
    Return,

    NewString,
    ResolveString,

    NewStruct,
    SetField,
    Deref,
    ResolveField,

    ArrayInit,
    ArrayAppend,
    ArrayIndex,

    ResolveVariable,
    NewVariable,
    NewParameter,

    Push,

    Intrinsic,

    Jump,
    JumpUnless,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Uint(u64),
    Bool(bool),

    Symbol(String),
    Data(String),
    Op(BinaryOp),

    Type(Type),

    Reg(Register),
    Regs(Vec<Register>),
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
            o => unreachable!("Expected OpKind::Symbol but found OpKind::{:?}", o),
        } 
    }
    
    pub fn as_uint(&self) -> u64 {
        match self {
            Operand::Uint(v) => v.clone(),
            o => unreachable!("Expected OpKind::Uint but found OpKind::{:?}", o),
        }
    }

    pub fn as_op(&self) -> BinaryOp {
        match self {
            Operand::Op(op) => op.clone(),
            o => unreachable!("Expected OpKind::Op but found OpKind::{:?}", o),
        }
    }

    pub fn as_data(&self) -> String {
        match self {
            Operand::Data(d) => d.clone(),
            o => unreachable!("Expected OpKind::Op but found OpKind::{:?}", o),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Operand::Bool(d) => d.clone(),
            o => unreachable!("Expected OpKind::Bool but found OpKind::{:?}", o),
        }
    }

    pub fn as_type(&self) -> Type {
        match self {
            Operand::Type(t) => t.clone(),
            o => unreachable!("Expected OpKind::Type but found OpKind::{:?}", o),
        }
    }

    pub fn as_reg(&self) -> Register {
        match self {
            Operand::Reg(reg) => reg.clone(),
            o => unreachable!("Expected OpKind::Reg but found OpKind::{:?}", o),
        }
    }

    pub fn as_regs(&self) -> Vec<Register> {
        match self {
            Operand::Regs(reg) => reg.clone(),
            o => unreachable!("Expected OpKind::Regs but found OpKind::{:?}", o),
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

    pub fn tripple(
        kind: OpKind, 
        one: Operand, 
        two: Operand,
        three: Operand,
    ) -> Self {
        let mut operands = Vec::new();
        operands.push(one);
        operands.push(two);
        operands.push(three);

        Op::new(kind, operands)
    }

    pub fn quadrouple(
        kind: OpKind, 
        one: Operand, 
        two: Operand,
        three: Operand,
        four: Operand,
    ) -> Self {
        let mut operands = Vec::new();
        operands.push(one);
        operands.push(two);
        operands.push(three);
        operands.push(four);

        Op::new(kind, operands)
    }
}
