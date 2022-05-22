use super::util;
use crate::ir::{
    generator::Generator,
    op::{Op, OpKind, Operand},
};

pub trait Expression {
    fn print(&self, indent: usize);
    fn generate(&self, generator: &mut Generator);
}

pub struct DoubleLiteral(f64);

impl DoubleLiteral {
    pub fn new(value: f64) -> Self {
        Self(value)
    }
}

impl Expression for DoubleLiteral {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "DoubleLiteral:".into());
        util::print_indent(indent + 1, format!("{}", self.0));
    }

    fn generate(&self, generator: &mut Generator) {
        let op = Op::single(OpKind::Push, Operand::Float(self.0));
        generator.append(op);
    }
}

pub struct EmptyExpression();

impl EmptyExpression {
    pub fn new() -> Self {
        Self()
    }
}

impl Expression for EmptyExpression {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "EmptyExpression".into());
    }

    fn generate(&self, generator: &mut Generator) {
        
    }
}

pub struct Identifier(String);

impl Identifier {
    pub fn new(name: String) -> Self {
        Self(name)
    }

    pub fn inner(&self) -> String {
        return self.0.clone();
    }
}

impl Expression for Identifier {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "Identifier:".into());
        util::print_indent(indent + 1, self.0.clone());
    }

    fn generate(&self, generator: &mut Generator) {
        let op = Op::single(OpKind::ResolveVariable, Operand::Symbol(self.0.clone()));
        generator.append(op);
    }
}

pub struct BinaryExpression {
    lhs: Box<dyn Expression>,
    rhs: Box<dyn Expression>,
}

impl BinaryExpression {
    pub fn new(lhs: Box<dyn Expression>, rhs: Box<dyn Expression>) -> Self {
        Self { lhs, rhs }
    }
}

impl Expression for BinaryExpression {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "BinaryExpression:".into());
        util::print_indent(indent + 1, "lhs:".into());
        self.lhs.print(indent + 2);

        util::print_indent(indent + 1, "rhs:".into());
        self.rhs.print(indent + 2);
    }

    fn generate(&self, generator: &mut Generator) {
        self.lhs.generate(generator); 
        self.lhs.generate(generator);
        let op = Op::none(OpKind::Add); 
        generator.append(op);
    }
}
