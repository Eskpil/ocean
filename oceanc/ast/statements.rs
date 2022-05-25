use super::expressions::{Expression, Identifier};
use super::util;
use crate::ir::{op::{Op, OpKind, Operand}, generator::{Generator}};
use std::boxed::Box;

pub trait Statement {
    fn print(&self, ident: usize);
    fn generate(&self, generator: &mut Generator);
}

pub struct Program {
    children: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Self { children: vec![] }
    }

    pub fn append(&mut self, stmt: Box<dyn Statement>) {
        self.children.push(stmt);
    }
}

impl Statement for Program {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "Program:".into());
        for (_, child) in self.children.iter().enumerate() {
            child.print(indent + 1);
        }
    }

    fn generate(&self, generator: &mut Generator) {
        for (_, child) in self.children.iter().enumerate() {
            child.generate(generator);
        }
    }
}

pub struct BlockStatement {
    children: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new() -> Self {
        Self { children: vec![] }
    }

    pub fn append(&mut self, stmt: Box<dyn Statement>) {
        self.children.push(stmt);
    }
}

impl Statement for BlockStatement {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "BlockStatement:".into());
        for (_, child) in self.children.iter().enumerate() {
            child.print(indent + 1);
        }
    }

    fn generate(&self, generator: &mut Generator) {
        generator.label_auto();
        
        for (_, child) in self.children.iter().enumerate() {
            child.generate(generator);
        }
    }
}

pub struct FunctionStatement {
    children: Vec<Box<dyn Statement>>,
    name: Identifier,
}

impl FunctionStatement {
    pub fn new(name: Identifier) -> Self {
        Self {
            name,
            children: vec![],
        }
    }

    pub fn append(&mut self, stmt: Box<dyn Statement>) {
        self.children.push(stmt);
    }
}

impl Statement for FunctionStatement {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "FunctionStatement:".into());
        self.name.print(indent + 1);
        util::print_indent(indent + 1, "Children:".into());
        for (_, child) in self.children.iter().enumerate() {
            child.print(indent + 2);
        }
    }

    fn generate(&self, generator: &mut Generator) {
        generator.label(self.name.inner().to_lowercase());
        
        for (_, child) in self.children.iter().enumerate() {
            child.generate(generator);
        }
        
        generator.append(Op::none(OpKind::LabelEnd));
    }

}

pub struct ExpressionStatement {
    expr: Box<dyn Expression>,
}

impl ExpressionStatement {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        Self { expr }
    }
}

impl Statement for ExpressionStatement {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "ExpressionStatement:".into());
        self.expr.print(indent + 1);
    }

    fn generate(&self, generator: &mut Generator) {
        self.expr.generate(generator);
    }
}

pub struct DeclarationStatement {
    identifier: Identifier,
    expr: Box<dyn Expression>,
}

impl DeclarationStatement {
    pub fn new(identifier: Identifier, expr: Box<dyn Expression>) -> Self {
        Self { identifier, expr }
    }
}

impl Statement for DeclarationStatement {
    fn print(&self, indent: usize) {
        util::print_indent(indent, "DeclarationStatement:".into());
        self.identifier.print(indent + 1);
        self.expr.print(indent + 1);
    }

    fn generate(&self, generator: &mut Generator) {
        self.expr.generate(generator);
        let op = Op::single(OpKind::NewVariable, Operand::Symbol(self.identifier.inner()));
        generator.append(op);
    }
}
