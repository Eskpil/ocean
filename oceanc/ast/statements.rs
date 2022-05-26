use super::expressions::{Expression};
use super::util;
use crate::ir::{op::{Op, OpKind, Operand}, generator::{Generator}};

#[derive(Debug, Clone)]
pub enum Statement {
    Program(Vec<Statement>),
    Block(Vec<Statement>),
    Function(String, Vec<Statement>),
    Expression(Expression),
    Declaration(String, Expression),
}

impl Statement {
    pub fn print(&self, indent: usize) {
        match self.clone() {
            Statement::Program(children) => {
                util::print_indent(indent, "Program:".into());
                for child in children.iter() {
                    child.print(indent + 1);
                }
            }
            Statement::Block(children) => {
                util::print_indent(indent, "BlockStatement:".into());
                for child in children.iter() {
                    child.print(indent + 1);
                }
            }
            Statement::Function(name, children) => {
                util::print_indent(indent, "FunctionStatement:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Children:".into());
                for child in children.iter() {
                    child.print(indent + 2);
                }
            }
            Statement::Expression(expr) => {
                util::print_indent(indent, "ExpressionStatement:".into());
                expr.print(indent + 1);
            }
            Statement::Declaration(name, expr) => {
                util::print_indent(indent, "DeclarationStatement:".into());

                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name.clone());
                expr.print(indent + 1);
            }
        } 
    }

    pub fn generate(&self, generator: &mut Generator) {
        match self.clone() {
            Statement::Program(children) => {
                for child in children.iter() {
                    child.generate(generator);
                }
            }
            Statement::Block(children) => {
                let label = generator.allocate_label();

                let op = Op::single(OpKind::Block, Operand::Symbol(label.clone()));

                generator.append(op);

                for child in children.iter() {
                    child.generate(generator);
                }
            } 
            Statement::Function(name, children) => {
                let op = Op::single(OpKind::Proc, Operand::Symbol(name.clone().to_lowercase()));

                generator.append(op);
        
                for child in children.iter() {
                    child.generate(generator);
                }
                
                generator.append(Op::none(OpKind::End));
            }
            Statement::Expression(expr) => {
                expr.generate(generator);
            }
            Statement::Declaration(name, expr) => {
                expr.generate(generator);
                let op = Op::single(OpKind::NewVariable, Operand::Symbol(name.clone()));
                generator.append(op);
            }
        }        
    }     
}
