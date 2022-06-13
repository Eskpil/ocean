use super::expressions::{Expression};
use super::definitions::{Definition, DefinedType};
use super::util;
use crate::ir::{
    op::{Op, OpKind, Operand}, 
    generator::{Generator}
};

#[derive(Debug, Clone)]
pub struct NamedParameter {
    pub name: String,
    pub defined_type: DefinedType,
} 

#[derive(Debug, Clone)]
pub enum Statement {
    Program(Vec<Statement>),
    Define(Definition),
    Block(Vec<Statement>),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    Function(String, Vec<NamedParameter>, Vec<Statement>, DefinedType, bool),
    Expression(Expression),
    While(Expression, Vec<Statement>),
    Declaration(String, Expression, bool),
    Return(Expression),
}

impl NamedParameter {
    pub fn new(name: String, defined_type: DefinedType) -> Self {
        Self {
            name,
            defined_type,
        }
    }
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
            Statement::Define(definition) => {
                util::print_indent(indent, "Definition:".into());
                definition.print(indent + 1);
            }
            Statement::Block(children) => {
                util::print_indent(indent, "BlockStatement:".into());
                for child in children.iter() {
                    child.print(indent + 1);
                }
            }
            Statement::If(cond, if_block, else_block) => {
                util::print_indent(indent, "IfStatement:".into());
                util::print_indent(indent + 1, "Condition:".into());
                cond.print(indent + 2);
                util::print_indent(indent + 1, "IfBlock:".into());
                for child in if_block.iter() {
                    child.print(indent + 2);
                }

                if let Some(block) = else_block {
                    util::print_indent(indent + 1, "ElseBlock:".into());    
                    for child in block.iter() {
                        child.print(indent + 2);
                    }
                }
            }
            Statement::Function(name, parameters, children, defined, external) => {
                util::print_indent(indent, "FunctionStatement:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Children:".into());
                for child in children.iter() {
                    child.print(indent + 2);
                }
                defined.print(indent + 1);
            }
            Statement::Expression(expr) => {
                util::print_indent(indent, "ExpressionStatement:".into());
                expr.print(indent + 1);
            }
            Statement::While(expr, children) => {
                util::print_indent(indent, "WhileStatement:".into());
                util::print_indent(indent + 1, "Expression:".into());
                expr.print(indent + 2);
                util::print_indent(indent + 1, "Body:".into());
                for child in children.iter() {
                    child.print(indent + 2);
                }
            }
            Statement::Declaration(name, expr, referenced) => {
                util::print_indent(indent, "DeclarationStatement:".into());

                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name.clone());
                util::print_indent(indent + 1, "Referenced:".into());
                util::print_indent(indent + 2, format!("{referenced}"));
                expr.print(indent + 1);
            }
            _ => {},
        } 
    }
}
