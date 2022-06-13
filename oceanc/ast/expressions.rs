use super::{util, BinaryOp};
use std::boxed::Box;
use crate::ir::{
    generator::Generator,
    op::{Op, OpKind, Operand},
};
use std::process;

#[derive(Debug, Clone)]
pub struct NamedArgument {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Empty,
    Literal(u64),
    StructInit(String, Vec<NamedArgument>),
    Bool(bool),
    StringLiteral(String),
    Identifier(String),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(BinaryOp, Box<Expression>),
    Call(String, Vec<NamedArgument>),
    Lookup(String, String),
}

impl NamedArgument {
    pub fn new(name: String, expr: Expression) -> Self {
        Self {
            name,
            value: expr,
        }
    }

    pub fn print(&self, indent: usize) {
        util::print_indent(indent, format!("Name: {}", self.name)); 
        self.value.print(indent);
    }
}

impl Expression {
    pub fn as_identifier(&self) -> String {
        match self.clone() {
            Expression::Identifier(i) => i.clone(),
            expr => { 
                eprintln!("Expression: {:?} cannot be converted into an Identifier", expr); 
                process::exit(1);
            }
        }
    }

    pub fn print(&self, indent: usize) {
        match self.clone() {
            Expression::Empty => {
                util::print_indent(indent, "EmptyExpression".into());
            }
            Expression::Bool(v) => {
                util::print_indent(indent, "Bool:".into());
                util::print_indent(indent + 1, format!("{}", v));
            }
            Expression::Literal(v) => {
                util::print_indent(indent, "Literal:".into());
                util::print_indent(indent + 1, format!("{}", v));
            }
            Expression::StructInit(name, arguments) => {
                util::print_indent(indent, "StructInit:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Arguments".into());
                for arg in arguments {
                    arg.print(indent + 2);
                }
            }
            Expression::StringLiteral(v) => {
                util::print_indent(indent, "StringLiteral:".into());
                util::print_indent(indent + 1, format!("\"{v}\""));
            }
            Expression::Identifier(v) => {
                util::print_indent(indent, "Identifier:".into());
                util::print_indent(indent + 1, v.clone());
            }
            Expression::Binary(op, lhs, rhs) => {
                util::print_indent(indent, "BinaryExpression:".into());
                util::print_indent(indent + 1, "op:".into());
                util::print_indent(indent + 2, op.to_string());
                util::print_indent(indent + 1, "lhs:".into());
                lhs.print(indent + 2);

                util::print_indent(indent + 1, "rhs:".into());
                rhs.print(indent + 2);
            }
            Expression::Unary(op, expr) => {
                util::print_indent(indent, "UnaryExpression:".into());
                util::print_indent(indent + 1, "op:".into());
                util::print_indent(indent + 2, op.to_string());
                util::print_indent(indent + 1, "expr".into());
                expr.print(indent + 2);
            }
            Expression::Call(name, arguments) => {
                util::print_indent(indent, "CallExpression:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Arguments".into());
                for arg in arguments {
                    arg.print(indent + 2);
                }
            }
            Expression::Lookup(on, field) => {
                util::print_indent(indent, "LookupExpression:".into());
                util::print_indent(indent + 1, "Struct:".into());
                util::print_indent(indent + 2, on);
                util::print_indent(indent + 1, "Field:".into());
                util::print_indent(indent + 2, field);
            }
        }    
    }
}
