use super::{util, BinaryOp};
use std::boxed::Box;
use crate::ir::{
    generator::Generator,
    op::{Op, OpKind, Operand},
};

#[derive(Debug, Clone)]
pub enum Expression {
    Empty,
    Literal(u64),
    Bool(bool),
    Identifier(String),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(BinaryOp, Box<Expression>),
    Call(String),
}

impl Expression {
    pub fn print(&self, indent: usize) {
        match self.clone() {
            Expression::Empty => {
                util::print_indent(indent, "EmptyExpression".into());
            }
            Expression::Bool(v) => {
                util::print_indent(indent, "Bool".into());
                util::print_indent(indent + 1, format!("{}", v));
            }
            Expression::Literal(v) => {
                util::print_indent(indent, "Literal:".into());
                util::print_indent(indent + 1, format!("{}", v));
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
            Expression::Call(name) => {
                util::print_indent(indent, "CallExpression:".into());
                util::print_indent(indent + 1, name);
            }
        }    
    }

    pub fn generate(&self, generator: &mut Generator) {
        match self.clone() {
            Expression::Empty => {
                                   
            }
            Expression::Bool(v) => {
                let mut op = Op::single(OpKind::Push, Operand::Uint(0));
                if v {
                    op = Op::single(OpKind::Push, Operand::Uint(1));
                }
                generator.append(op);
            }
            Expression::Literal(v) => {
               let op = Op::single(OpKind::Push, Operand::Uint(v));
               generator.append(op);
            }
            Expression::Identifier(v) => {
                let op = Op::single(OpKind::ResolveVariable, Operand::Symbol(v.clone()));
                generator.append(op);
            }
            Expression::Binary(op, lhs, rhs) => {
                lhs.generate(generator); 
                rhs.generate(generator);
                let op = Op::none(OpKind::Add); 
                generator.append(op);
            }
            Expression::Unary(op, expr) => {
                todo!("Generate unary expression");
            }
            Expression::Call(name) => {
                let op = Op::single(OpKind::Call, Operand::Symbol(name.clone()));
                generator.append(op);
            }
        }
    }
}

