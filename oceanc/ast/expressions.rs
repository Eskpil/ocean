use crate::lexer::Span;
use super::{util, BinaryOp};
use std::boxed::Box;
use std::process;

#[derive(Debug, Clone)]
pub struct NamedArgument {
    pub name: String,
    pub span: Span,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Empty(Span),
    Literal(Span, u64),
    StructInit(Span, String, Vec<NamedArgument>),
    ArrayInit(Span, Vec<Expression>),
    ArrayIndex(Span, String, u64),
    Bool(Span, bool),
    StringLiteral(Span, String),
    Identifier(Span, String),
    Binary(Span, BinaryOp, Box<Expression>, Box<Expression>),
    Unary(Span, BinaryOp, Box<Expression>),
    Call(Span, String, Vec<NamedArgument>),
    Lookup(Span, String, String),
}

impl NamedArgument {
    pub fn new(
        name: String, 
        expr: Expression,
        span: Span,
    ) -> Self {
        Self {
            name,
            span,
            value: expr,
        }
    }

    pub fn print(&self, indent: usize) {
        util::print_indent(indent, format!("Name: {}", self.name)); 
        self.value.print(indent);
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self.clone() {
            Self::Empty(s) => s,
            Self::Literal(s, _) => s,
            Self::StructInit(s, _, _) => s,
            Self::Bool(s, _) => s,
            Self::StringLiteral(s, _) => s,
            Self::Identifier(s, _) => s,
            Self::Binary(s, _, _, _) => s,
            Self::Unary(s, _, _) => s,
            Self::Call(s, _, _) => s,
            Self::Lookup(s, _, _) => s,
            Self::ArrayInit(s, _) => s,
            Self::ArrayIndex(s, _, _) => s,
        }
    }

    pub fn as_identifier(&self) -> String {
        match self.clone() {
            Expression::Identifier(_, i) => i.clone(),
            expr => { 
                eprintln!("Expression: {:?} cannot be converted into an Identifier", expr); 
                process::exit(1);
            }
        }
    }

    pub fn print(&self, indent: usize) {
        match self.clone() {
            Expression::Empty(_) => {
                util::print_indent(indent, "EmptyExpression".into());
            }
            Expression::Bool(_, v) => {
                util::print_indent(indent, "Bool:".into());
                util::print_indent(indent + 1, format!("{}", v));
            }
            Expression::Literal(_, v) => {
                util::print_indent(indent, "Literal:".into());
                util::print_indent(indent + 1, format!("{}", v));
            }
            Expression::StructInit(_, name, arguments) => {
                util::print_indent(indent, "StructInit:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Arguments".into());
                for arg in arguments {
                    arg.print(indent + 2);
                }
            }
            Expression::StringLiteral(_, v) => {
                util::print_indent(indent, "StringLiteral:".into());
                util::print_indent(indent + 1, format!("\"{v}\""));
            }
            Expression::Identifier(_, v) => {
                util::print_indent(indent, "Identifier:".into());
                util::print_indent(indent + 1, v.clone());
            }
            Expression::Binary(_, op, lhs, rhs) => {
                util::print_indent(indent, "BinaryExpression:".into());
                util::print_indent(indent + 1, "op:".into());
                util::print_indent(indent + 2, op.to_string());
                util::print_indent(indent + 1, "lhs:".into());
                lhs.print(indent + 2);

                util::print_indent(indent + 1, "rhs:".into());
                rhs.print(indent + 2);
            }
            Expression::Unary(_, op, expr) => {
                util::print_indent(indent, "UnaryExpression:".into());
                util::print_indent(indent + 1, "op:".into());
                util::print_indent(indent + 2, op.to_string());
                util::print_indent(indent + 1, "expr".into());
                expr.print(indent + 2);
            }
            Expression::Call(_, name, arguments) => {
                util::print_indent(indent, "CallExpression:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Arguments".into());
                for arg in arguments {
                    arg.print(indent + 2);
                }
            }
            Expression::Lookup(_, on, field) => {
                util::print_indent(indent, "LookupExpression:".into());
                util::print_indent(indent + 1, "Struct:".into());
                util::print_indent(indent + 2, on);
                util::print_indent(indent + 1, "Field:".into());
                util::print_indent(indent + 2, field);
            }
            _ => {}
        }    
    }
}
