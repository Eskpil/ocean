use crate::lexer::Span;
use super::expressions::{Expression};
use super::definitions::{Definition, DefinedType};
use super::util;
use crate::ir::{
    op::{Op, OpKind, Operand}, 
    generator::{Generator}
};

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub cond: Expression,

    pub if_block: Vec<Statement>,
    pub if_span: Span,

    pub else_block: Option<Vec<Statement>>,
    pub else_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct NamedParameter {
    pub name: String,
    pub span: Span,
    pub defined_type: DefinedType,
} 

#[derive(Debug, Clone)]
pub enum Statement {
    Program(Span, Vec<Statement>),
    Define(Span, Definition),
    Block(Span, Vec<Statement>),
    If(Span, IfStatement),
    Function(Span, String, Vec<NamedParameter>, Vec<Statement>, DefinedType, bool),
    Expression(Span, Expression),
    While(Span, Expression, Vec<Statement>, Span),
    Declaration(Span, String, Expression, bool),
    Return(Span, Expression),
}

impl IfStatement {
    pub fn new(
        cond: Expression,
        if_block: Vec<Statement>,
        if_span: Span,
        else_block: Option<Vec<Statement>>,
        else_span: Option<Span>,
    ) -> Self {
        Self {
            cond,
            if_block,
            if_span,
            else_block,
            else_span,
        } 
    }
}

impl NamedParameter {
    pub fn new(
        name: String, 
        defined_type: DefinedType,
        span: Span,
    ) -> Self {
        Self {
            name,
            span,
            defined_type,
        }
    }
}

impl Statement {
    pub fn print(&self, indent: usize) {
        match self.clone() {
            Statement::Program(_, children) => {
                util::print_indent(indent, "Program:".into());
                for child in children.iter() {
                    child.print(indent + 1);
                }
            }
            Statement::Define(_, definition) => {
                util::print_indent(indent, "Definition:".into());
                definition.print(indent + 1);
            }
            Statement::Block(_, children) => {
                util::print_indent(indent, "BlockStatement:".into());
                for child in children.iter() {
                    child.print(indent + 1);
                }
            }
            Statement::If(_, stmt) => {
                util::print_indent(indent, "IfStatement:".into());
                util::print_indent(indent + 1, "Condition:".into());
                stmt.cond.print(indent + 2);
                util::print_indent(indent + 1, "IfBlock:".into());
                for child in stmt.if_block.iter() {
                    child.print(indent + 2);
                }

                if let Some(block) = stmt.else_block {
                    util::print_indent(indent + 1, "ElseBlock:".into());    
                    for child in block.iter() {
                        child.print(indent + 2);
                    }
                }
            }
            Statement::Function(_, name, parameters, children, defined, external) => {
                util::print_indent(indent, "FunctionStatement:".into());
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, name);
                util::print_indent(indent + 1, "Children:".into());
                for child in children.iter() {
                    child.print(indent + 2);
                }
                defined.print(indent + 1);
            }
            Statement::Expression(_, expr) => {
                util::print_indent(indent, "ExpressionStatement:".into());
                expr.print(indent + 1);
            }
            Statement::While(_, expr, children, _) => {
                util::print_indent(indent, "WhileStatement:".into());
                util::print_indent(indent + 1, "Expression:".into());
                expr.print(indent + 2);
                util::print_indent(indent + 1, "Body:".into());
                for child in children.iter() {
                    child.print(indent + 2);
                }
            }
            Statement::Declaration(_, name, expr, referenced) => {
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
