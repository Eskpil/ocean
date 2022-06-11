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
    Function(String, Vec<NamedParameter>, Vec<Statement>),
    Expression(Expression),
    While(Expression, Vec<Statement>),
    Declaration(String, Expression),
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
            Statement::Function(name, parameters, children) => {
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
            Statement::While(expr, children) => {
                util::print_indent(indent, "WhileStatement:".into());
                util::print_indent(indent + 1, "Expression:".into());
                expr.print(indent + 2);
                util::print_indent(indent + 1, "Body:".into());
                for child in children.iter() {
                    child.print(indent + 2);
                }
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
            Statement::Define(_) => {},
            Statement::Block(children) => {
                let label = generator.allocate_label();

                let op = Op::single(OpKind::Block, Operand::Symbol(label.clone()));

                generator.append(op);

                for child in children.iter() {
                    child.generate(generator);
                }
            } 
            Statement::If(cond, if_block, else_block) => {
                let end_label = generator.allocate_label(); 
                let if_label = generator.allocate_label();
                cond.generate(generator);

                if let Some(block) = else_block {
                    let else_label = generator.allocate_label();
                    generator.append(Op::single(
                            OpKind::JumpUnless, 
                            Operand::Symbol(else_label.clone())
                    ));

                    generator.append(Op::single(OpKind::Block, Operand::Symbol(if_label.clone())));
                    for child in if_block.iter() {
                        child.generate(generator);
                    }

                    generator.append(Op::single(OpKind::Block, Operand::Symbol(else_label.clone())));
                    for child in block.iter() {
                        child.generate(generator);
                    }
                } else {
                    generator.append(Op::single(OpKind::JumpUnless, Operand::Symbol(end_label.clone())));
                    generator.append(Op::single(OpKind::Block, Operand::Symbol(if_label.clone())));
                    for child in if_block.iter() {
                        child.generate(generator);
                    }
                }

                generator.append(Op::single(OpKind::Block, Operand::Symbol(end_label.clone())));
            }
            Statement::Function(name, parameters, children) => {
                if children.len() > 0 {
                    let op = Op::double(
                        OpKind::Proc, 
                        Operand::Symbol(name.clone().to_lowercase()), 
                        Operand::Uint(parameters.len() as u64)
                    );

                    generator.append(op);

                    for param in parameters.iter() {
                        let op = Op::single(
                            OpKind::NewVariable, 
                            Operand::Symbol(param.name.clone())
                        );       

                        generator.append(op);
                    }

            
                    for child in children.iter() {
                        child.generate(generator);
                    }
                    
                    generator.append(Op::none(OpKind::End));
                }
            }
            Statement::Expression(expr) => {
                expr.generate(generator);
            }
            Statement::While(expr, body) => {
                let end_label = generator.allocate_label();
                let expression_label = generator.allocate_label();                  

                generator.append(Op::single(OpKind::Block, Operand::Symbol(expression_label.clone())));
                expr.generate(generator);
                generator.append(Op::single(OpKind::JumpUnless, Operand::Symbol(end_label.clone())));

                let body_label = generator.allocate_label();
                generator.append(Op::single(OpKind::Block, Operand::Symbol(body_label)));

                for child in body.iter() {
                    child.generate(generator);
                }
                generator.append(Op::single(OpKind::Jump, Operand::Symbol(expression_label.clone())));

                generator.append(Op::single(OpKind::Block, Operand::Symbol(end_label.clone())));
            }
            Statement::Declaration(name, expr) => {
                expr.generate(generator);
                let op = Op::single(OpKind::NewVariable, Operand::Symbol(name.clone()));
                generator.append(op);
            }
        }        
    }     
}
