pub mod project;
pub mod scope;

use crate::errors::{TypeError};
use crate::ast::BinaryOp;

pub type TypeId = usize;
pub type ScopeId = usize;

pub const INT_TYPE_ID: TypeId = 0;
pub const BOOL_TYPE_ID: TypeId = 1;
pub const STRING_TYPE_ID: TypeId = 2;

pub type DefinitionResult = Result<CheckedDefinition, TypeError>;
pub type StatementResult = Result<CheckedStatement, TypeError>;
pub type ExpressionResult = Result<CheckedExpression, TypeError>;


#[derive(Debug, Clone)]
pub struct CheckedNamedArgument {
    pub name: String,
    pub type_id: TypeId,
    pub expr: CheckedExpression,
}

#[derive(Debug, Clone)]
pub struct CheckedNamedParameter {
    pub name: String,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct CheckedField {
    pub name: String,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct CheckedFunctionCall {
    pub name: String,
    pub arguments: Vec<CheckedNamedArgument>,
}

#[derive(Debug, Clone)]
pub struct CheckedStruct {
    pub name: String,
    pub fields: Vec<CheckedField>,
    pub type_id: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct CheckedBinaryExpression {
    pub lhs: Box<CheckedExpression>, 
    pub rhs: Box<CheckedExpression>,
    pub op: BinaryOp,
}

#[derive(Debug, Clone)]
pub struct CheckedBlock {
    pub children: Vec<CheckedStatement>,
}

#[derive(Debug, Clone)]
pub struct CheckedFunction {
    pub name: String, 
    pub block: Option<CheckedBlock>,
    pub parameters: Vec<CheckedNamedParameter>,
}

#[derive(Debug, Clone)]
pub struct CheckedVariable {
    pub name: String,    
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct CheckedVariableDecl {
    pub name: String,    
    pub type_id: TypeId,
    pub expr: CheckedExpression,
}

#[derive(Debug, Clone)]
pub enum CheckedExpression {
    Literal(u64),
    Empty,
    StringLiteral(String),
    Identifier(String, TypeId),
    Binary(CheckedBinaryExpression),    
    Call(CheckedFunctionCall),
}

#[derive(Debug, Clone)]
pub enum CheckedDefinition {
    Struct(CheckedStruct),
}

#[derive(Debug, Clone)]
pub enum CheckedStatement {
    Define,
    Function(CheckedFunction), 
    Expression(CheckedExpression),
    VariableDecl(CheckedVariableDecl),
    Block(CheckedBlock),
}

impl CheckedFunctionCall {
    pub fn new(name: String, arguments: Vec<CheckedNamedArgument>) -> Self {
        Self {
            name,
            arguments,
        }
    }
}

impl CheckedNamedArgument {
    pub fn new(
        name: String, 
        type_id: TypeId, 
        expr: CheckedExpression
    ) -> Self {
        Self {
            name,
            type_id,
            expr,
        }
    }
}

impl CheckedNamedParameter {
    pub fn new(name: String, type_id: TypeId) -> Self {
        Self {
            name,
            type_id,
        }
    } 
}

impl CheckedBinaryExpression {
    pub fn new(
        op: BinaryOp, 
        lhs: Box<CheckedExpression>, 
        rhs: Box<CheckedExpression>
    ) -> Self {
        Self {
            op,
            lhs,
            rhs
        } 
    }
}

impl CheckedVariable {
    pub fn new(
        name: String, 
        type_id: TypeId, 
    ) -> Self {
        Self {
            name,
            type_id,
        }
    }    
}

impl CheckedVariableDecl {
    pub fn new(
        name: String, 
        type_id: TypeId, 
        expr: CheckedExpression,
    ) -> Self {
        Self {
            name,
            type_id,
            expr,
        }
    }    
}

impl CheckedFunction {
    pub fn new(
        name: String, 
        parameters: Vec<CheckedNamedParameter>,
        block: Option<CheckedBlock>
    ) -> Self {
        Self {
            name,
            parameters,
            block,
        }     
    }
}

impl CheckedBlock {
    pub fn new() -> Self {
        Self {
            children: vec![],
        }
    }
}

impl CheckedField {
    pub fn new(name: String, id: TypeId) -> Self {
        Self {
            name,
            type_id: id,
        } 
    }
}

impl CheckedStruct {
    pub fn new(name: String, fields: Vec<CheckedField>) -> Self {
        Self {
            name,
            fields,
            type_id: None,
        } 
    }
}
