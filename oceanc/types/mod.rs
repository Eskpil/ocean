pub mod project;
pub mod scope;

use crate::errors::{OceanError};
use crate::ast::BinaryOp;
use crate::lexer::Span;

pub type TypeId = usize;
pub type ScopeId = usize;

pub const VOID_TYPE_ID: TypeId = 0;
pub const INT_TYPE_ID: TypeId = 1;
pub const BOOL_TYPE_ID: TypeId = 2;
pub const STRING_TYPE_ID: TypeId = 3;
pub const PTR_TYPE_ID: TypeId = 4;

pub type StatementResult = Result<CheckedStatement, OceanError>;
pub type ExpressionResult = Result<CheckedExpression, OceanError>;

#[derive(Debug, Clone)]
pub struct CheckedLookup {
    pub lhs: CheckedVariable,
    pub rhs: String,

    pub type_id: TypeId,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct CheckedNamedArgument {
    pub name: String,
    pub type_id: TypeId,
    pub offset: usize,
    pub expr: CheckedExpression,

    pub span: Span,
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
    pub returning: TypeId,
}

#[derive(Debug, Clone)]
pub struct CheckedStructInit {
    pub name: String, 
    pub size: usize,
    pub arguments: Vec<CheckedNamedArgument>,
    pub returning: TypeId,
}

#[derive(Debug, Clone)]
pub struct CheckedStruct {
    pub name: String,
    pub size: usize,
    pub fields: Vec<CheckedField>,
    pub type_id: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct CheckedIfStatement {
    pub cond: CheckedExpression, 
    pub if_block: CheckedBlock,
    pub else_block: Option<CheckedBlock>,
}

#[derive(Debug, Clone)]
pub struct CheckedReturn {
    pub expr: CheckedExpression,
}

#[derive(Debug, Clone)]
pub struct CheckedWhileStatement {
    pub cond: CheckedExpression, 
    pub body: CheckedBlock,
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
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone)]
pub struct CheckedFunction {
    pub name: String, 
    pub external: bool,
    pub block: Option<CheckedBlock>,
    pub parameters: Vec<CheckedNamedParameter>,
    pub returning: TypeId,
}

#[derive(Debug, Clone)]
pub struct CheckedVariable {
    pub name: String,    
    pub type_id: TypeId,
    pub scope_id: ScopeId,

    pub is_referenced: bool,
}

#[derive(Debug, Clone)]
pub struct CheckedVariableDecl {
    pub name: String,    
    pub type_id: TypeId,
    pub scope_id: ScopeId,
    pub expr: CheckedExpression,
}

#[derive(Debug, Clone)]
pub enum CheckedExpression {
    Literal(u64),
    Empty,
    StringLiteral(String),
    Identifier(String, ScopeId),
    Binary(CheckedBinaryExpression),    
    Call(CheckedFunctionCall),
    StructInit(CheckedStructInit),
    Bool(bool),
    Lookup(CheckedLookup),
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
    If(CheckedIfStatement),
    Block(CheckedBlock),
    While(CheckedWhileStatement),
    Return(CheckedReturn),
}

impl CheckedIfStatement {
    pub fn new(
        cond: CheckedExpression,
        if_block: CheckedBlock,
        else_block: Option<CheckedBlock>,
    ) -> Self {
        Self {
            cond,
            if_block,
            else_block,
        }
    }
}

impl CheckedWhileStatement {
    pub fn new(
        cond: CheckedExpression,
        body: CheckedBlock,
    ) -> Self {
        Self {
            cond,
            body
        }
    }
}

impl CheckedFunctionCall {
    pub fn new(
        name: String, 
        arguments: Vec<CheckedNamedArgument>,
        returning: TypeId,
    ) -> Self {
        Self {
            name,
            arguments,
            returning,
        }
    }
}

impl CheckedStructInit {
    pub fn new(
        name: String,
        arguments: Vec<CheckedNamedArgument>,
        type_id: TypeId,
        size: usize,
    ) -> Self {
        Self {
            name,
            size,
            arguments,
            returning: type_id,
        }
    }
}

impl CheckedNamedArgument {
    pub fn new(
        name: String, 
        type_id: TypeId, 
        expr: CheckedExpression,
        span: Span,
    ) -> Self {
        Self {
            name,
            type_id,
            expr,
            span,
            offset: 0,
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
        scope_id: ScopeId,
        is_referenced: bool
    ) -> Self {
        Self {
            name,
            type_id,
            scope_id,
            is_referenced,
        }
    }    
}

impl CheckedVariableDecl {
    pub fn new(
        name: String, 
        type_id: TypeId, 
        scope_id: ScopeId,
        expr: CheckedExpression,
    ) -> Self {
        Self {
            name,
            type_id,
            scope_id,
            expr,
        }
    }    
}

impl CheckedFunction {
    pub fn new(
        name: String, 
        parameters: Vec<CheckedNamedParameter>,
        block: Option<CheckedBlock>,
        returning: TypeId,
        external: bool,
    ) -> Self {
        Self {
            name,
            parameters,
            block,
            returning,
            external,
        }     
    }
}

impl CheckedReturn {
    pub fn new(
        expr: CheckedExpression,
    ) -> Self {
        Self {
            expr
        }
    }
}

impl CheckedBlock {
    pub fn new(scope_id: ScopeId) -> Self {
        Self {
            children: vec![],
            scope_id,
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
    pub fn new(
        name: String, 
        fields: Vec<CheckedField>,
        size: usize,
    ) -> Self {
        Self {
            name,
            size,
            fields,
            type_id: None,
        } 
    }
}

impl CheckedLookup {
    pub fn new(
        lhs: CheckedVariable,
        rhs: String,
        id: TypeId,
        offset: usize,
    ) -> Self {
        Self {
            lhs, 
            rhs,
            type_id: id,
            offset,
        }
    }
}
