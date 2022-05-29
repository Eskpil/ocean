pub mod syntax;

use crate::types::{CheckedNamedArgument};

#[derive(Debug)]
pub enum TypeError {
    UnknownType(String),
    MismatchedTypes(String, String),
    VariableNotInScope(String),
    FunctionNotInScope(String),
    DuplicateFunctionParameter(String),
    DuplicateFunctionCallArgument(String),
    ExhaustiveFunctionCallArguments(usize, Vec<CheckedNamedArgument>),
    UnknownFunctionArgument(String),
}
