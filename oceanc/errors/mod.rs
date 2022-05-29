pub mod syntax;

#[derive(Debug)]
pub enum TypeError {
    UnknownType(String),
    MismatchedTypes(String, String),
    VariableNotInScope(String),
    FunctionNotInScope(String),
    DuplicateFunctionParameter(String),
}
