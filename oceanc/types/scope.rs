use super::{CheckedFunction, CheckedVariable, ScopeId};
use crate::errors::{TypeError};

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: Vec<CheckedVariable>,
    pub functions: Vec<CheckedFunction>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: vec![],
            functions: vec![],
        }
    }

    pub fn from(parent: &Scope) -> Self {
        let mut variables = Vec::<CheckedVariable>::new();         
        let mut functions = Vec::<CheckedFunction>::new();

        for variable in parent.variables.iter() {
            variables.push(variable.clone());
        }

        for function in parent.functions.iter() {
            functions.push(function.clone());
        }

        Self {
            variables,
            functions,
        }
    } 

    pub fn append_variable(&mut self, variable: CheckedVariable) {
        self.variables.push(variable);
    }

    pub fn append_function(&mut self, function: CheckedFunction) {
        self.functions.push(function);
    }

    pub fn find_variable(
        &self, 
        name: String
    ) -> Result<CheckedVariable, TypeError> {
        match self.variables.iter().find(|&x| x.name == name.clone()) {
            Some(var) => Ok(var.clone()),
            None => Err(TypeError::VariableNotInScope(name.clone())),
        }   
    }

    pub fn find_function(
        &self, 
        name: String
    ) -> Result<CheckedFunction, TypeError> {
        match self.functions.iter().find(|&x| x.name == name.clone()) {
            Some(var) => Ok(var.clone()),
            None => Err(TypeError::FunctionNotInScope(name.clone())),
        }   
    }

}
