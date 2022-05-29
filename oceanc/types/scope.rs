use super::{CheckedVariable, ScopeId};
use crate::errors::{TypeError};

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: Vec<CheckedVariable>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: vec![],
        }
    }

    pub fn from(parent: &Scope) -> Self {
        let mut variables = Vec::<CheckedVariable>::new();         

        for variable in parent.variables.iter() {
            variables.push(variable.clone());
        }

        Self {
            variables,
        }
    } 

    pub fn append_variable(&mut self, variable: CheckedVariable) {
        self.variables.push(variable);
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
}
