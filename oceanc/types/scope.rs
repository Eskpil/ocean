use super::{
    CheckedFunction, 
    CheckedVariable, 
    ScopeId,
    CheckedStruct,
};
use crate::errors::{TypeError};

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: Vec<CheckedVariable>,
    pub functions: Vec<CheckedFunction>,
    pub structs: Vec<CheckedStruct>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: vec![],
            functions: vec![],
            structs: vec![],
        }
    }

    pub fn from(parent: &Scope) -> Self {
        let mut variables = Vec::<CheckedVariable>::new();         
        let mut functions = Vec::<CheckedFunction>::new();
        let mut structs = Vec::<CheckedStruct>::new();

        for variable in parent.variables.iter() {
            variables.push(variable.clone());
        }

        for function in parent.functions.iter() {
            functions.push(function.clone());
        }

        for structure in parent.structs.iter() {
            structs.push(structure.clone());
        }

        Self {
            variables,
            functions,
            structs,
        }
    } 

    pub fn append_variable(&mut self, variable: CheckedVariable) {
        self.variables.push(variable);
    }

    pub fn append_function(&mut self, function: CheckedFunction) {
        self.functions.push(function);
    }

    pub fn append_struct(&mut self, structure: CheckedStruct) {
        self.structs.push(structure);
    }

    pub fn find_struct(
        &self,
        name: String,
    ) -> Result<CheckedStruct, TypeError> {
        match self.structs.iter().find(|&x| x.name == name.clone()) {
            Some(structure) => Ok(structure.clone()),
            None => Err(TypeError::StructNotInScope(name.clone())),
        }
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
