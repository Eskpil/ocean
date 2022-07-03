use super::{
    CheckedFunction, 
    CheckedVariable, 
    ScopeId,
    TypeId,
    StructId,
    CheckedStruct,
};
use crate::errors::{OceanError, Level, Step};

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
        name: String
    ) -> Result<CheckedStruct, OceanError> {
        match self.structs.iter().find(|&x| x.name == name.clone()) {
            Some(structure) => Ok(structure.clone()),
            None => Err(
                OceanError::no_span(
                    Level::Error,
                    Step::Checking,
                    format!(
                        "Function: {} does not exist in current scope.", 
                        name.clone()
                    ),                    
                )
            )
        }
    }

    pub fn find_struct_id(
        &self,
        name: String,
    ) -> Result<StructId, OceanError> {
        let mut idx: usize = 0;
        for structure in self.structs.iter() {
            if structure.name == name.clone() {
                return Ok(idx);
            }
            idx += 1; 
        }

        Err(OceanError::no_span(
            Level::Error,
            Step::Checking,
            format!(
                "Structure: {} does not exist in current scope.", 
                name.clone()
            ),                    
        ))
    }

    pub fn find_struct_by_id(
        &self,
        id: StructId,
    ) -> Result<CheckedStruct, OceanError> {
        match self.structs.get(id) {
            Some(s) => Ok(s.clone()),
            None => {
            Err(OceanError::no_span(
                Level::Error,
                Step::Checking,
                format!(
                    "Structure: {} does not exist in current scope.", 
                    id 
                ),                    
            ))               
            }
        }
    }

    pub fn find_variable(
        &self, 
        name: String
    ) -> Result<CheckedVariable, OceanError> {
        match self.variables.iter().find(|&x| x.name == name.clone()) {
            Some(var) => Ok(var.clone()),
            None => Err(
                OceanError::no_span(
                    Level::Error,
                    Step::Checking,
                    format!(
                        "Variable: {} does not exist in current scope.", 
                        name.clone()
                    ),                    
                )
            ),
        }   
    }

    pub fn find_function(
        &self, 
        name: String
    ) -> Result<CheckedFunction, OceanError> {
        match self.functions.iter().find(|&x| x.name == name.clone()) {
            Some(var) => Ok(var.clone()),
            None => Err(
                OceanError::no_span(
                    Level::Error,
                    Step::Checking,
                    format!(
                        "Function: {} does not exist in current scope.", 
                        name.clone()
                    ),                    
                )
            ),
        }   
    }

}
