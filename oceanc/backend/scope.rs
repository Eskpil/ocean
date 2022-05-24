use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct BackendScope {
    name: String,
    variables: HashMap<String, usize>, 
}

impl BackendScope {
    pub fn new(name: String) -> Self {
        Self {
            name,
            variables: HashMap::new(),
        }
    }

    pub fn append_variable(&mut self, name: String, addr: usize) {
       self.variables.insert(name, addr);
    }

    pub fn eject_variables(&self) -> HashMap<String, usize> {
        self.variables.clone()
    }
}
