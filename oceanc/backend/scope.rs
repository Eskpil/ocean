use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct BackendScope {
    name: String,
    // For counting the garbadge we put on our stack.
    pub count: usize,
    // local name, scoped name
    variables: HashMap<String, String>
}

impl BackendScope {
    pub fn new(name: String) -> Self {
        Self {
            name,
            variables: HashMap::new(), 
            count: 0,
        }
    }

    pub fn from(name: String, parent: BackendScope) -> Self {
        let mut variables = HashMap::new();

        for (local, scoped) in parent.variables.into_iter() {
            variables.insert(local, scoped);
        } 

        Self {
            name,
            variables,
            count: 0,
        }
    }

    pub fn append(&mut self, name: String) -> String {
        let full_name = format!("{}_{}", self.name, name.clone());
        self.variables.insert(name, full_name.clone());
        full_name
    }

    pub fn find(&mut self, name: String) -> String {
        self.variables.get(&name).unwrap().clone() 
    }

    pub fn eject_variables(&self) -> HashMap<String, String> {
        self.variables.clone()
    }
}
