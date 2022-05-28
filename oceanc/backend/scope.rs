use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Variable {
    pub scoped_name: String,
    pub size: u64,
}

#[derive(Debug, Clone)]
pub struct BackendScope {
    name: String,
    // For counting the garbadge we put on our stack.
    pub gc_count: usize,
    data_count: usize,
    variables: HashMap<String, Variable>,
    data: HashMap<String, String>,
}

impl BackendScope {
    pub fn new(name: String) -> Self {
        Self {
            name,
            variables: HashMap::new(), 
            data: HashMap::new(),
            gc_count: 0,
            data_count: 0,
        }
    }

    pub fn from(name: String, parent: BackendScope) -> Self {
        let mut variables = HashMap::new();
        let mut data = HashMap::new();

        for (local, scoped) in parent.variables.into_iter() {
            variables.insert(local, scoped);
        } 

        for (name, d) in parent.data.into_iter() {
            data.insert(name, d);
        }

        Self {
            name,
            variables,
            data,
            gc_count: 0,
            data_count: 0,
        }
    }

    pub fn append_data(&mut self, data: String) -> String {
        let scoped_name = format!("{}_data_{}", self.name, self.data_count); 
        self.data_count += 1;
        self.data.insert(scoped_name.clone(), data);
        scoped_name
    }

    pub fn append_variable(&mut self, name: String, size: u64) -> Variable {
        let full_name = format!("{}_{}", self.name, name.clone());
        let variable = Variable { scoped_name: full_name, size };
        self.variables.insert(name, variable.clone());
        variable
    }

    pub fn find_variable(&mut self, name: String) -> Variable {
        self.variables.get(&name).unwrap().clone() 
    }

    pub fn eject_variables(&self) -> HashMap<String, Variable> {
        self.variables.clone()
    }

    pub fn eject_data(&self) -> HashMap<String, String> {
        self.data.clone()
    }
}
