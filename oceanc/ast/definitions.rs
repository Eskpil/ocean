use super::util;

#[derive(Debug, Clone)]
pub enum DefinedType {
    Name(String),
    // Leave this to the type inference.
    Empty,
}

#[derive(Debug, Clone)]
pub struct FieldDefinition {
    pub name: String,
    pub defined_type: DefinedType,
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub name: String, 
    pub fields: Vec<FieldDefinition>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Struct(StructDefinition),
}

impl Definition {
    pub fn print(&self, indent: usize) {
        match self {
            Definition::Struct(d) => {
                util::print_indent(indent, "Struct:".into());  
                util::print_indent(indent + 1, "Name:".into());
                util::print_indent(indent + 2, d.name.clone());
                util::print_indent(indent + 1, "Fields:".into());

                for field in d.fields.iter() {
                    util::print_indent(indent + 2, "Name:".into());
                    util::print_indent(indent + 3, field.name.clone());
                    util::print_indent(indent + 2, "Type:".into());
                    util::print_indent(indent + 3, "bool".into());
                }
            }
        }
    }
}

impl StructDefinition {
    pub fn new(name: String, fields: Vec<FieldDefinition>) -> Self {
        Self {
            name,
            fields,
        }
    }
}

impl FieldDefinition {
    pub fn new(field_name: String, defined_type: DefinedType) -> Self {
        Self {
            name: field_name,
            defined_type,
        }
    }
}
