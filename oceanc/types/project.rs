use super::{
    TypeId,
    ScopeId,

    CheckedStruct, 
    CheckedField,
    DefinitionResult,
    StatementResult,
    ExpressionResult,
    CheckedDefinition,
    CheckedBlock,
    CheckedFunction,
    CheckedVariable,
    CheckedStatement,
    CheckedExpression,
    CheckedBinaryExpression,
    CheckedNamedParameter,
    CheckedNamedArgument,
    CheckedFunctionCall,

    BOOL_TYPE_ID,
    STRING_TYPE_ID,
    INT_TYPE_ID,

    scope::Scope,
};
use crate::ast::{
    BinaryOp,
    definitions::{
        Definition,
        StructDefinition,
        FieldDefinition,
    },
    statements::{
        Statement,
        NamedParameter,
    },
    expressions::{
        Expression,
        NamedArgument,
    },
};
use crate::errors::TypeError;
use std::boxed::Box;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Project {
    pub structs: Vec<CheckedStruct>, 
    pub functions: Vec<CheckedFunction>,
    pub types: HashMap<String, TypeId>,
    pub scopes: Vec<Scope>,
}

impl Project {
    pub fn new() -> Self {
        let mut types = HashMap::new();

        types.insert("Bool".to_string(), BOOL_TYPE_ID);
        types.insert("Int".to_string(), INT_TYPE_ID);
        types.insert("String".to_string(), STRING_TYPE_ID);

        let program_scope = Scope::new();

        Self {
            structs: vec![],
            functions: vec![],
            scopes: vec![program_scope],
            types,
        }
    }

    pub fn create_scope(&mut self, parent_id: ScopeId) -> ScopeId {
        let parent = &self.scopes[parent_id].clone();
        self.scopes.push(Scope::from(parent));

        self.scopes.len() - 1
    }


    pub fn add_variable_to_scope(
        &mut self, 
        variable: CheckedVariable,
        scope_id: ScopeId
    ) {
        let scope = &mut self.scopes[scope_id];

        scope.append_variable(variable);
    }

    pub fn add_function_to_scope(
        &mut self, 
        function: CheckedFunction,
        scope_id: ScopeId
    ) {
        let scope = &mut self.scopes[scope_id];

        scope.append_function(function);
    }

    pub fn find_variable(
        &mut self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<CheckedVariable, TypeError> {
        let scope = &self.scopes[scope_id]; 
        let var = scope.find_variable(name.clone())?;
        Ok(var)
    }

    pub fn find_function(
        &mut self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<CheckedFunction, TypeError> {
        let scope = &self.scopes[scope_id]; 
        let function = scope.find_function(name.clone())?;
        Ok(function)
    }

    pub fn allocate_type(&mut self, name: String) -> TypeId {
        self.types.insert(name, self.types.len());
        self.types.len()
    }

    pub fn lookup_type(&self, name: String) -> Result<TypeId, TypeError> {
        match self.types.get(&name) {
            Some(id) => Ok(id.clone()),
            None => Err(TypeError::UnknownType(name))        
        }
    }

    pub fn lookup_type_name(&self, id: TypeId) -> Option<String> {
        let mut found: Option<String> = None; 

        for (type_name, type_id) in self.types.iter() {
            if type_id.clone() == id.clone() {
                found = Some(type_name.to_string());
            }
        }

        found
    }

    pub fn typecheck_program(&mut self, program: &Statement) -> Result<(), TypeError> {
        let scope_id = 0;
        match program {
            Statement::Program(children) => {
                for child in children.iter() {
                    self.typecheck_statement(child, scope_id)?;
                } 
            }
            o => unreachable!("Expected Statement::Program but found {:?}", o)
        };
        Ok(())
    }

    pub fn typecheck_statement(
        &mut self, 
        statement: &Statement, 
        scope_id: ScopeId
    ) -> StatementResult {
        let checked_stmt = match statement {
            Statement::Define(definition) => {
                let checked = self.typecheck_definition(definition, scope_id)?;
                match checked {
                    CheckedDefinition::Struct(s) => self.structs.push(s),
                };

                CheckedStatement::Define
            },
            Statement::Function(name, parameters, children) => {
                let function = self.typecheck_function(
                    name.clone(), 
                    &parameters, 
                    children.clone(), 
                    scope_id
                )?;
                self.functions.push(function.clone());
                CheckedStatement::Function(function)
            }
            Statement::Declaration(name, expr) => {
                let variable = self.typecheck_variable(name.clone(), expr, scope_id)?;
                CheckedStatement::Variable(variable)
            }
            Statement::Block(body) => {
                let block = self.typecheck_block(body.clone(), scope_id)?;
                CheckedStatement::Block(block)     
            }
            Statement::Expression(expr) => {
                let checked_expr = self.typecheck_expression(expr, scope_id)?; 
                CheckedStatement::Expression(checked_expr)
            }
            o => todo!("Implement typechecking for {:?}", o),
        }; 

        Ok(checked_stmt)
    }

    pub fn get_expression_type_id(
        &mut self,
        expr: &CheckedExpression,
        scope_id: ScopeId,
    ) -> Result<TypeId, TypeError> {
        match expr {
            CheckedExpression::Literal => self.lookup_type("Int".to_string()),
            CheckedExpression::StringLiteral => self.lookup_type("String".to_string()),
            CheckedExpression::Identifier(name) => {
                let variable = self.find_variable(name.to_string(), scope_id)?; 
                Ok(variable.type_id)
            }
            CheckedExpression::Binary(_) => self.lookup_type("Int".to_string()),
            o => todo!("Get TypeId from {:?}", o),
        }  
    }

    pub fn typecheck_binary_expression(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        op: &BinaryOp,
        scope_id: ScopeId,
    ) -> Result<CheckedBinaryExpression, TypeError> {
        let checked_lhs = self.typecheck_expression(lhs, scope_id)?;      
        let checked_rhs = self.typecheck_expression(rhs, scope_id)?;

        let lhs_type_id = self.get_expression_type_id(&checked_lhs, scope_id)?;
        let rhs_type_id = self.get_expression_type_id(&checked_rhs, scope_id)?;

        if lhs_type_id != rhs_type_id {
            Err(TypeError::MismatchedTypes(
                    self.lookup_type_name(lhs_type_id).unwrap(), 
                    self.lookup_type_name(rhs_type_id).unwrap()
            ))             
        } else {
            let expr = CheckedBinaryExpression::new(
                *op, 
                Box::new(checked_lhs), 
                Box::new(checked_rhs)
            );

            Ok(expr)
        } 
    }

    pub fn typecheck_expression(
        &mut self,
        expression: &Expression,
        scope_id: ScopeId,
    ) -> ExpressionResult {
        let expr = match expression {
            Expression::Literal(v) => CheckedExpression::Literal,   
            Expression::StringLiteral(_) => CheckedExpression::StringLiteral,
            Expression::Identifier(name) => CheckedExpression::Identifier(name.to_string()),
            Expression::Call(name, arguments) => {
                let call = self.typecheck_function_call(
                    name.to_string(), 
                    arguments.clone(),
                    scope_id,
                )?;

                CheckedExpression::Call(call)
            }
            Expression::Binary(op, lhs, rhs) => {
                let expr = self.typecheck_binary_expression(&*lhs, &*rhs, op, scope_id)?;
                CheckedExpression::Binary(expr)
            }
            o => unreachable!("Implement typechecking for expression: {:?}", o)
        };

        Ok(expr)
    }
    
    pub fn typecheck_named_argument(
        &mut self,
        argument: &NamedArgument,
        scope_id: ScopeId,
    ) -> Result<CheckedNamedArgument, TypeError> {
        let name = argument.name.clone(); 
        let checked_expr = self.typecheck_expression(&argument.value, scope_id)?;
        let type_id = self.get_expression_type_id(&checked_expr, scope_id)?;
        Ok(CheckedNamedArgument::new(name, type_id))
    }

    pub fn typecheck_function_call(
        &mut self,
        name: String,
        arguments: Vec<NamedArgument>,
        scope_id: ScopeId,
    ) -> Result<CheckedFunctionCall, TypeError> {
        let mut checked_arguments = Vec::<CheckedNamedArgument>::new();

        for argument in arguments.iter() {
            match checked_arguments.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(checked) => {
                    return Err(TypeError::DuplicateFunctionCallArgument(argument.name.clone()));
                }
                None => {
                    let checked_argument = self.typecheck_named_argument(
                        argument, 
                        scope_id
                    )?;
                    checked_arguments.push(checked_argument);        
                }
            } 
        }

        let function = self.find_function(name.clone(), scope_id)?;  
        
        if checked_arguments.len() > function.parameters.len() {
            return Err(TypeError::ExhaustiveFunctionCallArguments(
                    function.parameters.len(), 
                    checked_arguments
            ));
        } 

        for argument in checked_arguments.iter() {
            match function.parameters.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(param) => {
                    if param.type_id != argument.type_id {
                        return Err(TypeError::MismatchedTypes(
                            self.lookup_type_name(param.type_id).unwrap(),
                            self.lookup_type_name(argument.type_id).unwrap(),
                        ))        
                    } 
                }  
                None => {
                    return Err(TypeError::UnknownFunctionArgument(argument.name.clone())); 
                }
            }            
        }

        let checked_call = CheckedFunctionCall::new(name.clone(), checked_arguments); 
        Ok(checked_call)
    }

    pub fn typecheck_variable(
        &mut self,
        name: String,
        expression: &Expression,
        scope_id: ScopeId,
    ) -> Result<CheckedVariable, TypeError> {
        let checked_expression = self.typecheck_expression(expression, scope_id)?;
        let type_id = self.get_expression_type_id(&checked_expression, scope_id)?;
        let checked_variable = CheckedVariable::new(name.clone(), type_id);
        self.add_variable_to_scope(checked_variable.clone(), scope_id);
        Ok(checked_variable)
    }

    pub fn typecheck_block(
        &mut self, 
        statements: Vec<Statement>,
        scope_id: ScopeId,
    ) -> Result<CheckedBlock, TypeError> {
        let mut checked_block = CheckedBlock::new();
        let block_scope_id = self.create_scope(scope_id);

        for statement in statements.iter() {
            let checked_statement = self.typecheck_statement(statement, block_scope_id)?; 
            checked_block.children.push(checked_statement);
        } 

        Ok(checked_block)     
    }

    pub fn typecheck_function_block(
        &mut self, 
        statements: Vec<Statement>,
        parameters: &Vec<CheckedNamedParameter>,
        scope_id: ScopeId,
    ) -> Result<CheckedBlock, TypeError> {
        let mut checked_block = CheckedBlock::new();
        let block_scope_id = self.create_scope(scope_id);

        for param in parameters.iter() {
            let var = CheckedVariable::new(param.name.clone(), param.type_id);
            self.add_variable_to_scope(var, block_scope_id);
        }

        for statement in statements.iter() {
            let checked_statement = self.typecheck_statement(statement, block_scope_id)?; 
            checked_block.children.push(checked_statement);
        } 

        Ok(checked_block)     
    }

    pub fn typecheck_named_parameter(
        &mut self,
        parameter: &NamedParameter,
        scope_id: ScopeId,
    ) -> Result<CheckedNamedParameter, TypeError> {
        let name = parameter.name.clone(); 
        let type_id = self.lookup_type(parameter.defined_type.to_name())?;
        Ok(CheckedNamedParameter::new(name, type_id))
    }

    pub fn typecheck_function(
        &mut self, 
        name: String, 
        parameters: &Vec<NamedParameter>,
        children: Vec<Statement>,
        scope_id: ScopeId,
    ) -> Result<CheckedFunction, TypeError> {
        let mut checked_parameters = Vec::<CheckedNamedParameter>::new();

        for parameter in parameters.iter() {
            match checked_parameters.iter().find(
                |&x| x.name == parameter.name.clone()
            ) {
                Some(checked) => {
                    return Err(TypeError::DuplicateFunctionParameter(parameter.name.clone()));
                }
                None => {
                    let checked_parameter = self.typecheck_named_parameter(
                        parameter, 
                        scope_id
                    )?;
                    checked_parameters.push(checked_parameter);        
                }
            }
        }

        let mut checked_function = CheckedFunction::new(
            name.clone(), 
            checked_parameters.clone(),
            None
        );
        if children.len() > 0 {
            let checked_block = self.typecheck_function_block(
                children, 
                &checked_parameters, 
                scope_id
            )?;
            checked_function = CheckedFunction::new(
                name.clone(), 
                checked_parameters.clone(),
                Some(checked_block)
            );
        }

        self.add_function_to_scope(checked_function.clone(), scope_id);
    
        Ok(checked_function)          
    }

    pub fn typecheck_definition(
        &mut self, 
        definition: &Definition,
        scope_id: ScopeId,
    ) -> DefinitionResult {
        let def = match definition {
            Definition::Struct(structure) => {
                let mut fields = Vec::<CheckedField>::new(); 

                for unchecked_field in structure.fields.iter() {
                    let id = self.lookup_type(unchecked_field.defined_type.to_name())?;
                    let checked_field = CheckedField::new(unchecked_field.name.clone(), id);  
                }
                
                let mut checked_struct = CheckedStruct::new(structure.name.clone(), fields);
                let type_id = self.allocate_type(checked_struct.name.clone());
                checked_struct.type_id = Some(type_id);

                CheckedDefinition::Struct(checked_struct)
            } 
        }; 

        Ok(def)
    }
}
