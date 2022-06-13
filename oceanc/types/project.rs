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
    CheckedIfStatement,
    CheckedVariableDecl,
    CheckedWhileStatement,
    CheckedReturn,
    CheckedStructInit,
    CheckedLookup,

    VOID_TYPE_ID,
    BOOL_TYPE_ID,
    STRING_TYPE_ID,
    INT_TYPE_ID,
    PTR_TYPE_ID,

    scope::Scope,
};
use crate::ast::{
    BinaryOp,
    definitions::{
        Definition,
        StructDefinition,
        FieldDefinition,
        DefinedType,
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

    pub returning: TypeId,
}

impl Project {
    pub fn new() -> Self {
        let mut types = HashMap::new();

        types.insert("Void".to_string(), VOID_TYPE_ID);
        types.insert("Bool".to_string(), BOOL_TYPE_ID);
        types.insert("Int".to_string(), INT_TYPE_ID);
        types.insert("String".to_string(), STRING_TYPE_ID);
        types.insert("Ptr".to_string(), PTR_TYPE_ID);

        let program_scope = Scope::new();

        Self {
            structs: vec![],
            functions: vec![],
            scopes: vec![program_scope],
            types,
            returning: VOID_TYPE_ID,
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

    pub fn add_struct_to_scope(
        &mut self, 
        structure: CheckedStruct,
        scope_id: ScopeId
    ) {
        let scope = &mut self.scopes[scope_id];

        scope.append_struct(structure);
    }

    pub fn find_variable(
        &self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<CheckedVariable, TypeError> {
        let scope = &self.scopes[scope_id]; 
        let var = scope.find_variable(name.clone())?;
        Ok(var)
    }

    pub fn scope_variables(
        &self,
        scope_id: ScopeId,
    ) -> Vec<CheckedVariable> {
        let scope = &self.scopes[scope_id];     
        let vars = scope.variables.clone();
        vars
    }

    pub fn find_function(
        &self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<CheckedFunction, TypeError> {
        let scope = &self.scopes[scope_id]; 
        let function = scope.find_function(name.clone())?;
        Ok(function)
    }

    pub fn find_struct(
        &self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<CheckedStruct, TypeError> {
        let scope = &self.scopes[scope_id];
        let structure = scope.find_struct(name.clone())?;
        Ok(structure)
    } 

    pub fn find_struct_by_id(
        &self,
        id: TypeId,
        scope_id: ScopeId,
    ) -> Result<CheckedStruct, TypeError> {
        let scope = &self.scopes[scope_id];
        let structure = scope.find_struct_by_id(id)?;
        Ok(structure)
    }

    pub fn get_type_size(&self, type_id: TypeId) -> Result<usize, TypeError> {
        if type_id == 0 {
            Ok(0)
        } else if type_id == 1 {
            Ok(8)
        } else if type_id == 2 {
            Ok(8)
        } else if type_id == 3 {
            Ok(8)
        } else {
            Ok(8) 
        }
    }

    pub fn allocate_type(&mut self, name: String) -> TypeId {
        let id = self.types.len();
        self.types.insert(name, id.clone());
        id
    }

    pub fn lookup_type(&self, name: String, scope_id: ScopeId) -> Result<TypeId, TypeError> {
        match self.types.get(&name) {
            Some(id) => Ok(id.clone()),
            None => { 
                match self.find_struct(name, scope_id) {
                    Ok(s) => Ok(s.type_id.unwrap()),
                    Err(err) => Err(err),
                }
            }
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
            Statement::Function(
                name, 
                parameters, 
                children, 
                defined, 
                external
            ) => {
                let function = self.typecheck_function(
                    name.clone(), 
                    &parameters, 
                    children.clone(), 
                    defined,
                    external,
                    scope_id
                )?;
                self.functions.push(function.clone());
                CheckedStatement::Function(function)
            }
            Statement::Declaration(name, expr, is_referenced) => {
                let decl = self.typecheck_variable(name.clone(), expr, *is_referenced, scope_id)?;
                CheckedStatement::VariableDecl(decl)
            }
            Statement::Block(body) => {
                let block = self.typecheck_block(body.clone(), scope_id)?;
                CheckedStatement::Block(block)     
            }
            Statement::If(cond, if_block, else_block) => {
                let stmt = self.typecheck_if_statement(
                    cond, 
                    if_block.clone(), 
                    else_block.clone(), 
                    scope_id
                )?;
                CheckedStatement::If(stmt)
            }
            Statement::Expression(expr) => {
                let checked_expr = self.typecheck_expression(expr, scope_id)?; 
                CheckedStatement::Expression(checked_expr)
            }
            Statement::While(cond, body) => {
                let checked_stmt = self.typecheck_while_statement(
                    cond,
                    body.clone(),
                    scope_id,
                )?;

                CheckedStatement::While(checked_stmt)
            }
            Statement::Return(expr) => {
                let stmt = self.typecheck_return(expr, scope_id)?;
                CheckedStatement::Return(stmt)
            }
            o => todo!("Implement typechecking for {:?}", o),
        }; 

        Ok(checked_stmt)
    }

    pub fn typecheck_while_statement(
        &mut self,
        cond: &Expression,
        body: Vec<Statement>,
        scope_id: ScopeId,
    ) -> Result<CheckedWhileStatement, TypeError> {
        let cond = self.typecheck_expression(cond, scope_id)?; 

        let type_id = self.get_expression_type_id(&cond, scope_id)?;

        if type_id != self.lookup_type("Bool".into(), scope_id)? {
            todo!("Error messages for expressions not returning boolean");  
        }

        let checked_body = self.typecheck_block(body, scope_id)?;
        
        Ok(CheckedWhileStatement::new(cond, checked_body))
    }

    pub fn typecheck_if_statement(
        &mut self,
        cond: &Expression,
        if_block: Vec<Statement>,
        else_block: Option<Vec<Statement>>,
        scope_id: ScopeId,
    ) -> Result<CheckedIfStatement, TypeError> {
        let cond = self.typecheck_expression(cond, scope_id)?;  

        let type_id = self.get_expression_type_id(&cond, scope_id)?;

        if type_id != self.lookup_type("Bool".into(), scope_id)? {
            todo!("Error messages for expressions not returning boolean");  
        }

        let checked_if_block = self.typecheck_block(if_block, scope_id)?;

        let mut checked_else_block: Option<CheckedBlock> = None;

        if let Some(block) = else_block {
            let output = self.typecheck_block(block, scope_id)?;
            checked_else_block = Some(output); 
        }

        Ok(CheckedIfStatement::new(cond, checked_if_block, checked_else_block))
    }

    pub fn get_expression_type_id(
        &mut self,
        expr: &CheckedExpression,
        scope_id: ScopeId,
    ) -> Result<TypeId, TypeError> {
        match expr {
            CheckedExpression::Literal(_) => self.lookup_type("Int".to_string(), scope_id),
            CheckedExpression::StringLiteral(_) => self.lookup_type("String".to_string(), scope_id),
            CheckedExpression::Identifier(name, scope) => {
                let var = self.find_variable(name.clone(), *scope)?;
                Ok(var.type_id)
            },
            CheckedExpression::Binary(expr) => {
                if expr.op.returns_bool() {
                    self.lookup_type("Bool".to_string(), scope_id)
                } else { 
                    self.lookup_type("Int".to_string(), scope_id)
                }
            }
            CheckedExpression::Lookup(expr) => Ok(expr.type_id),
            CheckedExpression::StructInit(init) => Ok(init.returning),
            CheckedExpression::Bool(_) => self.lookup_type("Bool".to_string(), scope_id),
            CheckedExpression::Call(call) => {
                let function = self.find_function(call.name.clone(), scope_id)?;

                Ok(function.returning)
            }
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
            Expression::Literal(v) => CheckedExpression::Literal(v.clone()),   
            Expression::StringLiteral(v) => CheckedExpression::StringLiteral(v.to_string()),
            Expression::Identifier(name) => {
                let variable = self.find_variable(name.to_string(), scope_id)?;
                CheckedExpression::Identifier(name.to_string(), scope_id) 
            }
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
            Expression::StructInit(name, arguments) => {
                let expr = self.typecheck_struct_init(name.clone(), arguments.clone(), scope_id)?;
                CheckedExpression::StructInit(expr)
            }
            Expression::Bool(v) => CheckedExpression::Bool(v.clone()),
            Expression::Lookup(lhs, rhs) => {
                let expr = self.typecheck_lookup(lhs.clone(), rhs.clone(), scope_id)?;
                CheckedExpression::Lookup(expr)
            }
            o => unreachable!("Implement typechecking for expression: {:?}", o)
        };

        Ok(expr)
    }
    
    pub fn typecheck_lookup(
        &mut self,
        lhs: String,
        rhs: String,
        scope_id: ScopeId,
    ) -> Result<CheckedLookup, TypeError> {
        let var = self.find_variable(lhs.clone(), scope_id)?;
        let structure = self.find_struct_by_id(var.type_id, scope_id)?;
        let mut type_id = VOID_TYPE_ID;
        let mut pair = false;
        let mut offset = 0;

        for field in structure.fields.iter() {
            if field.name == rhs {
                type_id = field.type_id;
                pair = true;
                break;
            } else {
                offset += self.get_type_size(field.type_id).unwrap();
            }
        }

        if pair {
            Ok(CheckedLookup::new(var.clone(), rhs, type_id, offset))
        } else {
            todo!("Errors for non pairs.");
        }
    }

    pub fn typecheck_struct_init(
        &mut self,
        name: String,
        args: Vec<NamedArgument>,
        scope_id: ScopeId,
    ) -> Result<CheckedStructInit, TypeError> {
        let type_id = self.lookup_type(name.clone(), scope_id)?;
        let mut checked_arguments = Vec::<CheckedNamedArgument>::new();

        for argument in args.iter() {
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

        let structure = self.find_struct(name.clone(), scope_id)?;

        if checked_arguments.len() > structure.fields.len() {
            return Err(TypeError::ExhaustiveFunctionCallArguments(
                    structure.fields.len(), 
                    checked_arguments
            ));
        } 

        let mut offset = 0;

        for argument in checked_arguments.iter_mut() {
            match structure.fields.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(field) => {
                    if field.type_id != argument.type_id {
                        return Err(TypeError::MismatchedTypes(
                            self.lookup_type_name(field.type_id).unwrap(),
                            self.lookup_type_name(argument.type_id).unwrap(),
                        ))        
                    } 

                    argument.offset = offset;
                    offset += self.get_type_size(field.type_id)?;
                }  
                None => {
                    return Err(TypeError::UnknownFunctionArgument(argument.name.clone())); 
                }
            }            
        }

        let checked_init = CheckedStructInit::new(
            name.clone(), 
            checked_arguments, 
            type_id,
            structure.size,
        ); 
        Ok(checked_init)
    }
    
    pub fn typecheck_named_argument(
        &mut self,
        argument: &NamedArgument,
        scope_id: ScopeId,
    ) -> Result<CheckedNamedArgument, TypeError> {
        let name = argument.name.clone(); 
        let checked_expr = self.typecheck_expression(&argument.value, scope_id)?;
        let type_id = self.get_expression_type_id(&checked_expr, scope_id)?;
        Ok(CheckedNamedArgument::new(name, type_id, checked_expr))
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

        let checked_call = CheckedFunctionCall::new(
            name.clone(), 
            checked_arguments, 
            function.returning
        ); 
        Ok(checked_call)
    }

    pub fn typecheck_return(
        &mut self,
        expression: &Expression,
        scope_id: ScopeId,
    ) -> Result<CheckedReturn, TypeError> {
        let checked_expression = self.typecheck_expression(expression, scope_id)?;   
        let type_id = self.get_expression_type_id(&checked_expression, scope_id)?;
        
        if type_id != self.returning {
            Err(TypeError::MismatchedTypes(
                self.lookup_type_name(type_id).unwrap(),
                self.lookup_type_name(self.returning).unwrap(),
            )) 
        } else {
            Ok(CheckedReturn::new(checked_expression))
        }
    }

    pub fn typecheck_variable(
        &mut self,
        name: String,
        expression: &Expression,
        is_referenced: bool,
        scope_id: ScopeId,
    ) -> Result<CheckedVariableDecl, TypeError> {
        let checked_expression = self.typecheck_expression(expression, scope_id)?;

        let type_id = self.get_expression_type_id(&checked_expression, scope_id)?;

        if is_referenced && type_id < PTR_TYPE_ID {
            return Err(TypeError::MismatchedTypes(
                self.lookup_type_name(type_id).unwrap(),
                "Struct".into(),
            )); 
        }

        if type_id == VOID_TYPE_ID {
            return Err(TypeError::VoidAssignment); 
        }

        // Used for IR generation.
        let checked_variable_decl = CheckedVariableDecl::new(
            name.clone(), 
            type_id, 
            scope_id,
            checked_expression,
        );

        let checked_variable = CheckedVariable::new(
            name.clone(),
            type_id,
            scope_id,
            is_referenced,
        );

        self.add_variable_to_scope(checked_variable.clone(), scope_id);
        Ok(checked_variable_decl)
    }

    pub fn typecheck_block(
        &mut self, 
        statements: Vec<Statement>,
        scope_id: ScopeId,
    ) -> Result<CheckedBlock, TypeError> {
        let block_scope_id = self.create_scope(scope_id);
        let mut checked_block = CheckedBlock::new(block_scope_id);

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
        let block_scope_id = self.create_scope(scope_id);
        let mut checked_block = CheckedBlock::new(block_scope_id);

        for param in parameters.iter() {
            let var = CheckedVariable::new(param.name.clone(), param.type_id, scope_id, false);
            self.add_variable_to_scope(var, block_scope_id);
        }

        for statement in statements.iter() {
            let checked_statement = self.typecheck_statement(
                statement, 
                block_scope_id
            )?; 
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
        let type_id = self.lookup_type(parameter.defined_type.to_name(), scope_id)?;
        Ok(CheckedNamedParameter::new(name, type_id))
    }

    pub fn typecheck_function(
        &mut self, 
        name: String, 
        parameters: &Vec<NamedParameter>,
        children: Vec<Statement>,
        defined_type: &DefinedType,
        external: &bool,
        scope_id: ScopeId,
    ) -> Result<CheckedFunction, TypeError> {
        let mut checked_parameters = Vec::<CheckedNamedParameter>::new();
        let mut returning: TypeId = VOID_TYPE_ID;

        if let DefinedType::Name(name) = defined_type.clone() {
            let type_id = self.lookup_type(name.clone(), scope_id)?;
            returning = type_id; 
        } 

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

        self.returning = returning;

        let mut checked_function = CheckedFunction::new(
            name.clone(), 
            checked_parameters.clone(),
            None,
            returning,
            *external,
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
                Some(checked_block),
                returning,
                *external,
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
                let mut size = 0;

                for unchecked_field in structure.fields.iter() {
                    let id = self.lookup_type(unchecked_field.defined_type.to_name(), scope_id)?;
                    size += self.get_type_size(id)?;
                    let checked_field = CheckedField::new(unchecked_field.name.clone(), id);  
                    fields.push(checked_field);
                }

                let mut checked_struct = CheckedStruct::new(
                    structure.name.clone(), 
                    fields, 
                    size
                );
                let type_id = self.allocate_type(checked_struct.name.clone());
                checked_struct.type_id = Some(type_id);

                self.add_struct_to_scope(checked_struct.clone(), scope_id);

                CheckedDefinition::Struct(checked_struct)
            } 
        }; 

        Ok(def)
    }
}
