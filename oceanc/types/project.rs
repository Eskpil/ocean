use super::{
    TypeId,
    ScopeId,
    StructId,

    Type,

    CheckedStruct, 
    CheckedField,
    StatementResult,
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
    CheckedArrayInit,
    CheckedArrayIndex,

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
        IfStatement,
    },
    expressions::{
        Expression,
        NamedArgument,
    },
};
use crate::errors::{OceanError, Level, Step};
use crate::lexer::Span;
use std::boxed::Box;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Prelude {
    pub structs: Vec<CheckedStruct>, 
}

#[derive(Debug)]
pub struct Project {
    pub structs: Vec<CheckedStruct>, 
    pub functions: Vec<CheckedFunction>,
    // pub types: HashMap<String, TypeId>,
    pub types: Vec<Type>,
    pub scopes: Vec<Scope>,

    pub returning: Type,
}

impl Project {
    pub fn new(prelude: &Statement) -> Self {
        let prelude_scope = Scope::new();

        let mut project = Self {
            structs: vec![],
            functions: vec![],
            scopes: vec![prelude_scope],
            types: vec![],
            returning: Type::Void,
        };

        match project.typecheck_program(prelude) {
            Ok(_) => {},
            Err(err) => {
                if err.level != Level::Ignore { 
                    err.report();
                }
            }
        };

        project.find_or_add_type_id(Type::Void);
        project.find_or_add_type_id(Type::Bool);
        project.find_or_add_type_id(Type::Usize);
        project.find_or_add_type_id(Type::OceanString);
        project.find_or_add_type_id(Type::Ptr);

        return project;         
    }

    pub fn find_struct_in_prelude(&mut self, name: String) -> Result<StructId, OceanError> {
        self.find_struct_id(name, 0)
    }

    pub fn find_or_add_type_id(&mut self, typ: Type) -> TypeId {
        let mut idx: usize = 0;
        for item in self.types.iter() {
            if *item == typ {
                return idx; 
            }
            idx += 1
        } 

        self.types.push(typ);

        return self.types.len() - 1;
    }

    pub fn find_type_id(&self, typ: Type) -> TypeId {
        let mut idx: usize = 0;
        for item in self.types.iter() {
            if *item == typ {
                return idx; 
            }
            idx += 1
        } 

        return 0;
    }

    pub fn add_struct(&mut self, structure: CheckedStruct) -> StructId {
        self.structs.push(structure);

        return self.structs.len() - 1;
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
    ) -> Result<CheckedVariable, OceanError> {
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
    ) -> Result<CheckedFunction, OceanError> {
        let scope = &self.scopes[scope_id]; 
        let function = scope.find_function(name.clone())?;
        Ok(function)
    }

    pub fn find_struct_id(
        &self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<StructId, OceanError> {
        let scope = &self.scopes[scope_id];
        let structure = scope.find_struct_id(name.clone())?;
        Ok(structure)
    } 

    pub fn find_struct(
        &self,
        name: String,
        scope_id: ScopeId,
    ) -> Result<CheckedStruct, OceanError> {
        let scope = &self.scopes[scope_id];
        let structure = scope.find_struct(name.clone())?;
        Ok(structure)
    } 

    pub fn find_struct_by_id(
        &self,
        id: StructId,
        scope_id: ScopeId,
    ) -> Result<CheckedStruct, OceanError> {
        let scope = &self.scopes[scope_id];
        let structure = scope.find_struct_by_id(id)?;
        Ok(structure)
    }

    pub fn get_type_size(&self, type_id: TypeId) -> Result<usize, OceanError> {
        Ok(8)
    }

    pub fn lookup_type_name(&self, id: TypeId) -> Option<String> {
        Some(format!("TypeID{id}"))
    }

    pub fn typecheck_program(&mut self, program: &Statement) -> Result<(), OceanError> {
        let scope_id = self.scopes.len() - 1;
        match program {
            Statement::Program(_, children) => {
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
            Statement::Define(span, definition) => {
                let checked = self.typecheck_definition(span, definition, scope_id)?;
                match checked {
                    CheckedDefinition::Struct(s) => self.structs.push(s),
                };

                CheckedStatement::Define
            },
            Statement::Function(
                span,
                name, 
                parameters, 
                children, 
                defined, 
                external
            ) => {
                let function = self.typecheck_function(
                    span,
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
            Statement::Declaration(
                span, 
                name, 
                expr, 
                is_referenced
            ) => {
                let decl = self.typecheck_variable(
                    span, 
                    name.clone(), 
                    expr, 
                    *is_referenced, 
                    scope_id
                )?;
                CheckedStatement::VariableDecl(decl)
            }
            Statement::Block(span, body) => {
                let block = self.typecheck_block(
                    span, 
                    body.clone(), 
                    scope_id
                )?;
                CheckedStatement::Block(block)     
            }
            Statement::If(
                span,
                if_stmt,
            ) => {
                let stmt = self.typecheck_if_statement(
                    span,
                    if_stmt,
                    scope_id
                )?;
                CheckedStatement::If(stmt)
            }
            Statement::Expression(
                span,
                expr
            ) => {
                let checked_expr = self.typecheck_expression(
                    span, 
                    expr, 
                    scope_id
                )?; 
                CheckedStatement::Expression(checked_expr)
            }
            Statement::While(
                span,
                cond, 
                body,
                body_span,
            ) => {
                let checked_stmt = self.typecheck_while_statement(
                    span,
                    cond,
                    body.clone(),
                    body_span,
                    scope_id,
                )?;

                CheckedStatement::While(checked_stmt)
            }
            Statement::Return(
                span, 
                expr
            ) => {
                let stmt = self.typecheck_return(span, expr, scope_id)?;
                CheckedStatement::Return(stmt)
            }
            o => todo!("Implement typechecking for {:?}", o),
        }; 

        Ok(checked_stmt)
    }

    pub fn typecheck_while_statement(
        &mut self,
        span: &Span,
        cond: &Expression,
        body: Vec<Statement>,
        body_span: &Span,
        scope_id: ScopeId,
    ) -> Result<CheckedWhileStatement, OceanError> {
        let checked_cond = self.typecheck_expression(&cond.span(), cond, scope_id)?; 

        let typ = self.get_expression_type(&cond.span(), &checked_cond, scope_id)?;

        if typ != Type::Bool {
            todo!("Error messages for expressions not returning boolean");  
        }

        let checked_body = self.typecheck_block(body_span, body, scope_id)?;
        
        Ok(CheckedWhileStatement::new(checked_cond, checked_body))
    }

    pub fn typecheck_if_statement(
        &mut self,
        span: &Span,
        stmt: &IfStatement,
        scope_id: ScopeId,
    ) -> Result<CheckedIfStatement, OceanError> {
        let cond = self.typecheck_expression(&stmt.cond.span(), &stmt.cond, scope_id)?;  

        let typ = self.get_expression_type(&stmt.cond.span(), &cond, scope_id)?;

        if typ != Type::Bool {
            todo!("Error messages for expressions not returning boolean");  
        }

        let checked_if_block = self.typecheck_block(
            &stmt.if_span, 
            stmt.if_block.clone(), 
            scope_id
        )?;

        let mut checked_else_block: Option<CheckedBlock> = None;

        if let Some(block) = &stmt.else_block {
            let output = self.typecheck_block(&stmt.else_span.as_ref().unwrap(), block.clone(), scope_id)?;
            checked_else_block = Some(output); 
        }

        Ok(CheckedIfStatement::new(cond, checked_if_block, checked_else_block))
    }

    pub fn get_expression_type(
        &mut self,
        span: &Span,
        expr: &CheckedExpression,
        scope_id: ScopeId,
    ) -> Result<Type, OceanError> {
        match expr {
            CheckedExpression::Literal(_) => Ok(Type::Usize),
            CheckedExpression::StringLiteral(_) => {
                let struct_id = self.find_struct_in_prelude("String".into())?;
                Ok(Type::Struct(struct_id))
            }
            CheckedExpression::Identifier(name, scope) => {
                let var = self.find_variable(name.clone(), *scope)?;
                Ok(var.typ)
            },
            CheckedExpression::Binary(expr) => {
                if expr.op.returns_bool() {
                    Ok(Type::Bool)
                } else { 
                    Ok(Type::Usize)
                }
            }
            CheckedExpression::Lookup(expr) => Ok(expr.typ.clone()),
            CheckedExpression::StructInit(init) => {
                let struct_id = self.find_struct_id(init.name.clone(), scope_id)?; 
                Ok(Type::Struct(struct_id))
            },
            CheckedExpression::Bool(_) => Ok(Type::Bool),
            CheckedExpression::Call(call) => {
                let function = self.find_function(call.name.clone(), scope_id)?;
                Ok(function.returning.clone())
            }
            CheckedExpression::ArrayInit(array) => {
                Ok(array.contains.clone())
            }
            CheckedExpression::ArrayIndex(index) => {
                Ok(index.typ.clone())
            }
            o => todo!("Get TypeId from {:?}", o),
        }  
    }

    pub fn typecheck_binary_expression(
        &mut self,
        span: &Span,
        lhs: &Expression,
        rhs: &Expression,
        op: &BinaryOp,
        scope_id: ScopeId,
    ) -> Result<CheckedBinaryExpression, OceanError> {
        let checked_lhs = self.typecheck_expression(&lhs.span(), lhs, scope_id)?;      
        let checked_rhs = self.typecheck_expression(&rhs.span(), rhs, scope_id)?;

        let lhs_type = self.get_expression_type(&lhs.span(), &checked_lhs, scope_id)?;
        let rhs_type = self.get_expression_type(&rhs.span(), &checked_rhs, scope_id)?;
        
        if lhs_type != rhs_type {
            Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    span.clone(),
                    format!(
                        "\x1b[1m{:?}\x1b[0m and \x1b[1m{:?}\x1b[0m are not suitable for binary operation: \x1b[1m{:?}\x1b[0m.",
                        lhs_type,
                        rhs_type,
                        op
                    )
                )
            )
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
        span: &Span,
        expression: &Expression,
        scope_id: ScopeId,
    ) -> Result<CheckedExpression, OceanError> {
        let expr = match expression {
            Expression::Literal(_, v) => CheckedExpression::Literal(v.clone()),   
            Expression::StringLiteral(_, v) => CheckedExpression::StringLiteral(v.to_string()),
            Expression::Identifier(
                _,
                name
            ) => {
                let variable = self.find_variable(
                    name.to_string(), 
                    scope_id
                )?;
                CheckedExpression::Identifier(name.to_string(), scope_id) 
            }
            Expression::Call(
                span, 
                name, 
                arguments
            ) => {
                let call = self.typecheck_function_call(
                    span,
                    name.to_string(), 
                    arguments.clone(),
                    scope_id,
                )?;

                CheckedExpression::Call(call)
            }
            Expression::Binary(
                span, 
                op, 
                lhs, 
                rhs
            ) => {
                let expr = self.typecheck_binary_expression(
                    span, 
                    &*lhs, 
                    &*rhs, 
                    op, 
                    scope_id
                )?;
                CheckedExpression::Binary(expr)
            }
            Expression::StructInit(
                span, 
                name, 
                arguments
            ) => {
                let expr = self.typecheck_struct_init(
                    span,
                    name.clone(), 
                    arguments.clone(), 
                    scope_id
                )?;
                CheckedExpression::StructInit(expr)
            }
            Expression::Bool(_, v) => CheckedExpression::Bool(v.clone()),
            Expression::Lookup(
                span, 
                lhs, 
                rhs
            ) => {
                let expr = self.typecheck_lookup(
                    span, 
                    lhs.clone(), 
                    rhs.clone(), 
                    scope_id
                )?;
                CheckedExpression::Lookup(expr)
            }
            Expression::ArrayInit(
                span,
                arguments
            ) => {
                let expr = self.typecheck_array_init(
                    span,
                    &mut arguments.clone(),
                    scope_id
                )?;
                CheckedExpression::ArrayInit(expr)
            }
            Expression::ArrayIndex(
                span,
                ident,
                index
            ) => {
                let expr = self.typecheck_array_index(
                    span,
                    ident.clone(),
                    index,
                    scope_id,
                )?;

                CheckedExpression::ArrayIndex(expr)
            }
            o => unreachable!("Implement typechecking for expression: {:?}", o)
        };

        Ok(expr)
    }

    pub fn typecheck_array_index(
        &mut self,
        span: &Span,
        ident: String,
        index: &u64,
        scope_id: ScopeId
    ) -> Result<CheckedArrayIndex, OceanError> {
        let var = self.find_variable(ident.clone(), scope_id)?;

        let expr = CheckedArrayIndex::new(
            ident.clone(), 
            *index, 
            var.typ, 
            scope_id
        );

        Ok(expr)
    }

    pub fn typecheck_array_init(
        &mut self,
        span: &Span,
        arguments: &mut Vec<Expression>,
        scope_id: ScopeId,
    ) -> Result<CheckedArrayInit, OceanError> {
        let reference_expression = arguments.remove(0);

        let checked_ref_expr = self.typecheck_expression(
            &reference_expression.span(), 
            &reference_expression, 
            scope_id
        )?;   

        let reference_type = self.get_expression_type(
            &reference_expression.span(),
            &checked_ref_expr,
            scope_id
        )?;

        let mut checked_arguments = Vec::<CheckedExpression>::new();

        checked_arguments.push(checked_ref_expr);

        for unchecked_expression in arguments.iter() {
            let checked_expression = self.typecheck_expression(
                &unchecked_expression.span(),
                &unchecked_expression,
                scope_id
            )?; 

            let checked_expression_type = self.get_expression_type(
                &unchecked_expression.span(),
                &checked_expression,
                scope_id
            )?;

            if checked_expression_type != reference_type {
                return Err(
                    OceanError::new(
                        Level::Error,
                        Step::Checking,
                        unchecked_expression.span(),
                        format!(
                            "Array is infeered to be of type: \x1b[1m{:?}\x1b[0m but found type: \x1b[1m{:?}\x1b[0m.",
                            reference_type, 
                            checked_expression_type 
                        )
                    )
                );  
            }

            checked_arguments.push(checked_expression);
        }

        let array = CheckedArrayInit::new(checked_arguments, reference_type);

        Ok(array)
    }
    
    pub fn typecheck_lookup(
        &mut self,
        span: &Span,
        lhs: String,
        rhs: String,
        scope_id: ScopeId,
    ) -> Result<CheckedLookup, OceanError> {
        let var = self.find_variable(lhs.clone(), scope_id)?;

        let mut struct_id: StructId = 0;

        match var.typ {
            Type::Struct(id) => struct_id = id,
            o => {
                return Err(
                    OceanError::new(
                        Level::Error,
                        Step::Checking,
                        span.clone(),
                        format!(
                            "Expected \x1b[1mStruct\x1b[0m but found: \x1b[1m{:?}\x1b[0m",
                            o
                        )
                    ) 
                ) 
            }
        }

        let structure = self.find_struct_by_id(struct_id, scope_id)?;
        let mut typ = Type::Void;
        let mut pair = false;
        let mut offset = 0;

        for field in structure.fields.iter() {
            if field.name == rhs {
                typ = field.typ.clone();
                pair = true;
                break;
            } else {
                let type_id = self.find_or_add_type_id(typ.clone());
                offset += self.get_type_size(type_id).unwrap();
            }
        }

        if pair {
            Ok(CheckedLookup::new(var.clone(), rhs, typ, offset))
        } else {
            todo!("Errors for non pairs.");
        }
    }

    pub fn typecheck_struct_init(
        &mut self,
        span: &Span,
        name: String,
        args: Vec<NamedArgument>,
        scope_id: ScopeId,
    ) -> Result<CheckedStructInit, OceanError> {
        let struct_id = self.find_struct_id(name.clone(), scope_id)?;
        let typ = Type::Struct(struct_id);
        let type_id = self.find_or_add_type_id(typ.clone());

        let mut checked_arguments = Vec::<CheckedNamedArgument>::new();

        for argument in args.iter() {
            match checked_arguments.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(checked) => {
                    return Err(
                        OceanError::new(
                            Level::Error,
                            Step::Checking,
                            span.clone(),
                            format!(
                                "Argument: \x1b[1m{}\x1b[0m has already been provided.", 
                                argument.name.clone()
                            ),
                        )
                    );
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
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    span.clone(),
                    format!(
                        "Exhaustive amount of init arguments, expected: \x1b[1m{}\x1b[0m but found: \x1b[1m{}]\x1b[0m",
                        structure.fields.len(),
                        checked_arguments.len(),
                    )
                )
            );
        } 

        if checked_arguments.len() < structure.fields.len() {
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    span.clone(),
                    format!(
                        "Inexhaustive amount of init arguments, expected: \x1b[1m{}\x1b[0m but found: \x1b[1m{}]\x1b[0m",
                        structure.fields.len(),
                        checked_arguments.len(),
                    )
                )
            );
        }  

        let mut offset = 0;

        for argument in checked_arguments.iter_mut() {
            match structure.fields.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(field) => {
                    if field.typ != argument.typ {
                        return Err(
                            OceanError::new(
                                Level::Error,
                                Step::Checking,
                                argument.span.clone(),
                                format!(
                                    "Mistmatched types between structure field and argument.\n
                                    Expected \x1b[1m{:?}\x1b[0m but found: \x1b[1m{:?}\x1b[0m",
                                    field.typ,
                                    argument.typ,
                                )
                            )
                        );
                    } 

                    let type_id = self.find_or_add_type_id(field.typ.clone());
                    argument.offset = offset;
                    offset += self.get_type_size(type_id)?;
                }  
                None => {
                    return Err(
                        OceanError::new(
                            Level::Error,
                            Step::Checking,
                            argument.span.clone(),
                            format!(
                                "Structure: \x1b[1m{}\x1b[0m does not have contain a field called: \x1b[1m{}\x1b[0m",
                                structure.name,
                                argument.name
                            )
                        )
                    )
                }
            }            
        }

        let checked_init = CheckedStructInit::new(
            name.clone(), 
            checked_arguments, 
            typ,
            structure.size,
        ); 
        Ok(checked_init)
    }
    
    pub fn typecheck_named_argument(
        &mut self,
        argument: &NamedArgument,
        scope_id: ScopeId,
    ) -> Result<CheckedNamedArgument, OceanError> {
        let name = argument.name.clone(); 
        let checked_expr = self.typecheck_expression(&argument.value.span(), &argument.value, scope_id)?;
        let typ = self.get_expression_type(&argument.value.span(), &checked_expr, scope_id)?;
        Ok(CheckedNamedArgument::new(name, typ, checked_expr, argument.span.clone()))
    }

    pub fn typecheck_function_call(
        &mut self,
        span: &Span,
        name: String,
        arguments: Vec<NamedArgument>,
        scope_id: ScopeId,
    ) -> Result<CheckedFunctionCall, OceanError> {
        let mut checked_arguments = Vec::<CheckedNamedArgument>::new();

        for argument in arguments.iter() {
            match checked_arguments.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(checked) => {
                    return Err(
                        OceanError::new(
                            Level::Error,
                            Step::Checking,
                            span.clone(),
                            format!(
                                "Argument: \x1b[1m{}\x1b[0m has already been provided.", 
                                argument.name.clone()
                            ),
                        )
                    );
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
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    span.clone(),
                    format!(
                        "Exhaustive amount of call arguments, expected: \x1b[1m{}\x1b[0m but found: \x1b[1m{}]\x1b[0m",
                        function.parameters.len(),
                        checked_arguments.len(),
                    )
                )
            );
        } 

        if checked_arguments.len() < function.parameters.len() {
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    span.clone(),
                    format!(
                        "Inexhaustive amount of call arguments, expected: \x1b[1m{}\x1b[0m but found: \x1b[1m{}]\x1b[0m",
                        function.parameters.len(),
                        checked_arguments.len(),
                    )
                )
            );
        } 

        for argument in checked_arguments.iter() {
            match function.parameters.iter().find(
                |&x| x.name == argument.name.clone()
            ) {
                Some(param) => {
                    if param.typ != argument.typ {
                        return Err(
                            OceanError::new(
                                Level::Error,
                                Step::Checking,
                                argument.span.clone(),
                                format!(
                                    "Mistmatched types between function parameter and argument in function: \x1b[1m{}\x1b[0m.\n
                                    Expected \x1b[1m{:?}\x1b[0m but found: \x1b[1m{:?}\x1b[0m",
                                    function.name,
                                    param.typ,
                                    argument.typ,
                                )
                            )
                        );
                    } 
                }  
                None => {
                    return Err(
                        OceanError::new(
                            Level::Error,
                            Step::Checking,
                            argument.span.clone(),
                            format!(
                                "Function: \x1b[1m{}\x1b[0m does not have contain a named paramter called: \x1b[1m{}\x1b[0m",
                                function.name,
                                argument.name
                            )
                        )
                    )
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
        span: &Span,
        expression: &Expression,
        scope_id: ScopeId,
    ) -> Result<CheckedReturn, OceanError> {
        let checked_expression = self.typecheck_expression(&expression.span(), expression, scope_id)?;   
        let typ = self.get_expression_type(&expression.span(), &checked_expression, scope_id)?;
        
        if typ != self.returning {
            Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    span.clone(),
                    format!(
                        "Mismatched types between return type: \x1b[1m{:?}\x1b[0m and expected return type: \x1b[1m{:?}\x1b[0m",
                        typ,
                        self.returning,
                    )
                )
            )
        } else {
            Ok(CheckedReturn::new(checked_expression))
        }
    }

    pub fn typecheck_variable(
        &mut self,
        span: &Span,
        name: String,
        expression: &Expression,
        is_referenced: bool,
        scope_id: ScopeId,
    ) -> Result<CheckedVariableDecl, OceanError> {
        let checked_expression = self.typecheck_expression(
            &expression.span(), 
            expression, 
            scope_id
        )?;

        let typ = self.get_expression_type(&expression.span(), &checked_expression, scope_id)?;

        if is_referenced {
            match typ {
                Type::Struct(_) => {}
                _ => {
                    return Err(
                        OceanError::new(
                            Level::Error,
                            Step::Checking,
                            expression.span(),
                            format!(
                                "Mistmatched types between: \x1b[1m{}\x1b[0m and anything that is a \x1b[1mstruct\x1b[0m.", 
                                self.lookup_type_name(10).unwrap()
                            )
                        )
                    );       
                }
            }
        }

        if typ == Type::Void {
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Checking,
                    expression.span(),
                    "Assignment to expression of type \x1b[1mVoid\x1b[0m is not allowed.".into(),
                )
            );
        }

        // Used for IR generation.
        let checked_variable_decl = CheckedVariableDecl::new(
            name.clone(), 
            typ.clone(),
            scope_id,
            checked_expression,
        );

        let checked_variable = CheckedVariable::new(
            name.clone(),
            typ.clone(),
            scope_id,
            is_referenced,
        );

        self.add_variable_to_scope(checked_variable.clone(), scope_id);

        Ok(checked_variable_decl)
    }

    pub fn typecheck_block(
        &mut self, 
        span: &Span,
        statements: Vec<Statement>,
        scope_id: ScopeId,
    ) -> Result<CheckedBlock, OceanError> {
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
    ) -> Result<CheckedBlock, OceanError> {
        let block_scope_id = self.create_scope(scope_id);
        let mut checked_block = CheckedBlock::new(block_scope_id);

        for param in parameters.iter() {
            let var = CheckedVariable::new(param.name.clone(), param.typ.clone(), scope_id, false);
            self.add_variable_to_scope(var, block_scope_id);
        }

        for statement in statements.iter() {
            let checked_statement = self.typecheck_statement(
                statement, 
                block_scope_id
            )?; 
            checked_block.children.push(checked_statement);
        } 

        let index = checked_block.children.len() - 1;

        match checked_block.children.get_mut(index) {
            Some(stmt) => {
                match stmt {
                    CheckedStatement::Return(_) => {
                        // Don't override a late return if one has already been defined.
                    }
                    _ => {
                        let ret = CheckedReturn::new(CheckedExpression::Empty);       
                        checked_block.children.push(CheckedStatement::Return(ret));
                    }
                }
            } 
            None => {
                panic!("Last element not found.");
            }
        }

        Ok(checked_block)     
    }

    pub fn typecheck_named_parameter(
        &mut self,
        parameter: &NamedParameter,
        scope_id: ScopeId,
    ) -> Result<CheckedNamedParameter, OceanError> {
        let name = parameter.name.clone();
        let typ = self.typecheck_type(&parameter.defined_type, scope_id)?;
        Ok(CheckedNamedParameter::new(name, typ))
    }

    pub fn typecheck_function(
        &mut self, 
        span: &Span,
        name: String, 
        parameters: &Vec<NamedParameter>,
        children: Vec<Statement>,
        defined_type: &DefinedType,
        external: &bool,
        scope_id: ScopeId,
    ) -> Result<CheckedFunction, OceanError> {
        let mut checked_parameters = Vec::<CheckedNamedParameter>::new();

        let typ = self.typecheck_type(defined_type, scope_id)?;

        for parameter in parameters.iter() {
            match checked_parameters.iter().find(
                |&x| x.name == parameter.name.clone()
            ) {
                Some(checked) => {
                    return Err(
                        OceanError::new(
                            Level::Error,
                            Step::Checking,
                            parameter.span.clone(),
                            format!(
                                "Function: \x1b[1m{}\x1b[0m already has a parameter called: \x1b[1m{}\x1b[0m.", 
                                name, 
                                &parameter.name
                            ),
                        )
                    );
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

        self.returning = typ.clone();

        let mut checked_function = CheckedFunction::new(
            name.clone(), 
            checked_parameters.clone(),
            None,
            typ.clone(),
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
                typ,
                *external,
            );
        }

        self.add_function_to_scope(checked_function.clone(), scope_id);
    
        Ok(checked_function)          
    }

    pub fn typecheck_type(
        &mut self,
        defined_type: &DefinedType,
        scope_id: ScopeId
    ) -> Result<Type, OceanError> {
        let typ = match defined_type {
            DefinedType::Name(name) => {
                if name == "usize" {
                    return Ok(Type::Usize);
                } else if name == "bool" {
                    return Ok(Type::Bool);
                } else if name == "String" {
                    let struct_id = self.find_struct_in_prelude("String".into())?;
                    return Ok(Type::Struct(struct_id));
                } else if name == "void" {
                    return Ok(Type::Void);
                } else {
                    panic!(format!("Handle type: {name}"));
                }
            }
            DefinedType::Array(of) => {
                let array_type_id = self.find_struct_in_prelude("Array".into())?;      
                let of_type = self.typecheck_type(of, scope_id)?;
                let of_type_id = self.find_or_add_type_id(of_type);

                Type::GenericInstance(array_type_id, vec![of_type_id])
            }  
            DefinedType::Empty => {
                Type::Bool
            }
        };  

        Ok(typ)
    }

    pub fn typecheck_definition(
        &mut self, 
        span: &Span,
        definition: &Definition,
        scope_id: ScopeId,
    ) -> Result<CheckedDefinition, OceanError> {
        let def = match definition {
            Definition::Struct(structure) => {
                let mut fields = Vec::<CheckedField>::new(); 
                let mut size = 0;

                for unchecked_field in structure.fields.iter() {
                    let typ = self.typecheck_type(&unchecked_field.defined_type, scope_id)?;
                    let id = self.find_or_add_type_id(typ.clone());
                    size += self.get_type_size(id)?;
                    let checked_field = CheckedField::new(unchecked_field.name.clone(), typ);  
                    fields.push(checked_field);
                }

                let mut checked_struct = CheckedStruct::new(
                    structure.name.clone(), 
                    fields, 
                    size
                );
                
                let struct_id = self.add_struct(checked_struct.clone());
                let type_id = self.find_or_add_type_id(Type::Struct(struct_id));

                self.add_struct_to_scope(checked_struct.clone(), scope_id);

                CheckedDefinition::Struct(checked_struct.clone())
            } 
        }; 

        Ok(def)
    }
}
