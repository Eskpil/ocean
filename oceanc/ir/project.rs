use crate::types::project::Project;
use super::op::{
    Op, 
    OpKind, 
    Operand,
    Type
};
use super::generator::Generator;
use crate::types::{
    TypeId,
    ScopeId,

    VOID_TYPE_ID,
    STRING_TYPE_ID,
    PTR_TYPE_ID,
    BOOL_TYPE_ID,
    INT_TYPE_ID,

    CheckedStatement,
    CheckedBlock,
    CheckedFunction,
    CheckedExpression,
    CheckedVariable,
    CheckedVariableDecl,
    CheckedBinaryExpression,
    CheckedFunctionCall,
    CheckedIfStatement,
    CheckedWhileStatement,
    CheckedReturn,
    CheckedStructInit,
    CheckedLookup,
};

pub fn generate_project(project: &Project, generator: &mut Generator) {
    for function in project.functions.iter() {
        generate_function(project, function, generator);
    }
} 

pub fn generate_block(
    project: &Project,
    block: &CheckedBlock,
    generator: &mut Generator,
) {
    for stmt in block.children.iter() {
        generate_statement(project, stmt, generator);
    } 

    let scope_id = block.scope_id.clone();

    for variable in project.scope_variables(scope_id) {

        if !variable.is_referenced && variable.type_id > PTR_TYPE_ID {
            let mut offset = 0;

            for scope_var in project.scope_variables(scope_id).iter() {
                if scope_var.name == variable.name {
                    break;
                } else {
                    offset += project.get_type_size(scope_var.type_id).unwrap() as u64; 
                } 
            }

            generator.append(Op::single(OpKind::Deref, Operand::Uint(offset)));
        }
    }
}

pub fn generate_statement(
    project: &Project, 
    stmt: &CheckedStatement, 
    generator: &mut Generator
) {
    match stmt {
        CheckedStatement::Define => {},   
        CheckedStatement::Function(func) => 
            generate_function(project, func, generator),
        CheckedStatement::Block(block) => { 
            let label = generator.allocate_label();
            generator.append(Op::single(OpKind::Block, Operand::Symbol(label)));
            generate_block(project, block, generator);
        }
        CheckedStatement::Expression(expr) => 
            generate_expression(project, expr, generator),
        CheckedStatement::VariableDecl(var) =>
            generate_variable_decl(project, var, generator),
        CheckedStatement::If(stmt) =>
            generate_if_statement(project, stmt, generator),
        CheckedStatement::While(stmt) =>
            generate_while_statement(project, stmt, generator),
        CheckedStatement::Return(stmt) =>
            generate_return_statement(project, stmt, generator),
        o => todo!("Implement generating of {:?} statement", o),
    }
}

pub fn generate_while_statement(
    project: &Project,
    stmt: &CheckedWhileStatement,
    generator: &mut Generator,
) {
    let end_label = generator.allocate_label();
    let expression_label = generator.allocate_label();                  

    generator.append(Op::single(OpKind::Block, Operand::Symbol(expression_label.clone())));
    generate_expression(project, &stmt.cond, generator);
    generator.append(Op::single(OpKind::JumpUnless, Operand::Symbol(end_label.clone())));

    let body_label = generator.allocate_label();
    generator.append(Op::single(OpKind::Block, Operand::Symbol(body_label)));

    generate_block(project, &stmt.body, generator);
    
    generator.append(Op::single(OpKind::Jump, Operand::Symbol(expression_label.clone())));

    generator.append(Op::single(OpKind::Block, Operand::Symbol(end_label.clone())));
}

pub fn generate_return_statement(
    project: &Project,
    stmt: &CheckedReturn,
    generator: &mut Generator,
) {
    generate_expression(project, &stmt.expr, generator);  
    generator.append(Op::none(OpKind::Return));
}

pub fn generate_if_statement(
    project: &Project,
    stmt: &CheckedIfStatement,
    generator: &mut Generator,
) {
    let end_label = generator.allocate_label();
    let true_block = generator.allocate_label();
    generate_expression(project, &stmt.cond, generator);

    if let Some(block) = &stmt.else_block {
        let else_block = generator.allocate_label(); 
        generator.append(Op::single(
                OpKind::JumpUnless, 
                Operand::Symbol(else_block.clone())
        ));

        generator.append(Op::single(
                OpKind::Block,
                Operand::Symbol(true_block.clone()),
        ));
        generate_block(project, &stmt.if_block, generator);
        
        generator.append(Op::single(
                OpKind::Jump,
                Operand::Symbol(end_label.clone()),
        ));

        generator.append(Op::single(
                OpKind::Block,
                Operand::Symbol(else_block.clone()),
        ));
        generate_block(project, &block, generator);
    } else {
        generator.append(Op::single(
            OpKind::JumpUnless,
            Operand::Symbol(end_label.clone()),
        ));
        generator.append(Op::single(
            OpKind::Block,
            Operand::Symbol(true_block.clone()),
        ));

        generate_block(project, &stmt.if_block, generator);
    }

    generator.append(Op::single(
        OpKind::Block,
        Operand::Symbol(end_label.clone()),
    ));
}

pub fn generate_variable_decl(
    project: &Project,
    var: &CheckedVariableDecl,
    generator: &mut Generator,
) {
    generate_expression(project, &var.expr, generator);  
    
    let scope_id = var.scope_id;
    let mut offset: u64 = 0;
    let mut is_reference = false;

    for scope_var in project.scope_variables(scope_id).iter() {
        if scope_var.name == var.name {
            break;
        } else {
            if scope_var.is_referenced {
                is_reference = true;
            }

            offset += project.get_type_size(scope_var.type_id).unwrap() as u64; 
        } 
    }

    let mut typ = Type::Object;

    if var.type_id == PTR_TYPE_ID || var.type_id == STRING_TYPE_ID {
        typ = Type::Ptr;
    } else if var.type_id == INT_TYPE_ID || var.type_id == BOOL_TYPE_ID {
        typ = Type::Num;
    } else {
        typ = Type::Object;
    } 

    if is_reference {
        typ = Type::Reference; 
    }

    let op = Op::double(
        OpKind::NewVariable, 
        Operand::Uint(offset),
        Operand::Type(typ),
    );

    generator.append(op);
}

pub fn generate_variable_lookup(
    project: &Project,
    name: String,
    scope_id: ScopeId,
    generator: &mut Generator,
) {
    let var = project.find_variable(name.clone(), scope_id).unwrap();

    let scope_id = var.scope_id; 
    let mut offset: u64 = 0;

    for scope_var in project.scope_variables(scope_id).iter() {
        if scope_var.name == var.name {
            break;
        } else {
            offset += project.get_type_size(scope_var.type_id).unwrap() as u64;
        }
    }

    let op = Op::single(
        OpKind::ResolveVariable,
        Operand::Uint(offset),
    );

    generator.append(op);
}

pub fn generate_binary_expression(
    project: &Project,
    expr: &CheckedBinaryExpression,
    generator: &mut Generator,
) {
    generate_expression(project, &*expr.lhs, generator);  
    generate_expression(project, &*expr.rhs, generator);
    let op = Op::single(OpKind::Intrinsic, Operand::Op(expr.op));
    generator.append(op);
}

pub fn generate_call_expression(
    project: &Project,
    call: &CheckedFunctionCall,
    generator: &mut Generator,
) {
    for arg in call.arguments.iter() {
        generate_expression(project, &arg.expr, generator);
    }  

    let mut returning = true;

    if call.returning == VOID_TYPE_ID {
        returning = false;
    }

    let op = Op::tripple(
        OpKind::Call,
        Operand::Symbol(call.name.clone()),
        Operand::Uint(call.arguments.len() as u64),
        Operand::Bool(returning),
    );

    generator.append(op);
}

pub fn generate_struct_init(
    project: &Project, 
    init: &CheckedStructInit,
    generator: &mut Generator,
) {
    generator.append(Op::single(OpKind::NewStruct, Operand::Uint(init.size as u64)));

    for argument in init.arguments.iter() {
        generate_expression(project, &argument.expr, generator);
        let size = project.get_type_size(argument.type_id).unwrap();

        let mut typ = Type::Num;

        if argument.type_id == STRING_TYPE_ID {
            typ = Type::Ptr;
        }

        generator.append(Op::double(
                OpKind::SetField, 
                Operand::Uint(argument.offset as u64),
                Operand::Type(typ),
        ));
    }
}

pub fn generate_expression(
    project: &Project,
    expr: &CheckedExpression,
    generator: &mut Generator,
) {
    match expr {
        CheckedExpression::Binary(expr) =>
            generate_binary_expression(project, expr, generator),
        CheckedExpression::Literal(v) => {
            let op = Op::single(OpKind::Push, Operand::Uint(*v));
            generator.append(op);
        }
        CheckedExpression::StringLiteral(v) => {
            let op = Op::single(OpKind::NewString, Operand::Data(v.clone()));
            generator.append(op);
        }
        CheckedExpression::Bool(v) => {
            let mut op = Op::single(OpKind::Push, Operand::Uint(0)); 
            if *v {
                op = Op::single(OpKind::Push, Operand::Uint(1));
            }
            generator.append(op);
        }
        CheckedExpression::Call(call) => 
            generate_call_expression(project, call, generator),
        CheckedExpression::StructInit(init) =>
            generate_struct_init(project, init, generator),
        CheckedExpression::Identifier(v, id) => 
            generate_variable_lookup(
                project,
                v.to_string(),
                *id,
                generator,
            ),
        CheckedExpression::Lookup(expr) =>
            generate_lookup(project, expr, generator),
        o => todo!("Implement expression: {:?}", o),
    }
}

pub fn generate_lookup(
    project: &Project,
    lookup: &CheckedLookup,
    generator: &mut Generator,
) {
    let scope_id = lookup.lhs.scope_id;     
    let mut offset: u64 = 0;

    for scope_var in project.scope_variables(scope_id).iter() {
        if scope_var.name == lookup.lhs.name {
            break;
        } else {
            offset += project.get_type_size(scope_var.type_id).unwrap() as u64;
        }
    }

    let op = Op::single(
        OpKind::ResolveVariable,
        Operand::Uint(offset),
    );

    generator.append(op);

    let mut typ = Type::Object;

    if lookup.type_id == PTR_TYPE_ID || lookup.type_id == STRING_TYPE_ID {
        typ = Type::Ptr;
    } else if lookup.type_id == INT_TYPE_ID || lookup.type_id == BOOL_TYPE_ID {
        typ = Type::Num;
    } else {
        typ = Type::Object;
    }

    let op = Op::double(
        OpKind::ResolveField,
        Operand::Uint(lookup.offset as u64),
        Operand::Type(typ),
    );

    generator.append(op);
}

pub fn generate_function(
    project: &Project,
    function: &CheckedFunction,
    generator: &mut Generator,
) {
    if let Some(block) = &function.block {
        let op = Op::double(
            OpKind::Proc,
            Operand::Symbol(function.name.clone()),
            Operand::Uint(function.parameters.len() as u64),
        ); 

        generator.append(op);

        let mut offset = 0;

        for param in function.parameters.iter() {
            let op = Op::single(
                OpKind::NewVariable,
                Operand::Uint(offset),
            );

            let size = project.get_type_size(param.type_id).unwrap() as u64;
            offset += size;

            generator.append(op);
        }

        generate_block(project, block, generator);

        generator.append(Op::none(OpKind::End));
    } else {
        if function.external {
            generator.add_external(function.name.clone());
        }    
    }
}
