use crate::types::project::Project;
use super::op::{Op, OpKind, Operand};
use super::generator::Generator;
use crate::types::{
    TypeId,

    CheckedStatement,
    CheckedBlock,
    CheckedFunction,
    CheckedExpression,
    CheckedVariable,
    CheckedVariableDecl,
    CheckedBinaryExpression,
    CheckedFunctionCall,
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
        CheckedStatement::Block(block) => 
            generate_block(project, block, generator),
        CheckedStatement::Expression(expr) => 
            generate_expression(project, expr, generator),
        CheckedStatement::VariableDecl(var) =>
            generate_variable(project, var, generator),
    }
}

pub fn generate_variable(
    project: &Project,
    var: &CheckedVariableDecl,
    generator: &mut Generator,
) {
    generate_expression(project, &var.expr, generator);  
    let op = Op::single(
        OpKind::NewVariable, 
        Operand::Symbol(var.name.clone())
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

pub fn generate_identifier_expression(
    project: &Project,
    name: String,
    type_id: TypeId,
    generator: &mut Generator,
) {
    let op = Op::single(OpKind::ResolveVariable, Operand::Symbol(name));  
    generator.append(op);

    if type_id == project.lookup_type("String".into()).unwrap() {
        let op = Op::none(OpKind::ResolveString); 
        generator.append(op);
    }
}

pub fn generate_call_expression(
    project: &Project,
    call: &CheckedFunctionCall,
    generator: &mut Generator,
) {
    for arg in call.arguments.iter() {
        generate_expression(project, &arg.expr, generator);
    }  

    let op = Op::double(
        OpKind::Call,
        Operand::Symbol(call.name.clone()),
        Operand::Uint(call.arguments.len() as u64),
    );

    generator.append(op);
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
        CheckedExpression::Call(call) => 
            generate_call_expression(project, call, generator),
        CheckedExpression::Identifier(v, id) => 
            generate_identifier_expression(
                project, 
                v.to_string(), 
                *id, 
                generator
            ),
        o => todo!("Implement expression: {:?}", o),
    }
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

        for param in function.parameters.iter() {
            let op = Op::single(
                OpKind::NewVariable,
                Operand::Symbol(param.name.clone()),
            );

            generator.append(op);
        }

        generate_block(project, block, generator);

        generator.append(Op::none(OpKind::End));
    }
}
