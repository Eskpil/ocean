use crate::types::project::Project;
use super::op::{
    Op, 
    OpKind, 
    Operand,
    Type
};
use super::register::Register;
use super::generator::Generator;
use crate::types::{
    TypeId,
    ScopeId,

    Type as ProjectType,

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
    CheckedArrayInit,
    CheckedArrayIndex,
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
    let block = &mut block.clone();

    let ret = block.children.pop().unwrap();

    for stmt in block.children.iter() {
        generate_statement(project, stmt, generator);
    } 

    let scope_id = block.scope_id.clone();

    for variable in project.scope_variables(scope_id) {

        if !variable.is_referenced {
            match variable.typ {
                ProjectType::Struct(id) => {
                    let mut offset = 0;

                    for scope_var in project.scope_variables(scope_id).iter() {
                        if scope_var.name == variable.name {
                            break;
                        } else {
                            let type_id = project.find_type_id(scope_var.typ.clone());
                            offset += project.get_type_size(type_id).unwrap() as u64; 
                        } 
                    }

                    generator.append(Op::single(OpKind::Deref, Operand::Uint(offset)));
                }
                _ => {}
            }
        }
    }

    generate_statement(project, &ret, generator);
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
        CheckedStatement::Expression(expr) => {
            let _ = generate_expression(project, expr, generator);
        }
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

    let reg = generate_expression(project, &stmt.cond, generator);
    generator.release_reg(reg);

    generator.append(Op::double(OpKind::JumpUnless, Operand::Reg(reg), Operand::Symbol(end_label.clone())));

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
    let reg = generate_expression(project, &stmt.expr, generator);  

    generator.release_reg(reg);

    generator.append(Op::single(OpKind::Return, Operand::Reg(reg)));
}

pub fn generate_if_statement(
    project: &Project,
    stmt: &CheckedIfStatement,
    generator: &mut Generator,
) {
    let end_label = generator.allocate_label();
    let true_block = generator.allocate_label();
    let reg = generate_expression(project, &stmt.cond, generator);

    if let Some(block) = &stmt.else_block {
        let else_block = generator.allocate_label(); 

        generator.release_reg(reg);

        generator.append(
            Op::double(
                OpKind::JumpUnless, 
                Operand::Reg(reg),
                Operand::Symbol(else_block.clone())
            )
        );

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
        generator.release_reg(reg);
        generator.append(
            Op::double(
                OpKind::JumpUnless,
                Operand::Reg(reg),
                Operand::Symbol(end_label.clone()),
            )
        );
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
    let reg = generate_expression(project, &var.expr, generator);  
    
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

            let type_id = project.find_type_id(scope_var.typ.clone());
            offset += project.get_type_size(type_id).unwrap() as u64; 
        } 
    }

    let mut typ = Type::Object;

    if var.typ == ProjectType::Usize || var.typ == ProjectType::Bool {
        typ = Type::Num;
    }

    if is_reference {
        typ = Type::Reference; 
    }

    let op = Op::tripple(
        OpKind::NewVariable, 
        Operand::Reg(reg),
        Operand::Uint(offset),
        Operand::Type(typ),
    );

    generator.release_reg(reg);

    generator.append(op);
}

pub fn generate_variable_lookup(
    project: &Project,
    name: String,
    scope_id: ScopeId,
    reg: Register,
    generator: &mut Generator,
) {
    let var = project.find_variable(name.clone(), scope_id).unwrap();

    let scope_id = var.scope_id; 
    let mut offset: u64 = 0;

    for scope_var in project.scope_variables(scope_id).iter() {
        if scope_var.name == var.name {
            break;
        } else {
            let type_id = project.find_type_id(scope_var.typ.clone());
            offset += project.get_type_size(type_id).unwrap() as u64;
        }
    }

    let op = Op::double(
        OpKind::ResolveVariable,
        Operand::Reg(reg),
        Operand::Uint(offset),
    );

    generator.append(op);
}

pub fn generate_binary_expression(
    project: &Project,
    expr: &CheckedBinaryExpression,
    dst: Register,
    generator: &mut Generator,
) {
    let lhs = generate_expression(project, &*expr.lhs, generator);  
    let rhs = generate_expression(project, &*expr.rhs, generator);

    let op = Op::quadrouple(
        OpKind::Intrinsic, 
        Operand::Op(expr.op), 
        Operand::Reg(dst), 
        Operand::Reg(lhs), 
        Operand::Reg(rhs)
    );

    generator.release_reg(lhs);
    generator.release_reg(rhs);

    generator.append(op);
}

pub fn generate_call_expression(
    project: &Project,
    call: &CheckedFunctionCall,
    dst: Register,
    generator: &mut Generator,
) {
    let mut arguments = Vec::<Register>::new();

    for arg in call.arguments.iter() {
        let reg = generate_expression(project, &arg.expr, generator);
        arguments.push(reg);
    }  

    let op = Op::tripple(
        OpKind::Call,
        Operand::Symbol(call.name.clone()),
        Operand::Regs(arguments.clone()),
        Operand::Reg(dst),
    );

    for arg in arguments {
        generator.release_reg(arg);
    }

    generator.append(op);
}

pub fn generate_struct_init(
    project: &Project, 
    init: &CheckedStructInit,
    dst: Register,
    generator: &mut Generator,
) {
    generator.append(
        Op::double(
            OpKind::NewStruct, 
            Operand::Reg(dst), 
            Operand::Uint(init.size as u64)
        )
    );

    for argument in init.arguments.iter() {
        let reg = generate_expression(project, &argument.expr, generator);
        let type_id = project.find_type_id(argument.typ.clone());
        let size = project.get_type_size(type_id).unwrap();

        let mut typ = Type::Object;

        if argument.typ == ProjectType::Usize || argument.typ == ProjectType::Bool {
            typ = Type::Num;
        }

        generator.append(Op::quadrouple(
                OpKind::SetField, 
                Operand::Reg(reg),
                Operand::Reg(dst),
                Operand::Uint(argument.offset as u64),
                Operand::Type(typ),
        ));

        generator.release_reg(reg);
    }
}

pub fn generate_expression(
    project: &Project,
    expr: &CheckedExpression,
    generator: &mut Generator,
) -> Register {
    let reg = generator.allocate_reg();

    match expr {
        CheckedExpression::Binary(expr) =>
            generate_binary_expression(project, expr, reg, generator),
        CheckedExpression::Literal(v) => {
            let op = Op::double(OpKind::Load, Operand::Reg(reg), Operand::Uint(*v));
            generator.append(op);
        }
        CheckedExpression::StringLiteral(v) => {
            let op = Op::double(OpKind::NewString, Operand::Reg(reg), Operand::Data(v.clone()));
            generator.append(op);
        }
        CheckedExpression::Bool(v) => {
            let mut op = Op::double(OpKind::Load, Operand::Reg(reg), Operand::Uint(0)); 

            if *v {
                op = Op::double(OpKind::Load, Operand::Reg(reg), Operand::Uint(1));
            }

            generator.append(op);
        }
        CheckedExpression::Call(call) => 
            generate_call_expression(project, call, reg, generator),
        CheckedExpression::StructInit(init) =>
            generate_struct_init(project, init, reg, generator),
        CheckedExpression::Identifier(v, id) => 
            generate_variable_lookup(
                project,
                v.to_string(),
                *id,
                reg,
                generator,
            ),
        CheckedExpression::Lookup(expr) =>
            generate_lookup(project, expr, reg, generator),
        CheckedExpression::ArrayInit(expr) =>
            generate_array_init(project, expr, reg, generator),
        CheckedExpression::ArrayIndex(expr) =>
            generate_array_index(project, expr, reg, generator),
        CheckedExpression::Empty => {}
        o => todo!("Implement expression: {:?}", o),
    };

    return reg;
}

pub fn generate_array_index(
    project: &Project,
    index: &CheckedArrayIndex,
    dst: Register,
    generator: &mut Generator
) {
    let mut offset: u64 = 0;

    for scope_var in project.scope_variables(index.scope_id).iter() {
        if scope_var.name == index.ident {
            break;
        } else {
            let type_id = project.find_type_id(scope_var.typ.clone());
            offset += project.get_type_size(type_id).unwrap() as u64; 
        } 
    }

    let array = generator.allocate_reg();
     
    let op = Op::double(
        OpKind::ResolveVariable,
        Operand::Reg(array),
        Operand::Uint(offset),
    );

    let mut typ = Type::Object;

    if index.typ == ProjectType::Usize || index.typ == ProjectType::Bool {
        typ = Type::Num;
    }

    generator.append(op);

    let op = Op::quadrouple(
        OpKind::ArrayIndex,
        Operand::Reg(array),
        Operand::Reg(dst),
        Operand::Uint(index.index),
        Operand::Type(typ),
    );

    generator.release_reg(array);

    generator.append(op);
}

pub fn generate_array_init(
    project: &Project,
    array: &CheckedArrayInit,
    reg: Register,
    generator: &mut Generator,
) {
    let array_size = array.arguments.len();   
    let type_id = project.find_type_id(array.contains.clone());
    let elem_size = project.get_type_size(type_id).unwrap();

    let op = Op::tripple(
        OpKind::ArrayInit,
        Operand::Reg(reg),
        Operand::Uint(elem_size as u64),
        Operand::Uint(array_size as u64),
    );

    generator.append(op);

    for arg in array.arguments.iter() {
        let val = generate_expression(project, arg, generator);

        let mut typ = Type::Object;

        if array.contains == ProjectType::Usize || array.contains == ProjectType::Bool {
            typ = Type::Num;
        }

        let op = Op::tripple(
            OpKind::ArrayAppend,
            Operand::Reg(val),
            Operand::Reg(reg),
            Operand::Type(typ),
        );  
        generator.append(op);

        generator.release_reg(val);
    }
}

pub fn generate_lookup(
    project: &Project,
    lookup: &CheckedLookup,
    dst: Register,
    generator: &mut Generator,
) {
    let scope_id = lookup.lhs.scope_id;     
    let mut offset: u64 = 0;

    for scope_var in project.scope_variables(scope_id).iter() {
        if scope_var.name == lookup.lhs.name {
            break;
        } else {
            let type_id = project.find_type_id(scope_var.typ.clone());
            offset += project.get_type_size(type_id).unwrap() as u64;
        }
    }

    let structure = generator.allocate_reg();

    let op = Op::double(
        OpKind::ResolveVariable,
        Operand::Reg(structure),
        Operand::Uint(offset),
    );

    generator.append(op);

    let mut typ = Type::Object;

    if lookup.typ == ProjectType::Usize || lookup.typ == ProjectType::Bool {
        Type::Num;
    }

    let op = Op::quadrouple(
        OpKind::ResolveField,
        Operand::Reg(structure),
        Operand::Reg(dst),
        Operand::Uint(lookup.offset as u64),
        Operand::Type(typ),
    );

    generator.release_reg(structure);

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
        let mut idx = 0;

        for param in function.parameters.iter() {
            let mut typ = Type::Object;

            if param.typ == ProjectType::Usize || param.typ == ProjectType::Bool {
                typ = Type::Num;
            } 

            let op = Op::tripple(
                OpKind::NewParameter,
                Operand::Uint(idx),
                Operand::Uint(offset),
                Operand::Type(typ),
            );

            let type_id = project.find_type_id(param.typ.clone());
            let size = project.get_type_size(type_id).unwrap() as u64;
            offset += size;

            generator.append(op);

            idx += 1;
        }

        generate_block(project, block, generator);
    } else {
        if function.external {
            generator.add_external(function.name.clone());
        }    
    }}


