use crate::ast::{
    BinaryOp, 
    expressions::{
        Expression,
        NamedArgument,
    }, 
    statements::{
        Statement,
        NamedParameter,
    }, 
    definitions::{
        StructDefinition, 
        Definition, 
        FieldDefinition, 
        DefinedType
    }
};
use crate::lexer::{Lexer, Token, TokenKind};
use crate::errors::syntax::SyntaxError;
use crate::unescape::{unescape};
use std::iter::Peekable;

type SyntaxResult<T> = Result<T, SyntaxError>;
type ParseResult<T> = Result<T, SyntaxError>;

pub struct Parser {
    lexer: Peekable<Lexer>,
}

/* Credit to https://domenicquirl.github.io/blog/parsing-basics/#binary-operators */
trait Operator {
    /// Prefix operators bind their operand to the right
    fn prefix_binding_power(&self) -> Option<((), u8)>;

    /// Infix operators bind two operands, lhs and rhs
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    /// Postfix operators bind their operand to the left
    fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> Option<((), u8)> {
        Some(match self {
            TokenKind::Sub => ((), 51),
            TokenKind::Not => ((), 101),
            _ => {
                return None;
            }
        })
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        Some(match self {
            TokenKind::Or => (1, 2),
            TokenKind::And => (3, 4),
            TokenKind::Equals | TokenKind::NotEquals => (5, 6),
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEquals | TokenKind::GreaterEquals => (7, 8),
            TokenKind::Add | TokenKind::Sub => (9, 10),
            TokenKind::Mul | TokenKind::Div => (11, 12),
            _ => return None,
        })
    }

    fn postfix_binding_power(&self) -> Option<(u8, ())> {
        None
    }
}

impl Parser {
    pub fn new(input: String) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
        } 
    }

    pub fn next_token(&mut self) -> SyntaxResult<Token> {
        self.lexer.next().ok_or_else(|| {
            SyntaxError::UnexpectedEndOfInput(Token {
                kind: TokenKind::Eof,
                value: String::new(),
                row: 0,
                col: 0,
            })
        })
    }

    #[inline(always)]
    pub fn peek(&mut self) -> TokenKind {
        self.lexer
            .peek()
            .map(|token| token.kind)
            .unwrap_or(TokenKind::Eof)
    }

    #[inline(always)]
    pub fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    #[inline(always)]
    pub fn multi_at(&mut self, kinds: &'static [TokenKind]) -> bool {
        kinds.contains(&self.peek())
    }

    pub fn consume(&mut self, expected: TokenKind) -> SyntaxResult<()> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                found: token,
            })
        } else {
            Ok(())
        }
    }

    pub fn consume_next(&mut self, expected: TokenKind) -> SyntaxResult<Token> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(SyntaxError::UnexpectedToken {
                expected: expected.to_string(),
                found: token,
            })
        } else {
            Ok(token)
        }
    }

    pub fn parse_literal(&mut self, lit: TokenKind) -> ParseResult<Expression> {
        let token = self.next_token()?;
        
        let literal = match lit {
            TokenKind::Literal => Expression::Literal(token.value.parse::<u64>().unwrap()), 
            TokenKind::True => Expression::Bool(true),
            TokenKind::False => Expression::Bool(false),
            TokenKind::Identifier => Expression::Identifier(token.value.clone()),
            TokenKind::StringLiteral => { 
                let value = token.value.clone();
                let unescaped = unescape(&value).unwrap();
                Expression::StringLiteral(unescaped)
            },
            token => unimplemented!("Error handeling or token: {:?}", token)
        };

        Ok(literal)
    }

    pub fn parse_expression(&mut self, binding_power: u8, provided_lhs: Option<Expression>) -> ParseResult<Expression> {
        let mut lhs = match provided_lhs {
            Some(e) => e,
            None => {
                match self.peek() {
                    lit @ TokenKind::Literal
                        | lit @ TokenKind::StringLiteral
                        | lit @ TokenKind::Identifier
                        | lit @ TokenKind::True
                        | lit @ TokenKind::False => self.parse_literal(lit)?,
                    _ => {
                        let token = self.next_token()?;
                        return Err(SyntaxError::UnexpectedToken {
                            expected: "expression".to_string(),
                            found: token,
                        });
                    }
                }
            }
        }; 

        if self.at(TokenKind::LeftBracket) {
            return self.parse_struct_init(lhs);
        }

        if self.at(TokenKind::LeftParen) {
            return self.parse_function_call(lhs);
        }

        loop {
             let op = match self.peek() {
                op @ TokenKind::Add
                | op @ TokenKind::Sub
                | op @ TokenKind::Mul
                | op @ TokenKind::Div
                | op @ TokenKind::And
                | op @ TokenKind::Or
                | op @ TokenKind::Less
                | op @ TokenKind::Greater
                | op @ TokenKind::Not
                | op @ TokenKind::LessEquals
                | op @ TokenKind::GreaterEquals
                | op @ TokenKind::NotEquals
                | op @ TokenKind::Equals => op,

                TokenKind::RightParen
                | TokenKind::Let
                | TokenKind::Comma
                | TokenKind::RightBracket
                | TokenKind::RightCurly
                | TokenKind::LeftCurly
                | TokenKind::Semicolon
                | TokenKind::Eof => break,

                _ => {
                    let token = self.next_token()?;
                    return Err(SyntaxError::UnexpectedToken {
                        expected: "operator or expression terminator".to_string(),
                        found: token,
                });
            }
            };

              if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                let op_token = self.consume_next(op)?;

                lhs = Expression::Unary(BinaryOp::from_token_kind(&op), Box::new(lhs));
                // parsed an operator --> go round the loop again
                continue;
            }

           if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                self.consume(op)?;

                let rhs = self.parse_expression(right_binding_power, None)?;
                lhs = Expression::Binary(BinaryOp::from_token_kind(&op), Box::new(lhs), Box::new(rhs));

                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    pub fn parse_struct_init(&mut self, lhs: Expression) -> ParseResult<Expression> {
        self.consume(TokenKind::LeftBracket)?;
        let mut arguments = Vec::<NamedArgument>::new();
        while !self.at(TokenKind::RightBracket) {
            let name = self.consume_next(TokenKind::Identifier)?.value.clone();
            self.consume(TokenKind::Colon)?;

            let expr = self.parse_expression(0, None)?; 
            let argument = NamedArgument::new(name, expr);

            arguments.push(argument);
            
            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::RightBracket)?;
        let expr = Expression::StructInit(lhs.as_identifier(), arguments);  
        Ok(expr)
    }

    pub fn parse_function_call(&mut self, lhs: Expression) -> ParseResult<Expression> {
        self.consume(TokenKind::RightParen); 
        let mut arguments = Vec::<NamedArgument>::new();
        while !self.at(TokenKind::RightParen) {
            let name = self.consume_next(TokenKind::Identifier)?.value.clone();
            self.consume(TokenKind::Colon)?;

            let expr = self.parse_expression(0, None)?; 
            let argument = NamedArgument::new(name, expr);

            arguments.push(argument);

            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::LeftParen);

        let name = lhs.as_identifier();

        let expr = Expression::Call(name, arguments);

        Ok(expr)
    }

    pub fn parse_block_body(&mut self) -> ParseResult<Vec<Statement>> {
        self.consume(TokenKind::LeftCurly);
        let mut body = Vec::<Statement>::new();

        while !self.at(TokenKind::RightCurly) {
            if self.multi_at(&[
                TokenKind::Identifier, 
                TokenKind::If, 
                TokenKind::Let, 
                TokenKind::Function, 
                TokenKind::While,
                TokenKind::LeftCurly,
            ]) {
                body.push(self.parse_statement()?);  
            } else {
                let expr = self.parse_expression(0, None)?;
                let stmt = Statement::Expression(expr);
                body.push(stmt);
            }

            if !self.at(TokenKind::Semicolon) {
                break;
            }

            self.consume(TokenKind::Semicolon)?;
        }

        self.consume(TokenKind::RightCurly)?;

        Ok(body)
    }
    
    pub fn parse_block(&mut self) -> ParseResult<Statement> {
        let body = self.parse_block_body()?;    
        let stmt = Statement::Block(body);

        Ok(stmt)
    }

    pub fn parse_ident_begin(&mut self) -> ParseResult<Statement> {
        let ident_value = self.next_token().unwrap().value.clone();
        let ident = Expression::Identifier(ident_value.clone());

        let stmt = match self.peek() {
            TokenKind::LeftParen => {
                let expr = self.parse_function_call(ident)?;
                let stmt = Statement::Expression(expr);
                stmt
            }
            _ => {
                let expr = self.parse_expression(0, Some(ident))?;
                let stmt = Statement::Expression(expr);
                stmt
            }
        };

        Ok(stmt)
    }

    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::Let)?;
        let ident = self.next_token().unwrap().value.clone();
        let mut expr = Expression::Empty;

        if self.at(TokenKind::Assignment) {
            self.consume(TokenKind::Assignment)?;
            expr = self.parse_expression(0, None)?;
        }

        return Ok(Statement::Declaration(ident, expr));
    }

    pub fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::If)?; 
        let cond = self.parse_expression(0, None)?;
        let if_block = self.parse_block_body()?;
        let mut else_block: Option<Vec<Statement>> = None;

        if self.peek() == TokenKind::Else {
            self.consume(TokenKind::Else)?;
            let block = self.parse_block_body()?;
            else_block = Some(block);
        }

        let stmt = Statement::If(cond, if_block, else_block);

        return Ok(stmt);
    }

    pub fn parse_while_loop(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::While)?;
        
        let expr = self.parse_expression(0, None)?;
        
        let body = self.parse_block_body()?;
        
        let stmt = Statement::While(expr, body);

        Ok(stmt)
    }

    pub fn parse_definition(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::Struct)?;
        let struct_name = self.consume_next(TokenKind::Identifier)?.value.clone();
        let mut fields = Vec::<FieldDefinition>::new();
        self.consume(TokenKind::LeftCurly)?;

        while !self.at(TokenKind::RightCurly) {
            let field_name = self.consume_next(TokenKind::Identifier)?.value.clone();  
            self.consume(TokenKind::Colon);
            let field_type = self.consume_next(TokenKind::Identifier)?.value.clone();
            let defined_type = DefinedType::Name(field_type);

            let field = FieldDefinition::new(field_name, defined_type);
            fields.push(field);
        }

        self.consume(TokenKind::RightCurly)?;

        let struct_definition = StructDefinition::new(struct_name, fields);
        let definition = Definition::Struct(struct_definition);
        let stmt = Statement::Define(definition);

        Ok(stmt)
    }

    pub fn parse_defined_type(&mut self) -> ParseResult<DefinedType> {
        match self.peek() {
            TokenKind::Identifier => {
                let name = self.consume_next(TokenKind::Identifier)?.value.clone();         
                Ok(DefinedType::Name(name))
            }   
            o => todo!("Parse defined type for: {:?}", o)
        } 
    }

    pub fn parse_function(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::Function)?; 
        let name = self.consume_next(TokenKind::Identifier)?.value.clone();
        let mut parameters = Vec::<NamedParameter>::new();
        self.consume(TokenKind::LeftParen)?;

        while !self.at(TokenKind::RightParen) {
            let name = self.consume_next(TokenKind::Identifier)?.value.clone();
            self.consume(TokenKind::Colon)?;

            let defined_type = self.parse_defined_type()?; 
            let parameter = NamedParameter::new(name, defined_type);

            parameters.push(parameter);

            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }

        self.consume(TokenKind::RightParen)?;

        let mut type_name = String::from("Void");

        if self.peek() == TokenKind::Arrow {
            self.consume(TokenKind::Arrow)?;  
            let identifier = self.consume_next(TokenKind::Identifier)?;     
            type_name = identifier.value.clone();
        }

        let mut body: Vec<Statement> = vec![];

        if self.peek() == TokenKind::Semicolon {
            self.consume(TokenKind::Semicolon)?;   
        } else {
            body = self.parse_block_body()?;
        }
        
        let function = Statement::Function(
            name, 
            parameters, 
            body, 
            DefinedType::name(type_name)
        );

        Ok(function)
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            TokenKind::Identifier => self.parse_ident_begin(),
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::LeftCurly => self.parse_block(),
            TokenKind::Struct => self.parse_definition(),
            TokenKind::Function => self.parse_function(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_loop(),
            TokenKind::Literal => {
                let expr = self.parse_expression(0, None)?;
                let stmt = Statement::Expression(expr);
                Ok(stmt)
            }
            TokenKind::Eof => Err(SyntaxError::End),
            token => todo!("Token: {:?} not implemented yet.", token)
        }
    }
}
