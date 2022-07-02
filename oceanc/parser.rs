use crate::ast::{
    BinaryOp, 
    expressions::{
        Expression,
        NamedArgument,
    }, 
    statements::{
        Statement,
        NamedParameter,
        IfStatement,
    }, 
    definitions::{
        StructDefinition, 
        Definition, 
        FieldDefinition, 
        DefinedType
    }
};
use crate::lexer::{Lexer, Token, TokenKind, Span};
use crate::errors::{OceanError, Level, Step};
use crate::unescape::{unescape};
use std::iter::Peekable;

type SyntaxResult<T> = Result<T, OceanError>;
type ParseResult<T> = Result<T, OceanError>;

pub struct Parser {
    lexer: Peekable<Lexer>,
    file_name: String,
    input: String,

    pub ended: bool,
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
    pub fn new(input: String, file_name: String) -> Self {
        Self {
            lexer: Lexer::new(input.clone(), file_name.clone()).peekable(),
            file_name: file_name.clone(),
            input: input.clone(),

            ended: false,
        } 
    }

    pub fn next_token(&mut self) -> SyntaxResult<Token> {
        self.lexer.next().ok_or_else(|| {
            let span = Span {
                file_name: self.file_name.clone(),
                row: self.input.len(),
                col: self.input.len(),
            };

            OceanError::new(
                Level::Error, 
                Step::Parsing, 
                span, 
                "Unexpected end of input.".into()
            )
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
            Err(OceanError::new(
                Level::Error,
                Step::Parsing,
                token.span,
                format!(
                    "Unexpected token: \x1b[1m{}\x1b[0m. Expected token: \x1b[1m{}\x1b[0m", 
                    token.kind.to_string(), 
                    expected.to_string()
                ),
            ))
        } else {
            Ok(())
        }
    }

    pub fn consume_next(&mut self, expected: TokenKind) -> SyntaxResult<Token> {
        let token = self.next_token()?;
        if token.kind != expected {
            Err(OceanError::new(
                Level::Error,
                Step::Parsing,
                token.span,
                format!(
                    "Unexpected token: \x1b[1m{}\x1b[0m. Expected token: \x1b[1m{}\x1b[0m", 
                    token.kind.to_string(), 
                    expected.to_string()
                ),
            ))

        } else {
            Ok(token)
        }
    }

    pub fn parse_identifier(&mut self, lhs: Token) -> ParseResult<Expression> {
        if self.peek() == TokenKind::Dot {
            self.consume(TokenKind::Dot)?;
            let rhs = self.consume_next(TokenKind::Identifier)?;
            Ok(Expression::Lookup(lhs.span.clone(), lhs.value.clone(), rhs.value.clone()))
        } else {
            Ok(Expression::Identifier(lhs.span.clone(), lhs.value.clone()))
        }
    }

    pub fn parse_literal(&mut self, lit: TokenKind) -> ParseResult<Expression> {
        let token = self.next_token()?;
        
        let literal = match lit {
            TokenKind::Literal => Expression::Literal(token.span, token.value.parse::<u64>().unwrap()), 
            TokenKind::True => Expression::Bool(token.span, true),
            TokenKind::False => Expression::Bool(token.span, false),
            TokenKind::Identifier => self.parse_identifier(token.clone())?,
            TokenKind::StringLiteral => { 
                let value = token.value.clone();
                let unescaped = unescape(&value).unwrap();
                Expression::StringLiteral(token.span, unescaped)
            },
            token => unimplemented!("Error handeling or token: {:?}", token)
        };

        Ok(literal)
    }

    pub fn parse_array(&mut self) -> ParseResult<Expression> {
        let start = self.consume_next(TokenKind::LeftBracket)?;  
        let mut arguments = Vec::<Expression>::new();

        while self.peek() != TokenKind::RightBracket {
            let expr = self.parse_expression(0, None)?; 
            arguments.push(expr);
            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }

        self.consume(TokenKind::RightBracket)?;

        let expr = Expression::ArrayInit(start.span, arguments);
        Ok(expr)
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
                    TokenKind::LeftBracket => {
                        self.parse_array()?
                    }
                    _ => {
                        let token = self.next_token()?;
                        return Err(
                            OceanError::new(
                                Level::Error, 
                                Step::Parsing, 
                                token.span, 
                                format!("Unexpected token: \x1b[1m{}\x1b[0m. Expected \x1b[1mExpression\x1b[0m.", token.kind.to_string())
                            )
                        );
                    }
                }
            }
        }; 

        if self.at(TokenKind::LeftBracket) {
            return self.parse_left_bracket_begin(lhs);
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
                    return Err(
                        OceanError::new(
                            Level::Error, 
                            Step::Parsing, 
                            token.span, 
                            format!(
                                "Expected \x1b[1moperator\x1b[0m or \x1b[1mexpression\x1b[0m terminater but found: \x1b[1m{}\x1b[0m.", 
                                token.kind.to_string()
                            )
                        )
                    )
                }
            };

              if let Some((left_binding_power, ())) = op.postfix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power then new one
                    // --> end of expression
                    break;
                }

                let op_token = self.consume_next(op)?;

                lhs = Expression::Unary(op_token.span.clone(), BinaryOp::from_token_kind(&op), Box::new(lhs));
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
                lhs = Expression::Binary(lhs.span(), BinaryOp::from_token_kind(&op), Box::new(lhs), Box::new(rhs));

                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    pub fn parse_left_bracket_begin(
        &mut self,
        lhs: Expression,
    ) -> ParseResult<Expression> {
        let start = self.consume_next(TokenKind::LeftBracket)?; 

        match self.peek() {
            TokenKind::Literal => self.parse_array_index(lhs, start.span),
            TokenKind::Identifier => self.parse_struct_init(lhs, start.span),
            _ => todo!("Error")
        }
    }

    pub fn parse_array_index(
        &mut self,
        lhs: Expression,
        start: Span,
    ) -> ParseResult<Expression> {
        let index = self.consume_next(TokenKind::Literal)?
            .value.parse::<u64>()
            .unwrap(); 

        self.consume(TokenKind::RightBracket)?;

        let expr = Expression::ArrayIndex(start.clone(), lhs.as_identifier(), index);

        Ok(expr)
    }

    pub fn parse_struct_init(
        &mut self, 
        lhs: Expression, 
        start: Span
    ) -> ParseResult<Expression> {
        let mut arguments = Vec::<NamedArgument>::new();
        while !self.at(TokenKind::RightBracket) {
            let name_token = self.consume_next(TokenKind::Identifier)?;
            let name = name_token.value.clone();
            self.consume(TokenKind::Colon)?;

            let expr = self.parse_expression(0, None)?; 
            let argument = NamedArgument::new(name, expr, name_token.span);

            arguments.push(argument);
            
            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::RightBracket)?;
        let expr = Expression::StructInit(start.clone(), lhs.as_identifier(), arguments);  
        Ok(expr)
    }

    pub fn parse_function_call(&mut self, lhs: Expression) -> ParseResult<Expression> {
        let start = self.consume_next(TokenKind::LeftParen)?; 
        let mut arguments = Vec::<NamedArgument>::new();
        while !self.at(TokenKind::RightParen) {
            let name_token = self.consume_next(TokenKind::Identifier)?;
            let name = name_token.value.clone();
            self.consume(TokenKind::Colon)?;

            let expr = self.parse_expression(0, None)?; 
            let argument = NamedArgument::new(name, expr, name_token.span);

            arguments.push(argument);

            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }
        self.consume(TokenKind::LeftParen);

        let name = lhs.as_identifier();

        let expr = Expression::Call(start.span, name, arguments);

        Ok(expr)
    }

    pub fn parse_block_body(&mut self) -> ParseResult<(Span, Vec<Statement>)> {
        let start = self.consume_next(TokenKind::LeftCurly)?;
        let mut body = Vec::<Statement>::new();

        while !self.at(TokenKind::RightCurly) {
            if self.multi_at(&[
                TokenKind::If, 
                TokenKind::Let, 
                TokenKind::Function, 
                TokenKind::While,
                TokenKind::LeftCurly,
                TokenKind::Return,
                TokenKind::Extern,
            ]) {
                body.push(self.parse_statement()?);  
            } else {
                let expr = self.parse_expression(0, None)?;
                let stmt = Statement::Expression(expr.span(), expr);
                body.push(stmt);
            }

            if !self.at(TokenKind::Semicolon) {
                break;
            }

            self.consume(TokenKind::Semicolon)?;
        }

        self.consume(TokenKind::RightCurly)?;

        Ok((start.span.clone(), body))
    }
    
    pub fn parse_block(&mut self) -> ParseResult<Statement> {
        let (span, body) = self.parse_block_body()?;    
        let stmt = Statement::Block(span, body);

        Ok(stmt)
    }

    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::Let)?;
        let ident_token = self.consume_next(TokenKind::Identifier)?;
        let ident = ident_token.value.clone();
        let mut expr = Expression::Empty(ident_token.span);
        let mut is_referenced = false;

        if self.at(TokenKind::Assignment) {
            self.consume(TokenKind::Assignment)?;

            if self.peek() == TokenKind::Ampersand {
               self.consume(TokenKind::Ampersand); 
               is_referenced = true;
            }
            expr = self.parse_expression(0, None)?;
        }

        return Ok(Statement::Declaration(start.span.clone(), ident, expr, is_referenced));
    }

    pub fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::If)?; 
        let cond = self.parse_expression(0, None)?;
        let (if_span, if_block) = self.parse_block_body()?;

        let mut else_block: Option<Vec<Statement>> = None;
        let mut else_span: Option<Span> = None;

        if self.peek() == TokenKind::Else {
            self.consume(TokenKind::Else)?;
            let (span, block) = self.parse_block_body()?;
            else_block = Some(block);
            else_span = Some(span); 
        }

        let x = IfStatement::new(
            cond, 
            if_block, 
            if_span, 
            else_block, 
            else_span
        );

        let stmt = Statement::If(start.span.clone(), x);

        return Ok(stmt);
    }

    pub fn parse_while_loop(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::While)?;
        
        let expr = self.parse_expression(0, None)?;
        
        let (span, body) = self.parse_block_body()?;
        
        let stmt = Statement::While(start.span.clone(), expr, body, span);

        Ok(stmt)
    }

    pub fn parse_definition(&mut self) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::Struct)?;
        let struct_name = self.consume_next(TokenKind::Identifier)?.value.clone();
        let mut fields = Vec::<FieldDefinition>::new();
        self.consume(TokenKind::LeftCurly)?;

        while !self.at(TokenKind::RightCurly) {
            let field_name = self.consume_next(TokenKind::Identifier)?.value.clone();  
            self.consume(TokenKind::Colon);

            let mut defined_type: Option<DefinedType> = None;
            let field_type = self.consume_next(TokenKind::Identifier)?.value.clone();
             

            if self.peek() == TokenKind::Less {
                self.consume(TokenKind::Less)?; 
                let has_field = self.consume_next(TokenKind::Identifier)?.value.clone();
                self.consume(TokenKind::Greater)?;

                defined_type = Some(DefinedType::Array(Box::new(DefinedType::name(has_field))));
            } else {
                defined_type = Some(DefinedType::Name(field_type));   
            } 

            let field = FieldDefinition::new(field_name, defined_type.unwrap());
            fields.push(field);
        }

        self.consume(TokenKind::RightCurly)?;

        let struct_definition = StructDefinition::new(struct_name, fields);
        let definition = Definition::Struct(struct_definition);
        let stmt = Statement::Define(start.span, definition);

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

    pub fn parse_return(&mut self) -> ParseResult<Statement> {
        self.consume(TokenKind::Return)?; 
        let expr = self.parse_expression(0, None)?;

        Ok(Statement::Return(expr.span(), expr))
    }

    pub fn parse_function(&mut self, external: bool) -> ParseResult<Statement> {
        let start = self.consume_next(TokenKind::Function)?; 
        let name = self.consume_next(TokenKind::Identifier)?.value.clone();
        let mut parameters = Vec::<NamedParameter>::new();
        self.consume(TokenKind::LeftParen)?;

        while !self.at(TokenKind::RightParen) {
            let name_token = self.consume_next(TokenKind::Identifier)?;
            let name = name_token.value.clone();
            self.consume(TokenKind::Colon)?;

            let defined_type = self.parse_defined_type()?; 
            let parameter = NamedParameter::new(name, defined_type, name_token.span);

            parameters.push(parameter);

            if !self.at(TokenKind::Comma) {
                break;
            } else {
                self.consume(TokenKind::Comma)?;
            }
        }

        let r_paren = self.consume_next(TokenKind::RightParen)?;

        let mut returning = DefinedType::Empty;

        if self.peek() == TokenKind::Arrow {
            self.consume(TokenKind::Arrow)?;  
            returning = self.parse_defined_type()?;
        }

        let mut body: Vec<Statement> = vec![];

        if self.peek() == TokenKind::Semicolon {
            self.consume(TokenKind::Semicolon)?;   
        } else if self.peek() == TokenKind::LeftCurly {
            (_, body) = self.parse_block_body()?;
        } else {
            return Err(
                OceanError::new(
                    Level::Error,
                    Step::Parsing,
                    r_paren.span,
                    format!(
                        "Unexpected token: \x1b[1m{}\x1b[0m. Expected either token: \"\x1b[1m{}\x1b[0m\" or: \"\x1b[1m{}\x1b[0m\"", 
                        self.peek().to_string(), 
                        TokenKind::Semicolon.to_string(),
                        TokenKind::LeftCurly.to_string(),
                    ),
                )   
            );           
        }
        
        let function = Statement::Function(
            start.span.clone(),
            name, 
            parameters, 
            body, 
            returning,
            external
        );

        Ok(function)
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::LeftCurly => self.parse_block(),
            TokenKind::Struct => self.parse_definition(),
            TokenKind::Function => self.parse_function(false),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_loop(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Extern => {
                self.consume(TokenKind::Extern);
                self.parse_function(true)
            }
            TokenKind::Literal => {
                let expr = self.parse_expression(0, None)?;
                let stmt = Statement::Expression(expr.span(), expr);
                Ok(stmt)
            }
            TokenKind::Eof => { 
                let token = self.consume_next(TokenKind::Eof)?;
                self.ended = true;
                Err(OceanError::new(Level::Ignore, Step::Parsing, token.span, "EofENDEof".into()))
            }
            token => todo!("Token: {:?} not implemented yet.", token)
        }
    }
}
