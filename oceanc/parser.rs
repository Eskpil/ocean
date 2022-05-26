use crate::ast::{BinaryOp, expressions::{Expression}, statements::{Statement}};
use crate::lexer::{Lexer, Token, TokenKind};
use crate::errors::syntax::SyntaxError;
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
            token => unimplemented!("Error handeling or token: {:?}", token)
        };

        Ok(literal)
    }

    pub fn parse_expression(&mut self, binding_power: u8) -> ParseResult<Expression> {
        let mut lhs = match self.peek() {
            lit @ TokenKind::Literal
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
        };

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
                | TokenKind::End
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

                let rhs = self.parse_expression(right_binding_power)?;
                lhs = Expression::Binary(BinaryOp::from_token_kind(&op), Box::new(lhs), Box::new(rhs));

                // parsed an operator --> go round the loop again
                continue;
            }
            break;
        }

        Ok(lhs)
    }

    pub fn parse_function_call(&mut self, lhs: Expression) -> ParseResult<Expression> {
        self.consume(TokenKind::RightParen); 
        self.consume(TokenKind::LeftParen);

        let name = lhs.as_identifier();

        let expr = Expression::Call(name);

        Ok(expr)
    }

    pub fn parse_block_body(&mut self) -> ParseResult<Vec<Statement>> {
        // Consume do keyword.
        self.next_token();                     
        let mut body = Vec::<Statement>::new();

        while !self.at(TokenKind::End) {
            if self.multi_at(&[TokenKind::Identifier, TokenKind::Let, TokenKind::Do]) {
                body.push(self.parse_statement()?);  
            } else {
                let expr = self.parse_expression(0)?;
                let stmt = Statement::Expression(expr);
                body.push(stmt);
            }

            if !self.at(TokenKind::Semicolon) {
                break;
            }
            self.consume(TokenKind::Semicolon)?;
        }

        self.consume_next(TokenKind::End)?;

        Ok(body)
    }
    
    pub fn parse_block(&mut self) -> ParseResult<Statement> {
        let body = self.parse_block_body()?;    
        let stmt = Statement::Block(body);

        Ok(stmt)
    }

    pub fn parse_ident_begin(&mut self) -> ParseResult<Statement> {
        let ident = self.next_token().unwrap();  

        let stmt = match self.peek() {
            TokenKind::Do => {
                let block = self.parse_block_body()?;         
                let stmt = Statement::Function(ident.value.clone(), block);
                stmt
            } 
            TokenKind::LeftParen => {
                let ident_expr = Expression::Identifier(ident.value.clone());
                let expr = self.parse_function_call(ident_expr)?;
                let stmt = Statement::Expression(expr);
                stmt
            }
            token => unimplemented!("Token: {:?} is not implemented yet.", token)
        };

        Ok(stmt)
    }

    pub fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        self.next_token();
        let ident = self.next_token().unwrap().value.clone();
        let mut expr = Expression::Empty;

        if self.at(TokenKind::Assignment) {
            self.consume(TokenKind::Assignment)?;
            expr = self.parse_expression(0)?;
        }

        return Ok(Statement::Declaration(ident, expr));
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.peek() {
            TokenKind::Identifier => self.parse_ident_begin(),
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Do => self.parse_block(),
            TokenKind::Eof => Err(SyntaxError::End),
            token => todo!("Token: {:?} not implemented yet.", token)
        }
    }
}
