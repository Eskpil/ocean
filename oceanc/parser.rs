use crate::ast::expressions::{
    BinaryExpression, DoubleLiteral, EmptyExpression, Expression, Identifier,
    CallExpression,
};
use crate::ast::statements::{
    BlockStatement, DeclarationStatement, ExpressionStatement, FunctionStatement, Program,
    Statement,
};
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser {
    lexer: Lexer,

    current: Token,
    idx: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let mut lexer = Lexer::new(source);
        let mut tokens = vec![];

        while !lexer.is_at_end() {
            tokens.push(lexer.next());
        }

        Self {
            lexer,

            current: Token::kind(TokenKind::Add),
            tokens,
            idx: 0,
        }
    }

    pub fn next_token(&mut self) {
        self.idx += 1;
        self.current = self.tokens.iter().nth(self.idx - 1).unwrap().clone();
    }

    pub fn parse_expression(&mut self) -> Box<dyn Expression> {
        if self.current.kind == TokenKind::Literal {
            let lhs = Box::new(DoubleLiteral::new(
                self.current.value.parse::<f64>().unwrap(),
            ));
            self.next_token();
            self.next_token();

            if self.current.kind == TokenKind::Literal {
                let rhs = Box::new(DoubleLiteral::new(
                    self.current.value.parse::<f64>().unwrap(),
                ));

                let expr = BinaryExpression::new(lhs, rhs);

                return Box::new(expr);
            } else {
                let rhs = Box::new(Identifier::new(self.current.value.clone()));

                let expr = BinaryExpression::new(lhs, rhs);

                return Box::new(expr);
            }
        }

        if self.current.kind == TokenKind::Identifier {
            let lhs = Identifier::new(self.current.value.clone());
            self.next_token();

            if self.current.kind == TokenKind::LeftParen {
                self.next_token();
                if self.current.kind == TokenKind::RightParen {
                    let expr = CallExpression::new(lhs);
                    return Box::new(expr);
                }
            }

            self.next_token();

            if self.current.kind == TokenKind::Literal {
                let rhs = Box::new(DoubleLiteral::new(
                    self.current.value.parse::<f64>().unwrap(),
                ));

                let expr = BinaryExpression::new(Box::new(lhs), rhs);

                return Box::new(expr);
            } else {
                let rhs = Box::new(Identifier::new(self.current.value.clone()));

                let expr = BinaryExpression::new(Box::new(lhs), rhs);

                return Box::new(expr);
            }
        }

        return Box::new(BinaryExpression::new(
            Box::new(DoubleLiteral::new(1.0)),
            Box::new(DoubleLiteral::new(2.0)),
        ));
    }

    pub fn parse_statement(&mut self, should_consume: bool) -> Box<dyn Statement> {
        if self.current.kind == TokenKind::Literal {
            let stmt = ExpressionStatement::new(self.parse_expression());

            if should_consume {
                self.next_token();
            }

            return Box::new(stmt);
        }

        if self.current.kind == TokenKind::Let {
            self.next_token(); // Consume Let token.
            let identifier = Identifier::new(self.current.value.clone());
            self.next_token(); // Consume Identifier as we are finished with it.

            if self.current.kind == TokenKind::Assignment {
                self.next_token(); // Consume Assignment token.
                let expr = self.parse_expression();

                if should_consume {
                    self.next_token();
                }

                let stmt = DeclarationStatement::new(identifier, expr);
                return Box::new(stmt);
            } else {
                let expr = EmptyExpression::new();
                let stmt = ExpressionStatement::new(Box::new(expr));
                return Box::new(stmt);
            }
        }

        if self.current.kind == TokenKind::Do {
            let mut block = BlockStatement::new();

            while self.current.kind != TokenKind::End {
                self.next_token();
                block.append(self.parse_statement(false));
            }

            self.next_token();

            return Box::new(block);
        }

        if self.current.kind == TokenKind::Identifier {
            let name = self.current.value.clone();
            self.next_token();

            if self.current.kind == TokenKind::Do {
                let mut function = FunctionStatement::new(Identifier::new(name));

                while self.current.kind != TokenKind::End {
                    self.next_token();
                    function.append(self.parse_statement(false));
                }

                self.next_token();

                return Box::new(function);
            }

            if self.current.kind == TokenKind::LeftParen {
                self.next_token(); // Consume LeftParen
                if self.current.kind == TokenKind::RightParen {
                    self.next_token(); // Consume RightParen
                    let expr = CallExpression::new(Identifier::new(name));  
                    let stmt = ExpressionStatement::new(Box::new(expr));

                    return Box::new(stmt);
                }
            }
        }

        let expr = EmptyExpression::new();
        let stmt = ExpressionStatement::new(Box::new(expr));

        Box::new(stmt)
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();

        self.next_token();

        while (self.tokens.len() - 1) >= self.idx {
            program.append(self.parse_statement(true));
        }

        program
    }
}
