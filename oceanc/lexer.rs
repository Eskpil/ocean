use std::collections::HashMap;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Literal,
    Identifier,
    StringLiteral,

    Assignment,
    Let,

    Do,
    End,

    Add,
    Sub,
    Div,
    Mul,

    LeftParen,
    RightParen,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,

    pub row: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct Lexer {
    pub source: String,
    pub start: usize,
    pub current: usize,
    pub keywords: HashMap<String, TokenKind>,

    pub row: usize,
    pub col: usize,
}

impl TokenKind {
    pub fn to_string(&self) -> String {
        match *self {
            TokenKind::Literal => "literal".into(),
            TokenKind::Identifier => "identifier".into(),
            TokenKind::StringLiteral => "string literal".into(),

            TokenKind::Assignment => "assignment".into(),
            TokenKind::Let => "let".into(),

            TokenKind::Do => "do".into(),
            TokenKind::End => "end".into(),

            TokenKind::Add => "+".into(),
            TokenKind::Sub => "-".into(),
            TokenKind::Div => "/".into(),
            TokenKind::Mul => "*".into(),

            TokenKind::RightParen => ")".into(),
            TokenKind::LeftParen => "(".into(),
        }
    }
}

impl Token {
    pub fn kind(kind: TokenKind) -> Token {
        Token {
            kind,
            value: kind.to_string(),
            col: 0,
            row: 0,
        }
    }

    pub fn kind_loc(kind: TokenKind, row: usize, col: usize) -> Token {
        Token {
            kind,
            value: kind.to_string(),
            row,
            col,
        }
    }

    pub fn value(kind: TokenKind, value: impl Into<String>) -> Token {
        Token {
            kind,
            value: value.into(),
            col: 0,
            row: 0,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        /* For now this is good enough but we might shoot ourselfs in the foot with it later. */
        self.kind == other.kind && self.value == other.value
    }
}

impl Lexer {
    pub fn new(source: String) -> Self {
        let mut keywords = HashMap::new();

        keywords.insert("let".into(), TokenKind::Let);
        keywords.insert("do".into(), TokenKind::Do);
        keywords.insert("end".into(), TokenKind::End);

        Self {
            source: source.into(),
            start: 0,
            current: 0,
            keywords,

            col: 1,
            row: 1,
        }
    }

    pub fn next(&mut self) -> Token {
        let c = self.peek();
        self.start = self.current;

        let token = match c {
            '\n' => {
                self.row += 1;
                self.col = 0;
                self.advance();
                self.next()
            }
            ')' => {
                let token = Token::kind_loc(TokenKind::RightParen, self.row, self.col);
                self.advance();
                token
            }
            '(' => {
                let token = Token::kind_loc(TokenKind::LeftParen, self.row, self.col);
                self.advance();
                token
            }
            '=' => {
                let token = Token::kind_loc(TokenKind::Assignment, self.row, self.col);
                self.advance();
                token
            }
            '+' => {
                let token = Token::kind_loc(TokenKind::Add, self.row, self.col);
                self.advance();
                token
            }
            '-' => {
                let token = Token::kind_loc(TokenKind::Sub, self.row, self.col);
                self.advance();
                token
            }
            '/' => {
                let token = Token::kind_loc(TokenKind::Div, self.row, self.col);
                self.advance();
                token
            }
            '*' => {
                let token = Token::kind_loc(TokenKind::Mul, self.row, self.col);
                self.advance();
                token
            }
            ' ' => {
                self.advance();
                self.next()
            }
            '\t' => {
                self.advance();
                self.next()
            }
            '"' => self.string(),
            _ => {
                if c.is_digit(10) {
                    return self.numeric();
                }

                self.identifier()
            }
        };

        token
    }

    pub fn numeric(&mut self) -> Token {
        let col = self.col;
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let value = &self.source[self.start..self.current];

        let token = Token {
            kind: TokenKind::Literal,
            value: value.into(),
            row: self.row,
            col,
        };

        token
    }

    pub fn identifier(&mut self) -> Token {
        let col = self.col;
        while self.peek().is_alphabetic() || self.peek().is_digit(10) {
            self.advance();
        }

        let value = &self.source[self.start..self.current];

        let mut token = Token {
            kind: TokenKind::Identifier,
            value: value.into(),
            row: self.row,
            col,
        };

        match self.keywords.get(value.into()) {
            Option::Some(kind) => {
                token.kind = kind.clone();
            }
            Option::None => {}
        }

        token
    }

    pub fn string(&mut self) -> Token {
        let col = self.col;

        /* Advance to consume the first " in the literal. */
        self.advance();

        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        /* Advance to consume the last " in the literal */

        self.advance();

        let value = &self.source[(self.start + 1)..(self.current - 1)];

        Token {
            kind: TokenKind::StringLiteral,
            value: value.into(),
            row: self.row,
            col,
        }
    }

    pub fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.current += 1;
        self.col += 1;

        let val = self.source.chars().nth(self.current - 1);

        return val.unwrap();
    }

    pub fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    pub fn peek_next(&self) -> char {
        return self.source.chars().nth(self.current + 1).unwrap();
    }

    pub fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        return self.source.chars().nth(self.current).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token, TokenKind};

    #[test]
    fn number() {
        let mut lexer = Lexer::new("123".into());

        assert_eq!(lexer.next(), Token::value(TokenKind::Literal, "123"));
    }

    #[test]
    fn float() {
        let mut lexer = Lexer::new("69.420".into());

        assert_eq!(lexer.next(), Token::value(TokenKind::Literal, "69.420"));
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("test".into());

        assert_eq!(lexer.next(), Token::value(TokenKind::Identifier, "test"));
    }

    #[test]
    fn keyword_let() {
        let mut lexer = Lexer::new("let".into());

        assert_eq!(lexer.next(), Token::kind(TokenKind::Let));
    }

    #[test]
    fn add() {
        let mut lexer = Lexer::new("+".into());

        assert_eq!(lexer.next(), Token::kind(TokenKind::Add));
    }

    #[test]
    fn sub() {
        let mut lexer = Lexer::new("-".into());

        assert_eq!(lexer.next(), Token::kind(TokenKind::Sub));
    }

    #[test]
    fn div() {
        let mut lexer = Lexer::new("/".into());

        assert_eq!(lexer.next(), Token::kind(TokenKind::Div));
    }

    #[test]
    fn mul() {
        let mut lexer = Lexer::new("*".into());

        assert_eq!(lexer.next(), Token::kind(TokenKind::Mul));
    }

    #[test]
    fn expr() {
        let mut lexer = Lexer::new("3+4".into());

        let first = lexer.next();
        let second = lexer.next();
        let third = lexer.next();

        assert_eq!(first, Token::value(TokenKind::Literal, "3"));
        assert_eq!(second, Token::kind(TokenKind::Add));
        assert_eq!(third, Token::value(TokenKind::Literal, "4"));
    }

    #[test]
    fn whitespace() {
        let mut lexer = Lexer::new("3  + 4".into());

        let first = lexer.next();
        let second = lexer.next();
        let third = lexer.next();

        assert_eq!(first, Token::value(TokenKind::Literal, "3"));
        assert_eq!(second, Token::kind(TokenKind::Add));
        assert_eq!(third, Token::value(TokenKind::Literal, "4"));
    }

    #[test]
    fn newline() {
        let mut lexer = Lexer::new("3 + \n4".into());

        let first = lexer.next();
        let second = lexer.next();
        let third = lexer.next();

        assert_eq!(first, Token::value(TokenKind::Literal, "3"));
        assert_eq!(second, Token::kind(TokenKind::Add));
        assert_eq!(third, Token::value(TokenKind::Literal, "4"));
    }

    #[test]
    fn assignment() {
        let mut lexer = Lexer::new("let a = 3 + 4".into());

        let first = lexer.next();
        let second = lexer.next();
        let third = lexer.next();
        let fourth = lexer.next();
        let fifth = lexer.next();
        let sixth = lexer.next();

        assert_eq!(first, Token::kind(TokenKind::Let));
        assert_eq!(second, Token::value(TokenKind::Identifier, "a"));
        assert_eq!(third, Token::kind(TokenKind::Assignment));
        assert_eq!(fourth, Token::value(TokenKind::Literal, "3"));
        assert_eq!(fifth, Token::kind(TokenKind::Add));
        assert_eq!(sixth, Token::value(TokenKind::Literal, "4"));
    }

    #[test]
    fn string_literal() {
        let mut lexer = Lexer::new("\"hello\"".into());

        assert_eq!(
            lexer.next(),
            Token::value(TokenKind::StringLiteral, "hello")
        );
    }
}
