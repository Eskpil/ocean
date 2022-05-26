use crate::lexer::Token;

#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken {
        expected: String,
        found: Token,
    }, 
    UnexpectedEndOfInput(Token),
    End,
}
