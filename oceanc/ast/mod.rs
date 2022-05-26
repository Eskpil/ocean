pub mod expressions;
pub mod statements;
mod util;

use crate::lexer::TokenKind;
use std::process;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Greater,
    Mod,
    GreaterEquals,
    LessEquals,
}

impl BinaryOp {
    pub fn from_token_kind(kind: &TokenKind) -> Self {
        match kind.clone() {
            TokenKind::Add => Self::Add,
            TokenKind::Sub => Self::Sub,
            TokenKind::Mul => Self::Mul,
            TokenKind::Div => Self::Div,
            TokenKind::Greater => Self::Greater,
            TokenKind::Less => Self::Less,
            TokenKind::Percent => Self::Mod,
            TokenKind::GreaterEquals => Self::GreaterEquals,
            TokenKind::LessEquals => Self::LessEquals,
            token => {
                eprintln!("Token: {:?} is unsuitable for binary operations.", token);
                process::exit(1);
            }
        } 
    }

    pub fn to_string(&self) -> String {
        let s = match self.clone() {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Greater => ">",
            Self::Less => "<",
            Self::Mod => "%",
            Self::GreaterEquals => ">=",
            Self::LessEquals => "<=",
        };

        s.into()
    }
}
