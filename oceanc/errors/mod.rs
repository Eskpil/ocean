pub mod syntax;

use crate::types::{CheckedNamedArgument};
use crate::lexer::Span;
use std::process;

#[derive(Debug)]
pub enum TypeError {
    UnknownType(String),
    MismatchedTypes(String, String),
    VariableNotInScope(String),
    StructNotInScope(String),
    FunctionNotInScope(String),
    DuplicateFunctionParameter(String),
    DuplicateFunctionCallArgument(String),
    ExhaustiveFunctionCallArguments(usize, Vec<CheckedNamedArgument>),
    UnknownFunctionArgument(String),
    VoidAssignment,
}

#[derive(Debug, Clone)]
pub enum Step {
    Parsing,
    Checking,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning, 
    Info,
    Ignore,
}

#[derive(Debug, Clone)]
pub struct OceanError {
    pub step: Step,
    pub level: Level,
    pub span: Option<Span>,
    pub message: String
}

impl OceanError {
    pub fn new(
        level: Level,
        step: Step,
        span: Span, 
        message: String
    ) -> Self {
        Self {
            step,
            level,
            span: Some(span),
            message,
        }
    }

    pub fn no_span(
        level: Level,
        step: Step,
        message: String,
    ) -> Self {
        Self {
            step,
            level,
            span: None,
            message,
        }
    }

    pub fn report(&self) {
        match &self.span {
            Some(span) => {
                match self.level {
                    Level::Error => {
                        eprintln!(
                            "\x1b[31m{}:{}:{}\x1b[0m \x1b[1m{:?}:\x1b[0m {}", 
                            span.file_name, 
                            span.row, 
                            span.col, 
                            self.step, 
                            self.message
                        );
                        process::exit(1);
                    }
                    Level::Warning => {
                        println!(
                            "\x1b[33m{}:{}:{}\x1b[0m \x1b[1m{:?}:\x1b[0m {}", 
                            span.file_name, 
                            span.row, 
                            span.col, 
                            self.step, 
                            self.message
                        );
                    }
                    Level::Info => {
                        println!(
                            "\x1b[36m{}:{}:{}\x1b[0m \x1b[1m{:?}:\x1b[0m {}", 
                            span.file_name, 
                            span.row, 
                            span.col, 
                            self.step, 
                            self.message
                        );
                    }
                    Level::Ignore => {

                    }
                }
            } 
            None => {
                 match self.level {
                    Level::Error => {
                        eprintln!(
                            "\x1b[31m{:?}\x1b[0m {}", 
                            self.step, 
                            self.message
                        );
                        process::exit(1);
                    }
                    Level::Warning => {
                        println!(
                            "\x1b[33m{:?}\x1b[0m {}", 
                            self.step, 
                            self.message
                        );
                    }
                    Level::Info => {
                        println!(
                            "\x1b[36m{:?}\x1b[0m {}", 
                            self.step, 
                            self.message
                        );
                    }
                    Level::Ignore => {

                    }
                }
            }
        }
    }
}
