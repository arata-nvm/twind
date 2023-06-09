use chumsky::prelude::Simple;
use thiserror::Error;

use super::{
    lexer::{Span, Token},
    typing::Type,
};

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("{span:?}: {msg}")]
    UnexpectedChar { span: Span, msg: String },

    #[error("{span:?}: {msg}")]
    UnexpectedToken { span: Span, msg: String },

    #[error("{expect} expected, but {found} found ")]
    UnexpectedValue { expect: String, found: String },

    #[error("cannot find variable `{name}`")]
    CannotFindVariable { name: String },

    #[error("{expect} expected, but {found:?} found ")]
    UnexpectedType { expect: Type, found: Option<Type> },
}

impl From<Simple<char>> for InterpreterError {
    fn from(s: Simple<char>) -> Self {
        InterpreterError::UnexpectedChar {
            span: s.span(),
            msg: s.to_string(),
        }
    }
}

impl From<Simple<Token>> for InterpreterError {
    fn from(s: Simple<Token>) -> Self {
        InterpreterError::UnexpectedToken {
            span: s.span(),
            msg: s.to_string(),
        }
    }
}
