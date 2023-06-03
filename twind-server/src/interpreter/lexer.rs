use std::fmt;

use chumsky::{prelude::*, primitive, recovery};

use super::error::InterpreterError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Integer(String),
    Operator(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Integer(s) => write!(f, "{s}"),
            Token::Operator(s) => write!(f, "{s}"),
        }
    }
}

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);
pub type TokenVec = Vec<Spanned<Token>>;

fn lexer() -> impl Parser<char, TokenVec, Error = Simple<char>> {
    let integer = text::digits(10).map(Token::Integer);

    let operator = one_of("+-*/<()").map(|c: char| Token::Operator(c.to_string()));

    let token = integer.or(operator);

    token
        .map_with_span(|token, span| (token, span))
        .padded()
        .recover_with(recovery::skip_then_retry_until([]))
        .repeated()
        .then_ignore(primitive::end())
}

pub fn tokenize(s: &str) -> (Option<TokenVec>, Vec<InterpreterError>) {
    let (tokens, errors) = lexer().parse_recovery(s);
    let errors = errors.into_iter().map(InterpreterError::from).collect();
    (tokens, errors)
}
