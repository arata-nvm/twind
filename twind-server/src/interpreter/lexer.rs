use std::fmt;

use chumsky::{prelude::*, primitive, recovery};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Integer(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Integer(s) => write!(f, "{s}"),
        }
    }
}

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);
pub type TokenVec = Vec<Spanned<Token>>;

fn lexer() -> impl Parser<char, TokenVec, Error = Simple<char>> {
    let integer = text::digits(10).map(Token::Integer);

    let token = integer.recover_with(recovery::skip_then_retry_until([]));

    token
        .map_with_span(|token, span| (token, span))
        .padded()
        .repeated()
        .then_ignore(primitive::end())
}

pub fn tokenize(s: &str) -> (Option<TokenVec>, Vec<Simple<char>>) {
    lexer().parse_recovery(s)
}
