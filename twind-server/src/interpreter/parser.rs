use chumsky::{prelude::*, primitive, Stream};

use super::{
    error::InterpreterError,
    lexer::{Spanned, Token, TokenVec},
};

pub type Program = Spanned<Vec<Spanned<Expression>>>;

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(Box<Integer>),
}

impl Expression {
    pub fn integer(value: i64) -> Self {
        Self::Integer(Box::new(Integer { value }))
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let integer = filter_map(|span, token| match token {
        Token::Integer(s) => Ok(Expression::integer(s.parse().unwrap())),
        _ => Err(Simple::expected_input_found(span, vec![], Some(token))),
    })
    .labelled("integer");

    integer
        .map_with_span(|expr, span| (expr, span))
        .repeated()
        .then_ignore(primitive::end())
        .map_with_span(|expressions, span| (expressions, span))
}

pub fn parse(tokens: TokenVec) -> (Option<Program>, Vec<InterpreterError>) {
    let len = tokens.last().map(|token| token.1.end).unwrap_or(0);
    let stream = Stream::from_iter(len..len + 1, tokens.into_iter());

    let (program, errors) = parser().parse_recovery(stream);
    let errors = errors.into_iter().map(InterpreterError::from).collect();
    (program, errors)
}
