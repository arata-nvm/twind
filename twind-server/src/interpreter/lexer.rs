use std::fmt;

use chumsky::{
    prelude::*,
    primitive, recovery,
    text::{ident, keyword},
};

use super::error::InterpreterError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Integer(String),
    Operator(Operator),
    Keyword(Keyword),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    ParenOpen,
    ParenClose,
    Lt,
    Eq,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    If,
    Then,
    Else,
    True,
    False,
    Let,
    In,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Integer(s) => write!(f, "{s}"),
            Token::Operator(s) => write!(f, "{s:?}"),
            Token::Keyword(s) => write!(f, "{s:?}"),
            Token::Identifier(s) => write!(f, "{s}"),
        }
    }
}

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);
pub type TokenVec = Vec<Spanned<Token>>;

fn lexer() -> impl Parser<char, TokenVec, Error = Simple<char>> {
    let identifier = ident().map(Token::Identifier);

    let integer = text::digits(10).map(Token::Integer);

    let operator = select! {
      '+' => Token::Operator(Operator::Add),
      '-' => Token::Operator(Operator::Sub),
      '*' => Token::Operator(Operator::Mul),
      '/' => Token::Operator(Operator::Div),
      '(' => Token::Operator(Operator::ParenOpen),
      ')' => Token::Operator(Operator::ParenClose),
      '<' => Token::Operator(Operator::Lt),
      '=' => Token::Operator(Operator::Eq),
    };

    let keyword = keyword("if")
        .to(Token::Keyword(Keyword::If))
        .or(keyword("then").to(Token::Keyword(Keyword::Then)))
        .or(keyword("else").to(Token::Keyword(Keyword::Else)))
        .or(keyword("true").to(Token::Keyword(Keyword::True)))
        .or(keyword("false").to(Token::Keyword(Keyword::False)))
        .or(keyword("let").to(Token::Keyword(Keyword::Let)))
        .or(keyword("in").to(Token::Keyword(Keyword::In)));

    let token = integer.or(operator).or(keyword).or(identifier);

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
