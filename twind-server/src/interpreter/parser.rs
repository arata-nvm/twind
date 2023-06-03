use chumsky::{prelude::*, primitive, Stream};

use super::{
    error::InterpreterError,
    lexer::{Operator, Spanned, Token, TokenVec},
};

pub type Program = Spanned<Vec<Spanned<Expression>>>;

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(Box<Integer>),
    Binary(Box<Binary>),
}

impl Expression {
    pub fn integer(value: i64) -> Self {
        Self::Integer(Box::new(Integer { value }))
    }

    pub fn binary(operator: BinaryOperator, lhs: Expression, rhs: Expression) -> Self {
        Self::Binary(Box::new(Binary { operator, lhs, rhs }))
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub operator: BinaryOperator,
    pub lhs: Expression,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,

    Lt,
}

fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let expression = recursive(|expression| {
        let integer = select! {
              Token::Integer(s) => Expression::integer(s.parse().unwrap()),
        }
        .labelled("integer");

        let atom = integer.or(expression.delimited_by(
            just(Token::Operator(Operator::ParenOpen)),
            just(Token::Operator(Operator::ParenClose)),
        ));

        let mul_div = atom
            .clone()
            .then(
                just(Token::Operator(Operator::Mul))
                    .to(BinaryOperator::Mul)
                    .or(just(Token::Operator(Operator::Div)).to(BinaryOperator::Div))
                    .then(atom)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expression::binary(op, lhs, rhs))
            .labelled("mul_div");

        let add_sub = mul_div
            .clone()
            .then(
                just(Token::Operator(Operator::Add))
                    .to(BinaryOperator::Add)
                    .or(just(Token::Operator(Operator::Sub)).to(BinaryOperator::Sub))
                    .then(mul_div)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expression::binary(op, lhs, rhs))
            .labelled("add_sub");

        let compare = add_sub
            .clone()
            .then(
                just(Token::Operator(Operator::Lt))
                    .to(BinaryOperator::Lt)
                    .then(add_sub)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expression::binary(op, lhs, rhs))
            .labelled("compare");

        compare
    });

    expression
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
