use chumsky::{prelude::*, Stream};

use super::{
    error::InterpreterError,
    lexer::{Keyword, Operator, Spanned, Token, TokenVec},
};

pub type Program = Spanned<Vec<Spanned<Expression>>>;

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(Box<Integer>),
    Binary(Box<Binary>),
    If(Box<If>),
}

impl Expression {
    pub fn integer(value: i64) -> Self {
        Self::Integer(Box::new(Integer { value }))
    }

    pub fn binary(operator: BinaryOperator, lhs: Expression, rhs: Expression) -> Self {
        Self::Binary(Box::new(Binary { operator, lhs, rhs }))
    }

    pub fn r#if(condition: Expression, val_then: Expression, val_else: Expression) -> Self {
        Self::If(Box::new(If {
            condition,
            val_then,
            val_else,
        }))
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

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub val_then: Expression,
    pub val_else: Expression,
}

fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let expression = recursive(|expression| {
        let integer = select! {
              Token::Integer(s) => Expression::integer(s.parse().unwrap()),
        }
        .labelled("integer");

        let atom = integer.or(expression.clone().delimited_by(
            just(Token::Operator(Operator::ParenOpen)),
            just(Token::Operator(Operator::ParenClose)),
        ));

        let op = just(Token::Operator(Operator::Mul))
            .to(BinaryOperator::Mul)
            .or(just(Token::Operator(Operator::Div)).to(BinaryOperator::Div));
        let mul_div = atom
            .clone()
            .then(op.then(atom).repeated())
            .foldl(|lhs, (op, rhs)| Expression::binary(op, lhs, rhs))
            .labelled("mul_div");

        let op = just(Token::Operator(Operator::Add))
            .to(BinaryOperator::Add)
            .or(just(Token::Operator(Operator::Sub)).to(BinaryOperator::Sub));
        let add_sub = mul_div
            .clone()
            .then(op.then(mul_div).repeated())
            .foldl(|lhs, (op, rhs)| Expression::binary(op, lhs, rhs))
            .labelled("add_sub");

        let op = just(Token::Operator(Operator::Lt)).to(BinaryOperator::Lt);
        let compare = add_sub
            .clone()
            .then(op.then(add_sub).repeated())
            .foldl(|lhs, (op, rhs)| Expression::binary(op, lhs, rhs))
            .labelled("compare");

        let r#if = just(Token::Keyword(Keyword::If))
            .ignore_then(expression.clone())
            .then_ignore(just(Token::Keyword(Keyword::Then)))
            .then(expression.clone())
            .then_ignore(just(Token::Keyword(Keyword::Else)))
            .then(expression)
            .map(|((condition, val_then), val_else)| {
                Expression::r#if(condition, val_then, val_else)
            })
            .labelled("if");

        r#if.or(compare)
    });

    expression
        .map_with_span(|expr, span| (expr, span))
        .repeated()
        .then_ignore(end())
        .map_with_span(|expressions, span| (expressions, span))
}

pub fn parse(tokens: TokenVec) -> (Option<Program>, Vec<InterpreterError>) {
    let len = tokens.last().map(|token| token.1.end).unwrap_or(0);
    let stream = Stream::from_iter(len..len + 1, tokens.into_iter());

    let (program, errors) = parser().parse_recovery(stream);
    let errors = errors.into_iter().map(InterpreterError::from).collect();
    (program, errors)
}
