use chumsky::{prelude::*, Stream};

use super::{
    error::InterpreterError,
    lexer::{Keyword, Operator, Token, TokenVec},
};

pub type Program = Vec<Expression>;

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    Boolean(bool),
    Integer(i64),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Let(String, Box<Expression>, Option<Box<Expression>>),
    Function(String, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
    OperatorFunction(BinaryOperator),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
}

fn curry_function(param_names: Vec<String>, expr: Expression) -> Expression {
    param_names.into_iter().fold(expr, |expr, param_name| {
        Expression::Function(param_name, Box::new(expr))
    })
}

fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let expression = recursive(|expression| {
        let identifier = select! { Token::Identifier(s) => s };

        let value = select! {
              Token::Keyword(Keyword::True) => Expression::Boolean(true),
              Token::Keyword(Keyword::False) => Expression::Boolean(false),
              Token::Integer(s) => Expression::Integer(s.parse().unwrap()),
              Token::Identifier(s) => Expression::Identifier(s),
        }
        .labelled("value");

        let operator_func = just(Token::Operator(Operator::ParenOpen))
            .ignore_then(select! {
              Token::Operator(Operator::Add) => BinaryOperator::Add,
              Token::Operator(Operator::Sub) => BinaryOperator::Sub,
              Token::Operator(Operator::Mul) => BinaryOperator::Mul,
              Token::Operator(Operator::Div) => BinaryOperator::Div,
              Token::Operator(Operator::Lt) => BinaryOperator::Lt,
            })
            .then_ignore(just(Token::Operator(Operator::ParenClose)))
            .map(Expression::OperatorFunction);

        let atom = value
            .or(operator_func)
            .or(expression.clone().delimited_by(
                just(Token::Operator(Operator::ParenOpen)),
                just(Token::Operator(Operator::ParenClose)),
            ))
            .labelled("atom");

        let apply = atom
            .clone()
            .then(atom.clone().repeated())
            .foldl(|func, arg| Expression::Apply(Box::new(func), Box::new(arg)))
            .or(atom)
            .labelled("apply");

        let op = just(Token::Operator(Operator::Mul))
            .to(BinaryOperator::Mul)
            .or(just(Token::Operator(Operator::Div)).to(BinaryOperator::Div));
        let mul_div = apply
            .clone()
            .then(op.then(apply).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Binary(op, Box::new(lhs), Box::new(rhs)))
            .labelled("mul_div");

        let op = just(Token::Operator(Operator::Add))
            .to(BinaryOperator::Add)
            .or(just(Token::Operator(Operator::Sub)).to(BinaryOperator::Sub));
        let add_sub = mul_div
            .clone()
            .then(op.then(mul_div).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Binary(op, Box::new(lhs), Box::new(rhs)))
            .labelled("add_sub");

        let op = just(Token::Operator(Operator::Lt)).to(BinaryOperator::Lt);
        let compare = add_sub
            .clone()
            .then(op.then(add_sub).repeated())
            .foldl(|lhs, (op, rhs)| Expression::Binary(op, Box::new(lhs), Box::new(rhs)))
            .labelled("compare");

        let r#if = just(Token::Keyword(Keyword::If))
            .ignore_then(expression.clone())
            .then_ignore(just(Token::Keyword(Keyword::Then)))
            .then(expression.clone())
            .then_ignore(just(Token::Keyword(Keyword::Else)))
            .then(expression.clone())
            .map(|((condition, val_then), val_else)| {
                Expression::If(Box::new(condition), Box::new(val_then), Box::new(val_else))
            })
            .labelled("if");

        let r#let = just(Token::Keyword(Keyword::Let))
            .ignore_then(identifier)
            .then(identifier.repeated().or_not())
            .then_ignore(just(Token::Operator(Operator::Eq)))
            .then(expression.clone())
            .then(
                just(Token::Keyword(Keyword::EndLet))
                    .map(|_| None)
                    .or(just(Token::Keyword(Keyword::In))
                        .ignore_then(expression.clone())
                        .map(Some)),
            )
            .map(
                |(((name, param_names), expr_to_bind), expr)| match param_names {
                    Some(param_names) => Expression::Let(
                        name,
                        Box::new(curry_function(param_names, expr_to_bind)),
                        expr.map(Box::new),
                    ),
                    None => Expression::Let(name, Box::new(expr_to_bind), expr.map(Box::new)),
                },
            );

        let function = just(Token::Keyword(Keyword::Fun))
            .ignore_then(identifier.repeated())
            .then_ignore(just(Token::Keyword(Keyword::Arrow)))
            .then(expression)
            .foldr(|param, expr| Expression::Function(param, Box::new(expr)));

        r#if.or(r#let).or(function).or(compare)
    });

    expression.repeated().then_ignore(end())
}

pub fn parse(tokens: TokenVec) -> (Option<Program>, Vec<InterpreterError>) {
    let len = tokens.last().map(|token| token.1.end).unwrap_or(0);
    let stream = Stream::from_iter(len..len + 1, tokens.into_iter());

    let (program, errors) = parser().parse_recovery(stream);
    let errors = errors.into_iter().map(InterpreterError::from).collect();
    (program, errors)
}
