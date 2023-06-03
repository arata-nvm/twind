use chumsky::{prelude::*, Stream};

use super::{
    error::InterpreterError,
    lexer::{Keyword, Operator, Spanned, Token, TokenVec},
};

pub type Program = Spanned<Vec<Spanned<Statement>>>;

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Expression(Expression),
}

impl Statement {
    pub fn r#let(name: String, expr_to_bind: Expression) -> Self {
        Self::Let(LetStatement { name, expr_to_bind })
    }

    pub fn expression(expr: Expression) -> Self {
        Self::Expression(expr)
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub name: String,
    pub expr_to_bind: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Box<Identifier>),
    Boolean(Box<Boolean>),
    Integer(Box<Integer>),
    Binary(Box<Binary>),
    If(Box<If>),
    Let(Box<LetExpression>),
    Function(Box<Function>),
}

impl Expression {
    pub fn identifier(name: String) -> Self {
        Self::Identifier(Box::new(Identifier { name }))
    }
    pub fn boolean(value: bool) -> Self {
        Self::Boolean(Box::new(Boolean { value }))
    }

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

    pub fn r#let(name: String, expr_to_bind: Expression, expr: Expression) -> Self {
        Self::Let(Box::new(LetExpression {
            name,
            expr_to_bind,
            expr,
        }))
    }

    pub fn function(param_name: String, expr: Expression) -> Self {
        Self::Function(Box::new(Function { param_name, expr }))
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub value: bool,
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

#[derive(Debug, Clone)]
pub struct LetExpression {
    pub name: String,
    pub expr_to_bind: Expression,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub param_name: String,
    pub expr: Expression,
}

fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let identifier = select! { Token::Identifier(s) => s };

    let expression = recursive(|expression| {
        let value = select! {
              Token::Keyword(Keyword::True) => Expression::boolean(true),
              Token::Keyword(Keyword::False) => Expression::boolean(false),
              Token::Integer(s) => Expression::integer(s.parse().unwrap()),
              Token::Identifier(s) => Expression::identifier(s),
        }
        .labelled("value");

        let atom = value.or(expression.clone().delimited_by(
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
            .then(expression.clone())
            .map(|((condition, val_then), val_else)| {
                Expression::r#if(condition, val_then, val_else)
            })
            .labelled("if");

        let r#let = just(Token::Keyword(Keyword::Let))
            .ignore_then(identifier)
            .then_ignore(just(Token::Operator(Operator::Eq)))
            .then(expression.clone())
            .then_ignore(just(Token::Keyword(Keyword::In)))
            .then(expression.clone())
            .map(|((name, expr_to_bind), expr)| Expression::r#let(name, expr_to_bind, expr));

        let function = just(Token::Keyword(Keyword::Fun))
            .ignore_then(identifier)
            .then_ignore(just(Token::Keyword(Keyword::Arrow)))
            .then(expression)
            .map(|(param_name, expr)| Expression::function(param_name, expr));

        r#if.or(r#let).or(function).or(compare)
    });

    let r#let = just(Token::Keyword(Keyword::Let))
        .ignore_then(identifier)
        .then_ignore(just(Token::Operator(Operator::Eq)))
        .then(expression.clone())
        .then_ignore(just(Token::Keyword(Keyword::EndLet)))
        .map(|(name, expr_to_bind)| Statement::r#let(name, expr_to_bind));

    let statement = r#let.or(expression.map(Statement::expression));

    statement
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
