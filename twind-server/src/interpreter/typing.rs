use std::fmt;

use super::{error::InterpreterError, parser::Expression};

#[derive(Debug)]
pub enum Type {
    Void,
    Boolean,
    Integer,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Boolean => write!(f, "bool"),
            Type::Integer => write!(f, "int"),
        }
    }
}

pub fn infer_type(expr: &Expression) -> Result<Type, InterpreterError> {
    match expr {
        Expression::Identifier(_) => todo!(),
        Expression::Boolean(_) => Ok(Type::Boolean),
        Expression::Integer(_) => Ok(Type::Integer),
        Expression::Binary(_, _, _) => todo!(),
        Expression::If(_, _, _) => todo!(),
        Expression::Let(_, _, _) => todo!(),
        Expression::Function(_, _) => todo!(),
        Expression::Apply(_, _) => todo!(),
        Expression::OperatorFunction(_) => todo!(),
    }
}
