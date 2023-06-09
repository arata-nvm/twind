use std::fmt;

use crate::interpreter::parser::BinaryOperator;

use super::{environment, error::InterpreterError, parser::Expression};

pub type Environment = environment::Environment<Type>;

#[derive(Debug, Clone, PartialEq, Eq)]
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

pub fn infer_type(expr: &Expression, env: &mut Environment) -> Result<Type, InterpreterError> {
    match expr {
        Expression::Identifier(name) => env
            .lookup(name)
            .ok_or(InterpreterError::CannotFindVariable { name: name.clone() }),
        Expression::Boolean(_) => Ok(Type::Boolean),
        Expression::Integer(_) => Ok(Type::Integer),
        Expression::Binary(op, lhs, rhs) => {
            let lhs = infer_type(lhs, env)?;
            let rhs = infer_type(rhs, env)?;
            match (op, lhs, rhs) {
                (BinaryOperator::Lt, Type::Integer, Type::Integer) => Ok(Type::Boolean),
                (_, Type::Integer, Type::Integer) => Ok(Type::Integer),
                _ => Err(InterpreterError::UnexpectedType {
                    expect: Type::Integer,
                    found: None,
                }),
            }
        }
        Expression::If(cond, val_then, val_else) => {
            let cond = infer_type(cond, env)?;
            let val_then = infer_type(val_then, env)?;
            let val_else = infer_type(val_else, env)?;

            if !matches!(cond, Type::Boolean) {
                return Err(InterpreterError::UnexpectedType {
                    expect: cond,
                    found: Some(Type::Boolean),
                });
            }

            if val_then != val_else {
                return Err(InterpreterError::UnexpectedType {
                    expect: val_then,
                    found: Some(val_else),
                });
            }

            Ok(val_then)
        }
        Expression::Let(name, expr_to_bind, expr) => {
            let expr_to_bind = infer_type(&**expr_to_bind, env)?;
            if let Some(expr) = expr {
                infer_type(expr, &mut env.expanded(name.clone(), expr_to_bind))
            } else {
                Ok(Type::Void)
            }
        }
        Expression::LetRec(_, _, _) => todo!(),
        Expression::Function(_, _) => todo!(),
        Expression::Apply(_, _) => todo!(),
        Expression::OperatorFunction(_) => todo!(),
    }
}
