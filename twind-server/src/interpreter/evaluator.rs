use std::collections::HashMap;

use super::{
    error::InterpreterError,
    parser::{Binary, BinaryOperator, Boolean, Expression, Identifier, If, Integer, Let},
};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Void,
    Boolean(bool),
    Integer(i64),
}

impl Value {
    pub fn to_boolean(self) -> Result<bool, InterpreterError> {
        match self {
            Value::Boolean(b) => Ok(b),
            _ => Err(InterpreterError::UnexpectedValue {
                expect: "boolean".to_string(),
                found: format!("{self:?}"),
            }),
        }
    }

    pub fn to_integer(self) -> Result<i64, InterpreterError> {
        match self {
            Value::Integer(i) => Ok(i),
            _ => Err(InterpreterError::UnexpectedValue {
                expect: "integer".to_string(),
                found: format!("{self:?}"),
            }),
        }
    }
}

#[derive(Debug, Default)]
pub struct Evaluator {
    environment: HashMap<String, Value>,
}

impl Evaluator {
    pub fn evaluate(&mut self, expr: Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Identifier(identifier) => {
                let Identifier { name } = *identifier;
                match self.environment.get(&name) {
                    Some(value) => Ok(*value),
                    None => Err(InterpreterError::CannotFindVariable { name }),
                }
            }
            Expression::Boolean(boolean) => {
                let Boolean { value } = *boolean;
                Ok(Value::Boolean(value))
            }
            Expression::Integer(integer) => {
                let Integer { value } = *integer;
                Ok(Value::Integer(value))
            }
            Expression::Binary(binary) => {
                let Binary { operator, lhs, rhs } = *binary;
                let lhs = self.evaluate(lhs)?.to_integer()?;
                let rhs = self.evaluate(rhs)?.to_integer()?;

                match operator {
                    BinaryOperator::Add => Ok(Value::Integer(lhs + rhs)),
                    BinaryOperator::Sub => Ok(Value::Integer(lhs - rhs)),
                    BinaryOperator::Mul => Ok(Value::Integer(lhs * rhs)),
                    BinaryOperator::Div => Ok(Value::Integer(lhs / rhs)),
                    BinaryOperator::Lt => Ok(Value::Boolean(lhs < rhs)),
                }
            }
            Expression::If(r#if) => {
                let If {
                    condition,
                    val_then,
                    val_else,
                } = *r#if;

                let condition = self.evaluate(condition)?.to_boolean()?;
                if condition {
                    self.evaluate(val_then)
                } else {
                    self.evaluate(val_else)
                }
            }
            Expression::Let(r#let) => {
                let Let {
                    name,
                    expr_to_bind,
                    expr,
                } = *r#let;

                let expr_to_bind = self.evaluate(expr_to_bind)?;
                self.environment.insert(name, expr_to_bind);

                self.evaluate(expr)
            }
        }
    }
}
