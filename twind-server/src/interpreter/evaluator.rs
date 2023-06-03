use super::{
    error::InterpreterError,
    parser::{Binary, BinaryOperator, Expression, Integer},
};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Void,
    Integer(i64),
}

impl Value {
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
pub struct Evaluator;

impl Evaluator {
    pub fn evaluate(&mut self, expr: Expression) -> Result<Value, InterpreterError> {
        match expr {
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
                }
            }
        }
    }
}
