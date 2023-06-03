use super::{
    error::InterpreterError,
    parser::{Binary, BinaryOperator, Expression, Integer},
};

#[derive(Debug)]
pub enum Value {
    Void,
    Integer(i64),
}

impl Value {
    pub fn as_integer(self) -> Result<i64, InterpreterError> {
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
                let lhs = self.evaluate(lhs)?.as_integer()?;
                let rhs = self.evaluate(rhs)?.as_integer()?;

                match operator {
                    BinaryOperator::Add => Ok(Value::Integer(lhs + rhs)),
                }
            }
        }
    }
}
