use super::parser::{Expression, Integer};

#[derive(Debug)]
pub enum Value {
    Void,
    Integer(i64),
}

#[derive(Debug, Default)]
pub struct Evaluator;

impl Evaluator {
    pub fn evaluate(&mut self, expr: &Expression) -> Value {
        match expr {
            Expression::Integer(integer) => {
                let Integer { value } = **integer;
                Value::Integer(value)
            }
        }
    }
}
