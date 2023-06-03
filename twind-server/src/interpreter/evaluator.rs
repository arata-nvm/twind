use std::collections::HashMap;

use super::{
    error::InterpreterError,
    parser::{
        Apply, Binary, BinaryOperator, Boolean, Expression, Function, Identifier, If, Integer,
        LetExpression, LetStatement, Statement,
    },
};

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Boolean(bool),
    Integer(i64),
    Function(String, Expression),
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

    pub fn to_function(self) -> Result<(String, Expression), InterpreterError> {
        match self {
            Value::Function(param_name, expr) => Ok((param_name, expr)),
            _ => Err(InterpreterError::UnexpectedValue {
                expect: "function".to_string(),
                found: format!("{self:?}"),
            }),
        }
    }
}

#[derive(Debug, Default)]
pub struct Evaluator {
    environment: Vec<(String, Value)>,
    scopes: Vec<usize>,
}

impl Evaluator {
    pub fn evaluate(&mut self, stmt: Statement) -> Result<Value, InterpreterError> {
        match stmt {
            Statement::Let(r#let) => {
                let LetStatement { name, expr_to_bind } = r#let;

                let expr_to_bind = self.evaluate_expr(expr_to_bind)?;
                self.add_variable(name, expr_to_bind);

                Ok(Value::Void)
            }
            Statement::Expression(expr) => self.evaluate_expr(expr),
        }
    }

    fn evaluate_expr(&mut self, expr: Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Identifier(identifier) => {
                let Identifier { name } = *identifier;
                match self.find_variable(&name) {
                    Some(value) => Ok(value.clone()),
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
                let lhs = self.evaluate_expr(lhs)?.to_integer()?;
                let rhs = self.evaluate_expr(rhs)?.to_integer()?;

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

                let condition = self.evaluate_expr(condition)?.to_boolean()?;
                if condition {
                    self.evaluate_expr(val_then)
                } else {
                    self.evaluate_expr(val_else)
                }
            }
            Expression::Let(r#let) => {
                let LetExpression {
                    name,
                    expr_to_bind,
                    expr,
                } = *r#let;

                let expr_to_bind = self.evaluate_expr(expr_to_bind)?;
                self.add_variable(name, expr_to_bind);

                self.evaluate_expr(expr)
            }
            Expression::Function(function) => {
                let Function { param_name, expr } = *function;
                Ok(Value::Function(param_name, expr))
            }
            Expression::Apply(apply) => {
                let Apply { func, arg } = *apply;
                let (param_name, expr) = self.evaluate_expr(func)?.to_function()?;

                self.push_context();
                let arg = self.evaluate_expr(arg)?;
                self.add_variable(param_name, arg);
                let ret_val = self.evaluate_expr(expr);
                self.pop_context();

                ret_val
            }
        }
    }

    fn add_variable(&mut self, name: String, value: Value) {
        self.environment.push((name, value));
    }

    fn find_variable(&self, name: &String) -> Option<Value> {
        self.environment
            .iter()
            .rev()
            .find(|(var_name, _)| var_name == name)
            .map(|(_, value)| value.clone())
    }

    fn push_context(&mut self) {
        self.scopes.push(self.environment.len());
    }

    fn pop_context(&mut self) {
        let scope = self.scopes.pop().unwrap();
        self.environment.shrink_to(scope);
    }
}
