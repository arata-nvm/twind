use super::{
    environment,
    error::InterpreterError,
    parser::{BinaryOperator, Expression},
};

pub type Environment = environment::Environment<Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Boolean(bool),
    Integer(i64),
    Function(String, Expression, Environment),
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

    pub fn to_function(self) -> Result<(String, Expression, Environment), InterpreterError> {
        match self {
            Value::Function(param_name, expr, env) => Ok((param_name, expr, env)),
            _ => Err(InterpreterError::UnexpectedValue {
                expect: "function".to_string(),
                found: format!("{self:?}"),
            }),
        }
    }
}

pub fn evaluate(expr: Expression, env: &mut Environment) -> Result<Value, InterpreterError> {
    match expr {
        Expression::Identifier(name) => match env.lookup(&name) {
            Some(value) => Ok(value),
            None => Err(InterpreterError::CannotFindVariable { name }),
        },
        Expression::Boolean(value) => Ok(Value::Boolean(value)),
        Expression::Integer(value) => Ok(Value::Integer(value)),
        Expression::Binary(operator, lhs, rhs) => {
            let lhs = evaluate(*lhs, env)?.to_integer()?;
            let rhs = evaluate(*rhs, env)?.to_integer()?;

            match operator {
                BinaryOperator::Add => Ok(Value::Integer(lhs + rhs)),
                BinaryOperator::Sub => Ok(Value::Integer(lhs - rhs)),
                BinaryOperator::Mul => Ok(Value::Integer(lhs * rhs)),
                BinaryOperator::Div => Ok(Value::Integer(lhs / rhs)),
                BinaryOperator::Lt => Ok(Value::Boolean(lhs < rhs)),
            }
        }
        Expression::If(condition, val_then, val_else) => {
            let condition = evaluate(*condition, env)?.to_boolean()?;
            if condition {
                evaluate(*val_then, env)
            } else {
                evaluate(*val_else, env)
            }
        }
        Expression::Let(name, expr_to_bind, expr) => match expr {
            None => {
                let expr_to_bind = evaluate(*expr_to_bind, env)?;
                env.expand(name, expr_to_bind);
                Ok(Value::Void)
            }
            Some(expr) => {
                let mut newenv = env.clone();
                newenv.expand(name, evaluate(*expr_to_bind, env)?);
                evaluate(*expr, &mut newenv)
            }
        },
        Expression::Function(param_name, expr) => {
            Ok(Value::Function(param_name, *expr, env.clone()))
        }
        Expression::Apply(func, arg) => {
            let (param_name, expr, mut newenv) = evaluate(*func, env)?.to_function()?;
            newenv.expand(param_name, evaluate(*arg, env)?);
            evaluate(expr, &mut newenv)
        }
        Expression::OperatorFunction(op) => Ok(Value::Function(
            ".lhs".to_string(),
            Expression::Function(
                ".rhs".to_string(),
                Box::new(Expression::Binary(
                    op,
                    Box::new(Expression::Identifier(".lhs".to_string())),
                    Box::new(Expression::Identifier(".rhs".to_string())),
                )),
            ),
            env.clone(),
        )),
    }
}
