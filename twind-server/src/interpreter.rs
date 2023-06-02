use self::{error::InterpreterError, evaluator::Value};

mod error;
mod evaluator;
mod lexer;
mod parser;

pub fn interpret(s: &str) -> Result<Value, Vec<InterpreterError>> {
    let (tokens, errors) = lexer::tokenize(s);
    if !errors.is_empty() {
        return Err(errors.into_iter().map(InterpreterError::from).collect());
    }

    let (program, errors) = parser::parse(tokens.unwrap());
    if !errors.is_empty() {
        return Err(errors.into_iter().map(InterpreterError::from).collect());
    }

    let mut e = evaluator::Evaluator;
    let mut last_value = Value::Void;
    for (expr, _) in program.unwrap().0 {
        last_value = e.evaluate(&expr);
    }

    Ok(last_value)
}
