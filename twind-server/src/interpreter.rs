use thiserror::Error;

use self::evaluator::Value;

mod evaluator;
mod lexer;
mod parser;

#[derive(Error, Debug)]
pub enum InterpreterError {}

pub fn interpret(s: &str) -> Result<Value, Vec<String>> {
    let (tokens, errors) = lexer::tokenize(s);
    if !errors.is_empty() {
        panic!("{errors:?}");
    }

    let (program, errors) = parser::parse(tokens.unwrap());
    if !errors.is_empty() {
        panic!("{errors:?}");
    }

    let mut e = evaluator::Evaluator;
    let mut last_value = Value::Void;
    for (expr, _) in program.unwrap().0 {
        last_value = e.evaluate(&expr);
    }

    Ok(last_value)
}
