use self::{error::InterpreterError, evaluator::Value};

mod error;
mod evaluator;
mod lexer;
mod parser;

pub fn interpret(s: &str) -> Result<Value, Vec<InterpreterError>> {
    let (tokens, errors) = lexer::tokenize(s);
    let Some(tokens) = tokens else {
      return Err(errors);
    };

    let (program, errors) = parser::parse(tokens);
    let Some(program) = program else {
      return Err(errors);
    };

    let mut e = evaluator::Evaluator;
    let mut last_value = Value::Void;
    for (expr, _) in program.0 {
        last_value = e.evaluate(&expr);
    }

    Ok(last_value)
}
