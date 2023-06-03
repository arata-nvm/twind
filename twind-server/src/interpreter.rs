use self::{error::InterpreterError, evaluator::Value};

pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;

pub fn interpret(s: &str, e: &mut evaluator::Evaluator) -> Result<Value, Vec<InterpreterError>> {
    let (tokens, errors) = lexer::tokenize(s);
    let Some(tokens) = tokens else {
      return Err(errors);
    };

    let (program, errors) = parser::parse(tokens);
    let Some(program) = program else {
      return Err(errors);
    };

    let mut last_value = Value::Void;
    for (stmt, _) in program.0 {
        last_value = e.evaluate(stmt).map_err(|err| vec![err])?;
    }

    Ok(last_value)
}
