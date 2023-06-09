use self::{
    error::InterpreterError,
    evaluator::{Environment, Value},
};

pub mod environment;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;

pub fn interpret(s: &str, e: &mut Environment) -> Result<Value, Vec<InterpreterError>> {
    let (tokens, errors) = lexer::tokenize(s);
    let Some(tokens) = tokens else {
      return Err(errors);
    };

    let (program, errors) = parser::parse(tokens);
    let Some(program) = program else {
      return Err(errors);
    };

    let mut last_value = Value::Void;
    for expr in program {
        last_value = evaluator::evaluate(expr, e).map_err(|err| vec![err])?;
    }

    Ok(last_value)
}
