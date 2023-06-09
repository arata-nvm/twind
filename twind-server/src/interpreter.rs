use self::{
    error::InterpreterError,
    evaluator::{Environment, Value},
    typing::Type,
};

pub mod environment;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod typing;

pub fn interpret(s: &str, e: &mut Environment) -> Result<(Value, Type), Vec<InterpreterError>> {
    let (tokens, errors) = lexer::tokenize(s);
    let Some(tokens) = tokens else {
      return Err(errors);
    };

    let (program, errors) = parser::parse(tokens);
    let Some(program) = program else {
      return Err(errors);
    };

    let mut last_value = Value::Void;
    let mut last_type = Type::Void;
    for expr in program {
        last_type = typing::infer_type(&expr).map_err(|err| vec![err])?;
        last_value = evaluator::evaluate(expr, e).map_err(|err| vec![err])?;
    }

    Ok((last_value, last_type))
}
