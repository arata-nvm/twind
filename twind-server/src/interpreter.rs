use self::{
    error::InterpreterError,
    evaluator::Value,
    typing::{reorder_type_schme, Type},
};

pub mod environment;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod typing;

pub fn interpret(
    s: &str,
    venv: &mut evaluator::Environment,
    tenv: &mut typing::Environment,
) -> Result<(Value, Type), Vec<InterpreterError>> {
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
        last_type = typing::infer(expr.clone(), tenv).map_err(|err| vec![err])?;
        last_value = evaluator::evaluate(expr, venv).map_err(|err| vec![err])?;
    }

    Ok((last_value, reorder_type_schme(last_type)))
}
