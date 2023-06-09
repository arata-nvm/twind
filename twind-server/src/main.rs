use std::{
    env, fs,
    io::{stdin, stdout, Write},
};

use interpreter::{evaluator, typing};

mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl_loop();
    } else {
        let s = fs::read_to_string(&args[1]).expect("cannot read file");
        let mut venv = evaluator::Environment::new();
        let mut tenv = typing::Environment::new();
        interpret(&s, &mut venv, &mut tenv);
    }
}

fn repl_loop() {
    let mut venv = evaluator::Environment::new();
    let mut tenv = typing::Environment::new();

    loop {
        print!("> ");
        stdout().flush().expect("failed to stdout::flush");

        let mut s = String::new();
        stdin()
            .read_line(&mut s)
            .expect("failed to stdin::read_line");

        interpret(&s, &mut venv, &mut tenv);
    }
}

fn interpret(s: &str, venv: &mut evaluator::Environment, tenv: &mut typing::Environment) {
    match interpreter::interpret(s, venv, tenv) {
        Ok((value, typ)) => {
            println!("{value}");
            println!(": {typ}");
        }
        Err(errors) => {
            println!("There are {} errors:", errors.len());
            for err in errors {
                println!("{err}");
            }
        }
    }
}
