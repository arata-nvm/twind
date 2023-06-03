use std::{
    env, fs,
    io::{stdin, stdout, Write},
};

use interpreter::evaluator;

mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl_loop();
    } else {
        let s = fs::read_to_string(&args[1]).expect("cannot read file");
        let mut e = evaluator::Evaluator::default();
        interpret(&s, &mut e);
    }
}

fn repl_loop() {
    let mut e = evaluator::Evaluator::default();

    loop {
        print!("> ");
        stdout().flush().expect("failed to stdout::flush");

        let mut s = String::new();
        stdin()
            .read_line(&mut s)
            .expect("failed to stdin::read_line");

        interpret(&s, &mut e);
    }
}

fn interpret(s: &str, e: &mut evaluator::Evaluator) {
    match interpreter::interpret(s, e) {
        Ok(value) => println!("{value:?}"),
        Err(errors) => {
            println!("There are {} errors:", errors.len());
            for err in errors {
                println!("{err}");
            }
        }
    }
}
