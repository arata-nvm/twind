use std::{
    env, fs,
    io::{stdin, stdout, Write},
};

use interpreter::evaluator::Environment;

mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl_loop();
    } else {
        let s = fs::read_to_string(&args[1]).expect("cannot read file");
        let mut e = Environment::new();
        interpret(&s, &mut e);
    }
}

fn repl_loop() {
    let mut e = Environment::new();

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

fn interpret(s: &str, e: &mut Environment) {
    match interpreter::interpret(s, e) {
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
