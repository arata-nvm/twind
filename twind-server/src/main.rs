use std::{
    env, fs,
    io::{stdin, stdout, Write},
};

mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl_loop();
    } else {
        let s = fs::read_to_string(&args[1]).expect("cannot read file");
        interpret(&s);
    }
}

fn repl_loop() {
    loop {
        print!("> ");
        stdout().flush().expect("failed to stdout::flush");

        let mut s = String::new();
        stdin()
            .read_line(&mut s)
            .expect("failed to stdin::read_line");

        interpret(&s);
    }
}

fn interpret(s: &str) {
    match interpreter::interpret(s) {
        Ok(value) => println!("{value:?}"),
        Err(errors) => {
            println!("There are {} errors:", errors.len());
            for err in errors {
                println!("{err}");
            }
        }
    }
}
