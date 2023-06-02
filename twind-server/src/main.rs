use std::{env, fs, process::exit};

use interpreter::interpret;

mod interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} [filename]", args[0]);
        exit(1);
    }

    let s = fs::read_to_string(&args[1]).expect("cannot read file");
    let value = interpret(&s);
    println!("{value:?}")
}
