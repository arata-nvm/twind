use interpreter::interpret;

mod interpreter;

fn main() {
    let s = "42";

    println!("s = {s:?}");
    let value = interpret(s);
    println!("value = {value:?}");
}
