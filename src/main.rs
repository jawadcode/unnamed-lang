use std::io::{self, Write};

use unnamed_lang::parser::Parser;

fn main() {
    loop {
        let mut input = String::new();
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        let mut parser = Parser::new(&input);
        match parser.parse_stmt() {
            Ok(expr) => println!("{}", expr),
            Err(err) => err.display(&input, "repl.ul"),
        }
    }
}
