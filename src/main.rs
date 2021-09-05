use std::{env, fs, process};

use unnamed_lang::{
    interpreter::Interpreter,
    parser::{error::SyntaxError, Parser},
};

fn main() {
    let mut args = env::args();
    let filename = args.nth(1).unwrap();
    let contents = fs::read_to_string(&filename).unwrap();

    let mut parser = Parser::new(&contents);
    let mut stmts = Vec::new();
    loop {
        match parser.parse_stmt() {
            Ok(stmt) => stmts.push(stmt),
            Err(err) => {
                if let SyntaxError::End = err {
                    break;
                }
                err.display(&contents, &filename);
                process::exit(1);
            }
        }
    }

    let mut interpreter = Interpreter::default();
    if let Err(err) = interpreter.run(&stmts) {
        err.display(&contents, &filename)
    }
}
