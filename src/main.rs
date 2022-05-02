mod ast;
mod interpreter;
mod parser;
mod scanner;

use crate::interpreter::evaluate_stmt;
use crate::scanner::Scanner;
use anyhow::Result;
use parser::Parser;
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::path;

fn main() -> Result<()> {
    let args = env::args().skip(1).collect::<Vec<String>>();
    if args.len() > 1 {
        println!("Incorrect Usage: rox [script]");
    } else if args.len() == 1 {
        let x = &args[0];
        run_file(path::Path::new(&x))?;
    } else {
        run_prompt()?;
    }
    Ok(())
}

fn run_file(file_path: &path::Path) -> Result<()> {
    let source = fs::read_to_string(file_path)?;
    run(source);
    Ok(())
}

fn run(source: String) {
    let scanner = Scanner::new(&source);
    let mut parser = Parser::new(scanner);
    let statements = parser.parse().expect("something went wrong parsing");

    for statement in statements {
        evaluate_stmt(statement).expect("Error while evaluation");
    }
}

fn run_prompt() -> Result<()> {
    for line in io::stdin().lock().lines() {
        run(line?);
    }
    Ok(())
}
