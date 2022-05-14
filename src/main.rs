mod ast;
mod interpreter;
mod parser;
mod scanner;

use crate::interpreter::{Environment, Evaluate};
use crate::parser::Parser;
use crate::scanner::Scanner;
use anyhow::Result;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::path;
use std::rc::Rc;

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
    let env = Environment::new(None);
    run(source, env)?;
    Ok(())
}

fn run(source: String, env: Rc<RefCell<Environment>>) -> Result<()> {
    let scanner = Scanner::new(&source);
    let mut parser = Parser::new(scanner);
    let statements = parser.parse()?;
    for statement in statements {
        statement.evaluate(env.clone())?
    }
    Ok(())
}

fn run_prompt() -> Result<()> {
    let env = Environment::new(None);

    for line in io::stdin().lock().lines() {
        if let Err(error) = run(line?, env.clone()) {
            println!("{error}")
        }
    }
    Ok(())
}
