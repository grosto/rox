mod analysis;
mod ast;
mod globals;
mod interpreter;
mod parser;
mod scanner;

use crate::interpreter::{Environment, WrappedEnvironment};
use analysis::analyze_statements;
use anyhow::Result;
use interpreter::evaluate_statements;
use parser::parse_source_code;
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
    let env = Environment::global();
    run(source, env)?;
    Ok(())
}

fn run(source: String, env: WrappedEnvironment) -> Result<()> {
    let statements = parse_source_code(&source)?;
    analyze_statements(&statements);
    evaluate_statements(statements, env)?;
    Ok(())
}

fn run_prompt() -> Result<()> {
    let env = Environment::global();

    for line in io::stdin().lock().lines() {
        if let Err(error) = run(line?, env.clone()) {
            println!("{error}")
        }
    }
    Ok(())
}
