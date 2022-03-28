mod scanner;

use crate::scanner::Scanner;
use anyhow::Result;
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
    for token in scanner {
        println!("{:?}", token);
    }
}

fn run_prompt() -> Result<()> {
    for line in io::stdin().lock().lines() {
        run(line?);
    }
    Ok(())
}
