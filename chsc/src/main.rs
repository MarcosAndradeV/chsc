#![allow(unused)]

use std::path::PathBuf;
use std::{fs, process};

use crate::generator::generate;
use crate::lower_ast::lower_ast_to_ir;
use crate::parser::parse_module;
use crate::utils::*;

mod ast;
mod chslexer;
mod generator;
mod ir;
mod lower_ast;
mod parser;

mod arena;
mod fasm_backend;
mod utils;

fn main() {
    if let Err(err) = app() {
        handle_app_error(&err);
    }
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// I HATE RUST
fn app() -> Result<(), AppError> {
    // Arena for happy borrow checker
    let strings = arena::Arena::new();

    let mut args = std::env::args().skip(1);
    let mut file_path = None;
    let mut run = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "help" => {
                usage();
                return Ok(());
            }
            "version" => {
                println!("version: {VERSION}");
                return Ok(());
            }
            "run" => {
                run = true;
            }
            input_file => {
                validate_input_file(input_file)?;
                file_path = Some(strings.alloc(arg));
            }
        }
    }

    let Some(file_path) = file_path else {
        usage();
        return Err(AppError::ArgumentError("No input file".to_string()));
    };

    let source = fs::read_to_string(&file_path).map_err(|e| AppError::FileError {
        path: file_path.to_string(),
        error: e,
    })?;
    let source = strings.alloc(source);

    let program_ast = parse_module(&strings, &file_path, &source)?;
    let program_ir = lower_ast_to_ir(program_ast)?;
    let asm_code = generate(program_ir, false)?;

    let input_path = PathBuf::from(file_path);
    let asm_path = input_path.with_extension("asm");
    let exe_path = input_path.with_extension("");

    fs::write(&asm_path, asm_code.to_string()).map_err(|e| AppError::FileError {
        path: asm_path.to_string_lossy().to_string(),
        error: e,
    })?;

    run_fasm(&asm_path, &exe_path)?;

    if run {
        run_exe(&exe_path).inspect(|(code, stdout, stderr)| {
            print!("{stdout}{stderr}");
            process::exit(*code);
        })?;
    }

    Ok(())
}

fn usage() {
    println!("Usage: chsc [<COMMAND>] <input-file>");
    println!("Commands:");
    println!("  run <input-file>   Compile <input-file> and run it");
    println!("  help               Print this help message");
    println!("  version            Print version");
}
