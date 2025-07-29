#![allow(unused)]

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::process;

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
mod utils;

fn main() {
    if let Err(err) = app() {
        handle_app_error(&err);
    }
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub static BACKEND: std::sync::LazyLock<Backend> =
std::sync::LazyLock::new(|| parse_backend());
// pub static OS: Os = parse_os();
// pub static ARCH: Arch = parse_arch();

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
    let mut imported_modules = HashSet::new();

    let program_ast = parse_module(&strings, &mut  imported_modules, &file_path, &source)?;
    let program_ir = lower_ast_to_ir(program_ast)?;

    let input_path = PathBuf::from(file_path);
    let exe_path = input_path.with_extension("");

    match *BACKEND {
        Backend::FASM => {
            let asm_path = input_path.with_extension("asm");
            let asm_code = generator::fasm_generator::generate(program_ir, false)?;
            fs::write(&asm_path, asm_code.to_string()).map_err(|e| AppError::FileError {
                path: asm_path.to_string_lossy().to_string(),
                error: e,
            })?;
            run_fasm(&asm_path, &exe_path)?;
        }
        Backend::C => {
            let c_path = input_path.with_extension("c");
            let c_code = generator::c_generator::generate(program_ir)?;
            fs::write(&c_path, c_code.to_string()).map_err(|e| AppError::FileError {
                path: c_path.to_string_lossy().to_string(),
                error: e,
            })?;
            run_cc(&c_path, &exe_path)?;
        }
    }

    if run {
        run_exe(&exe_path).inspect(|(code, stdout, stderr)| {
            print!("{stdout}{stderr}");
            process::exit(*code);
        })?;
    }

    Ok(())
}

fn usage() {
    println!("Usage: chsc <module>");
    println!("   or: chsc <command> [arguments]");
    println!("Commands:");
    println!("  run <module>  Compile module and run");
    println!("  help          Print this help message");
    println!("  version       Print version");
}
