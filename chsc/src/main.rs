#![allow(unused)]

use std::env::args;
use std::fs;
use std::path::PathBuf;

use fasm_backend::{
    Cond, DataDef, DataDirective, DataExpr, Function, Instr, Module, Register, Value,
};

use crate::chslexer::*;
use crate::ir::*;

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

/// I HATE RUST
fn app() -> Result<(), AppError> {
    // Arena for happy borrow checker
    let mut strings = arena::Arena::new();

    let mut args = args();
    args.next();

    let Some(file_path) = args.next() else {
        todo!()
    };

    let file_path = strings.alloc(file_path);

    let config = read_config("chs.toml")?;
    validate_input_file(&file_path)?;

    let source = fs::read_to_string(&file_path).map_err(|e| AppError::FileError {
        path: file_path.clone(),
        error: e,
    })?;
    let source = strings.alloc(source);

    let mut program_ast = parse_module(&strings, &file_path, &source)?;

    let mut program_ir = lower_ast_to_ir(program_ast)?;

    let asm_code = generate(program_ir, false)?;

    let (asm_path, o_path, exe_path) = generate_output_paths(&file_path)?;

    fs::write(&asm_path, asm_code.to_string()).map_err(|e| AppError::FileError {
        path: asm_path.to_string_lossy().to_string(),
        error: e,
    })?;

    println!("Generated assembly: {}", asm_path.display());

    run_fasm(&asm_path, &exe_path)?;
    println!("Assembly successful: {}", exe_path.display());

    if config.run {
        println!("Running executable...");
        let output = run_exe(&exe_path.to_string_lossy().to_string())?;
        print!("{}", output);
    }

    Ok(())
}

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct ConfigFile{config:Config}

#[derive(Debug, Deserialize)]
struct Config {
    name: String,
    version: String,
    run: bool,
}

fn read_config(path: &str) -> Result<Config, AppError> {
    let toml_str = std::fs::read_to_string(path).map_err(|e| AppError::FileError {
        path: path.to_string(),
        error: e,
    })?;
    match toml::from_str::<ConfigFile>(&toml_str) {
        Ok(ConfigFile{config}) => Ok(config),
        Err(e) => Err(AppError::ConfigParseError(e.to_string())),
    }
}
