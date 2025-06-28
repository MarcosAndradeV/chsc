#![allow(unused)]

use std::fs;

use fasm_backend::{
    Cond, DataDef, DataDirective, DataExpr, Function, Instr, Module, Register, Value,
};

use crate::ast::*;
use crate::chslexer::*;

use crate::generator::generate;
use crate::opt::*;
use crate::utils::*;

mod ast;
mod chslexer;
mod fasm_backend;
mod opt;
mod parser;
mod utils;
mod generator;

fn main() {
    match app() {
        Err(err) => {
            handle_app_error(&err);
        }
        _ => (),
    }
}

/// I HATE RUST
fn app() -> Result<(), AppError> {
    let (file_path, compiler_flags, run, use_c, debug_ast) = parse_args()?;

    validate_input_file(&file_path)?;

    let source = fs::read_to_string(&file_path).map_err(|e| AppError::FileError {
        path: file_path.clone(),
        error: e,
    })?;

    let mut program_ast =
        parser::parse(&file_path, &source).map_err(|e| AppError::ParseError(format!("{}", e)))?;

    // const_fold(&mut program_ast);
    // for func in &mut program_ast.funcs {
    //     mark_used_variables(func);
    // }

    if debug_ast {
        print_program(&program_ast);
        return Ok(());
    }

    let asm_code = generate(program_ast, use_c)?;

    let (asm_path, o_path, exe_path) = generate_output_paths(&file_path)?;

    fs::write(&asm_path, asm_code.to_string()).map_err(|e| AppError::FileError {
        path: asm_path.to_string_lossy().to_string(),
        error: e,
    })?;

    println!("Generated assembly: {}", asm_path.display());

    let target_path = if use_c { &o_path } else { &exe_path };
    run_fasm(&asm_path, target_path)?;
    println!("Assembly successful: {}", target_path.display());

    if use_c {
        run_cc(&o_path, &exe_path, &compiler_flags)?;
        println!("Linking successful: {}", exe_path.display());
    }

    if run {
        println!("Running executable...");
        let output = run_exe(&exe_path.to_string_lossy().to_string())?;
        print!("{}", output);
    }

    Ok(())
}
