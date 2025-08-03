#![allow(unused)]

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
use std::process;

use arena::Arena;
use ir::{Body, Func, Program, Stmt};

use crate::lower_ast::lower_ast_to_ir;
use crate::parser::parse_module;
use crate::utils::*;

mod ast;
mod chslexer;
mod generator;
mod interpreter;
mod ir;
mod lower_ast;
mod parser;
mod type_checker;

mod arena;
mod utils;

fn main() {
    if let Err(err) = app() {
        handle_app_error(&err);
    }
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn app() -> Result<(), AppError> {
    let c = Compiler::new();

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
                println!("chsc version: {VERSION}");
                return Ok(());
            }
            "run" => {
                run = true;
            }
            input_file => {
                validate_input_file(input_file)?;
                file_path = Some(c.strings.alloc(arg));
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
    let source = c.strings.alloc(source);
    let mut imported_modules = HashSet::new();

    let program_ast = parse_module(&c, &mut imported_modules, &file_path, &source)?;
    let mut program_ir = lower_ast_to_ir(program_ast)?;
    // type_checker::check(&program_ir);
    let mut used_func = vec![];
    mark_used(&program_ir, &mut used_func, "main");
    for name in used_func {
        if let Some(f) = program_ir.funcs.iter_mut().find(|f| f.name.source == name) {
            f.used = true;
        }
    }

    let input_path = PathBuf::from(file_path);
    let exe_path = input_path.with_extension("");

    match parse_backend() {
        Backend::FASM => {
            let asm_path = input_path.with_extension("asm");
            let asm_code = generator::fasm_generator::generate(program_ir, false)?;
            fs::write(&asm_path, asm_code.to_string()).map_err(|e| AppError::FileError {
                path: asm_path.to_string_lossy().to_string(),
                error: e,
            })?;
            run_fasm(&asm_path, &exe_path)?;
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

fn mark_used<'src>(program_ir: &Program<'src>, used_func: &mut Vec<&'src str>, root: &'src str) {
    if let Some(f) = program_ir.funcs.iter().find(|f| f.name.source == root) {
        used_func.push(f.name.source);
        for stmt in f.body.stmts.iter() {
            if let Stmt::Funcall {
                result: _,
                caller,
                args: _,
            } = stmt
            {
                mark_used(program_ir, used_func, caller.source);
            }
        }
    }
}

fn usage() {
    println!("Usage: chsc <input-file>");
    println!("   or: chsc <command> [arguments]");
    println!("Commands:");
    println!("  run <input-file>  Compile module and run");
    println!("  help              Print this help message");
    println!("  version           Print version");
}

struct Compiler {
    backend: Backend,
    os: Os,
    arch: Arch,
    strings: Arena<String>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            backend: parse_backend(),
            os: parse_os(),
            arch: parse_arch(),
            strings: Arena::new(),
        }
    }
}
