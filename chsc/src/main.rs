#![allow(unused)]

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;

use arena::Arena;
use ir::{Body, Func, Program, Stmt};

use crate::ast::{Module, Name};
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
    let c = Compiler::new();
    if let Err(err) = app(&c) {
        c.report_errors();
        std::process::exit(1);
    }
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn app<'src>(c: &'src Compiler<'src>) -> Result<(), ()> {
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
                file_path = Some(arg);
            }
        }
    }

    let Some(file_path) = file_path else {
        usage();
        c.compiler_error("No input file".to_string())?
    };

    let file_path = c.add_file_path(file_path);
    let source = c.read_source_file(&file_path)?.unwrap();

    parse_module(&c, &file_path, &source)?;

    let mut program_ir = lower_ast_to_ir(&c).unwrap();
    // type_checker::check(&program_ir);
    let mut used_func = vec![];
    mark_used(&program_ir, &mut used_func, "main");
    for name in used_func {
        if let Some(f) = program_ir.funcs.iter_mut().find(|f| f.name.source == name) {
            f.used = true;
        }
    }

    if c.has_errors() {
        return Err(());
    }

    let input_path = PathBuf::from(file_path);
    let o_path = input_path.with_extension("o");
    let exe_path = input_path.with_extension("");

    match parse_backend() {
        Backend::FASM => {
            let asm_path = input_path.with_extension("asm");
            let asm_code = generator::fasm_generator::generate(program_ir, true).unwrap();
            fs::write(&asm_path, asm_code.to_string())
                .map_err(|e| AppError::FileError {
                    path: asm_path.to_string_lossy().to_string(),
                    error: e,
                })
                .unwrap();
            run_fasm(&asm_path, &o_path).unwrap();
            run_cc(&o_path, &exe_path, &["-l:libchs.a", "-Lstdlib/libchs"]).unwrap();
        }
    }

    if run {
        run_exe(&exe_path)
            .inspect(|(code, stdout, stderr)| {
                print!("{stdout}{stderr}");
                std::process::exit(*code);
            })
            .unwrap();
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

#[derive(Default)]
struct Compiler<'src> {
    backend: Backend,
    os: Os,
    arch: Arch,
    diag: RefCell<Vec<Diag>>,
    sources: RefCell<Vec<String>>,
    file_paths: RefCell<Vec<String>>,
    modules: RefCell<HashMap<&'src str, Module<'src>>>,
    name_space: RefCell<HashMap<&'src str, Name<'src>>>,
}

impl<'src> Compiler<'src> {
    fn new() -> Self {
        Self::default()
    }

    fn compiler_error<T>(&self, format: String) -> Result<T, ()> {
        self.diag.borrow_mut().push(Diag::Error(format));
        Err(())
    }

    fn read_source_file(&'src self, file_path: &'src str) -> Result<Option<&'src str>, ()> {
        validate_input_file(self, file_path)?;
        if self.has_module(file_path) {
            return Ok(None);
        }
        match std::fs::read_to_string(file_path) {
            Ok(source) => {
                let source = self.add_source(source);
                Ok(Some(source))
            }
            Err(e) => self.compiler_error(format!("Unexpected error while reading file: {e}")),
        }
    }

    fn add_source(&'src self, source: String) -> &'src String {
        let mut sources = self.sources.borrow_mut();
        let len = sources.len();
        sources.push(source.clone());
        unsafe { sources.as_ptr().add(len).as_ref().unwrap() }
    }

    fn add_file_path(&'src self, file_path: String) -> &'src String {
        let mut file_paths = self.file_paths.borrow_mut();
        let len = file_paths.len();
        file_paths.push(file_path.clone());
        unsafe { file_paths.as_ptr().add(len).as_ref().unwrap() }
    }

    fn report_errors(&self) -> bool {
        let mut diag = self.diag.borrow_mut();
        if !diag.is_empty() {
            for d in diag.drain(..) {
                match d {
                    Diag::Error(e) => eprintln!("{e}"),
                }
            }
            return true;
        } else {
            return false;
        }
    }

    fn has_errors(&self) -> bool {
        let diag = self.diag.borrow();
        diag.iter().find(|d| matches!(d, Diag::Error(..))).is_some()
    }

    fn add_module(&self, file_path: &'src str, module: Module<'src>) {
        self.modules.borrow_mut().insert(file_path, module);
    }

    fn has_module(&self, file_path: &str) -> bool {
        self.modules.borrow().contains_key(file_path)
    }
}

enum Diag {
    Error(String),
}
