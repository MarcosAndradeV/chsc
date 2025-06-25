use std::error;
use std::fmt::{self, Display};
use std::{collections::HashMap, env::args, ffi::OsStr, fs, path::PathBuf, process::Command};

pub const STDLIB_PATH: &str = "stdlib";

#[derive(Debug)]
pub enum BuildError {
    ProcessError {
        command: String,
        error: std::io::Error,
    },
    CompilationError {
        command: String,
        stdout: String,
        stderr: String,
    },
    InvalidPath {
        path: String,
    },
}

impl Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuildError::ProcessError { command, error } => {
                write!(f, "Failed to execute '{}': {}", command, error)
            }
            BuildError::CompilationError {
                command,
                stdout,
                stderr,
            } => {
                write!(
                    f,
                    "'{}' failed:\nStdout: {}\nStderr: {}",
                    command, stdout, stderr
                )
            }
            BuildError::InvalidPath { path } => {
                write!(f, "Invalid path: {}", path)
            }
        }
    }
}

impl std::error::Error for BuildError {}

pub fn run_cc<I, O, A, F>(
    input_path: I,
    output_path: O,
    compiler_flags: F,
) -> Result<(), BuildError>
where
    I: AsRef<OsStr>,
    O: AsRef<OsStr>,
    A: AsRef<OsStr>,
    F: IntoIterator<Item = A>,
{
    let input_str = input_path.as_ref().to_string_lossy();
    let output_str = output_path.as_ref().to_string_lossy();

    if !std::path::Path::new(&input_str.as_ref()).exists() {
        return Err(BuildError::InvalidPath {
            path: input_str.to_string(),
        });
    }

    let mut cc_command = Command::new("cc");
    cc_command
        .arg("-o")
        .arg(output_path)
        .arg(input_path)
        .args(compiler_flags);

    let output = cc_command.output().map_err(|e| BuildError::ProcessError {
        command: "cc".to_string(),
        error: e,
    })?;

    if !output.status.success() {
        return Err(BuildError::CompilationError {
            command: "cc".to_string(),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    Ok(())
}

pub fn run_fasm<I, O>(input_path: I, output_path: O) -> Result<(), BuildError>
where
    I: AsRef<OsStr>,
    O: AsRef<OsStr>,
{
    let input_str = input_path.as_ref().to_string_lossy();

    if !std::path::Path::new(&input_str.as_ref()).exists() {
        return Err(BuildError::InvalidPath {
            path: input_str.to_string(),
        });
    }

    let mut command = Command::new("fasm");
    command.arg(input_path).arg(output_path);

    let output = command.output().map_err(|e| BuildError::ProcessError {
        command: "fasm".to_string(),
        error: e,
    })?;

    if !output.status.success() {
        return Err(BuildError::CompilationError {
            command: "fasm".to_string(),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    Ok(())
}

pub fn run_exe<I>(input_path: I) -> Result<String, BuildError>
where
    I: AsRef<OsStr> + Display,
{
    let path_str = format!("./{}", input_path);
    let exe_path = std::path::Path::new(&path_str);

    if !exe_path.exists() {
        return Err(BuildError::InvalidPath { path: path_str });
    }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = exe_path.metadata().map_err(|e| BuildError::ProcessError {
            command: path_str.clone(),
            error: e,
        })?;
        let permissions = metadata.permissions();
        if permissions.mode() & 0o111 == 0 {
            return Err(BuildError::InvalidPath {
                path: format!("{} (not executable)", path_str),
            });
        }
    }

    let mut command = Command::new(&path_str);
    let output = command.output().map_err(|e| BuildError::ProcessError {
        command: path_str.clone(),
        error: e,
    })?;

    // if !output.status.success() {
    //     return Err(BuildError::CompilationError {
    //         command: path_str,
    //         stdout: String::from_utf8_lossy(&output.stdout).to_string(),
    //         stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    //     });
    // }

    Ok(format!(
        "{stdout}\n{stderr}",
        stdout = String::from_utf8_lossy(&output.stdout).to_string(),
        stderr = String::from_utf8_lossy(&output.stderr).to_string(),
    ))
}

pub fn handle_build_error(error: &BuildError) {
    eprintln!("Build error: {}", error);

    match error {
        BuildError::ProcessError { command, error } => {
            if error.kind() == std::io::ErrorKind::NotFound {
                eprintln!(
                    "Hint: Make sure '{}' is installed and in your PATH",
                    command
                );
            }
        }
        BuildError::InvalidPath { path } => {
            eprintln!("Hint: Check that the file exists and has correct permissions");
        }
        BuildError::CompilationError { .. } => {
            eprintln!("Hint: Check the error messages above for compilation issues");
        }
    }
}

#[derive(Debug)]
pub enum AppError {
    Build(BuildError),
    ArgumentError(String),
    FileError { path: String, error: std::io::Error },
    ParseError(String),
    GenerationError(Option<String>, String),
}

impl Display for AppError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AppError::Build(err) => write!(f, "{}", err),
            AppError::ArgumentError(msg) => write!(f, "Argument error: {}", msg),
            AppError::FileError { path, error } => write!(f, "File error '{}': {}", path, error),
            AppError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            AppError::GenerationError(_, msg) => write!(f, "Code generation error: {}", msg),
        }
    }
}

impl std::error::Error for AppError {}

impl From<BuildError> for AppError {
    fn from(err: BuildError) -> Self {
        AppError::Build(err)
    }
}

pub fn parse_args() -> Result<(String, Vec<String>, bool, bool), AppError> {
    let mut args = args();

    args.next()
        .ok_or_else(|| AppError::ArgumentError("Executable name missing".to_string()))?;

    let mut compiler_flags = vec!["-no-pie".to_string()];
    let mut run = false;
    let mut use_c = false;
    let mut file_path: Option<String> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-C" => {
                let flag = args
                    .next()
                    .ok_or_else(|| AppError::ArgumentError("Expected flag after -C".to_string()))?;
                compiler_flags.push(flag);
            }
            arg if arg.starts_with("-C") => {
                if arg.len() <= 2 {
                    return Err(AppError::ArgumentError(
                        "Invalid -C flag format".to_string(),
                    ));
                }
                compiler_flags.push(arg[2..].to_string());
            }
            "-r" | "--run" => {
                run = true;
            }
            "--use-c" => {
                use_c = true;
            }
            "-h" | "--help" => {
                println!("Usage: chsc [OPTIONS] <input>");
                println!("Options:");
                println!("  -r, --run     Run the executable after compilation");
                println!();
                println!("  --use-c       Use C compiler for linking");
                println!("  -C <flag>     Pass flag to C compiler");
                println!("  -C<flag>      Pass flag to C compiler (no space)");
                println!();
                println!("  -h, --help    Show this help message");
                std::process::exit(0);
            }
            arg if arg.starts_with("-") => {
                return Err(AppError::ArgumentError(format!("Unknown option: {}", arg)));
            }
            _ => {
                if file_path.is_some() {
                    return Err(AppError::ArgumentError(format!(
                        "Unexpected extra argument: {}",
                        arg
                    )));
                }
                file_path = Some(arg);
            }
        }
    }

    let file_path = file_path
        .ok_or_else(|| AppError::ArgumentError("Usage: chsc [OPTIONS] <input>".to_string()))?;

    Ok((file_path, compiler_flags, run, use_c))
}

pub fn validate_input_file(file_path: &str) -> Result<(), AppError> {
    let path = std::path::Path::new(file_path);

    if !path.exists() {
        return Err(AppError::FileError {
            path: file_path.to_string(),
            error: std::io::Error::new(std::io::ErrorKind::NotFound, "File does not exist"),
        });
    }

    if !path.is_file() {
        return Err(AppError::FileError {
            path: file_path.to_string(),
            error: std::io::Error::new(std::io::ErrorKind::InvalidInput, "Path is not a file"),
        });
    }

    Ok(())
}

pub fn generate_output_paths(file_path: &str) -> Result<(PathBuf, PathBuf, PathBuf), AppError> {
    let input_path = PathBuf::from(file_path);

    let asm_path = input_path.with_extension("asm");
    let o_path = input_path.with_extension("o");
    let exe_path = input_path.with_extension("");

    if let Some(parent_dir) = input_path.parent() {
        if !parent_dir.exists() {
            return Err(AppError::FileError {
                path: parent_dir.to_string_lossy().to_string(),
                error: std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Output directory does not exist",
                ),
            });
        }
    }

    Ok((asm_path, o_path, exe_path))
}

pub fn handle_app_error(error: &AppError) {
    match error {
        AppError::Build(build_err) => handle_build_error(build_err),
        AppError::ArgumentError(_) => {
            eprintln!("Run with -h or --help for usage information");
        }
        AppError::FileError { path, error } => match error.kind() {
            std::io::ErrorKind::NotFound => {
                eprintln!("Hint: Check that the file '{}' exists", path);
            }
            std::io::ErrorKind::PermissionDenied => {
                eprintln!("Hint: Check file permissions for '{}'", path);
            }
            _ => {}
        },
        AppError::ParseError(error) => {
            eprintln!("{}", error);
        }
        AppError::GenerationError(hint, error) => {
            eprintln!("{error}");
            if let  Some(hint) = hint {
                eprintln!("Hint: {hint}");
            }
        }
    }
}
