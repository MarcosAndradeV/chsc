use std::ffi::OsStr;
use std::process::{Command, Output};

pub const STDLIB_PATH: &str = "stdlib";

pub fn run_fasm<I, O>(input_path: I, output_path: O) -> Result<(), AppError>
where
    I: AsRef<OsStr>,
    O: AsRef<OsStr>,
{
    let input_str = input_path.as_ref().to_string_lossy();

    if !std::path::Path::new(&input_str.as_ref()).exists() {
        return Err(AppError::InvalidPath {
            path: input_str.to_string(),
        });
    }

    let mut command = Command::new("fasm");
    command.arg(input_path).arg(output_path);

    let output = command.output().map_err(|e| AppError::ProcessError {
        command: "fasm".to_string(),
        error: e,
    })?;

    if !output.status.success() {
        return Err(AppError::CompilationError {
            command: "fasm".to_string(),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        });
    }

    Ok(())
}

impl From<std::io::Error> for AppError {
    fn from(err: std::io::Error) -> Self {
        AppError::IoError(err)
    }
}

pub fn run_exe<I>(input_path: I) -> Result<(i32, String, String), AppError>
where
    I: AsRef<OsStr>,
{
    let output: Output = Command::new(&input_path).output().map_err(|e| {
        AppError::IoError(std::io::Error::new(
            e.kind(),
            format!(
                "Failed to start process '{}': {}",
                input_path.as_ref().to_string_lossy(),
                e
            ),
        ))
    })?;


        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let code = output.status.code().unwrap_or(-1);
        Ok((code, stdout, stderr))

}

#[derive(Debug)]
pub enum AppError {
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
    IoError(std::io::Error),
    Utf8Error(String),
    ExecutionFailed(i32, String, String), // (exit code, stderr)
    ArgumentError(String),
    FileError {
        path: String,
        error: std::io::Error,
    },
    ParseError(String),
    TypeError(String),
    GenerationError(Option<String>, String),
    ConfigParseError(String),
}

impl std::fmt::Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AppError::ProcessError { command, error } => {
                write!(f, "Failed to execute '{}': {}", command, error)
            }
            AppError::CompilationError {
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
            AppError::InvalidPath { path } => {
                write!(f, "Invalid path: {}", path)
            }
            AppError::IoError(e) => write!(f, "IO error: {}", e),
            AppError::Utf8Error(e) => write!(f, "UTF-8 conversion error: {}", e),
            AppError::ExecutionFailed(code, stdout, stderr) => {
                write!(
                    f,
                    "Process failed (exit code {}):\n{}{}",
                    code, stdout, stderr
                )
            }
            AppError::ArgumentError(msg) => write!(f, "Argument error: {}", msg),
            AppError::FileError { path, error } => write!(f, "File error '{}': {}", path, error),
            AppError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            AppError::TypeError(msg) => write!(f, "Type error: {}", msg),
            AppError::GenerationError(hint, msg) => {
                writeln!(f, "Generation Error: {msg}");
                if let Some(hint) = hint {
                    writeln!(f, "Hint: {hint}");
                }
                Ok(())
            }
            AppError::ConfigParseError(msg) => {
                write!(f, "Parse configuration file failed: {}", msg)
            }
        }
    }
}

impl std::error::Error for AppError {}

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

pub fn handle_app_error(error: &AppError) {
    eprintln!("{error}");
}
