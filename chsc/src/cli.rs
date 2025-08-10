use std::{
    env,
    path::{Display, PathBuf},
    process::exit,
};

#[derive(Default)]
pub struct Cli {
    pub input_path: String,

    pub version: bool,

    pub help: bool,

    pub run: bool,
}

impl std::fmt::Display for Cli {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Usage: chsc [Flags] <file.chs>");
        writeln!(f, "Flags:");
        writeln!(f, "   -v, --version");
        writeln!(f, "   -h, --help");
        writeln!(f, "   -r, --run");
        Ok(())
    }
}

impl Cli {
    pub fn parse() -> Option<Self> {
        let mut cli = Self::default();
        let mut args = env::args().skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                "-v" | "--version" => {
                    cli.version = true;
                    return Some(cli);
                }
                "-h" | "--help" => {
                    cli.help = true;
                    return Some(cli);
                }
                "-r" | "--run" => {
                    cli.run = true;
                }
                flag if flag.starts_with("-") => {
                    cli.usage();
                    println!("Error: Unknow flag {arg}");
                    return None;
                }
                _ => {
                    cli.input_path = arg;
                    break;
                },
            }
        }

        if cli.input_path.is_empty() {
            cli.usage();
            println!("Error: No input file");
            return None;
        }

        Some(cli)
    }

    pub fn usage(&self) {
        print!("{self}")
    }
}
