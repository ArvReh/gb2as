//! Simple assembler for the Gameboy

#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]
#![warn(clippy::pedantic)]

use std::convert::From;
use std::fmt::{Debug,Formatter};
use std::io::{BufReader, self, Write};
use std::fs::File;
use std::path::{Path,PathBuf};

use argh::FromArgs;

mod parser;
mod emitter;
mod common;

use common::{Operand, SyntaxTree, Value};

enum Error {
    Io(std::io::Error),
    Parse(parser::Error),
    Emit(emitter::Error),
}

impl Debug for Error {
   fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Io(x) => write!(f, "File Error: {}", x.to_string()),
            Self::Parse(x) => write!(f, "Parse Error: {}", x),
            Self::Emit(x) => write!(f, "Emit Error: {}", x),
        }
   }
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self { Self::Parse(value) } 
}
impl From<emitter::Error> for Error {
    fn from(value: emitter::Error) -> Self { Self::Emit(value) } 
}
impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self { Self::Io(value) } 
}

/// A rudimentary assembler for the gameboy. Without the bells & whistles.
#[derive(FromArgs)]
struct AsmArgs {
    /// input file path to be assembled, '-' for stdin
    #[argh(positional)]
    file: String,

    /// output file path, default same name as input
    #[argh(option)]
    output: Option<String>,
}

fn main() -> Result<(),Error> {
    let args: AsmArgs = argh::from_env();

    // Parse
    let mut syntax_tree;
    if args.file == "-" {
        // Read source from stdin
        let reader = io::stdin().lock();
        syntax_tree = parser::parse(reader)?;
    } else {
        // Read source from file provided in cmdline argument 
        let reader = BufReader::new(File::open(Path::new(&args.file))?);
        syntax_tree = parser::parse(reader)?;
    }
    
    // Emit machine code
    let output_path = if let Some(s) = args.output {
        PathBuf::from(s)    
    } else {
        if args.file == "-" {
            PathBuf::from("output.gb")
        } else {
            Path::new(&args.file).with_extension("gb")
        }
    };
    let mut output = File::create(output_path)?;
    output.write_all(&emitter::emit(syntax_tree)?)?;
    Ok(())
}
