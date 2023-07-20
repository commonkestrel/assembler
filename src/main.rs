pub(crate) mod ascii;
pub(crate) mod diagnostic;
mod lexer;
use diagnostic::Diagnostic;
pub(crate) use diagnostic::{ResultScream, OptionScream};

use std::sync::OnceLock;
use clap::Parser;
use clap_verbosity_flag::{ WarnLevel, Level };

#[derive(Parser, Debug)]
#[command()]
struct Args {
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity<WarnLevel>,
}

pub static VERBOSITY: OnceLock<Verbosity> = OnceLock::new();

fn main() {
    let args = Args::parse();
    let verbose = match args.verbose.log_level() {
        Some(level) => {
            match level {
                Level::Error => Verbosity::Error,
                Level::Warn => Verbosity::Warn,
                Level::Info => Verbosity::Help,
                Level::Debug | Level::Trace => Verbosity::Info,
            }
        },
        None => Verbosity::Quiet,
    };

    VERBOSITY.set(verbose).expect_or_scream("`VERBOSITY` should not be set");
    println!("{:?}", VERBOSITY.get());

    Diagnostic::error("hey something bad happened").emit();
    Diagnostic::warning("watch out for this pal").emit();
    Diagnostic::help("by the way, do this next time").emit();
    Diagnostic::note("checking disk 2").emit();
    Err::<usize, usize>(42).unwrap_or_scream();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verbosity {
    Quiet = 0,
    Error = 1,
    Warn = 2,
    Help = 3,
    Info = 4,
}

impl PartialOrd for Verbosity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (*self as u8).partial_cmp(&(*other as u8))
    }
}

impl Ord for Verbosity {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self as u8).cmp(&(*other as u8))
    }
}