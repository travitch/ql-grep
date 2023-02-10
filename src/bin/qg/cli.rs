use clap::{arg, Parser};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(name = "qg")]
#[command(about = "A grep for source code built on tree-sitter", long_about = None)]
pub struct Cli {
    #[arg(value_name = "QUERY", help = "A literal CodeQL query", group = "query")]
    pub query_string: Option<String>,
    #[arg(short, long, help = "A file to read the query from", group = "query")]
    pub query_path: Option<PathBuf>,
    #[arg(short, long, help = "A directory to search from", value_name = "DIR")]
    pub root: Option<PathBuf>,
    #[arg(short, long, help = "Prefix each matched line with its line number")]
    pub number_lines: bool,
    #[arg(long, help = "Print out the supported CodeQL types and methods")]
    pub print_library: bool,
    #[arg(long, help = "A file to write log events to (instead of stderr)")]
    pub log_file: Option<PathBuf>,
    #[arg(long, help = "The number of threads to use (default: all)")]
    pub num_threads: Option<usize>,
}
