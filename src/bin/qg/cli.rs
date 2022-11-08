use clap::{Parser, arg};
use std::path::{PathBuf};

#[derive(Debug, Parser)]
#[command(name = "tg")]
#[command(about = "A grep for source code built on tree-sitter", long_about = None)]
pub struct Cli {
    #[arg(value_name = "QUERY", help = "A literal CodeQL query", group = "query")]
    pub query_string : Option<String>,
    #[arg(short, long, help = "A file to read the query from", group = "query")]
    pub query_path : Option<PathBuf>,
    #[arg(short, long, help = "A directory to search from", value_name = "DIR")]
    pub root : Option<PathBuf>,
}