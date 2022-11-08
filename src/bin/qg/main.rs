use clap::{Parser};
use crossbeam_channel;
use ignore;
use tracing::{Level, warn};
use tracing_subscriber::FmtSubscriber;
use std::path::{PathBuf};
use std::thread;

use ql_grep::query::{Query, parse_query};
use ql_grep::source_file::SourceFile;
use ql_grep::plan::build_query_plan;
use ql_grep::evaluate::{QueryResult, QueryResults, evaluate_plan};

mod cli;

/// Parse a query provided as either a literal string or a path to a file on
/// disk
fn make_query(query_string : &Option<String>, query_path : &Option<PathBuf>) -> anyhow::Result<Query> {
    match query_string {
        Some(s) => {
            parse_query(s)
        },
        None => {
            match query_path {
                Some(p) => {
                    let s = std::fs::read_to_string(p)?;
                    parse_query(s)
                },
                None => {
                    panic!("Impossible: one of query_string or query_path is guaranteed by the option parser")
                }
            }
        }
    }
}

fn visit_file(query : &Query,
              send : crossbeam_channel::Sender<QueryResults>,
              ent : Result<ignore::DirEntry, ignore::Error>) -> ignore::WalkState {
    match ent {
        Err(err) => {
            warn!("While parsing directory entry, `{}`", err);
        },
        Ok(dir_ent) => {
            match SourceFile::new(dir_ent.path()) {
                Err(err) => {
                    warn!("While parsing {}, {}", dir_ent.path().display(), err);
                },
                Ok(sf) => {
                    let query_plan = build_query_plan(sf.ast.language(), query).unwrap();
                    let result = evaluate_plan(&query_plan, sf).unwrap();
                    // Send the result to the aggregator thread
                    let _ = send.send(result);
                }
            }
        }
    }
    ignore::WalkState::Continue
}

struct Statistics {
    num_files_parsed : usize,
    num_matches : usize
}

impl Statistics {
    fn new() -> Self {
        Statistics {
            num_files_parsed: 0,
            num_matches: 0
        }
    }
}

struct FormatOptions {
    number_lines : bool
}

/// Print this result to the console
fn print_match(fmt_opts : &FormatOptions, sf : &SourceFile, qr : &QueryResult) {
    match qr {
        QueryResult::Constant(v) => {
            println!("{}", sf.file_path.display());
            println!("  {:?}", v);
        },
        QueryResult::Node(rng) => {
            println!("{}:{}-{}", sf.file_path.display(), rng.start_point.row, rng.end_point.row);
            let all_bytes = sf.source.as_bytes();
            let slice = &all_bytes[rng.start_byte .. rng.end_byte];
            match std::str::from_utf8(slice) {
                Err(_) => {
                    warn!("Error decoding string");
                },
                Ok(s) => {
                    if !fmt_opts.number_lines {
                        println!("{}", s);
                    } else {
                        let mut line_num = rng.start_point.row;
                        for line in s.lines() {
                            println!("{} {}", line_num, line);
                            line_num += 1;
                        }
                    }
                }
            }
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = cli::Cli::parse();

    // Set up logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::TRACE)
        .finish();
    tracing::subscriber::set_global_default(subscriber)
        .expect("setting default subscriber failed");

    let fmt_opts = FormatOptions {
        number_lines: args.number_lines
    };

    let cwd = std::env::current_dir()?;
    let root_dir = args.root.unwrap_or(cwd);

    let query = make_query(&args.query_string, &args.query_path)?;
    println!("Query: \n{:?}", query.query_ast.root_node().to_sexp());

    let (send, recv) = crossbeam_channel::bounded::<QueryResults>(4096);

    // Spawn a thread to collect all of the intermediate results produced by
    // worker threads
    let accumulator_handle = thread::spawn(move || {
        let mut stats = Statistics::new();

        loop {
            let item = recv.recv();
            match item {
                Err(_) => { warn!("Exiting accumulator"); break ; },
                Ok(qr) => {
                    stats.num_files_parsed += 1;
                    stats.num_matches += qr.results.len();
                    for res in qr.results {
                        print_match(&fmt_opts, &qr.source_file, &res);
                    }
                }
            }
        }

        stats
    });

    ignore::WalkBuilder::new(root_dir)
        .threads(8)
        .build_parallel()
        .run(|| {
            let q = &query;
            let sender = send.clone();
            Box::new(move |ent| visit_file(q, sender.clone(), ent))
        });

    // Drop the last send channel so that the accumulator will cleanly terminate
    drop(send);

    match accumulator_handle.join() {
        Err(err) => {
            std::panic::resume_unwind(err);
        },
        Ok(result) => {
            println!("Summary:");
            println!("  {} files parsed", result.num_files_parsed);
            println!("  {} objects matched", result.num_matches);
        }
    }

    Ok(())
}
