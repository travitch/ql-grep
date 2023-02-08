use clap::Parser;
use crossbeam_channel::{bounded, Sender};
use ignore::{DirEntry, WalkBuilder, WalkState};
use std::path::PathBuf;
use std::thread;
use tracing::{error, info, warn, Level};
use tracing_subscriber::{EnvFilter, FmtSubscriber};

use ql_grep::{
    compile_query, evaluate_plan, parse_query, plan_query, typecheck_query, QueryResult,
    Select, SourceFile, Syntax, Typed, LIBRARY_DATA,
};

mod cli;

/// Parse a query provided as either a literal string or a path to a file on
/// disk
fn make_query(
    query_string: &Option<String>,
    query_path: &Option<PathBuf>,
) -> anyhow::Result<Select<Syntax>> {
    match query_string {
        Some(s) => parse_query(s),
        None => match query_path {
            Some(p) => {
                let s = std::fs::read_to_string(p)?;
                parse_query(s)
            }
            None => {
                panic!("Impossible: one of query_string or query_path is guaranteed by the option parser")
            }
        },
    }
}

struct QueryResults {
    results: Vec<QueryResult>,
    source_file: SourceFile,
}

fn process_query(
    query: &Select<Typed>,
    sf: &SourceFile,
    ast: &tree_sitter::Tree,
) -> anyhow::Result<Vec<QueryResult>> {
    let mut cursor = tree_sitter::QueryCursor::new();
    let query_plan = plan_query(query)?;
    let compiled_query = compile_query(sf, ast, &query_plan)?;
    let result = evaluate_plan(sf, ast, &mut cursor, &compiled_query)?;
    Ok(result)
}

fn visit_file(
    query: &Select<Typed>,
    send: Sender<QueryResults>,
    ent: Result<DirEntry, ignore::Error>,
) -> WalkState {
    match ent {
        Err(err) => {
            info!("While parsing directory entry, `{}`", err);
        }
        Ok(dir_ent) => {
            match SourceFile::new(dir_ent.path()) {
                Err(err) => {
                    info!("While parsing {}, {}", dir_ent.path().display(), err);
                }
                Ok((sf, ast)) => {
                    let mut res_storage = Vec::new();
                    match process_query(query, &sf, &ast) {
                        Err(e) => {
                            error!("Error while parsing `{}`: {}", dir_ent.path().display(), e);
                        }
                        Ok(mut result) => {
                            res_storage.append(&mut result);
                        }
                    }

                    // Send the result to the aggregation thread
                    let qr = QueryResults {
                        results: res_storage,
                        source_file: sf,
                    };
                    let _ = send.send(qr);
                }
            }
        }
    }
    WalkState::Continue
}

struct Statistics {
    num_files_parsed: usize,
    num_matches: usize,
}

impl Statistics {
    fn new() -> Self {
        Statistics {
            num_files_parsed: 0,
            num_matches: 0,
        }
    }
}

struct FormatOptions {
    number_lines: bool,
}

/// Print this result to the console
fn print_match(fmt_opts: &FormatOptions, sf: &SourceFile, qr: &QueryResult) {
    match qr {
        QueryResult::Constant(v) => {
            println!("{}", sf.file_path.display());
            println!("  {v:?}");
        }
        QueryResult::Node(rng) => {
            println!(
                "{}:{}-{}",
                sf.file_path.display(),
                rng.start_point.row,
                rng.end_point.row
            );
            let all_bytes = sf.source.as_bytes();
            let slice = &all_bytes[rng.start_byte..rng.end_byte];
            match std::str::from_utf8(slice) {
                Err(_) => {
                    warn!("Error decoding string");
                }
                Ok(s) => {
                    if !fmt_opts.number_lines {
                        println!("{s}");
                    } else {
                        let mut line_num = rng.start_point.row;
                        for line in s.lines() {
                            println!("{line_num} {line}");
                            line_num += 1;
                        }
                    }
                }
            }
        }
    }
}

fn print_library() {
    println!("{LIBRARY_DATA}");
}

fn main() -> anyhow::Result<()> {
    let args = cli::Cli::parse();

    if args.print_library {
        print_library();
        std::process::exit(0);
    }

    // Set up logging
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::WARN)
        .with_env_filter(EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let fmt_opts = FormatOptions {
        number_lines: args.number_lines,
    };

    let cwd = std::env::current_dir()?;
    let root_dir = args.root.unwrap_or(cwd);

    let query = make_query(&args.query_string, &args.query_path)?;
    let typed_select = typecheck_query(query)?;
    let (send, recv) = bounded::<QueryResults>(4096);

    // Spawn a thread to collect all of the intermediate results produced by
    // worker threads
    let accumulator_handle = thread::spawn(move || {
        let mut stats = Statistics::new();

        loop {
            let item = recv.recv();
            match item {
                Err(_) => {
                    break;
                }
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

    WalkBuilder::new(root_dir)
        .threads(8)
        .build_parallel()
        .run(|| {
            let q = &typed_select;
            let sender = send.clone();
            Box::new(move |ent| visit_file(q, sender.clone(), ent))
        });

    // Drop the last send channel so that the accumulator will cleanly terminate
    drop(send);

    match accumulator_handle.join() {
        Err(err) => {
            std::panic::resume_unwind(err);
        }
        Ok(result) => {
            println!("Summary:");
            println!("  {} files parsed", result.num_files_parsed);
            println!("  {} objects matched", result.num_matches);
        }
    }

    Ok(())
}
