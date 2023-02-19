use clap::{CommandFactory, Parser};
use crossbeam_channel::{bounded, Receiver, Sender};
use ignore::{DirEntry, WalkBuilder, WalkState};
use is_terminal::IsTerminal;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Mutex;
use std::thread;
use tracing::{error, info, Level};
use tracing_subscriber::FmtSubscriber;

use ql_grep::{
    compile_query, evaluate_plan, make_tree_interface, parse_query, plan_query, typecheck_query, QueryPlan, QueryResult, Select,
    SourceError, SourceFile, Syntax, TreeInterface, TypedQuery, LIBRARY_DATA,
};

mod cli;
mod result_printer;

/// Create a query result printer by parsing out the chosen command line arguments
fn make_result_printer(args: cli::Cli, is_term_connected: bool) -> Box<dyn result_printer::QueryResultPrinter> {
    let base_printer : Box<dyn result_printer::QueryResultPrinter> = if args.disable_ansi || !is_term_connected {
        Box::new(result_printer::PlainWriter::new())
    } else {
        Box::new(result_printer::ANSIWriter::new())
    };

    if args.number_lines {
        return Box::new(result_printer::LineNumberWriter::new(base_printer))
    }

    base_printer
}

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
    query_plan: &QueryPlan,
    sf: &SourceFile,
    ast: &tree_sitter::Tree,
) -> anyhow::Result<Vec<QueryResult>> {
    let mut cursor = tree_sitter::QueryCursor::new();
    let tree_interface: Rc<dyn TreeInterface> = make_tree_interface(sf.lang);
    let compiled_query = compile_query(sf.lang, ast.language(), Rc::clone(&tree_interface), &query_plan)?;
    let result = evaluate_plan(sf, ast, Rc::clone(&tree_interface), &mut cursor, &compiled_query)?;
    Ok(result)
}

fn visit_file(
    query_plan: &QueryPlan,
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
                    match err.downcast_ref::<SourceError>() {
                        Some(SourceError::MissingExtension(p)) => {
                            // Don't bother warning if the path is a directory
                            if !p.is_dir() {
                                info!("No extension for file {}", p.as_path().display());
                            }
                        }
                        _ => {
                            info!("While parsing {}, {}", dir_ent.path().display(), err);
                        }
                    }
                }
                Ok((sf, ast)) => {
                    let mut res_storage = Vec::new();
                    match process_query(query_plan, &sf, &ast) {
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

fn spawn_accumulator_thread(
    args: cli::Cli,
    recv: Receiver<QueryResults>,
) -> thread::JoinHandle<Statistics> {
    thread::spawn(move || {
        let mut stats = Statistics::new();
        let mut output_dest : Box<dyn std::io::Write> = Box::new(std::io::stdout());
        let result_writer = make_result_printer(args, std::io::stdout().is_terminal());

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
                        result_printer::print_query_result(&result_writer, &qr.source_file, &res, &mut output_dest)
                            .unwrap_or_else(|e| {
                                error!("Error while printing a result: {:?}", e);
                            });
                    }
                }
            }
        }

        stats
    })
}

fn print_library() {
    println!("{LIBRARY_DATA}");
}

fn initialize_logging(log_file_path: &Option<PathBuf>) -> anyhow::Result<()> {
    let subscriber_builder = FmtSubscriber::builder()
        .with_max_level(Level::DEBUG);
    match log_file_path {
        None => {
            let subscriber = subscriber_builder.with_writer(std::io::stderr).finish();
            tracing::subscriber::set_global_default(subscriber)
                .expect("setting default subscriber failed");
        }
        Some(p) => {
            let f = File::create(p)?;
            let subscriber = subscriber_builder.with_writer(Mutex::new(f)).finish();
            tracing::subscriber::set_global_default(subscriber)
                .expect("setting default subscriber failed");
        }
    };
    Ok(())
}

fn print_query_ir(query: &Select<Syntax>, typed_query: &TypedQuery, query_plan: &QueryPlan) {
    println!("# Parsed Query");
    println!("{}", query.to_pretty(100));

    println!("\n# Typechecked Query");
    println!("{}", typed_query.query.to_pretty(100));

    println!("\n# Query Plan");
    println!("{}", query_plan.to_pretty(100));
}

fn main() -> anyhow::Result<()> {
    let args = cli::Cli::parse();

    if args.print_library {
        print_library();
        std::process::exit(0);
    }

    if args.print_manpage {
        let manpage = clap_mangen::Man::new(cli::Cli::command());
        let mut buffer = Vec::new();
        manpage.render(&mut buffer)?;
        std::io::stdout().write(buffer.as_slice())?;
        std::process::exit(0);
    }

    // Set up logging
    initialize_logging(&args.log_file)?;

    let thread_count = args.num_threads
        .clone()
        .unwrap_or(std::cmp::max(1, num_cpus::get()) - 1);

    let cwd = std::env::current_dir()?;
    let root_dir = args.root.clone().unwrap_or(cwd);

    let query = make_query(&args.query_string, &args.query_path)?;
    let typed_query = typecheck_query(&query)?;
    let query_plan = plan_query(&typed_query)?;

    if args.print_query_ir {
        print_query_ir(&query, &typed_query, &query_plan);
        std::process::exit(0);
    }

    let (send, recv) = bounded::<QueryResults>(4096);

    // Spawn a thread to collect all of the intermediate results produced by
    // worker threads over the channel.
    let accumulator_handle = spawn_accumulator_thread(args.clone(), recv);

    // Run in parallel, but reserve at least one core if the user has not
    // explicitly requested a number of threads to avoid totally drowning the
    // system
    WalkBuilder::new(root_dir)
        .threads(thread_count)
        .build_parallel()
        .run(|| {
            let q = &query_plan;
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
