use clap::{Parser};
use crossbeam_channel;
use env_logger;
use ignore;
use log::{warn};
use std::path::{PathBuf};
use std::thread;

use ql_grep::query::{Query, parse_query};
use ql_grep::source_file::SourceFile;
use ql_grep::plan::build_query_plan;
use ql_grep::evaluate::evaluate_plan;

mod cli;

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
              send : crossbeam_channel::Sender<usize>,
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
                    let result = evaluate_plan(&query_plan, &sf).unwrap();
                    println!("Result: {:?}", result);
                    let _ = send.send(1);
                }
            }
        }
    }
    ignore::WalkState::Continue
}

fn main() -> anyhow::Result<()> {
    let args = cli::Cli::parse();

    env_logger::init();

    let cwd = std::env::current_dir()?;
    let root_dir = args.root.unwrap_or(cwd);

    let query = make_query(&args.query_string, &args.query_path)?;
    println!("Query: \n{:?}", query.query_ast.root_node().to_sexp());

    let (send, recv) = crossbeam_channel::bounded(4096);

    // Spawn a thread to collect all of the intermediate results produced by
    // worker threads
    let accumulator_handle = thread::spawn(move || {
        let mut num_parsed = 0;

        loop {
            let item = recv.recv();
            match item {
                Err(_) => { warn!("Exiting accumulator"); break ; },
                Ok(n) => {
                    num_parsed += n;
                }
            }
        }

        num_parsed
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
            println!("Parsed {} files", result);
        }
    }

    Ok(())
}
