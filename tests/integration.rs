#![cfg(test)]
use crossbeam_channel::{Sender, bounded};
use ignore::{DirEntry, WalkBuilder, WalkState};
use serde::Deserialize;
use test_generator::test_resources;
use std::thread;
use toml::de;

use ql_grep::query::{Query, parse_query};
use ql_grep::query::ir::Typed;
use ql_grep::query::typecheck::typecheck_query;
use ql_grep::plan::build_query_plan;
use ql_grep::evaluate::{QueryResult, evaluate_plan};
use ql_grep::source_file::SourceFile;

/// A single test case to run ql-grep over, with expected results
#[derive(Deserialize)]
struct TestCase {
    /// The query to run
    query: String,
    /// The name of the codebase (the path will be constructed by the test harness)
    codebase: String,
    /// The number of matches the query should produce
    num_matches: usize,
}

struct QueryResults {
    results : Vec<QueryResult>,
    source_file : SourceFile
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

fn visit_file(query : &Query<Typed>,
              send : Sender<QueryResults>,
              ent : Result<DirEntry, ignore::Error>) -> WalkState
{
    let dir_ent = ent.unwrap();
    match SourceFile::new(dir_ent.path()) {
        Err(_) => {

        },
        Ok((sf, ast)) => {
            let mut res_storage = Vec::new();
            {
                let mut cursor = tree_sitter::QueryCursor::new();
                let query_plan = build_query_plan(&sf, &ast, query).unwrap();
                let mut result = evaluate_plan(&sf, &ast, &mut cursor, &query_plan).unwrap();
                res_storage.append(&mut result);
            }
            // Send the result to the aggregation thread
            let qr = QueryResults {
                results: res_storage,
                source_file: sf
            };
            let _ = send.send(qr);
        }
    }



    WalkState::Continue
}

#[test_resources("tests/integration/*.toml")]
fn execute_query(toml_file_path : &str) {
    let toml_file_contents = std::fs::read_to_string::<std::path::PathBuf>(toml_file_path.into()).unwrap();
    let test_case : TestCase = de::from_str(&toml_file_contents).unwrap();

    let mut root_dir = std::env::current_dir().unwrap();
    root_dir.push("tests");
    root_dir.push("codebases");
    root_dir.push(test_case.codebase);

    let parsed_query = parse_query(test_case.query).unwrap();
    let typed_select = typecheck_query(parsed_query.select).unwrap();
    let typed_query = Query {
        query_ast: parsed_query.query_ast,
        select: typed_select,
    };

    let (send, recv) = bounded::<QueryResults>(4096);

    let accumulator_handle = thread::spawn(move || {
        let mut stats = Statistics::new();

        loop {
            let item = recv.recv();
            match item {
                Err(_) => { break ; },
                Ok(qr) => {
                    stats.num_files_parsed += 1;
                    stats.num_matches += qr.results.len();
                }
            }
        }

        stats
    });

    WalkBuilder::new(root_dir)
        .threads(4)
        .build_parallel()
        .run(|| {
            let q = &typed_query;
            let sender = send.clone();
            Box::new(move |ent| visit_file(q, sender.clone(), ent))
        });

    drop(send);

    match accumulator_handle.join() {
        Err(err) => {
            std::panic::resume_unwind(err);
        },
        Ok(result) => {
            assert_eq!(result.num_matches, test_case.num_matches);
        }
    }
}
