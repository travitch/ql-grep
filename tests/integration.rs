#![cfg(test)]
use crossbeam_channel::{bounded, Sender};
use ignore::{DirEntry, WalkBuilder, WalkState};
use serde::Deserialize;
use std::rc::Rc;
use std::thread;
use test_generator::test_resources;
use toml::de;

use ql_grep::{
    compile_query, evaluate_plan, make_tree_interface, parse_query, plan_query, typecheck_query,
    QueryResult, SourceFile, TypedQuery,
};

/// A single test case to run ql-grep over, with expected results
///
/// The on-disk format is TOML [tag:toml-test-cases]
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
    results: Vec<QueryResult>,
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

fn visit_file(
    query: &TypedQuery,
    send: Sender<QueryResults>,
    ent: Result<DirEntry, ignore::Error>,
) -> WalkState {
    let dir_ent = ent.unwrap();
    match SourceFile::new(dir_ent.path()) {
        Err(_) => {}
        Ok((sf, ast)) => {
            let mut res_storage = Vec::new();
            {
                let mut cursor = tree_sitter::QueryCursor::new();
                let query_plan = plan_query(query).unwrap();
                let tree_interface = make_tree_interface(sf.lang);
                // If a query fails to compile for one source language, that is
                // potentially okay - it might just mean that a feature in the
                // query is not supported for all target languages.  The
                // expected results will let us know if the failure is
                // unexpected.
                match compile_query(
                    sf.lang,
                    ast.language(),
                    Rc::clone(&tree_interface),
                    &query_plan,
                ) {
                    Ok(compiled_query) => {
                        let mut result = evaluate_plan(
                            &sf,
                            &ast,
                            Rc::clone(&tree_interface),
                            &mut cursor,
                            &compiled_query,
                        )
                        .unwrap();
                        res_storage.append(&mut result);
                    }
                    Err(_) => {}
                }
            }
            // Send the result to the aggregation thread
            let qr = QueryResults {
                results: res_storage,
            };
            let _ = send.send(qr);
        }
    }

    WalkState::Continue
}

#[test_resources("tests/integration/*.toml")]
fn execute_query(toml_file_path: &str) {
    let toml_file_contents =
        std::fs::read_to_string::<std::path::PathBuf>(toml_file_path.into()).unwrap();
    let test_case: TestCase = de::from_str(&toml_file_contents).unwrap();

    let mut root_dir = std::env::current_dir().unwrap();
    root_dir.push("tests");
    root_dir.push("codebases");
    root_dir.push(test_case.codebase);

    let parsed_query = parse_query(test_case.query).unwrap();
    let typed_query = typecheck_query(&parsed_query).unwrap();

    let (send, recv) = bounded::<QueryResults>(4096);

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
        }
        Ok(result) => {
            assert_eq!(result.num_matches, test_case.num_matches);
        }
    }
}
