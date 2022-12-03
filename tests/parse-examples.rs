#![cfg(test)]
use test_generator::test_resources;

use ql_grep::query::parse_query;
use ql_grep::query::typecheck::typecheck_query;

/// For each test file, parse it and then typecheck the AST
///
/// A separate test suite compares execution against expected results
#[test_resources("examples/*.ql")]
fn parse_example(ql_file_path: &str) {
    let ql_file_contents =
        std::fs::read_to_string::<std::path::PathBuf>(ql_file_path.into()).unwrap();
    let query_syntax = parse_query(ql_file_contents).unwrap();
    let _typed_query = typecheck_query(query_syntax.select).unwrap();
}
