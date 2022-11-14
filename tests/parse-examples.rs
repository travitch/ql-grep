#![cfg(test)]
extern crate test_generator;

use test_generator::test_resources;

use ql_grep::query::parse_query;
use ql_grep::query::typecheck::typecheck_query;

#[test_resources("examples/*.ql")]
fn parse_example(ql_file_contents : &str) -> anyhow::Result<()> {
    let query_syntax = parse_query(ql_file_contents)?;
    let _typed_query = typecheck_query(query_syntax.select)?;

    Ok(())
}
