/// Support for generating jsonl (line-delimited JSON) output
use ql_grep::{QueryResult, SourceFile};
use std::path::Path;
use serde::Serialize;

#[derive(Serialize)]
pub struct Location<'a> {
    source_file: &'a Path,
    start_row: usize,
    start_col: usize,
    end_row: usize,
    end_col: usize,
}

#[derive(Serialize)]
pub enum OutputValue<'a> {
    MatchedEntity {
        location: Location<'a>,
        matched_object: &'a str,
    },
    DecodeError {
        location: Location<'a>,
    },
    MatchedConstant {
        value: String,
    },
}

impl <'a> OutputValue<'a> {
    pub fn new(source_file: &'a SourceFile, result: &'a QueryResult) -> OutputValue<'a> {
        match result {
            QueryResult::Constant(c) => OutputValue::MatchedConstant { value: c.to_string() },
            QueryResult::Node(rng, _highlights) => {
                // FIXME: Could capture highlights in this as a list
                let loc = Location {
                    source_file: source_file.file_path.as_path(),
                    start_row: rng.start_point.row,
                    start_col: rng.start_point.column,
                    end_row: rng.end_point.row,
                    end_col: rng.end_point.column,
                };
                let all_bytes = source_file.source.as_bytes();
                let slice = &all_bytes[rng.start_byte .. rng.end_byte];
                match std::str::from_utf8(slice) {
                    Ok(s) => OutputValue::MatchedEntity {
                        location: loc,
                        matched_object: s,
                    },
                    Err(_) => OutputValue::DecodeError { location: loc }
                }
            }
        }
    }

    pub fn to_string(&self) -> anyhow::Result<String> {
        let json = serde_json::to_string(self)?;
        Ok(json)
    }
}
