/// This module contains an interface for different methods of printing query
/// matches.
///
/// This is intended to be flexible enough to print to the terminal, files, and
/// in alternative formats like JSON.

use gcollections::ops::bounded::Bounded;
use gcollections::ops::set::Intersection;
use interval::interval_set::{IntervalSet, ToIntervalSet};
use ql_grep::{QueryResult, SourceFile};
use std::io::Write;
use tracing::warn;

pub struct HighlightInfo<'a> {
    /// This is the offset of the current slice being printed from the start of
    /// the file.  The intent is that this is sufficient information to
    /// determine how much of the slice intersects with any of the highlighted
    /// regions.
    byte_offset: usize,
    /// The source range of the overall entity being printed
    entity_range: tree_sitter::Range,
    /// The ranges to highlight, as identified by the query evaluator
    ///
    /// These never change during printing
    highlights: &'a IntervalSet<usize>,
}

pub trait QueryResultPrinter {
    /// Write the given bytes to the writer
    ///
    /// The `bytes_offset` is the offset of this byte slice from the beginning
    /// of the file.  It is provided to interpret the highlighted ranges, which
    /// are specified as absolute region from the beginning of the file.
    ///
    /// Note that this is not a very clean abstraction because they meanings of
    /// some of the metadata change between printing layers. Therefore, this is
    /// not exposed outside of the module.
    fn write_result(&self, bytes: &[u8], info: &HighlightInfo, w: &mut Box<dyn Write>) -> anyhow::Result<()>;
}

/// This backend writes the bytes directly to the given `Writer` without any
/// additional interpretation.
pub struct PlainWriter {
}

impl PlainWriter {
    pub fn new() -> Self {
        PlainWriter {}
    }
}

impl QueryResultPrinter for PlainWriter {
    fn write_result(&self, bytes: &[u8], _info: &HighlightInfo, w: &mut Box<dyn Write>) -> anyhow::Result<()> {
        w.write_all(bytes)?;
        Ok(())
    }
}

/// This backend highlights the indicated regions in bold using ANSI escape sequences
pub struct ANSIWriter {
}

impl ANSIWriter {
    pub fn new() -> Self {
        ANSIWriter {}
    }
}

#[derive(Debug, Eq, PartialEq)]
enum HighlightSlices<'a> {
    /// An un-highlighted sub-slice
    Normal(&'a [u8]),
    /// A highlighted sub-slice
    Highlighted(&'a [u8]),
}

/// Split the given slice at the indices required by the highlight intervals.
///
/// The intervals are offset by `base_offset` relative to the start index of
/// `bytes` (which is 0).
fn split_slice_by_intervals<'a>(bytes: &'a [u8], base_offset: usize, highlight_intervals: &IntervalSet<usize>) -> Vec<HighlightSlices<'a>> {
    // A simple interval to cover the entire byte range available here
    // to extract the relevant highlight intervals
    let covering_intervals = vec!((base_offset, base_offset + bytes.len())).to_interval_set();
    let relevant_highlights = highlight_intervals.intersection(&covering_intervals);
    let mut current_offset = 0;
    let mut slices = Vec::new();
    for i in relevant_highlights {
        // If the current offset is not inside of the interval, we have to make
        // a normal slice first and then slice out the highlighted section.
        let adjusted_lower = i.lower() - base_offset;
        let adjusted_upper = i.upper() - base_offset;
        if current_offset < adjusted_lower {
            let normal = &bytes[current_offset .. adjusted_lower];
            slices.push(HighlightSlices::Normal(normal));
        }

        let highlighted = &bytes[adjusted_lower .. adjusted_upper];
        slices.push(HighlightSlices::Highlighted(highlighted));
        current_offset = adjusted_upper;
    }
    // We have processed all of the highlight intervals, so collect the
    // remainder of the input slice into one final normal slice.
    if current_offset < bytes.len() {
        let (_, tail) = bytes.split_at(current_offset);
        slices.push(HighlightSlices::Normal(tail));
    }

    slices
}

impl QueryResultPrinter for ANSIWriter {
    fn write_result(&self, bytes: &[u8], info: &HighlightInfo, w: &mut Box<dyn Write>) -> anyhow::Result<()> {
        // The interval set merges overlapping intervals, so we can aggressively
        // split lines based on any overlapping intervals
        let slices = split_slice_by_intervals(bytes, info.byte_offset, info.highlights);

        for slice in slices {
            match slice {
                HighlightSlices::Normal(normal_slice) => {
                    w.write_all(normal_slice)?;
                }
                HighlightSlices::Highlighted(hslice) => {
                    let style = ansi_term::Style::new().bold();
                    ansi_term::ANSIByteStrings(&[
                        style.paint(hslice),
                    ]).write_to(w)?;
                }
            }
        }
        Ok(())
    }
}

/// Print each line prefixed with a line number, delegating the writing of the
/// line content to another writer
pub struct LineNumberWriter {
    delegated_writer: Box<dyn QueryResultPrinter>,
}

impl LineNumberWriter {
    pub fn new(delegate_to: Box<dyn QueryResultPrinter>) -> Self {
        LineNumberWriter {
            delegated_writer: delegate_to,
        }
    }
}

impl QueryResultPrinter for LineNumberWriter {
    fn write_result(&self, bytes: &[u8], info: &HighlightInfo, w: &mut Box<dyn Write>) -> anyhow::Result<()> {
        // Split the bytes into strings and delegate the printing of each one to
        // the inner writer.
        //
        // Fix up the offsets in the ranges
        match std::str::from_utf8(bytes) {
            Err(_) => {
                // Just delegate since we cannot safely do this
                let range = info.entity_range;
                warn!("Error decoding bytes in range {range:?}");
                self.delegated_writer.write_result(bytes, info, w)?;
            }
            Ok(s) => {
                let mut line_number = info.entity_range.start_point.row;
                let mut this_offset = info.byte_offset;
                for line in s.lines() {
                    let line_bytes = line.as_bytes();
                    write!(w, "{line_number}: ")?;
                    let this_info = HighlightInfo {
                        byte_offset: this_offset,
                        entity_range: info.entity_range,
                        highlights: info.highlights,
                    };
                    self.delegated_writer.write_result(line_bytes, &this_info, w)?;
                    // Insert a newline manually because we have trimmed them off
                    writeln!(w)?;
                    line_number += 1;
                    // We increase the byte offset by 1 to account for the newline.
                    //
                    // FIXME: This might need to account for Windows line endings
                    this_offset = this_offset + line_bytes.len() + 1;
                }
            }
        }

        Ok(())
    }
}

pub fn print_query_result(
    printer: &dyn QueryResultPrinter,
    source_file: &SourceFile,
    qr: &QueryResult,
    w: &mut Box<dyn Write>,
) -> anyhow::Result<()> {
    match qr {
        QueryResult::Constant(v) => {
            writeln!(w, "{}", source_file.file_path.display())?;
            writeln!(w, "  {v}")?;
        }
        QueryResult::Node(rng, highlights) => {
            writeln!(w, "{}:{}-{}", source_file.file_path.display(), rng.start_point.row, rng.end_point.row)?;
            let all_bytes = source_file.source.as_bytes();
            let slice = &all_bytes[rng.start_byte .. rng.end_byte];

            let highlight_intervals =
                highlights
                    .iter()
                    .map(|h| (h.start_byte, h.end_byte))
                    .collect::<Vec<_>>()
                    .to_interval_set();

            let info = HighlightInfo {
                byte_offset: rng.start_byte,
                entity_range: *rng,
                highlights: &highlight_intervals,
            };
            printer.write_result(slice, &info, w)?;
        }
    }

    writeln!(w)?;
    Ok(())
}


#[test]
fn test_split_empty_intervals() {
    let full_string = "word1 word2 word3";
    let empty_intervalset = Vec::new().to_interval_set();
    let res = split_slice_by_intervals(full_string.as_bytes(), 0, &empty_intervalset);
    let expected = vec!(HighlightSlices::Normal(full_string.as_bytes()));
    assert_eq!(expected, res);
}

#[test]
fn test_split_highlight_all() {
    let full_string = "word1 word2 word3";
    let highlights = vec!((0, 1000)).to_interval_set();
    let res = split_slice_by_intervals(full_string.as_bytes(), 0, &highlights);
    let expected = vec!(HighlightSlices::Highlighted(full_string.as_bytes()));
    assert_eq!(expected, res);
}

#[test]
fn test_split_highlight_first_byte_only() {
    let full_string = "word1 word2 word3";
    let highlights = vec!((0, 1)).to_interval_set();
    let res = split_slice_by_intervals(full_string.as_bytes(), 0, &highlights);
    let expected = vec!(
        HighlightSlices::Highlighted("w".as_bytes()),
        HighlightSlices::Normal(full_string[1..].as_bytes()),
        );
    assert_eq!(expected, res);
}

#[test]
fn test_split_highlight_last_byte_only() {
    let full_string = "word1 word2 word3";
    let highlights = vec!((16, 17)).to_interval_set();
    let res = split_slice_by_intervals(full_string.as_bytes(), 0, &highlights);
    let expected = vec!(
        HighlightSlices::Normal(full_string[..16].as_bytes()),
        HighlightSlices::Highlighted(full_string[16..].as_bytes()),
        );
    assert_eq!(expected, res);
}

#[test]
fn test_split_highlight_all_but_last_byte() {
    let full_string = "word1 word2 word3";
    let highlights = vec!((0, 16)).to_interval_set();
    let res = split_slice_by_intervals(full_string.as_bytes(), 0, &highlights);
    let expected = vec!(
        HighlightSlices::Highlighted(full_string[..16].as_bytes()),
        HighlightSlices::Normal(full_string[16..].as_bytes()),
        );
    assert_eq!(expected, res);
}

#[test]
fn test_split_highlight_interior() {
    let full_string = "word1 word2 word3";
    let highlights = vec!((6, 11)).to_interval_set();
    let res = split_slice_by_intervals(full_string.as_bytes(), 0, &highlights);
    let expected = vec!(
        HighlightSlices::Normal("word1 ".as_bytes()),
        HighlightSlices::Highlighted("word2".as_bytes()),
        HighlightSlices::Normal(" word3".as_bytes()),
        );
    assert_eq!(expected, res);
}
