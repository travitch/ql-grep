#[derive(Debug, Clone)]
pub struct WithRanges<T> {
    /// The value being carried by this wrapper
    pub value: T,
    /// The ranges that contributed to this value being selected
    ///
    /// This is primarily the source ranges of ground terms, but could also be
    /// any other syntactic indicator that a query should highlight in the
    /// results.
    pub ranges: Vec<tree_sitter::Range>,
}

impl <T> WithRanges<T> {
    /// Construct a `WithRanges` from just a value with an empty list of ranges
    pub fn value(t: T) -> Self {
        WithRanges {
            value: t,
            ranges: Vec::new(),
        }
    }

    pub fn new(t: T, rs: Vec<Vec<tree_sitter::Range>>) -> Self {
        let mut combined_ranges = Vec::new();
        for mut v in rs {
            combined_ranges.append(&mut v);
        }

        WithRanges {
            value: t,
            ranges: combined_ranges,
        }
    }

    pub fn new_single(t: T, r: tree_sitter::Range) -> Self {
        WithRanges {
            value: t,
            ranges: vec!(r),
        }
    }
}
