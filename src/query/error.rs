use tree_sitter;

#[derive(thiserror::Error, Debug)]
pub enum QueryError {
    #[error("Error parsing CodeQL query")]
    QueryParseError,
    #[error("Missing expected child node type ({1}) in at {0:?}")]
    MissingExpectedChildNode(tree_sitter::Range, String),
    #[error("Invalid select with {1} children at {0:?}")]
    InvalidSelect(tree_sitter::Range, usize),
    #[error("Expected node type `{1}` at {2:?}, but found `{0}`")]
    UnexpectedNodeType(String, String, tree_sitter::Range),
    #[error("Malformed node of type `{0}` at {1:?}")]
    MalformedNode(String, tree_sitter::Range),
    #[error("Unknown variable type `{0}` declared at {1:?}")]
    UnknownVariableType(String, tree_sitter::Range),
    #[error("Unsupported type `{0}` at {1:?}")]
    UnsupportedType(String, tree_sitter::Range),
    #[error("Invalid comparison operator `{0}` at {1:?}")]
    InvalidComparisonOp(String, tree_sitter::Range),
    #[error("Invalid aggregate operator `{0}` at {1:?}")]
    InvalidAggregateOp(String, tree_sitter::Range),
}
