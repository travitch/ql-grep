use crate::query::ir::*;
use crate::query::val_type::Type;
use crate::source_file::Language;

#[derive(thiserror::Error, Debug)]
pub enum PlanError {
    #[error("Expected 1 selected expression but found {0}")]
    NonSingletonSelect(usize),
    #[error("Unsupported target for a select expression: `{0:?}`")]
    UnsupportedSelectTarget(Expr_<Typed>),
    #[error("Unsupported type `{0:?}` for language {1:?}")]
    UnsupportedTypeForLanguage(Type, Language),
    #[error("Use of undefined variable `{0}`")]
    UndeclaredVariable(String),
    #[error("Invalid aggregate `{0:?}` with arity {1}")]
    InvalidAggregateArity(AggregateOp, usize),
    #[error("Querying {0} is not supported in {1} for this language")]
    NotSupported(String, String),
    #[error("Unsupported feature in compilation: {0}")]
    GeneralUnsupported(String),
}
