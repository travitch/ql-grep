use crate::compile::interface::{CallableRef, FormalArgument, LanguageType, NodeMatcher};
use crate::query::ir::CachedRegex;

pub enum NodeFilter {
    Predicate(NodeMatcher<bool>),
    PredicateListComputation(NodeMatcher<Vec<bool>>),
    TypeComputation(NodeMatcher<LanguageType>),
    TypeListComputation(NodeMatcher<Vec<LanguageType>>),
    NumericComputation(NodeMatcher<i32>),
    StringComputation(NodeMatcher<String>),
    StringListComputation(NodeMatcher<Vec<String>>),
    RegexComputation(NodeMatcher<CachedRegex>),
    CallableComputation(NodeMatcher<CallableRef>),
    ArgumentComputation(NodeMatcher<FormalArgument>),
    ArgumentListComputation(NodeMatcher<Vec<FormalArgument>>),
}

impl NodeFilter {
    /// Return a string representation of the NodeFilter's carried type for debugging purposes
    pub fn kind(&self) -> String {
        match self {
            NodeFilter::Predicate(_) => "bool".into(),
            NodeFilter::PredicateListComputation(_) => "[bool]".into(),
            NodeFilter::TypeComputation(_) => "Type".into(),
            NodeFilter::TypeListComputation(_) => "[Type]".into(),
            NodeFilter::NumericComputation(_) => "int".into(),
            NodeFilter::StringComputation(_) => "string".into(),
            NodeFilter::StringListComputation(_) => "[string]".into(),
            NodeFilter::RegexComputation(_) => "Regex".into(),
            NodeFilter::CallableComputation(_) => "Callable".into(),
            NodeFilter::ArgumentComputation(_) => "Parameter".into(),
            NodeFilter::ArgumentListComputation(_) => "[Parameter".into(),
        }
    }
}
