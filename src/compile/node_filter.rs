use crate::compile::interface::{
    CallableRef, Callsite, FormalArgument, LanguageType, NodeListMatcher, NodeMatcher,
};
use crate::preprocess::Import;
use crate::query::ir::CachedRegex;

pub enum NodeFilter {
    Predicate(NodeMatcher<bool>),
    PredicateListComputation(NodeListMatcher<bool>),
    TypeComputation(NodeMatcher<LanguageType>),
    TypeListComputation(NodeListMatcher<LanguageType>),
    NumericComputation(NodeMatcher<i32>),
    StringComputation(NodeMatcher<String>),
    StringListComputation(NodeListMatcher<String>),
    RegexComputation(NodeMatcher<CachedRegex>),
    CallableComputation(NodeMatcher<CallableRef>),
    CallsiteComputation(NodeMatcher<Callsite>),
    CallsiteListComputation(NodeListMatcher<Callsite>),
    ArgumentComputation(NodeMatcher<FormalArgument>),
    ArgumentListComputation(NodeListMatcher<FormalArgument>),
    ImportComputation(NodeMatcher<Import>),
    ImportListComputation(NodeListMatcher<Import>),
    /// Currently it is not possible to reference other files during evaluation
    /// (and it is unlikely that it ever will be possible), so no real
    /// information is required during evaluation (the File reference is in the
    /// evaluation context)
    FileComputation,
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
            NodeFilter::CallsiteComputation(_) => "Callsite".into(),
            NodeFilter::CallsiteListComputation(_) => "[Callsite]".into(),
            NodeFilter::ArgumentComputation(_) => "Parameter".into(),
            NodeFilter::ArgumentListComputation(_) => "[Parameter]".into(),
            NodeFilter::ImportComputation(_) => "Import".into(),
            NodeFilter::ImportListComputation(_) => "[Import]".into(),
            NodeFilter::FileComputation => "File".into(),
        }
    }
}
