use tree_sitter::{Node, QueryMatches, TextProvider};

use crate::query::ir::{Type};
use crate::source_file::SourceFile;

pub struct TopLevelMatcher {
    /// The Tree Sitter query to match the given "top-level" structure
    ///
    /// A "top-level" structure is a type declared in the From clause of the QL
    /// query that will ultimately be returned to the user.
    pub query : String,
    /// This is the name given to the top-level entity in the query, in case it
    /// needs to be referenced later.
    pub tag : String
}

/// An interface for providing a query and a processor for the results to
/// produce a result of type `R`
///
/// For example, a query might select argument nodes and the matcher could
/// extract a list of argument structures.
pub struct NodeMatcher<'a, 'tree : 'a, T : TextProvider<'a>, R>
{
    pub query : String,
    pub extract : Box<dyn Fn(QueryMatches<'a, 'tree, T>) -> R>
}

pub struct FormalArgument<'a> {
    pub name : &'a str,
    pub declared_type : Option<&'a str>
}

/// The interface for generating tree matchers for each language
///
/// While the primitives are very similar across all languages, the
/// language-specific ASTs have significant variation that makes generating
/// completely uniform queries impractical.
///
/// In cases where structure is sufficiently uniform, we should use normal
/// functions instead.
pub trait TreeInterface<'a, 'tree, T : TextProvider<'a>> {
    /// The underlying source file
    fn source(&self) -> &'a SourceFile;

    /// Return a matcher for a type declared in the From clause of a QL query
    ///
    /// This can fail if the given type is not supported for the language
    /// implementing this interface.
    fn top_level_type(&self, t : &Type) -> Option<TopLevelMatcher>;

    /// A node matcher that extracts formal arguments from a callable node
    /// (e.g., a method or function)
    fn callable_arguments(&self) ->
        Option<NodeMatcher<'a, 'tree, T, Vec<FormalArgument<'a>>>>;
}
