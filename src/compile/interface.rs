use std::collections::HashMap;
use std::rc::Rc;
use tree_sitter::Node;

use crate::query::val_type::Type;

pub struct TopLevelMatcher {
    /// The Tree Sitter query to match the given "top-level" structure
    ///
    /// A "top-level" structure is a type declared in the From clause of the QL
    /// query that will ultimately be returned to the user.
    pub query: String,
    /// This is the name given to the top-level entity in the query, in case it
    /// needs to be referenced later.
    pub tag: String,
}

/// A reference type intended to be held in NodeFilters and related types
///
/// The carried value is a variable name.  We use this reference type instead of
/// holding tree sitter nodes directly because those introduce some awkward
/// lifetime parameters everywhere in a way that is hard to manage.  This type
/// is used as an index into a table during evaluation instead, where the table
/// (see the `EvaluationContext`) owns the actual nodes.
///
/// We also don't parse every node into a full data type because it would be far
/// too expensive.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallableRef(String);

impl CallableRef {
    pub fn new(var_name: &str) -> Self {
        CallableRef(var_name.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterRef(String);

impl ParameterRef {
    pub fn new(var_name: &str) -> Self {
        ParameterRef(var_name.into())
    }
}

/// References to variables that can be bound to an initial tree sitter node
///
/// This is used to communicate the variable(s) that need to be initialized in
/// the evaluator
pub enum BoundNode {
    Callable(CallableRef),
}

impl BoundNode {
    /// Construct a BoundNode given a type
    ///
    /// Not everything is supported here (just what would appear in a select
    /// clause)
    pub fn new(name: &str, ty: &Type) -> Self {
        match ty {
            Type::Callable => BoundNode::Callable(CallableRef(name.into())),
            Type::Function => BoundNode::Callable(CallableRef(name.into())),
            Type::Method => BoundNode::Callable(CallableRef(name.into())),
            _ => panic!("Unimplemented BoundNode type `{ty}`"),
        }
    }
}

/// All of the context needed during evaluation
///
/// These are largely tables of objects that would have lifetimes too complex to
/// track as references, so we track them by an integer ID instead
pub struct EvaluationContext<'a> {
    /// All of the nodes corresponding to the Callable currently under
    /// evaluation
    callables: HashMap<CallableRef, Node<'a>>,
    parameters: HashMap<ParameterRef, FormalArgument>,
}

impl<'a> EvaluationContext<'a> {
    pub fn new() -> Self {
        EvaluationContext {
            callables: HashMap::new(),
            parameters: HashMap::new(),
        }
    }

    /// Retrieve the node for a `CallableRef`
    ///
    /// This panics if there is no corresponding map entry, as it is a
    /// programming error
    pub fn lookup_callable(&self, cr: &CallableRef) -> &Node<'a> {
        self.callables.get(cr).unwrap()
    }

    pub fn bind_node<'b>(&'b mut self, binder: &BoundNode, n: Node<'a>) {
        match binder {
            BoundNode::Callable(CallableRef(name)) => {
                self.callables.insert(CallableRef(name.into()), n);
            }
        }
    }

    pub fn lookup_parameter(&self, pr: &ParameterRef) -> &FormalArgument {
        self.parameters.get(pr).unwrap()
    }

    pub fn bind_parameter(&mut self, pr: &ParameterRef, param: &FormalArgument) {
        self.parameters.insert(pr.clone(), param.clone());
    }

    /// Drop any bindings that are in the map
    ///
    /// This is meant to be used after evaluating a top-level node, as we don't
    /// need to carry any data between nodes.
    pub fn flush_bindings(&mut self) {
        self.callables.clear();
        self.parameters.clear();
    }
}

/// An interface for providing a query and a processor for the results to
/// produce a result of type `R`
///
/// For example, a query might select argument nodes and the matcher could
/// extract a list of argument structures.
///
/// FIXME: Add another field that records ranges to highlight in results (i.e.,
/// the code that contributes to a Node being included)
pub struct NodeMatcher<R> {
    pub extract: Rc<dyn for<'b, 'a> Fn(&'b mut EvaluationContext<'a>, &'a [u8]) -> R>,
}

/// A representation of language-level types (e.g., Java or C types)
///
/// This is a type for clarity in the data model. For now it is just a `String`,
/// but eventually we will probably want more structure
#[derive(Clone, Debug)]
pub struct LanguageType(String);

impl LanguageType {
    pub fn new(name: &str) -> Self {
        LanguageType(name.into())
    }

    pub fn as_type_string(&self) -> String {
        self.0.clone()
    }
}

/// The run-time type (i.e., something that can be returned by a NodeMatcher) for Parameters
///
/// Note that the name is optional because some languages allow unnamed
/// arguments to indicate that they are unused (e.g., C).
///
/// The type is optional because some languages are untyped.
#[derive(Clone, Debug)]
pub struct FormalArgument {
    pub name: Option<String>,
    pub declared_type: Option<LanguageType>,
    pub index: usize,
}

/// The interface for generating tree matchers for each language
///
/// While the primitives are very similar across all languages, the
/// language-specific ASTs have significant variation that makes generating
/// completely uniform queries impractical.
///
/// In cases where structure is sufficiently uniform, we should use normal
/// functions instead.
///
/// [tag:tree_interface_definition]
pub trait TreeInterface {
    /// Return a matcher for a type declared in the From clause of a QL query
    ///
    /// This can fail if the given type is not supported for the language
    /// implementing this interface.
    fn top_level_type(&self, t: &Type) -> Option<TopLevelMatcher>;

    /// A node matcher that extracts formal arguments from a callable node
    /// (e.g., a method or function)
    fn callable_arguments(
        &self,
        node: &NodeMatcher<CallableRef>,
    ) -> Option<NodeMatcher<Vec<FormalArgument>>>;

    /// A node matcher that extracts the name of a callable node.
    ///
    /// This is tailored to just callables because getting the name of other
    /// types of nodes requires different patterns
    fn callable_name(&self, node: &NodeMatcher<CallableRef>) -> Option<NodeMatcher<String>>;

    /// Returns True if the given callable contains a parse error (according to
    /// tree-sitter)
    fn callable_has_parse_error(&self, node: &NodeMatcher<CallableRef>) -> NodeMatcher<bool>;
}
