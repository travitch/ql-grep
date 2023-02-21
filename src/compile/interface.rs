use std::collections::HashMap;
use std::rc::Rc;
use tree_sitter::Node;

use crate::preprocess::{FileImportIndex, Import};
use crate::query::val_type::Type;
use crate::query::ir::VarIdent;
use crate::with_ranges::WithRanges;

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
pub struct CallableRef(VarIdent);

impl CallableRef {
    pub fn new(var_name: VarIdent) -> Self {
        CallableRef(var_name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterRef(VarIdent);

impl ParameterRef {
    pub fn new(var_name: VarIdent) -> Self {
        ParameterRef(var_name)
    }
}

/// A reference to an expression node
///
/// The nodes themselves are stored in the `EvaluationContext` so that they can
/// be lazily decoded.
///
/// Note that the reference id is the node id from tree-sitter
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct ExprRef(usize);

impl ExprRef {
    pub fn new(id: usize) -> Self {
        ExprRef(id)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct CallsiteRef(VarIdent);

impl CallsiteRef {
    pub fn new(var_name: VarIdent) -> Self {
        CallsiteRef(var_name)
    }
}

/// All of the context needed during evaluation
///
/// These are largely tables of objects that would have lifetimes too complex to
/// track as references, so we track them by an integer ID instead
pub struct EvaluationContext<'a> {
    /// The bytes of the source file, which are used to interpret AST nodes
    source_bytes: &'a [u8],
    /// All of the nodes corresponding to the Callable currently under
    /// evaluation
    callables: HashMap<CallableRef, Node<'a>>,
    /// All of the parsed parameters for the given function
    parameters: HashMap<ParameterRef, WithRanges<FormalArgument>>,
    /// References to (not-yet interpreted) expression nodes
    expr_nodes: HashMap<ExprRef, Node<'a>>,
    /// Callsites bound to variables
    callsites: HashMap<CallsiteRef, WithRanges<Callsite>>,
    /// The index of the imports in the current file
    ///
    /// This is an `Option` to distinguish the case of "did not run the
    /// analysis" from "the file has no imports".  This enables us to panic if
    /// we needed to run the analysis but did not and detect those
    /// implementation errors.
    file_imports: Option<FileImportIndex>,
}

impl<'a> EvaluationContext<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        EvaluationContext {
            source_bytes: source,
            callables: HashMap::new(),
            parameters: HashMap::new(),
            expr_nodes: HashMap::new(),
            callsites: HashMap::new(),
            file_imports: None,
        }
    }

    pub fn get_source(&self) -> &'a [u8] {
        self.source_bytes
    }

    pub fn add_expression_bindings(&mut self, bindings: Vec<(ExprRef, Node<'a>)>) {
        for (eref, node) in bindings {
            self.expr_nodes.insert(eref, node);
        }
    }

    pub fn lookup_expression(&self, expr_id: &ExprRef) -> Node<'a> {
        *self.expr_nodes.get(expr_id).unwrap()
    }

    pub fn attach_file_import_index(&mut self, file_imports: FileImportIndex) {
        self.file_imports = Some(file_imports);
    }

    /// Retrieve the node for a `CallableRef`
    ///
    /// This panics if there is no corresponding map entry, as it is a
    /// programming error
    pub fn lookup_callable(&self, cr: &CallableRef) -> &Node<'a> {
        self.callables.get(cr).unwrap()
    }

    pub fn bind_node<'b>(&'b mut self, binder: &CallableRef, n: Node<'a>) {
        self.callables.insert(binder.clone(), n);
    }

    pub fn lookup_parameter(&self, pr: &ParameterRef) -> &WithRanges<FormalArgument> {
        self.parameters.get(pr).unwrap()
    }

    pub fn bind_parameter(&mut self, pr: &ParameterRef, param: &WithRanges<FormalArgument>) {
        self.parameters.insert(pr.clone(), param.clone());
    }

    pub fn lookup_callsite(&self, cr: &CallsiteRef) -> &WithRanges<Callsite> {
        self.callsites.get(cr).unwrap()
    }

    pub fn bind_callsite(&mut self, cr: &CallsiteRef, callsite: &WithRanges<Callsite>) {
        self.callsites.insert(cr.clone(), callsite.clone());
    }

    /// Drop any bindings that are in the map
    ///
    /// This is meant to be used after evaluating a top-level node, as we don't
    /// need to carry any data between nodes.
    pub fn flush_bindings(&mut self) {
        self.callables.clear();
        self.parameters.clear();
    }

    pub fn imports(&self) -> &[Import] {
        let idx = self.file_imports.as_ref().unwrap_or_else(|| {
            panic!("The query plan required a computed FileImportIndex, but it was not available")
        });
        idx.imports()
    }
}

/// An interface for providing a query and a processor for the results to
/// produce a result of type `R`
///
/// For example, a query might select argument nodes and the matcher could
/// extract a list of argument structures.
pub struct NodeMatcher<R> {
    pub extract: Rc<dyn for<'b, 'a> Fn(&'b mut EvaluationContext<'a>) -> Option<WithRanges<R>>>,
}

/// A variant of `NodeMatcher` for sequences/relations
///
/// Note that, unlike the scalar `NodeMatcher`, this version does not need an
/// `Option` to indicate failure. It can simply return an empty sequence.
pub struct NodeListMatcher<R> {
    pub extract:
        Rc<dyn for<'b, 'a> Fn(&'b mut EvaluationContext<'a>) -> Vec<WithRanges<R>>>,
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

/// The evaluation-time representation of a call site
#[derive(Clone, Debug)]
pub struct Callsite {
    /// The name of the callee; note that this could be a real function name or
    /// a reference to a local variable (in the case of a function pointer)
    pub target_name: String,
    /// The argument expressions
    pub arguments: Vec<ExprRef>,
}

impl Callsite {
    pub fn new(name: String, args: Vec<ExprRef>) -> Self {
        Self {
            target_name: name,
            arguments: args,
        }
    }
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

    /// Parse all of the imports in the file and return an index of them.
    ///
    /// Note that this is meant to be used pre-evaluation, rather than during
    /// compilation.
    fn file_imports(&self, root: &Node, source: &[u8]) -> FileImportIndex;

    /// A node matcher that extracts formal arguments from a callable node
    /// (e.g., a method or function)
    fn callable_arguments(
        &self,
        node: &NodeMatcher<CallableRef>,
    ) -> Option<NodeListMatcher<FormalArgument>>;

    /// A node matcher that extracts the name of a callable node.
    ///
    /// This is tailored to just callables because getting the name of other
    /// types of nodes requires different patterns
    fn callable_name(&self, node: &NodeMatcher<CallableRef>) -> Option<NodeMatcher<String>>;

    /// Returns True if the given callable contains a parse error (according to
    /// tree-sitter)
    fn callable_has_parse_error(&self, node: &NodeMatcher<CallableRef>) -> NodeMatcher<bool>;

    /// Returns the return type of the callable
    fn callable_return_type(
        &self,
        node: &NodeMatcher<CallableRef>,
    ) -> Option<NodeMatcher<LanguageType>>;

    /// Return the callsites in the given callable
    fn callable_call_sites(&self, node: &NodeMatcher<CallableRef>) -> NodeListMatcher<Callsite>;

    /// Returns true if the given node is a string literal expression
    fn expr_is_string_literal(&self, node: &Node) -> bool;
}
