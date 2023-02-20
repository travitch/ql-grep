use once_cell::sync::Lazy;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::errors::PlanError;
use crate::compile::interface::TreeInterface;
use crate::compile::method_library::handler::Handler;
use crate::compile::NodeFilter;
use crate::query::ir::*;

/// Get the name of a callable using the language interface
///
/// Implements:
/// - [ref:library:Function:getName]
/// - [ref:library:Method:getName]
/// - [ref:library:Callable:getName]
fn callable_get_name<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    // This is not necessarily an assertion, as this may not be supported for
    // the current language (which we don't know).
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let name_matcher = ti
                .callable_name(callable_matcher)
                .ok_or_else(|| PlanError::NotSupported("getNames".into(), "callable".into()))?;
            Ok(NodeFilter::StringComputation(name_matcher))
        }
        _ => {
            panic!("Implementation error: only a callable should be possible here");
        }
    }
}

/// This is a relational method that returns a *list* of parameters
///
/// Implements:
/// - [ref:library:Function:getAParameter]
/// - [ref:library:Method:getAParameter]
/// - [ref:library:Callable:getAParameter]
fn callable_get_a_parameter<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let arg_matcher = ti
                .callable_arguments(callable_matcher)
                .ok_or_else(|| PlanError::NotSupported("arguments".into(), "callable".into()))?;
            Ok(NodeFilter::ArgumentListComputation(arg_matcher))
        }
        _ => {
            panic!("Implementation error: only a callable should be possible here");
        }
    }
}

/// Returns True if the method contains a parse error
///
/// Implements:
/// - [ref:library:Callable:hasParseError]
/// - [ref:library:Function:hasParseError]
/// - [ref:library:Method:hasParseError]
fn callable_has_parse_error<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let error_matcher = ti.callable_has_parse_error(callable_matcher);
            Ok(NodeFilter::Predicate(error_matcher))
        }
        _ => {
            panic!("Implementation error: only callables should be possible here");
        }
    }
}

/// Gets the return type of the callable
///
/// Implements:
/// - [ref:library:Callable:getType]
/// - [ref:library:Function:getType]
/// - [ref:library:Method:getType]
fn callable_get_return_type<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let ty_matcher = ti.callable_return_type(callable_matcher).ok_or_else(|| {
                PlanError::NotSupported("get-return-type".into(), "callable".into())
            })?;
            Ok(NodeFilter::TypeComputation(ty_matcher))
        }
        _ => {
            panic!("Implementation error: only callables should be possible here");
        }
    }
}

/// Get a reference to the file containing the callable.
///
/// Note that this doesn't actually do much, as there is only ever one file in
/// scope and the reference is in the evaluation context.
///
/// Implements:
/// - [ref:library:Callable:getFile]
/// - [ref:library:Function:getFile]
/// - [ref:library:Method:getFile]
fn callable_get_file<'a>(
    _ti: Rc<dyn TreeInterface>,
    _base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    Ok(NodeFilter::FileComputation)
}

/// Get all of the call sites in the given callable
///
/// Implements:
/// - [ref:library:Callable:getACall]
/// - [ref:library:Function:getACall]
/// - [ref:library:Method:getACall]
fn callable_call_sites<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(c) => {
            Ok(NodeFilter::CallsiteListComputation(ti.callable_call_sites(c)))
        }
        nf => {
            panic!("Impossible value type for `callable_call_sites`: {}", nf.kind());
        }
    }
}

pub static CALLABLE_METHODS : Lazy<Vec<(&str, Handler)>> = Lazy::new(|| vec![
    ("getName", Handler(Arc::new(callable_get_name))),
    ("getAParameter", Handler(Arc::new(callable_get_a_parameter))),
    ("hasParseError", Handler(Arc::new(callable_has_parse_error))),
    ("getFile", Handler(Arc::new(callable_get_file))),
    ("getType", Handler(Arc::new(callable_get_return_type))),
    ("getACall", Handler(Arc::new(callable_call_sites))),
]);
