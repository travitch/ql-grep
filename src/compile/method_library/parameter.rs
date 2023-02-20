use once_cell::sync::Lazy;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::interface::{LanguageType, NodeMatcher, TreeInterface};
use crate::compile::method_library::handler::Handler;
use crate::compile::NodeFilter;
use crate::query::ir::*;
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;


/// Get the name of a formal parameter (of a callable object)
///
/// Implements:
/// - [ref:library:Parameter:getName]
fn parameter_get_name<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let get_argument = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_argument(ctx, source).map(|argument_result| {
                        WithRanges::new(
                            argument_result
                                .value
                                .name
                                .unwrap_or_else(|| "<none>".into()),
                            vec![argument_result.ranges],
                        )
                    })
                }),
            };
            Ok(NodeFilter::StringComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getName");
        }
    }
}

/// Get the type of a formal parameter (of a callable object)
///
/// Implements:
/// - [ref:library:Parameter:getType]
fn parameter_get_type<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let get_argument = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    // FIXME: Probably want to have a better (i.e., more
                    // structured) representation if the type is missing
                    let default_ty = LanguageType::new("<Any>");
                    get_argument(ctx, source).map(|argument_result| {
                        WithRanges::new(
                            argument_result.value.declared_type.unwrap_or(default_ty),
                            vec![argument_result.ranges],
                        )
                    })
                }),
            };

            Ok(NodeFilter::TypeComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getType");
        }
    }
}

/// Get the index of the parameter in the parameter list
///
/// Implements:
/// - [ref:library:Parameter:getIndex]
fn parameter_get_index<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let get_argument = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_argument(ctx, source).map(|argument_result| {
                        WithRanges::new(
                            argument_result.value.index as i32,
                            vec![argument_result.ranges],
                        )
                    })
                }),
            };
            Ok(NodeFilter::NumericComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getIndex");
        }
    }
}

pub static PARAMETER_METHODS : Lazy<Vec<(Type, &str, Handler)>> = Lazy::new(|| vec![
    (Type::Parameter, "getName", Handler(Arc::new(parameter_get_name))),
    (Type::Parameter, "getType", Handler(Arc::new(parameter_get_type))),
    (Type::Parameter, "getIndex", Handler(Arc::new(parameter_get_index))),
]);
