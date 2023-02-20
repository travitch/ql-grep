use once_cell::sync::Lazy;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::interface::{NodeMatcher, TreeInterface};
use crate::compile::method_library::handler::Handler;
use crate::compile::NodeFilter;
use crate::query::ir::*;
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;

/// Get the (string) target of a call
///
/// This is usually the name of the function, but could be the name of a
/// function pointer in C/C++
///
/// Implements:
/// - [ref:library:Call:getTarget]
fn call_get_target(
    _ti: Rc<dyn TreeInterface>,
    base: &NodeFilter,
    operands: &Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallsiteComputation(c) => {
            let get_callsite = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_callsite(ctx, source).map(|callsite_res| {
                        WithRanges::new(callsite_res.value.target_name.clone(), vec![callsite_res.ranges])
                    })
                }),
            };
            Ok(NodeFilter::StringComputation(comp))
        }
        nf => {
            panic!("Impossible value for `call_get_target`: {}", nf.kind());
        }
    }
}

/// Get the argument at a given index in a call's argument list
///
/// Implements:
/// - [ref:library:Call:getArgument]
fn call_get_argument(
    _ti: Rc<dyn TreeInterface>,
    base: &NodeFilter,
    operands: &Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.len() == 1);
    match base {
        NodeFilter::CallsiteComputation(c) => {
            let get_callsite = Rc::clone(&c.extract);
            let arg_idx = match operands[0].expr {
                Expr_::ConstantExpr(Constant::Integer(i)) => {
                    assert!(i >= 0);
                    i as usize
                },
                // FIXME: Argument lists should be evaluated into NodeFilters at
                // this point so that we can just execute it to get a concrete
                // value
                _ => panic!("Invalid argument to Call.getArgument"),
            };
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_callsite(ctx, source).map(|callsite_res| {
                        let arg_list = callsite_res.value.arguments;
                        if arg_idx < arg_list.len() {
                            Some(WithRanges::value(arg_list[arg_idx]))
                        } else {
                            None
                        }
                    }).flatten()
                }),
            };
            Ok(NodeFilter::ExprComputation(comp))
        }
        nf => {
            panic!("Impossible value for `call_get_argument`: {}", nf.kind());
        }
    }
}

pub static CALL_METHODS : Lazy<Vec<(Type, &str, Handler)>> = Lazy::new(|| vec![
    (Type::Call, "getTarget", Handler(Arc::new(call_get_target))),
    (Type::Call, "getArgument", Handler(Arc::new(call_get_argument))),
]);
