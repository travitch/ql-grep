use once_cell::sync::Lazy;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::interface::{NodeMatcher, TreeInterface};
use crate::compile::method_library::handler::Handler;
use crate::compile::NodeFilter;
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
    operands: &Vec<NodeFilter>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallsiteComputation(c) => {
            let get_callsite = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx| {
                    get_callsite(ctx).map(|callsite_res| {
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
    operands: &Vec<NodeFilter>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.len() == 1);
    match base {
        NodeFilter::CallsiteComputation(c) => {
            let get_callsite = Rc::clone(&c.extract);
            let get_arg_idx = match &operands[0] {
                NodeFilter::NumericComputation(num_comp) => Rc::clone(&num_comp.extract),
                nf => panic!("Invalid operand type: {}", nf.kind()),
            };
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx| {
                    get_arg_idx(ctx).and_then(|arg_idx_i| {
                        get_callsite(ctx).and_then(|callsite_res| {
                            let arg_list = callsite_res.value.arguments;
                            assert!(arg_idx_i.value >= 0);
                            let arg_idx = arg_idx_i.value as usize;
                            if arg_idx < arg_list.len() {
                                Some(WithRanges::value(arg_list[arg_idx]))
                            } else {
                                None
                            }
                        })
                    })
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
