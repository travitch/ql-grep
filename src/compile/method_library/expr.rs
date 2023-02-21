use once_cell::sync::Lazy;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::interface::{NodeMatcher, TreeInterface};
use crate::compile::method_library::handler::Handler;
use crate::compile::NodeFilter;
use crate::query::ir::*;
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;

/// Returns true if the given expression is a string literal
///
/// Implements:
/// - [ref:library:Expr:isStringLiteral]
fn expr_is_string_literal(
    ti: Rc<dyn TreeInterface>,
    base: &NodeFilter,
    operands: &Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ExprComputation(c) => {
            let get_expr = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx| {
                    get_expr(ctx).map(|expr_ref| {
                        let expr_node = ctx.lookup_expression(&expr_ref.value);
                        WithRanges::new_single(ti.expr_is_string_literal(&expr_node), expr_node.range())
                    })
                }),
            };
            Ok(NodeFilter::Predicate(comp))
        }
        nf => {
            panic!("Impossible value for call `expr_is_string_literal`: {}", nf.kind());
        }
    }
}

pub static EXPR_METHODS : Lazy<Vec<(Type, &str, Handler)>> = Lazy::new(|| vec![
    (Type::Expr, "isStringLiteral", Handler(Arc::new(expr_is_string_literal))),
]);
