use std::rc::Rc;
use std::sync::Arc;

use crate::compile::interface::TreeInterface;
use crate::compile::NodeFilter;
use crate::query::ir::*;

/// A handler for method calls (qualified accesses) in the planner
///
/// The arguments are:
///
/// 1. The `TreeInterface` that abstracts the tree-sitter AST query facility
/// 2. The translated node filter of the receiver object of the method (which may not be needed)
/// 3. The typed operands of the method call
///
/// The callback returns a node filter that is suitable for evaluating the method call
///
/// FIXME: The arguments need to be run-time values
#[derive(Clone)]
pub struct Handler(
    pub Arc<
        dyn for<'a> Fn(
                Rc<dyn TreeInterface>,
                &'a NodeFilter,
                &'a Vec<Expr<Typed>>,
            ) -> anyhow::Result<NodeFilter>
            + Send
            + Sync,
    >,
);
