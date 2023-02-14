use std::collections::HashSet;
use std::rc::Rc;

use crate::compile::interface::{EvaluationContext, TreeInterface};
use crate::compile::node_filter::NodeFilter;
use crate::compile::{CompiledQuery, QueryAction};
use crate::preprocess::FilePreprocessingPass;
use crate::query::ir::Constant;
use crate::source_file;

#[derive(Debug)]
pub enum QueryResult {
    /// A constant value that requires no computation
    Constant(Constant),
    /// A matched range of code that references the original `source_file::SourceFile`
    Node(tree_sitter::Range),
}

/// Run-time values generated by the evaluator
///
/// This will gain more interesting types as we get more interesting features
/// implemented in the language
#[derive(Debug)]
enum Value {
    /// Embed a constant from the underlying IR
    Constant(Constant),
}

/// Returns the result of evaluating a node filter on the given node
///
/// This is currently set up as a very naive interpreter. In the future, the
/// query planner could embed some of the numeric and logical computations into
/// the individual filters to reduce the evaluation cost.
fn evaluate_filter<'a, 'b: 'a>(
    target: &'b source_file::SourceFile,
    ctx: &'a mut EvaluationContext<'b>,
    flt: &NodeFilter,
) -> anyhow::Result<Value> {
    match flt {
        NodeFilter::Predicate(nm) => {
            let b = (nm.extract)(ctx, target.source.as_bytes());
            Ok(Value::Constant(Constant::Boolean(b)))
        }
        NodeFilter::NumericComputation(nm) => {
            let i = (nm.extract)(ctx, target.source.as_bytes());
            Ok(Value::Constant(Constant::Integer(i)))
        }
        NodeFilter::StringComputation(nm) => {
            let s = (nm.extract)(ctx, target.source.as_bytes());
            Ok(Value::Constant(Constant::String_(s)))
        }
        NodeFilter::TypeComputation(_nm) => {
            panic!("Top-level evaluation of types is not supported");
        }
        NodeFilter::PredicateListComputation(nm) => {
            // We evaluate a list of predicates p as `any(p)` (i.e., it is true
            // if any of the predicates evaluates to true)
            let ps = (nm.extract)(ctx, target.source.as_bytes());
            Ok(Value::Constant(Constant::Boolean(ps.iter().any(|p| *p))))
        }
        NodeFilter::ArgumentComputation(_c) => {
            panic!("Not evaluation arguments yet");
        }
        NodeFilter::ArgumentListComputation(_c) => {
            panic!("No concrete list evaluation needed yet");
        }
        NodeFilter::StringListComputation(_c) => {
            panic!("No concrete list evaluation needed yet");
        }
        NodeFilter::CallableComputation(_c) => {
            panic!("Not evaluating callables yet");
        }
        NodeFilter::RegexComputation(_c) => {
            panic!("Not evaluating regexes yet");
        }
        NodeFilter::TypeListComputation(_c) => {
            panic!("Not evaluating type lists");
        }
    }
}

fn preprocess_file<'a>(
    passes: &HashSet<FilePreprocessingPass>,
    ast: &'a tree_sitter::Tree,
    source: &'a [u8],
    tree_interface: Rc<dyn TreeInterface>,
    eval_ctx: &mut EvaluationContext<'a>,
) {
    if passes.contains(&FilePreprocessingPass::Imports) {
        eval_ctx.attach_file_import_index(tree_interface.file_imports(&ast.root_node(), source));
    }
}

// FIXME: During evaluation, flush any nodes that don't satisfy their predicate from the evaluation context

pub fn evaluate_plan<'a>(
    target: &'a source_file::SourceFile,
    ast: &'a tree_sitter::Tree,
    tree_interface: Rc<dyn TreeInterface>,
    cursor: &'a mut tree_sitter::QueryCursor,
    plan: &'a CompiledQuery,
) -> anyhow::Result<Vec<QueryResult>> {
    // FIXME: Add rich error reporting here, with supportive logging in the main driver that consumes these results
    let mut eval_ctx = EvaluationContext::new();
    preprocess_file(&plan.file_preprocessing, ast, target.source.as_bytes(), tree_interface, &mut eval_ctx);
    let mut matches = Vec::new();
    match &plan.steps {
        QueryAction::ConstantValue(v) => {
            matches.push(QueryResult::Constant(v.clone()));
        }
        QueryAction::TSQuery(flt, q, root_var) => {
            for qm in cursor.matches(q, ast.root_node(), target.source.as_bytes()) {
                let accept_node = {
                    eval_ctx.bind_node(root_var, qm.captures[0].node);
                    let res = evaluate_filter(target, &mut eval_ctx, flt)?;
                    match res {
                        Value::Constant(Constant::Boolean(b)) => b,
                        Value::Constant(_) => {
                            // This is a panic because the query planner
                            // should have raised a more structured
                            // error already. If we get here, this is a
                            // coding error.
                            panic!("Invalid filter return type");
                        }
                    }
                };
                if accept_node {
                    let str_range = qm.captures[0].node.range();
                    matches.push(QueryResult::Node(str_range));
                }

                // FIXME: It might be better to wrap the context in some kind of
                // block that makes it easy to remove all bindings that we
                // introduced.
                eval_ctx.flush_bindings();
            }
        }
    }

    Ok(matches)
}
