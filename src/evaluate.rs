use tree_sitter;

use crate::plan::{NodeFilter, QueryAction, QueryPlan};
use crate::query::ir::{EqualityOp, CompOp, Constant};
use crate::source_file;

#[derive(Debug)]
pub enum QueryResult {
    /// A constant value that requires no computation
    Constant(Constant),
    /// A matched range of code that references the original `source_file::SourceFile`
    Node(tree_sitter::Range)
}

/// Run-time values generated by the evaluator
///
/// This will gain more interesting types as we get more interesting features
/// implemented in the language
#[derive(Debug)]
enum Value {
    /// Embed a constant from the underlying IR
    Constant(Constant)
}

/// Returns true if the node passes the filter (or if there is no filter)
fn evaluate_filter(target : &source_file::SourceFile, flt : &NodeFilter, n : &tree_sitter::Node) -> anyhow::Result<Value> {
    match flt {
        NodeFilter::Constant(k) => {
            return Ok(Value::Constant(k.clone()));
        },
        NodeFilter::Predicate(nm) => {
            let mut cur = tree_sitter::QueryCursor::new();
            let query = tree_sitter::Query::new(n.language(), nm.query.as_ref())?;
            let matches = cur.matches(&query, *n, target.source.as_bytes());
            let b = (nm.extract)(matches, target.source.as_bytes());
            return Ok(Value::Constant(Constant::Boolean(b)));
        },
        NodeFilter::NumericComputation(nm) => {
            let mut cur = tree_sitter::QueryCursor::new();
            let query = tree_sitter::Query::new(n.language(), nm.query.as_ref())?;
            let matches = cur.matches(&query, *n, target.source.as_bytes());
            let i = (nm.extract)(matches, target.source.as_bytes());
            return Ok(Value::Constant(Constant::Integer(i)));
        },
        NodeFilter::NumericComparison(lhs, op, rhs) => {
            let lhs_e = evaluate_filter(target, lhs, n)?;
            let rhs_e = evaluate_filter(target, rhs, n)?;
            match (&lhs_e, &rhs_e) {
                (Value::Constant(Constant::Integer(lhs_i)), Value::Constant(Constant::Integer(rhs_i))) => {
                    let res = match op {
                        CompOp::LT => lhs_i < rhs_i,
                        CompOp::LE => lhs_i <= rhs_i,
                        CompOp::GT => lhs_i > rhs_i,
                        CompOp::GE => lhs_i >= rhs_i,
                        // CompOp::EQ => lhs_i == rhs_i,
                        // CompOp::NE => lhs_i != rhs_i
                    };
                    return Ok(Value::Constant(Constant::Boolean(res)));
                }
                _ => {
                    // Also an implementation error, as the query planner should
                    // render this impossible
                    panic!("Invalid evaluation results for numeric comparison: {:?} and {:?}", lhs_e, rhs_e);
                }
            }
        },
        NodeFilter::NumericEquality(lhs, op, rhs) => {
            let lhs_e = evaluate_filter(target, lhs, n)?;
            let rhs_e = evaluate_filter(target, rhs, n)?;
            match (&lhs_e, &rhs_e) {
                (Value::Constant(Constant::Integer(lhs_i)), Value::Constant(Constant::Integer(rhs_i))) => {
                    let res = match op {
                        EqualityOp::EQ => lhs_i == rhs_i,
                        EqualityOp::NE => lhs_i != rhs_i
                    };
                    return Ok(Value::Constant(Constant::Boolean(res)));
                }
                _ => {
                    // Also an implementation error, as the query planner should
                    // render this impossible
                    panic!("Invalid evaluation results for numeric comparison: {:?} and {:?}", lhs_e, rhs_e);
                }
            }
        }
    }
}

pub fn evaluate_plan<'a>(target : &'a source_file::SourceFile,
                         ast : &'a tree_sitter::Tree,
                         cursor : &'a mut tree_sitter::QueryCursor,
                         plan : &'a QueryPlan) -> anyhow::Result<Vec<QueryResult>> {
    // FIXME: Add rich error reporting here, with supportive logging in the main driver that consumes these results
    let mut matches = Vec::new();
    match &plan.steps {
        QueryAction::ConstantValue(v) => {
            matches.push(QueryResult::Constant(v.clone()));
        },
        QueryAction::TSQuery(flt, q) => {
            for qm in cursor.matches(&q, ast.root_node(), target.source.as_bytes()) {
                let accept_node = {
                    match &flt {
                        None => true,
                        Some(f) => {
                            let res = evaluate_filter(&target, &f, &qm.captures[0].node)?;
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
                        }
                    }
                };
                if accept_node {
                    let str_range = qm.captures[0].node.range();
                    matches.push(QueryResult::Node(str_range));
                }
            }
        }
    }

   Ok(matches)
}
