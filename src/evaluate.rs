use tree_sitter;

use crate::plan;
use crate::query::ir;
use crate::source_file;

#[derive(Debug)]
pub enum QueryResult {
    /// A constant value that requires no computation
    Constant(ir::QLValue),
    /// A matched range of code that references the original `source_file::SourceFile`
     Node(tree_sitter::Range)
}

/// A container for a list of query results with the source file that gave rise to them
///
/// The source file is required to interpret the ranges associated with each node
pub struct QueryResults {
    pub results : Vec<QueryResult>,
    pub source_file : source_file::SourceFile
}

pub fn evaluate_plan(plan : &plan::QueryPlan, target : source_file::SourceFile) -> anyhow::Result<QueryResults> {
    // FIXME: Add rich error reporting here, with supportive logging in the main driver that consumes these results
    let mut matches = Vec::new();
    match &plan.steps {
        plan::QueryAction::ConstantValue(v) => {
            matches.push(QueryResult::Constant(v.clone()));
        },
        plan::QueryAction::TSQuery(q) => {
            let mut cursor = tree_sitter::QueryCursor::new();
            for qm in cursor.matches(&q, target.ast.root_node(), target.source.as_bytes()) {
                let str_range = qm.captures[0].node.range();
                matches.push(QueryResult::Node(str_range));
            }
        }
    }

    let qr = QueryResults {
        results: matches,
        source_file: target
    };
    Ok(qr)
}
