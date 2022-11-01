use tree_sitter;

use crate::plan;
use crate::query::ir;
use crate::source_file;

#[derive(Debug)]
pub enum QueryResult {
    Constant(ir::QLValue),
    // FIXME: Make this a slice of the original string with the correct lifetimes
    Node(String)
}

pub fn evaluate_plan(plan : &plan::QueryPlan, target : &source_file::SourceFile) -> anyhow::Result<Vec<QueryResult>> {
    let mut res = Vec::new();
    match &plan.steps {
        plan::QueryAction::ConstantValue(v) => {
            res.push(QueryResult::Constant(v.clone()));
        },
        plan::QueryAction::TSQuery(q) => {
            let mut cursor = tree_sitter::QueryCursor::new();
            for qm in cursor.matches(&q, target.ast.root_node(), target.source.as_bytes()) {
                let str_range = qm.captures[0].node.utf8_text(target.source.as_bytes())?;
                res.push(QueryResult::Node(str_range.into()));
            }
        }
    }
    Ok(res)
}
