use tree_sitter;

use crate::plan;
use crate::query::ir;
use crate::source_file;

#[derive(Debug)]
pub enum QueryResult<'a> {
    /// A constant value that requires no computation
    Constant(ir::QLValue),
    /// A matched range of code that references the original `source_file::SourceFile`
    Node(&'a str)
}

pub fn evaluate_plan<'a>(plan : &plan::QueryPlan, target : &'a source_file::SourceFile) -> anyhow::Result<Vec<QueryResult<'a>>> {
    // FIXME: Add rich error reporting here, with supportive logging in the main driver that consumes these results
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
