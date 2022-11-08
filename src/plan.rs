use std::collections::HashMap;
use tree_sitter;

use crate::query::ir;
use crate::query;

#[derive(thiserror::Error, Debug)]
pub enum PlanError {
    #[error("Expected 1 selected expression but found {0}")]
    NonSingletonSelect(usize),
    #[error("Unsupported target for a select expression: `{0:?}`")]
    UnsupportedSelectTarget(ir::Expr),
    #[error("Unsupported type for a select expression: `{0:?}`")]
    UnsupportedSelectType(ir::Type)
}

/// The actions that comprise a query plan
pub enum QueryAction {
    /// A query that can be evaluated directly by the Tree Sitter engine
    TSQuery(tree_sitter::Query),
    /// A trivial result that is a constant
    ConstantValue(ir::QLValue)
}

/// A query plan that can be evaluated to produce a (stream of) results
pub struct QueryPlan {
    pub steps : QueryAction
}

struct Context {
    symbol_table : HashMap<String, ir::Type>
}

impl Context {
    fn new(query : &query::Query) -> Self {
        let mut t = HashMap::new();

        for var_decl in query.select.var_decls.iter() {
            t.insert(var_decl.name.clone(), var_decl.type_);
        }

        Context {
            symbol_table: t
        }
    }
}

/// Build a query plan for the given query in the given language
///
/// Query plans are language-specific (because the Tree Sitter grammar for each
/// language is fairly different). Note that the caller should cache query plans
/// to avoid recomputing them.
pub fn build_query_plan(lang : tree_sitter::Language, query : &query::Query) -> anyhow::Result<QueryPlan> {
    // The basic idea is that we want to do as much processing as we can inside
    // of Tree Sitter's query language, as it will be the most efficient.
    //
    // We will then perform a layer of refined processing to the extent
    // necessary (e.g., applying regex matches to identifier names).
    //
    // Finally, we will do more advanced program analysis using datalog. The
    // previous two steps will submit tuples to a Datalog database. The
    // top-level query plan will saturate the tuple store and read out results.

    // As a first pass, do the coarse selection of all of the "select"
    // expressions using Tree Sitter queries. Later, we can resolve all of the
    // "where" clauses with post-hoc analysis outside of the Tree Sitter engine
    // Note that we may want to refine the coarse queries. Luckily, we can do so
    // compositionally: instead of requiring us to make a monolithic query that
    // does everything at once, we can select e.g., functions and then perform
    // subsequent refined queries on the returned nodes.
    let num_selected = query.select.select_exprs.len();
    if num_selected != 1 {
        return Err(anyhow::anyhow!(PlanError::NonSingletonSelect(num_selected)));
    }

    let ctx = Context::new(&query);


    match &query.select.select_exprs[0].expr {
        ir::Expr::ValueExpr(v) => {
            let p = QueryPlan {
                steps: QueryAction::ConstantValue(v.clone())
            };
            return Ok(p);
        },
        ir::Expr::VarRef(var) => {
            // FIXME: Return a better error here
            let ty = ctx.symbol_table.get(var).unwrap();
            match ty {
                ir::Type::Function => {
                    let query_string = "(function_definition) @function.definition";
                    let ts_query = tree_sitter::Query::new(lang, query_string)?;
                    let p = QueryPlan {
                        steps: QueryAction::TSQuery(ts_query)
                    };
                    return Ok(p);
                },
                ir::Type::Method => {
                    let query_string = "(method_declaration) @method.declaration";
                    let ts_query = tree_sitter::Query::new(lang, query_string)?;
                    let p = QueryPlan {
                        steps: QueryAction::TSQuery(ts_query)
                    };
                    return Ok(p);
                },
                _ => {
                    return Err(anyhow::anyhow!(PlanError::UnsupportedSelectType(*ty)));
                }
            }
        },
        unsupported => {
            return Err(anyhow::anyhow!(PlanError::UnsupportedSelectTarget(unsupported.clone())));
        }
    }
}
