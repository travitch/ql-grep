pub mod interface;
mod cpp;
mod java;

use std::collections::HashMap;

use crate::plan::interface::{TreeInterface, NodeMatcher};
use crate::plan::java::JavaTreeInterface;
use crate::plan::cpp::CPPTreeInterface;
use crate::query::ir::{Repr, Typed, Expr, Expr_, Type, Constant, EqualityOp, CompOp, AggregateOp};
use crate::query;
use crate::source_file::{Language, SourceFile};

#[derive(thiserror::Error, Debug)]
pub enum PlanError {
    #[error("Expected 1 selected expression but found {0}")]
    NonSingletonSelect(usize),
    #[error("Unsupported target for a select expression: `{0:?}`")]
    UnsupportedSelectTarget(Expr_<Typed>),
    #[error("Unsupported type for a select expression: `{0:?}`")]
    UnsupportedSelectType(Type),
    #[error("Unsupported type `{0:?}` for language {1:?}")]
    UnsupportedTypeForLanguage(Type, Language),
    #[error("Use of undefined variable `{0}`")]
    UndeclaredVariable(String),
    #[error("Invalid aggregate `{0:?}` with arity {1}")]
    InvalidAggregateArity(AggregateOp, usize),
    #[error("Arguments not supported for this language")]
    ArgumentsNotSupported
}

pub enum NodeFilter {
    Constant(Constant),
    Predicate(NodeMatcher<bool>),
    NumericComputation(NodeMatcher<i32>),
    NumericComparison(Box<NodeFilter>, CompOp, Box<NodeFilter>),
    NumericEquality(Box<NodeFilter>, EqualityOp, Box<NodeFilter>),
}

pub struct Matching;

impl Repr for Matching {
    type Type = Type;
    type Evaluator<T> = NodeMatcher<T>;
}

/// The actions that comprise a query plan
pub enum QueryAction {
    /// A query that can be evaluated directly by the Tree Sitter engine
    TSQuery(Option<NodeFilter>, tree_sitter::Query),
    /// A trivial result that is a constant
    ConstantValue(Constant)
}

/// A query plan that can be evaluated to produce a (stream of) results
pub struct QueryPlan {
    pub steps : QueryAction
}

struct Context {
    symbol_table : HashMap<String, Type>
}

impl Context {
    fn new(query : &query::Query<Typed>) -> Self {
        let mut t = HashMap::new();

        for var_decl in query.select.var_decls.iter() {
            t.insert(var_decl.name.clone(), var_decl.type_);
        }

        Context {
            symbol_table: t
        }
    }
}

fn make_tree_interface<'a>(file : &'a SourceFile) -> Box<dyn TreeInterface + 'a> {
    match file.lang {
        Language::CPP => Box::new(CPPTreeInterface::new(file)) as Box<dyn TreeInterface>,
        Language::Java => Box::new(JavaTreeInterface::new(file)) as Box<dyn TreeInterface>,
        Language::Python => unimplemented!()
    }
}

fn compile_expr<'a>(ti : &Box<dyn TreeInterface + 'a>, e : &'a Expr<Typed>) -> anyhow::Result<NodeFilter> {
    match &e.expr {
        Expr_::ConstantExpr(v) => Ok(NodeFilter::Constant(v.clone())),
        Expr_::RelationalComparison(lhs, op, rhs) => {
            let lhs_f = compile_expr(ti, &*lhs)?;
            let rhs_f = compile_expr(ti, &*rhs)?;
            Ok(NodeFilter::NumericComparison(Box::new(lhs_f), *op, Box::new(rhs_f)))
        },
        Expr_::EqualityComparison(lhs, op, rhs) => {
            let lhs_f = compile_expr(ti, &*lhs)?;
            let rhs_f = compile_expr(ti, &*rhs)?;
            Ok(NodeFilter::NumericEquality(Box::new(lhs_f), *op, Box::new(rhs_f)))
        },
        Expr_::Aggregate(op, exprs) => {
            if exprs.len() != 1 {
                return Err(anyhow::anyhow!(PlanError::InvalidAggregateArity(*op, exprs.len())));
            }

            match (op, &exprs[0].expr.expr) {
                (AggregateOp::Count, Expr_::QualifiedAccess(base, field)) => {
                    // FIXME: Check that the base is a var ref (and check the type of that variable)
                    if field == "getAParameter" {
                        let arg_matcher = ti.callable_arguments().ok_or(PlanError::ArgumentsNotSupported)?;
                        let arg_count_matcher = NodeMatcher {
                            query: arg_matcher.query,
                            extract: Box::new(move |matches, src| (arg_matcher.extract)(matches, src).len() as i32)
                        };
                        let flt = NodeFilter::NumericComputation(arg_count_matcher);
                        return Ok(flt);
                    }

                    unimplemented!();
                },
                _ => {
                    unimplemented!();
                }
            }
        },
        _ => unimplemented!()
    }
}

/// Build a query plan for the given query in the given language
///
/// Query plans are language-specific (because the Tree Sitter grammar for each
/// language is fairly different). Note that the caller should cache query plans
/// to avoid recomputing them.
pub fn build_query_plan<'a>(source : &'a SourceFile,
                            ast : &'a tree_sitter::Tree,
                            query : &'a query::Query<Typed>) -> anyhow::Result<QueryPlan> {
    // The basic idea is that we want to do as much processing as we can inside
    // of Tree Sitter's query language, as it will be the most efficient.
    //
    // We will then perform a layer of refined processing to the extent
    // necessary (e.g., applying regex matches to identifier names).
    //
    // Finally, we will do more advanced program analysis using catalog. The
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

    let tree_interface : Box<dyn TreeInterface> = make_tree_interface(source);
    let ctx = Context::new(&query);

    match &query.select.select_exprs[0].expr.expr {
        Expr_::ConstantExpr(v) => {
            let p = QueryPlan {
                steps: QueryAction::ConstantValue(v.clone())
            };
            return Ok(p);
        },
        Expr_::VarRef(var) => {
            let ty = ctx.symbol_table.get(var).ok_or(anyhow::anyhow!(PlanError::UndeclaredVariable(var.into())))?;
            let unsupported = PlanError::UnsupportedTypeForLanguage(*ty, source.lang);
            let top_level = tree_interface.top_level_type(ty).ok_or(anyhow::anyhow!(unsupported))?;
            let ts_query = tree_sitter::Query::new(ast.language(), &top_level.query)?;
            let flt = match &query.select.where_formula {
                None => anyhow::Ok(None),
                Some(w) => {
                    let flt = compile_expr(&tree_interface, &w)?;
                    anyhow::Ok(Some(flt))
                }
            }?;
            let p = QueryPlan {
                steps: QueryAction::TSQuery(flt, ts_query)
            };
            return Ok(p);
        },
        unsupported => {
            return Err(anyhow::anyhow!(PlanError::UnsupportedSelectTarget((*unsupported).clone())));
        }
    }
}
