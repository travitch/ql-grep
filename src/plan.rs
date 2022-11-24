pub mod interface;
mod cpp;
mod java;
mod method_library;
mod errors;

use std::collections::HashMap;

use crate::plan::interface::{CallableRef, TreeInterface, NodeMatcher, FormalArgument, BoundNode};
use crate::plan::java::JavaTreeInterface;
use crate::plan::cpp::CPPTreeInterface;
use crate::query::ir::{Repr, Typed, Expr, Expr_, Constant, EqualityOp, CompOp, AggregateOp, CachedRegex};
use crate::query::val_type::Type;
use crate::query;
use crate::source_file::{Language, SourceFile};
use crate::plan::errors::PlanError;
use crate::plan::method_library::{Handler, method_impl_for};

pub enum NodeFilter {
    Predicate(NodeMatcher<bool>),
    NumericComputation(NodeMatcher<i32>),
    StringComputation(NodeMatcher<String>),
    RegexComputation(NodeMatcher<CachedRegex>),
    CallableComputation(NodeMatcher<CallableRef>),
    ArgumentListComputation(NodeMatcher<Vec<FormalArgument>>),
}

pub struct Matching;

impl Repr for Matching {
    type Type = Type;
}

/// The actions that comprise a query plan
pub enum QueryAction {
    /// A query that can be evaluated directly by the Tree Sitter engine
    ///
    /// The string is the root variable bound by the query
    ///
    /// This may need to be generalized in more complex cases (or that might just trigger datalog mode)
    TSQuery(Option<NodeFilter>, tree_sitter::Query, BoundNode),
    /// A trivial result that is a constant
    ConstantValue(Constant)
}

/// A query plan that can be evaluated to produce a (stream of) results
pub struct QueryPlan {
    pub steps : QueryAction
}

struct Context {
    symbol_table : HashMap<String, Type>,
}

impl Context {
    fn new(query : &query::Query<Typed>) -> Self {
        let mut t = HashMap::new();

        for var_decl in query.select.var_decls.iter() {
            t.insert(var_decl.name.clone(), var_decl.type_.clone());
        }

        Context {
            symbol_table: t,
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

fn compile_expr<'a>(ti : &'a Box<dyn TreeInterface + 'a>, e : &'a Expr<Typed>) -> anyhow::Result<NodeFilter> {
    match &e.expr {
        Expr_::ConstantExpr(v) => {
            match v {
                Constant::Boolean(b) => {
                    let this_b = *b;
                    let m = NodeMatcher {
                        extract: Box::new(move |_, _| this_b)
                    };
                    Ok(NodeFilter::Predicate(m))
                },
                Constant::Integer(i) => {
                    let this_i = *i;
                    let m = NodeMatcher {
                        extract: Box::new(move |_, _| this_i)
                    };
                    Ok(NodeFilter::NumericComputation(m))
                },
                Constant::String_(s) => {
                    let this_s = s.clone();
                    let m = NodeMatcher {
                        extract: Box::new(move |_, _| this_s.clone())
                    };
                    Ok(NodeFilter::StringComputation(m))
                },
                Constant::Regex(cr) => {
                    let this_cr = cr.clone();
                    let m = NodeMatcher {
                        extract: Box::new(move |_, _| this_cr.clone())
                    };
                    Ok(NodeFilter::RegexComputation(m))
                }
            }
        },
        Expr_::VarRef(s) => {
            match &e.type_ {
                Type::Callable | Type::Function | Type::Method => {
                    let this_s = s.clone();
                    let m = NodeMatcher {
                        extract: Box::new(move |_, _| CallableRef::new(this_s.as_ref()))
                    };
                    Ok(NodeFilter::CallableComputation(m))
                },
                ty => {
                    panic!("References to variables of type `{}` are not yet supported", ty);
                }
            }
        },
        Expr_::RelationalComparison(lhs, op, rhs) => {
            let lhs_f = compile_expr(ti, lhs)?;
            let rhs_f = compile_expr(ti, rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::NumericComputation(lhs_n), NodeFilter::NumericComputation(rhs_n)) => {
                    match op {
                        CompOp::LT => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_n.extract)(ctx, source) < (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        CompOp::LE => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_n.extract)(ctx, source) <= (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        CompOp::GT => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_n.extract)(ctx, source) > (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        CompOp::GE => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_n.extract)(ctx, source) >= (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                    }
                },
                _ => {
                    // The type checker should have rejected this case
                    panic!("Impossible: relational comparison applied to unsupported type");
                }
            }
        },
        Expr_::EqualityComparison(lhs, op, rhs) => {
            assert!(lhs.type_ == rhs.type_);
            let lhs_f = compile_expr(ti, lhs)?;
            let rhs_f = compile_expr(ti, rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::NumericComputation(lhs_n), NodeFilter::NumericComputation(rhs_n)) => {
                    match op {
                        EqualityOp::EQ => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_n.extract)(ctx, source) == (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        EqualityOp::NE => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_n.extract)(ctx, source) != (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        }
                    }
                },
                (NodeFilter::StringComputation(lhs_s), NodeFilter::StringComputation(rhs_s)) => {
                    match op {
                        EqualityOp::EQ => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_s.extract)(ctx, source) == (rhs_s.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        EqualityOp::NE => {
                            let m = NodeMatcher {
                                extract: Box::new(move |ctx, source| (lhs_s.extract)(ctx, source) != (rhs_s.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        }
                    }
                },
                _ => {
                    panic!("Impossible equality comparison");
                }
            }
        },
        Expr_::LogicalConjunction(lhs, rhs) => {
            assert!(lhs.type_ == Type::PrimBoolean && rhs.type_ == Type::PrimBoolean);
            let lhs_f = compile_expr(ti, lhs)?;
            let rhs_f = compile_expr(ti, rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Box::new(move |ctx, source| (lhs_p.extract)(ctx, source) && (rhs_p.extract)(ctx, source))
                    };
                    Ok(NodeFilter::Predicate(m))
                },
                _ => {
                    panic!("Impossible logical conjunction types");
                }
            }
        },
        Expr_::LogicalDisjunction(lhs, rhs) => {
            assert!(lhs.type_ == Type::PrimBoolean && rhs.type_ == Type::PrimBoolean);
            let lhs_f = compile_expr(ti, lhs)?;
            let rhs_f = compile_expr(ti, rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Box::new(move |ctx, source| (lhs_p.extract)(ctx, source) || (rhs_p.extract)(ctx, source))
                    };
                    Ok(NodeFilter::Predicate(m))
                },
                _ => {
                    panic!("Impossible logical disjunction types");
                }
            }
        },
        Expr_::Aggregate(op, exprs) => {
            if exprs.len() != 1 {
                return Err(anyhow::anyhow!(PlanError::InvalidAggregateArity(*op, exprs.len())));
            }

            match op {
                AggregateOp::Count => {
                    // Count should have a single operand that evaluates to a list

                    let op_f = compile_expr(ti, &exprs[0].expr)?;
                    match op_f {
                        NodeFilter::ArgumentListComputation(arg_matcher) => {
                            let arg_count_matcher = NodeMatcher {
                                extract: Box::new(move |ctx, source| (arg_matcher.extract)(ctx, source).len() as i32)
                            };
                            return Ok(NodeFilter::NumericComputation(arg_count_matcher));
                        },
                        _ => {
                            panic!("Invalid argument to count (expected a list computation)");
                        }
                    }
                }
            }
        },
        Expr_::QualifiedAccess(base, method, operands) => {
            // We store the method implementations in a map to keep this case of
            // the match reasonably-sized.  We look up method implementations
            // based on the method name and the base type computed by the type
            // checker.
            let handler = method_impl_for(base.type_.clone(), method);
            match handler {
                Some(Handler(f)) => {
                    let base_comp = compile_expr(ti, base)?;
                    f(ti, base_comp, operands)
                },
                None => {
                    panic!("No handler implemented for method `{}` of type `{:?}`", method, base.type_);
                }
            }
        },
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
    let ctx = Context::new(query);

    match &query.select.select_exprs[0].expr.expr {
        Expr_::ConstantExpr(v) => {
            let p = QueryPlan {
                steps: QueryAction::ConstantValue(v.clone())
            };
            Ok(p)
        },
        Expr_::VarRef(var) => {
            let ty = ctx.symbol_table.get(var).ok_or_else(|| anyhow::anyhow!(PlanError::UndeclaredVariable(var.into())))?;
            let unsupported = PlanError::UnsupportedTypeForLanguage(ty.clone(), source.lang);
            let top_level = tree_interface.top_level_type(ty).ok_or_else(|| anyhow::anyhow!(unsupported))?;
            let ts_query = tree_sitter::Query::new(ast.language(), &top_level.query)?;
            let flt = match &query.select.where_formula {
                None => anyhow::Ok(None),
                Some(w) => {
                    let flt = compile_expr(&tree_interface, w)?;
                    anyhow::Ok(Some(flt))
                }
            }?;
            let bound_node = BoundNode::new(var, ty);
            let p = QueryPlan {
                steps: QueryAction::TSQuery(flt, ts_query, bound_node)
            };
            Ok(p)
        },
        unsupported => {
            Err(anyhow::anyhow!(PlanError::UnsupportedSelectTarget((*unsupported).clone())))
        }
    }
}
