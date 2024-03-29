pub mod backend;
mod errors;
pub mod interface;
mod method_library;
pub mod node_filter;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::compile::errors::PlanError;
use crate::compile::interface::{CallableRef, CallsiteRef, EvaluationContext, ImportRef, NodeListMatcher, NodeMatcher, ParameterRef, TreeInterface};
use crate::compile::method_library::{method_impl_for, Handler};
use crate::compile::node_filter::NodeFilter;
use crate::plan::QueryPlan;
use crate::preprocess::FilePreprocessingPass;
use crate::query::ir::{AggregateOp, CompOp, Constant, EqualityOp, Expr, Expr_, Typed, VarDecl, VarIdent};
use crate::query::val_type::Type;
use crate::source_file::Language;
use crate::with_ranges::WithRanges;

/// The actions that comprise a query plan
pub enum QueryAction {
    /// A query that can be evaluated directly by the Tree Sitter engine
    ///
    /// The string is the root variable bound by the query
    ///
    /// This may need to be generalized in more complex cases (or that might just trigger datalog mode)
    TSQuery(NodeFilter, tree_sitter::Query, CallableRef),
    /// A trivial result that is a constant
    ConstantValue(Constant),
}

/// A query plan that can be evaluated to produce a (stream of) results
pub struct CompiledQuery {
    pub file_preprocessing: HashSet<FilePreprocessingPass>,
    pub steps: QueryAction,
}

struct Context {
    symbol_table: HashMap<VarIdent, Type>,
}

impl Context {
    fn new(var_decls: &[VarDecl]) -> Self {
        let mut t = HashMap::new();

        for var_decl in var_decls.iter() {
            t.insert(var_decl.name.clone(), var_decl.type_.clone());
        }

        Context { symbol_table: t }
    }
}

/// Compile a constant into a constant evaluator
fn compile_constant(c: &Constant) -> NodeFilter {
    match c {
        Constant::Boolean(b) => {
            let this_b = *b;
            let m = NodeMatcher {
                extract: Rc::new(move |_| Some(WithRanges::value(this_b))),
            };
            NodeFilter::Predicate(m)
        }
        Constant::Integer(i) => {
            let this_i = *i;
            let m = NodeMatcher {
                extract: Rc::new(move |_| Some(WithRanges::value(this_i))),
            };
            NodeFilter::NumericComputation(m)
        }
        Constant::String_(s) => {
            let this_s = s.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |_| Some(WithRanges::value(this_s.clone()))),
            };
            NodeFilter::StringComputation(m)
        }
        Constant::Regex(cr) => {
            let this_cr = cr.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |_| Some(WithRanges::value(this_cr.clone()))),
            };
            NodeFilter::RegexComputation(m)
        }
    }
}

fn compile_var_ref(var_name: &VarIdent, var_type: &Type) -> anyhow::Result<NodeFilter> {
    // This case covers all references to variables declared in the From
    // clause of the query
    match var_type {
        Type::Callable | Type::Function | Type::Method => {
            let this_s = var_name.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |_| Some(WithRanges::value(CallableRef::new(this_s.clone())))),
            };
            Ok(NodeFilter::CallableComputation(m))
        }
        Type::Parameter => {
            let this_s = var_name.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |ctx| {
                    let param_ref = ParameterRef::new(this_s.clone());
                    Some(ctx.lookup_parameter(&param_ref).clone())
                }),
            };
            Ok(NodeFilter::ArgumentComputation(m))
        }
        Type::Call => {
            let this_var_name = var_name.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |ctx| {
                    let callsite_ref = CallsiteRef::new(this_var_name.clone());
                    Some(ctx.lookup_callsite(&callsite_ref).clone())
                }),
            };
            Ok(NodeFilter::CallsiteComputation(m))
        }
        Type::Import => {
            let this_var_name = var_name.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |ctx| {
                    let import_ref = ImportRef::new(this_var_name.clone());
                    Some(ctx.lookup_import(&import_ref).clone())
                }),
            };
            Ok(NodeFilter::ImportComputation(m))
        }
        ty => {
            let msg = format!("References to variables of type `{ty}` are not yet supported");
            Err(anyhow::anyhow!(PlanError::GeneralUnsupported(msg)))
        }
    }
}

fn compile_relational_comparison(
    lhs: NodeFilter,
    op: CompOp,
    rhs: NodeFilter,
) -> anyhow::Result<NodeFilter> {
    match (lhs, rhs) {
        (NodeFilter::NumericComputation(lhs_n), NodeFilter::NumericComputation(rhs_n)) => {
            let lhs_f = Rc::clone(&lhs_n.extract);
            let rhs_f = Rc::clone(&rhs_n.extract);
            match op {
                CompOp::LT => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value < rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                CompOp::LE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value <= rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                CompOp::GT => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value > rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                CompOp::GE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value >= rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
            }
        }
        _ => {
            // The type checker should have rejected this case
            panic!("Impossible: relational comparison applied to unsupported type");
        }
    }
}

fn compile_equality_comparison(
    lhs: NodeFilter,
    op: EqualityOp,
    rhs: NodeFilter,
) -> anyhow::Result<NodeFilter> {
    match (lhs, rhs) {
        (NodeFilter::NumericComputation(lhs_n), NodeFilter::NumericComputation(rhs_n)) => {
            let lhs_f = Rc::clone(&lhs_n.extract);
            let rhs_f = Rc::clone(&rhs_n.extract);
            match op {
                EqualityOp::EQ => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value == rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                EqualityOp::NE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value != rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
            }
        }
        (NodeFilter::StringComputation(lhs_s), NodeFilter::StringComputation(rhs_s)) => {
            let lhs_f = Rc::clone(&lhs_s.extract);
            let rhs_f = Rc::clone(&rhs_s.extract);
            match op {
                EqualityOp::EQ => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value == rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                EqualityOp::NE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            lhs_f(ctx).and_then(|lhs_result| {
                                rhs_f(ctx).map(|rhs_result| {
                                    WithRanges::new(
                                        lhs_result.value != rhs_result.value,
                                        vec![lhs_result.ranges, rhs_result.ranges],
                                    )
                                })
                            })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
            }
        }
        _ => {
            panic!("Impossible equality comparison");
        }
    }
}

fn generic_compile_bind<F, V>(
    val_gen: &NodeListMatcher<V>,
    eval_func: &NodeMatcher<bool>,
    do_bind_val: F,
) -> NodeFilter
where
  F: Fn(&mut EvaluationContext, WithRanges<V>) + 'static,
  V: Clone + 'static,
{
    let this_val_gen = Rc::clone(&val_gen.extract);
    let this_eval_func = Rc::clone(&eval_func.extract);
    let m = NodeMatcher {
        extract: Rc::new(move |ctx| {
            let inner_val_gen = Rc::clone(&this_val_gen);
            let mut collected_ranges = Vec::new();
            let vals = inner_val_gen(ctx);
            for val in vals {
                do_bind_val(ctx, val.clone());
                if let Some(mut this_result) = this_eval_func(ctx) {
                    // NOTE: This short circuits evaluation once any match is
                    // found. That is only desirable if the enclosing construct
                    // (e.g., a function) is the thing being selected.
                    if this_result.value {
                        return Some(this_result);
                    }


                    // These ranges are only used if *all* of the
                    // arguments fail to satisfy the predicate
                    collected_ranges.append(&mut this_result.ranges);
                }
            }

            Some(WithRanges {
                value: false,
                ranges: collected_ranges,
            })
        }),
    };

    NodeFilter::Predicate(m)
}

fn compile_bind_expr(
    bound_var: &VarDecl,
    compiled_binder: NodeFilter,
    eval_func: NodeMatcher<bool>,
) -> NodeFilter {
    match compiled_binder {
        NodeFilter::ArgumentListComputation(param_comp) => {
            let param_ref = ParameterRef::new(bound_var.name.clone());
            generic_compile_bind(&param_comp, &eval_func, move |ctx, param| {
                let param_ref_inner = param_ref.clone();
                ctx.bind_parameter(&param_ref_inner, &param);
            })
        }
        NodeFilter::ImportListComputation(import_comp) => {
            let import_ref = ImportRef::new(bound_var.name.clone());
            generic_compile_bind(&import_comp, &eval_func, move |ctx, imp| {
                let import_ref_inner = import_ref.clone();
                ctx.bind_import(&import_ref_inner, imp);
            })
        }
        NodeFilter::CallsiteListComputation(callsite_comp) => {
            let callsite_ref = CallsiteRef::new(bound_var.name.clone());
            generic_compile_bind(&callsite_comp, &eval_func, move |ctx, callsite| {
                let callsite_ref_inner = callsite_ref.clone();
                ctx.bind_callsite(&callsite_ref_inner, &callsite);
            })
        }
        _ => {
            panic!("Unexpected binder type: {}", compiled_binder.kind());
        }
    }
}

fn compile_expr(ti: Rc<dyn TreeInterface>, e: &Expr<Typed>) -> anyhow::Result<NodeFilter> {
    match &e.expr {
        Expr_::ConstantExpr(v) => Ok(compile_constant(v)),
        Expr_::VarRef(s) => compile_var_ref(s, &e.type_),
        Expr_::RelationalComparison { lhs, op, rhs } => {
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            compile_relational_comparison(lhs_f, *op, rhs_f)
        }
        Expr_::Bind {
            bound_var,
            relation_expr,
            evaluated_expr,
        } => {
            // Wrap the computation to be evaluated in a wrapper of the
            // appropriate type that binds `var_decl` for each possible value.
            // If there are no bindings, return False.  The expression under the
            // binding *should* always be boolean.
            //
            // Critically, if there are no matches, the binder must evaluate to
            // false to reflect that there were no values to relationally bind.
            let compiled_eval_expr = compile_expr(Rc::clone(&ti), evaluated_expr)?;
            let eval_func = match compiled_eval_expr {
                NodeFilter::Predicate(nm) => nm,
                nf => {
                    panic!("Invalid non-predicate node filter as expression evaluated under a binder: {}", nf.kind());
                }
            };

            let compiled_binder = compile_expr(Rc::clone(&ti), relation_expr)?;
            Ok(compile_bind_expr(bound_var, compiled_binder, eval_func))
        }
        Expr_::EqualityComparison { lhs, op, rhs } => {
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            compile_equality_comparison(lhs_f, *op, rhs_f)
        }
        Expr_::LogicalNegation { predicate } => {
            assert!(predicate.type_ == Type::PrimBoolean);
            let predicate_f = compile_expr(Rc::clone(&ti), predicate)?;
            match predicate_f {
                NodeFilter::Predicate(predicate_p) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            (predicate_p.extract)(ctx).map(|mut b_ranges| {
                                b_ranges.value = !b_ranges.value;
                                b_ranges
                            })
                        }),
                    };

                    Ok(NodeFilter::Predicate(m))
                }
                nf => {
                    panic!("Impossible type for logical negation: {}", nf.kind());
                }
            }
        }
        Expr_::LogicalConjunction { lhs, rhs } => {
            assert!(lhs.type_ == Type::PrimBoolean && rhs.type_ == Type::PrimBoolean,
                "Expected boolean operands but got lhs: {}, rhs: {}", lhs.type_, rhs.type_);
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            // If either conjunct fails to evaluate for whatever
                            // reason, treat it as evaluating to false
                            let lhs_res = (lhs_p.extract)(ctx).unwrap_or(WithRanges::value(false));
                            let rhs_res = (rhs_p.extract)(ctx).unwrap_or(WithRanges::value(false));
                            // If the result evaluates to false, we might want
                            // to drop the ranges; however, it could be the case
                            // that the result is later negated, so we shouldn't
                            // assume that the ranges aren't relevant
                            Some(WithRanges::new(
                                lhs_res.value && rhs_res.value,
                                vec![lhs_res.ranges, rhs_res.ranges],
                            ))
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                _ => {
                    panic!("Impossible logical conjunction types");
                }
            }
        }
        Expr_::LogicalDisjunction { lhs, rhs } => {
            assert!(lhs.type_ == Type::PrimBoolean && rhs.type_ == Type::PrimBoolean);
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx| {
                            (lhs_p.extract)(ctx)
                                .map(|lhs_res| {
                                    // Short circuit if possible
                                    if lhs_res.value {
                                        lhs_res
                                    } else {
                                        (rhs_p.extract)(ctx)
                                            // This `or` case fires if the second disjunct did not produce
                                            // any result (and the first disjunct failed), so the whole disjunct
                                            // is considered failed
                                            .unwrap_or(WithRanges::value(false))
                                    }
                                })
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                _ => {
                    panic!("Impossible logical disjunction types");
                }
            }
        }
        Expr_::Aggregate { op, operands } => {
            if operands.len() != 1 {
                return Err(anyhow::anyhow!(PlanError::InvalidAggregateArity(
                    *op,
                    operands.len()
                )));
            }

            match op {
                AggregateOp::Count => {
                    // Count should have a single operand that evaluates to a list

                    let op_f = compile_expr(ti, &operands[0].expr)?;
                    match op_f {
                        NodeFilter::ArgumentListComputation(arg_matcher) => {
                            let arg_count_matcher = NodeMatcher {
                                extract: Rc::new(move |ctx| {
                                    let vec_res = (arg_matcher.extract)(ctx);
                                    // We discard the sub-ranges here because
                                    // they aren't that interesting for this
                                    // case, as it is just all arguments... That
                                    // could easily be changed.
                                    Some(WithRanges::value(vec_res.len() as i32))
                                }),
                            };
                            Ok(NodeFilter::NumericComputation(arg_count_matcher))
                        }
                        _ => {
                            panic!("Invalid argument to count (expected a list computation)");
                        }
                    }
                }
            }
        }
        Expr_::QualifiedAccess {
            base,
            method_name,
            operands,
        } => {
            // We store the method implementations in a map to keep this case of
            // the match reasonably-sized.  We look up method implementations
            // based on the method name and the base type computed by the type
            // checker.

            // We look up the handler based on the scalar type if this is
            // Relational<T> (otherwise we just use T).  We do this because we
            // implicitly treat Relational<T> values as List<T> for evaluation
            // purposes
            let scalar_base_type = base.type_.base_if_relational();
            let handler = method_impl_for(scalar_base_type, method_name.clone());
            match handler {
                Some(Handler(f)) => {
                    let base_comp = compile_expr(Rc::clone(&ti), base)?;
                    let ops1 = operands.iter()
                        .map(|op| compile_expr(Rc::clone(&ti), op).unwrap())
                        .collect();
                    f(Rc::clone(&ti), &base_comp, &ops1)
                }
                None => {
                    panic!(
                        "No handler implemented for method `{}` of type `{}`",
                        method_name, base.type_
                    );
                }
            }
        }
    }
}

/// Build a query plan for the given query in the given language
///
/// Query plans are language-specific (because the Tree Sitter grammar for each
/// language is fairly different). Note that the caller should cache query plans
/// to avoid recomputing them.
pub fn compile_query(
    lang: Language,
    ts_lang: tree_sitter::Language,
    tree_interface: Rc<dyn TreeInterface>,
    query_plan: &QueryPlan,
) -> anyhow::Result<CompiledQuery> {
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
    let num_selected = query_plan.selected_exprs.len();
    if num_selected != 1 {
        return Err(anyhow::anyhow!(PlanError::NonSingletonSelect(num_selected)));
    }

    let ctx = Context::new(&query_plan.var_decls);

    match &query_plan.selected_exprs[0].expr.expr {
        Expr_::ConstantExpr(v) => {
            let p = CompiledQuery {
                file_preprocessing: HashSet::new(),
                steps: QueryAction::ConstantValue(v.clone()),
            };
            Ok(p)
        }
        Expr_::VarRef(var) => {
            let ty = ctx
                .symbol_table
                .get(var)
                .ok_or_else(|| anyhow::anyhow!(PlanError::UndeclaredVariable(var.clone())))?;
            let unsupported = PlanError::UnsupportedTypeForLanguage(ty.clone(), lang);
            // NOTE: If the top-level selected type is not supported, the query
            // is just not compiled for this file
            let top_level = tree_interface
                .top_level_type(ty)
                .ok_or_else(|| anyhow::anyhow!(unsupported))?;
            let ts_query = tree_sitter::Query::new(ts_lang, &top_level.query)?;
            let flt = compile_expr(tree_interface, &query_plan.where_formula)?;
            let bound_node = CallableRef::new(var.clone());
            let p = CompiledQuery {
                file_preprocessing: query_plan.file_preprocessing.clone(),
                steps: QueryAction::TSQuery(flt, ts_query, bound_node),
            };
            Ok(p)
        }
        unsupported => Err(anyhow::anyhow!(PlanError::UnsupportedSelectTarget(
            (*unsupported).clone()
        ))),
    }
}
