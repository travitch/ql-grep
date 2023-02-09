mod backend;
mod errors;
pub mod interface;
mod lift;
mod method_library;
pub mod node_filter;

use std::collections::HashMap;
use std::rc::Rc;

use crate::compile::backend::cpp::CPPTreeInterface;
use crate::compile::backend::java::JavaTreeInterface;
use crate::compile::errors::PlanError;
use crate::compile::interface::{BoundNode, CallableRef, NodeMatcher, ParameterRef, TreeInterface};
use crate::compile::lift::transform_node_filter;
use crate::compile::method_library::{method_impl_for, Handler};
use crate::compile::node_filter::NodeFilter;
use crate::plan::QueryPlan;
use crate::query::ir::{AggregateOp, CompOp, Constant, EqualityOp, Expr, Expr_, Typed, VarDecl};
use crate::query::val_type::Type;
use crate::source_file::{Language, SourceFile};

/// The actions that comprise a query plan
pub enum QueryAction {
    /// A query that can be evaluated directly by the Tree Sitter engine
    ///
    /// The string is the root variable bound by the query
    ///
    /// This may need to be generalized in more complex cases (or that might just trigger datalog mode)
    TSQuery(NodeFilter, tree_sitter::Query, BoundNode),
    /// A trivial result that is a constant
    ConstantValue(Constant),
}

/// A query plan that can be evaluated to produce a (stream of) results
pub struct CompiledQuery {
    pub steps: QueryAction,
}

struct Context {
    symbol_table: HashMap<String, Type>,
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

fn make_tree_interface(file: &SourceFile) -> Rc<dyn TreeInterface> {
    match file.lang {
        Language::CPP => Rc::new(CPPTreeInterface::new(file)) as Rc<dyn TreeInterface>,
        Language::Java => Rc::new(JavaTreeInterface::new(file)) as Rc<dyn TreeInterface>,
        Language::Python => unimplemented!(),
    }
}

/// Compile a constant into a constant evaluator
fn compile_constant(c: &Constant) -> NodeFilter {
    match c {
        Constant::Boolean(b) => {
            let this_b = *b;
            let m = NodeMatcher {
                extract: Rc::new(move |_, _| this_b),
            };
            NodeFilter::Predicate(m)
        }
        Constant::Integer(i) => {
            let this_i = *i;
            let m = NodeMatcher {
                extract: Rc::new(move |_, _| this_i),
            };
            NodeFilter::NumericComputation(m)
        }
        Constant::String_(s) => {
            let this_s = s.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |_, _| this_s.clone()),
            };
            NodeFilter::StringComputation(m)
        }
        Constant::Regex(cr) => {
            let this_cr = cr.clone();
            let m = NodeMatcher {
                extract: Rc::new(move |_, _| this_cr.clone()),
            };
            NodeFilter::RegexComputation(m)
        }
    }
}

fn compile_var_ref(var_name: &str, var_type: &Type) -> anyhow::Result<NodeFilter> {
    // This case covers all references to variables declared in the From
    // clause of the query
    match var_type {
        Type::Callable | Type::Function | Type::Method => {
            let this_s = var_name.to_string();
            let m = NodeMatcher {
                extract: Rc::new(move |_, _| CallableRef::new(this_s.as_ref())),
            };
            Ok(NodeFilter::CallableComputation(m))
        }
        Type::Parameter => {
            let this_s = var_name.to_string();
            let m = NodeMatcher {
                extract: Rc::new(move |ctx, _| {
                    let param_ref = ParameterRef::new(this_s.as_ref());
                    ctx.lookup_parameter(&param_ref).clone()
                }),
            };
            Ok(NodeFilter::ArgumentComputation(m))
        }
        ty => {
            let msg = format!("References to variables of type `{ty}` are not yet supported");
            Err(anyhow::anyhow!(PlanError::GeneralUnsupported(
                msg.to_string()
            )))
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
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) < rhs_f(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                CompOp::LE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) <= rhs_f(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                CompOp::GT => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) > rhs_f(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                CompOp::GE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) >= rhs_f(ctx, source)
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
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) == rhs_f(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                EqualityOp::NE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) != rhs_f(ctx, source)
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
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) == rhs_f(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                EqualityOp::NE => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            lhs_f(ctx, source) != rhs_f(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
            }
        }
        (NodeFilter::StringListComputation(lhs_s), NodeFilter::StringComputation(rhs_s)) => {
            match op {
                EqualityOp::EQ => {
                    let rhs_fn = Rc::clone(&rhs_s.extract);
                    let lifted = transform_node_filter(
                        Type::PrimBoolean,
                        &NodeFilter::StringListComputation(lhs_s),
                        Rc::new(move |elt: Rc<NodeFilter>| {
                            let rhs_fn_ref = Rc::clone(&rhs_fn);
                            match &*elt {
                                NodeFilter::StringComputation(sc) => {
                                    let sc_ref = Rc::clone(&sc.extract);
                                    let m = NodeMatcher {
                                        extract: Rc::new(move |ctx, source| {
                                            sc_ref(ctx, source) == rhs_fn_ref(ctx, source)
                                        }),
                                    };
                                    Ok(NodeFilter::Predicate(m))
                                }
                                _ => {
                                    panic!("Invalid branch, expected a string computation")
                                }
                            }
                        }),
                    )
                    .unwrap();
                    Ok(lifted)
                }
                EqualityOp::NE => {
                    let rhs_fn = Rc::clone(&rhs_s.extract);
                    let lifted = transform_node_filter(
                        Type::PrimBoolean,
                        &NodeFilter::StringListComputation(lhs_s),
                        Rc::new(move |elt: Rc<NodeFilter>| {
                            let rhs_fn_ref = Rc::clone(&rhs_fn);
                            match &*elt {
                                NodeFilter::StringComputation(sc) => {
                                    let sc_ref = Rc::clone(&sc.extract);
                                    let m = NodeMatcher {
                                        extract: Rc::new(move |ctx, source| {
                                            sc_ref(ctx, source) != rhs_fn_ref(ctx, source)
                                        }),
                                    };
                                    Ok(NodeFilter::Predicate(m))
                                }
                                _ => {
                                    panic!("Invalid branch, expected a string computation")
                                }
                            }
                        }),
                    )
                    .unwrap();
                    Ok(lifted)
                }
            }
        }
        _ => {
            panic!("Impossible equality comparison");
        }
    }
}

fn compile_expr(ti: Rc<dyn TreeInterface>, e: &Expr<Typed>) -> anyhow::Result<NodeFilter> {
    match &e.expr {
        Expr_::ConstantExpr(v) => Ok(compile_constant(v)),
        Expr_::VarRef(s) => compile_var_ref(s, &e.type_),
        Expr_::RelationalComparison(lhs, op, rhs) => {
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            compile_relational_comparison(lhs_f, *op, rhs_f)
        }
        Expr_::Bind(var_decl, relation_expr, eval_expr) => {
            // Wrap the computation to be evaluated in a wrapper of the
            // appropriate type that binds `var_decl` for each possible value.
            // If there are no bindings, return False.  The expression under the
            // binding *should* always be boolean.
            //
            // Critically, if there are no matches, the binder must evaluate to
            // false to reflect that there were no values to relationally bind.
            let compiled_eval_expr = compile_expr(Rc::clone(&ti), eval_expr)?;
            let eval_func = match compiled_eval_expr {
                NodeFilter::Predicate(nm) => nm,
                nf => {
                    panic!("Invalid non-predicate node filter as expression evaluated under a binder: {}", nf.kind());
                }
            };

            let compiled_binder = compile_expr(Rc::clone(&ti), relation_expr)?;
            match compiled_binder {
                NodeFilter::ArgumentListComputation(param_comp) => {
                    let param_ref = ParameterRef::new(&var_decl.name);
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            let param_ref_inner = param_ref.clone();
                            let mut result = false;
                            let params = (param_comp.extract)(ctx, source);
                            for param in params {
                                ctx.bind_parameter(&param_ref_inner, &param);
                                // NOTE: This short circuits evaluation once any
                                // match is found. That is only desirable if the
                                // enclosing construct (e.g., a function) is the
                                // thing being selected.
                                result = result || (eval_func.extract)(ctx, source);
                            }
                            result
                        }),
                    };

                    Ok(NodeFilter::Predicate(m))
                }
                _ => {
                    panic!("Unexpected binder type: {}", compiled_binder.kind());
                }
            }
        }
        Expr_::EqualityComparison(lhs, op, rhs) => {
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            compile_equality_comparison(lhs_f, *op, rhs_f)
        }
        Expr_::LogicalConjunction(lhs, rhs) => {
            assert!(lhs.type_ == Type::PrimBoolean && rhs.type_ == Type::PrimBoolean);
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            (lhs_p.extract)(ctx, source) && (rhs_p.extract)(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                _ => {
                    panic!("Impossible logical conjunction types");
                }
            }
        }
        Expr_::LogicalDisjunction(lhs, rhs) => {
            assert!(lhs.type_ == Type::PrimBoolean && rhs.type_ == Type::PrimBoolean);
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| {
                            (lhs_p.extract)(ctx, source) || (rhs_p.extract)(ctx, source)
                        }),
                    };
                    Ok(NodeFilter::Predicate(m))
                }
                _ => {
                    panic!("Impossible logical disjunction types");
                }
            }
        }
        Expr_::Aggregate(op, exprs) => {
            if exprs.len() != 1 {
                return Err(anyhow::anyhow!(PlanError::InvalidAggregateArity(
                    *op,
                    exprs.len()
                )));
            }

            match op {
                AggregateOp::Count => {
                    // Count should have a single operand that evaluates to a list

                    let op_f = compile_expr(ti, &exprs[0].expr)?;
                    match op_f {
                        NodeFilter::ArgumentListComputation(arg_matcher) => {
                            let arg_count_matcher = NodeMatcher {
                                extract: Rc::new(move |ctx, source| {
                                    (arg_matcher.extract)(ctx, source).len() as i32
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
        Expr_::QualifiedAccess(base, method, operands) => {
            // We store the method implementations in a map to keep this case of
            // the match reasonably-sized.  We look up method implementations
            // based on the method name and the base type computed by the type
            // checker.

            // We look up the handler based on the scalar type if this is
            // Relational<T> (otherwise we just use T).  We do this because we
            // implicitly treat Relational<T> values as List<T> for evaluation
            // purposes
            let scalar_base_type = base.type_.base_if_relational();
            let handler = method_impl_for(scalar_base_type, method.clone());
            match handler {
                Some(Handler(f)) => {
                    let base_comp = compile_expr(Rc::clone(&ti), base)?;
                    // If this is a list type, lift via
                    // `transform_node_filter`. Otherwise, just evaluate
                    // directly
                    let target_type = e.type_.clone();
                    let ops1 = operands.clone();
                    let ops2 = operands.clone();
                    let ti1 = Rc::clone(&ti);
                    transform_node_filter(
                        target_type,
                        &base_comp,
                        Rc::new(move |elt: Rc<NodeFilter>| f(Rc::clone(&ti1), &elt, &ops1)),
                    )
                    .map_or_else(|| f(Rc::clone(&ti), &base_comp, &ops2), Ok)
                }
                None => {
                    panic!(
                        "No handler implemented for method `{}` of type `{}`",
                        method, base.type_
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
pub fn compile_query<'a>(
    source: &'a SourceFile,
    ast: &'a tree_sitter::Tree,
    query_plan: &'a QueryPlan,
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

    let tree_interface: Rc<dyn TreeInterface> = make_tree_interface(source);
    let ctx = Context::new(&query_plan.var_decls);

    match &query_plan.selected_exprs[0].expr.expr {
        Expr_::ConstantExpr(v) => {
            let p = CompiledQuery {
                steps: QueryAction::ConstantValue(v.clone()),
            };
            Ok(p)
        }
        Expr_::VarRef(var) => {
            let ty = ctx
                .symbol_table
                .get(var)
                .ok_or_else(|| anyhow::anyhow!(PlanError::UndeclaredVariable(var.into())))?;
            let unsupported = PlanError::UnsupportedTypeForLanguage(ty.clone(), source.lang);
            let top_level = tree_interface
                .top_level_type(ty)
                .ok_or_else(|| anyhow::anyhow!(unsupported))?;
            let ts_query = tree_sitter::Query::new(ast.language(), &top_level.query)?;
            let flt = compile_expr(tree_interface, &query_plan.where_formula)?;
            let bound_node = BoundNode::new(var, ty);
            let p = CompiledQuery {
                steps: QueryAction::TSQuery(flt, ts_query, bound_node),
            };
            Ok(p)
        }
        unsupported => Err(anyhow::anyhow!(PlanError::UnsupportedSelectTarget(
            (*unsupported).clone()
        ))),
    }
}
