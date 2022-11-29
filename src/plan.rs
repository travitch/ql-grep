pub mod interface;
mod cpp;
mod java;
mod method_library;
mod errors;

use std::collections::HashMap;
use std::rc::Rc;

use crate::plan::interface::{CallableRef, LanguageType, TreeInterface, NodeMatcher, FormalArgument, BoundNode};
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
    PredicateListComputation(NodeMatcher<Vec<bool>>),
    TypeComputation(NodeMatcher<LanguageType>),
    TypeListComputation(NodeMatcher<Vec<LanguageType>>),
    NumericComputation(NodeMatcher<i32>),
    StringComputation(NodeMatcher<String>),
    StringListComputation(NodeMatcher<Vec<String>>),
    RegexComputation(NodeMatcher<CachedRegex>),
    CallableComputation(NodeMatcher<CallableRef>),
    ArgumentComputation(NodeMatcher<FormalArgument>),
    ArgumentListComputation(NodeMatcher<Vec<FormalArgument>>),
}

impl NodeFilter {
    /// Return a string representation of the NodeFilter's carried type for debugging purposes
    pub fn kind(&self) -> String {
        match self {
            NodeFilter::Predicate(_) => "bool".into(),
            NodeFilter::PredicateListComputation(_) => "[bool]".into(),
            NodeFilter::TypeComputation(_) => "Type".into(),
            NodeFilter::TypeListComputation(_) => "[Type]".into(),
            NodeFilter::NumericComputation(_) => "int".into(),
            NodeFilter::StringComputation(_) => "string".into(),
            NodeFilter::StringListComputation(_) => "[string]".into(),
            NodeFilter::RegexComputation(_) => "Regex".into(),
            NodeFilter::CallableComputation(_) => "Callable".into(),
            NodeFilter::ArgumentComputation(_) => "Parameter".into(),
            NodeFilter::ArgumentListComputation(_) => "[Parameter".into()
        }
    }
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

fn make_tree_interface(file : &SourceFile) -> Rc<dyn TreeInterface> {
    match file.lang {
        Language::CPP => Rc::new(CPPTreeInterface::new(file)) as Rc<dyn TreeInterface>,
        Language::Java => Rc::new(JavaTreeInterface::new(file)) as Rc<dyn TreeInterface>,
        Language::Python => unimplemented!()
    }
}

fn compile_expr<'a>(ti : Rc<dyn TreeInterface>, e : &'a Expr<Typed>) -> anyhow::Result<NodeFilter> {
    match &e.expr {
        Expr_::ConstantExpr(v) => {
            match v {
                Constant::Boolean(b) => {
                    let this_b = *b;
                    let m = NodeMatcher {
                        extract: Rc::new(move |_, _| this_b)
                    };
                    Ok(NodeFilter::Predicate(m))
                },
                Constant::Integer(i) => {
                    let this_i = *i;
                    let m = NodeMatcher {
                        extract: Rc::new(move |_, _| this_i)
                    };
                    Ok(NodeFilter::NumericComputation(m))
                },
                Constant::String_(s) => {
                    let this_s = s.clone();
                    let m = NodeMatcher {
                        extract: Rc::new(move |_, _| this_s.clone())
                    };
                    Ok(NodeFilter::StringComputation(m))
                },
                Constant::Regex(cr) => {
                    let this_cr = cr.clone();
                    let m = NodeMatcher {
                        extract: Rc::new(move |_, _| this_cr.clone())
                    };
                    Ok(NodeFilter::RegexComputation(m))
                }
            }
        },
        Expr_::VarRef(s) => {
            // This case covers all references to variables declared in the From
            // clause of the query
            match &e.type_ {
                Type::Callable | Type::Function | Type::Method => {
                    let this_s = s.clone();
                    let m = NodeMatcher {
                        extract: Rc::new(move |_, _| CallableRef::new(this_s.as_ref()))
                    };
                    Ok(NodeFilter::CallableComputation(m))
                },
                ty => {
                    panic!("References to variables of type `{}` are not yet supported", ty);
                }
            }
        },
        Expr_::RelationalComparison(lhs, op, rhs) => {
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::NumericComputation(lhs_n), NodeFilter::NumericComputation(rhs_n)) => {
                    match op {
                        CompOp::LT => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_n.extract)(ctx, source) < (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        CompOp::LE => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_n.extract)(ctx, source) <= (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        CompOp::GT => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_n.extract)(ctx, source) > (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        CompOp::GE => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_n.extract)(ctx, source) >= (rhs_n.extract)(ctx, source))
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
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::NumericComputation(lhs_n), NodeFilter::NumericComputation(rhs_n)) => {
                    match op {
                        EqualityOp::EQ => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_n.extract)(ctx, source) == (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        EqualityOp::NE => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_n.extract)(ctx, source) != (rhs_n.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        }
                    }
                },
                (NodeFilter::StringComputation(lhs_s), NodeFilter::StringComputation(rhs_s)) => {
                    match op {
                        EqualityOp::EQ => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_s.extract)(ctx, source) == (rhs_s.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        },
                        EqualityOp::NE => {
                            let m = NodeMatcher {
                                extract: Rc::new(move |ctx, source| (lhs_s.extract)(ctx, source) != (rhs_s.extract)(ctx, source))
                            };
                            Ok(NodeFilter::Predicate(m))
                        }
                    }
                },
                (NodeFilter::StringListComputation(lhs_s), NodeFilter::StringComputation(rhs_s)) => {
                    match op {
                        EqualityOp::EQ => {
                            let rhs_fn = Rc::clone(&rhs_s.extract);
                            let lifted = transform_node_filter(Type::PrimBoolean, &NodeFilter::StringListComputation(lhs_s), Rc::new(move |elt : Rc<NodeFilter>| {
                                let rhs_fn_ref = Rc::clone(&rhs_fn);
                                match &*elt {
                                    NodeFilter::StringComputation(sc) => {
                                        let sc_ref = Rc::clone(&sc.extract);
                                        let m = NodeMatcher {
                                            extract: Rc::new(move |ctx, source| sc_ref(ctx, source) == rhs_fn_ref(ctx, source))
                                        };
                                        Ok(NodeFilter::Predicate(m))
                                    },
                                    _ => {
                                        panic!("Invalid branch, expected a string computation")
                                    }
                                }
                            }))?;
                            Ok(lifted)
                        },
                        EqualityOp::NE => {
                            let rhs_fn = Rc::clone(&rhs_s.extract);
                            let lifted = transform_node_filter(Type::PrimBoolean, &NodeFilter::StringListComputation(lhs_s), Rc::new(move |elt : Rc<NodeFilter>| {
                                let rhs_fn_ref = Rc::clone(&rhs_fn);
                                match &*elt {
                                    NodeFilter::StringComputation(sc) => {
                                        let sc_ref = Rc::clone(&sc.extract);
                                        let m = NodeMatcher {
                                            extract: Rc::new(move |ctx, source| sc_ref(ctx, source) != rhs_fn_ref(ctx, source))
                                        };
                                        Ok(NodeFilter::Predicate(m))
                                    },
                                    _ => {
                                        panic!("Invalid branch, expected a string computation")
                                    }
                                }
                            }))?;
                            Ok(lifted)
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
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| (lhs_p.extract)(ctx, source) && (rhs_p.extract)(ctx, source))
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
            let lhs_f = compile_expr(Rc::clone(&ti), lhs)?;
            let rhs_f = compile_expr(Rc::clone(&ti), rhs)?;
            match (lhs_f, rhs_f) {
                (NodeFilter::Predicate(lhs_p), NodeFilter::Predicate(rhs_p)) => {
                    let m = NodeMatcher {
                        extract: Rc::new(move |ctx, source| (lhs_p.extract)(ctx, source) || (rhs_p.extract)(ctx, source))
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
                                extract: Rc::new(move |ctx, source| (arg_matcher.extract)(ctx, source).len() as i32)
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
                    let ops = operands.clone();
                    // FIXME: This spot needs to refer to all of the list
                    // types. That is pretty fragile. Instead,
                    // transform_node_filter should reject scalars with an
                    // Option return so that we can just automatically get newly
                    // supported types
                    match base_comp {
                        NodeFilter::ArgumentListComputation(_) | NodeFilter::StringListComputation(_) | NodeFilter::TypeListComputation(_) => {
                            transform_node_filter(target_type, &base_comp, Rc::new(move |elt : Rc<NodeFilter>| {
                                f(Rc::clone(&ti), &elt, &ops)
                            }))
                        },
                        _ => f(ti, &base_comp, &ops)
                    }
                },
                None => {
                    panic!("No handler implemented for method `{}` of type `{}`", method, base.type_);
                }
            }
        }
    }
}

fn as_predicate(nf : NodeFilter) -> NodeMatcher<bool> {
    match nf {
        NodeFilter::Predicate(nm) => nm,
        _ => panic!("Impossible, expected Predicate")
    }
}

fn as_string(nf : NodeFilter) -> NodeMatcher<String> {
    match nf {
        NodeFilter::StringComputation(nm) => nm,
        _ => panic!("Impossible, expected String")
    }
}

fn as_type(nf : NodeFilter) -> NodeMatcher<LanguageType> {
    match nf {
        NodeFilter::TypeComputation(nm) => nm,
        _ => panic!("Impossible, expected Type")
    }
}

/// This is the core transformation for each case in `transform_node_filter`
///
/// This creates a new `NodeMatcher` that:
///
/// 1. Wraps a single element from the base element collection into a suitable
///    singleton NodeFilter that the transformer can be applied to
///
/// 2. Apply the transformer (from the original type to the target type) to
///    produce a NodeFilter wrapping a single element
///
/// 3. Extract the single result at the expected type and append it to the
///    result collection
fn transform_body<From, To, W, F, X>(base_elts : &NodeMatcher<Vec<From>>,
                                     wrap_one : W,
                                     extract_one : F,
                                     transformer : Rc<X>) -> NodeMatcher<Vec<To>>
where
    From: Clone + 'static,
    W: Fn(NodeMatcher<From>) -> NodeFilter + 'static,
    F: Fn(NodeFilter) -> NodeMatcher<To> + 'static,
    X: Fn(Rc<NodeFilter>) -> anyhow::Result<NodeFilter> + 'static
{
    let xfrm = Rc::clone(&transformer);
    let args_extract = Rc::clone(&base_elts.extract);


    NodeMatcher {
        extract: Rc::new(move |ctx, source| {
            let mut res = Vec::new();

            for arg in args_extract(ctx, source) {
                // Wrap each element in the carrier type for single elements so
                // that the scalar processor (the transformer, above) can
                // process it unmodified
                let wrapper_matcher = NodeMatcher {
                    extract: Rc::new(move |_ctx, _source| arg.clone())
                };

                let wrapper_filter = wrap_one(wrapper_matcher);
                let result_filter = xfrm(Rc::new(wrapper_filter)).unwrap();
                let comp = extract_one(result_filter);
                let val = (comp.extract)(ctx, source);
                res.push(val);
            }

            res
        })
    }
}


/// A combinator to help lift operations over relations when needed
///
/// Most of the operations in the planner work over scalars, but need to be
/// lifted over some Relational<T> values.  This combinator takes a NodeFilter
/// that returns a relational value, applies the provided function to each
/// element in the relation, and re-wraps the results into a relational value of
/// the appropriate type (specified by the `result_type`)
///
/// NOTE: This requires that the result type of `F` is `result_type`. The type
/// checker ensured this, so we just assume it here.
fn transform_node_filter<'a, F>(result_type : Type, relation : &NodeFilter, transformer : Rc<F>) -> anyhow::Result<NodeFilter>
where
    F: Fn(Rc<NodeFilter>) -> anyhow::Result<NodeFilter> + 'static
{
    let xfrm : Rc<F> = Rc::clone(&transformer);
    match relation {
        NodeFilter::ArgumentListComputation(arg_list_matcher) => {
            match result_type.base_if_relational() {
                Type::PrimString => {
                    let matcher = transform_body(arg_list_matcher, Box::new(|nm| NodeFilter::ArgumentComputation(nm)), as_string, xfrm);
                    Ok(NodeFilter::StringListComputation(matcher))
                },
                Type::Type => {
                    let matcher = transform_body(arg_list_matcher, Box::new(|nm| NodeFilter::ArgumentComputation(nm)), as_type, xfrm);
                    Ok(NodeFilter::TypeListComputation(matcher))
                },
                _ => panic!("Unsupported conversion from Argument (only String is supported), result type `{}`", result_type)
            }
        },
        NodeFilter::StringListComputation(string_list_matcher) => {
            match result_type.base_if_relational() {
                Type::PrimBoolean => {
                    let matcher = transform_body(string_list_matcher, Box::new(|nm| NodeFilter::StringComputation(nm)), as_predicate, xfrm);
                    Ok(NodeFilter::PredicateListComputation(matcher))
                },
                _ => {
                    panic!("Unsupported conversion from `string` to `{}`", result_type);
                }
            }
        },
        NodeFilter::TypeListComputation(type_list_matcher) => {
            match result_type.base_if_relational() {
                Type::PrimString => {
                    let matcher = transform_body(type_list_matcher, Box::new(|nm| NodeFilter::TypeComputation(nm)), as_string, xfrm);
                    Ok(NodeFilter::StringListComputation(matcher))
                },
                _ => {
                    panic!("Unsupported conversion from `Type` to `{}`", result_type);
                }
            }
        },
        _ => {
            panic!("Attempted to transform non-list/relational result");
        }
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

    let tree_interface : Rc<dyn TreeInterface> = make_tree_interface(source);
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
                    let flt = compile_expr(tree_interface, w)?;
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
