/// This module implements the query planner.
///
/// The query planner runs after type checking and before compilation.  Its
/// primary job is to re-arrange the query to be more reflective of the ultimate
/// evaluation strategy. In particular, it collects joins into a natural order
/// that can be efficiently evaluated. The order is determined by the type
/// containment relationships defined in the library.
use pretty::RcDoc;
use std::collections::HashSet;

use crate::library::index::library_index;
use crate::preprocess::FilePreprocessingPass;
use crate::query::ir::{AsExpr, EqualityOp, Expr, Expr_, Typed, VarDecl, VarIdent};
use crate::query::typecheck::TypedQuery;
use crate::query::val_type::Type;

#[cfg(test)]
use crate::query::ir::Constant;
#[cfg(test)]
use crate::query::parse_query;
#[cfg(test)]
use crate::query::typecheck::typecheck_query;

/// This wraps up the contents of the original query in a more structured form
#[derive(Debug)]
pub struct QueryPlan {
    pub selected_exprs: Vec<AsExpr<Typed>>,
    pub where_formula: Expr<Typed>,
    pub var_decls: Vec<VarDecl>,
    pub root_var: VarDecl,
    pub file_preprocessing: HashSet<FilePreprocessingPass>,
}

#[derive(thiserror::Error, Debug)]
pub enum PlanError {
    #[error(
        "At least one variable is not related to the others by a containment relationship: {0:?}"
    )]
    UnrelatedVariablesError(Vec<VarDecl>),
}

/// A mutable environment for the planner
struct PlanEnv {
    /// A source of identifiers for synthetic variables generated during the
    /// rewriting process
    synthetic_id_source: usize,
    /// The variables generated during the rewriting process, collected here so
    /// that they can be added to the `QueryPlan`
    synthetic_vars: Vec<VarDecl>,
}

impl PlanEnv {
    fn new() -> Self {
        PlanEnv {
            synthetic_id_source: 0,
            synthetic_vars: Vec::new(),
        }
    }

    fn fresh_var(&mut self, ty: Type) -> VarIdent {
        let var_id = self.synthetic_id_source;
        self.synthetic_id_source += 1;
        let vid = VarIdent::SyntheticIdent(var_id);
        let var_decl = VarDecl {
            name: vid.clone(),
            type_: ty,
        };
        self.synthetic_vars.push(var_decl);
        vid
    }
}

/// Remove any types "reachable" (via the containment relationship) from `ty`
fn remove_reachable_from(all_var_types: &mut HashSet<Type>, ty: &Type) {
    all_var_types.remove(ty);

    // This is an unwrap because it is a programming error (or an error in the
    // library) if an entry is missing here.
    let type_index = library_index().get(ty).unwrap();
    for contained_ty in &type_index.contained_types {
        remove_reachable_from(all_var_types, contained_ty);
    }
}

/// Find the variable decl that is of a type that contains all of the other
/// types referenced by variables declared in this query
///
/// This captures the current restriction that queries must be *local*. For
/// example, if a query references both a `Function` f and a `Stmt s`, return
/// `f` because it is "larger" and contains the statement.
fn find_outermost_var_decl(decls: &Vec<VarDecl>) -> anyhow::Result<VarDecl> {
    let mut res = None;

    // Create a set of all of the declared variables
    //
    // Remove elements from this set as we process the top-level decls
    let mut all_var_types: HashSet<Type> =
        HashSet::from_iter(decls.iter().map(|vd| vd.type_.clone()));

    // Iterate over each variable; remove any reachable from the currently
    // outermost variable (because they are contained). If the current outermost
    // variable is contained in the next one, update the outermost. If a
    // variable is unrelated, add it to a set that *must* be covered
    // eventually. If that to-cover set is not empty, raise the appropriate
    // error.
    for decl in decls {
        if res.is_none() {
            res = Some(decl.clone());
        }

        // If this type is not in `all_var_types`, we have already seen a more
        // general type
        if all_var_types.get(&decl.type_).is_none() {
            continue;
        }

        res = Some(decl.clone());

        // Now remove this and any reachable types from `all_var_types`
        remove_reachable_from(&mut all_var_types, &decl.type_);
    }

    // FIXME: Also validate that all of the variables are related (i.e., there
    // are no variables

    res.ok_or_else(|| {
        anyhow::anyhow!(PlanError::UnrelatedVariablesError(Vec::from_iter(
            decls.iter().cloned()
        )))
    })
}

struct DecomposedRelationalExpression {
    /// The fresh variable allocated to bind relational values to
    fresh_var: VarDecl,
    /// The expression that generates relational values (a function call)
    relational_generator: Expr<Typed>,
    /// The scalar version of the input expression that was decomposed; the
    /// fresh variable stands in place of the relational expression.
    scalar_evaluation: Expr<Typed>,
}

/// Decompose the given relational expression.
///
/// The only syntactic forms that generate relational values are method calls
/// (possibly chained calls):
///
/// > a.foo().bar().baz()
///
/// This function should:
///
/// 1. Generate a fresh variable `f`
/// 2. Extract `a.foo()` as the relational generator
/// 3. Construct the implied scalar evaluation: `f.bar().baz()`
///
/// To do this, recursively walk down the chain of method applications until the
/// base is a variable.  Any unexpected forms should trigger an error.
fn decompose_relational_expression(
    plan_env: &mut PlanEnv,
    rel_expr: &Expr<Typed>
) -> anyhow::Result<DecomposedRelationalExpression> {
    match &rel_expr.expr {
        Expr_::QualifiedAccess { base, method_name, operands } => {
            // Keep descending until the base is no longer relational. If we
            // find a base that is not relational, the *current* term (rel_expr)
            // is the relational generator.
            match base.type_ {
                Type::Relational(_) => {
                    let mut decomp = decompose_relational_expression(plan_env, base)?;
                    let new_scalar = Expr {
                        expr: Expr_::QualifiedAccess {
                            base: Box::new(decomp.scalar_evaluation),
                            method_name: method_name.clone(),
                            operands: operands.clone(),
                        },
                        type_: rel_expr.type_.base_if_relational(),
                    };
                    decomp.scalar_evaluation = new_scalar;
                    Ok(decomp)
                }
                _ => {
                    let fresh_var_ty = rel_expr.type_.base_if_relational();
                    let var_ident = plan_env.fresh_var(fresh_var_ty.clone());
                    let var_decl = VarDecl::new(var_ident.clone(), fresh_var_ty.clone());
                    let var_ref = Expr {
                        expr: Expr_::VarRef(var_ident.clone()),
                        type_: fresh_var_ty.clone(),
                    };
                    let decomp = DecomposedRelationalExpression {
                        fresh_var: var_decl,
                        relational_generator: rel_expr.clone(),
                        scalar_evaluation: var_ref,
                    };
                    Ok(decomp)
                }
            }
        }
        _ => panic!("Expected only qualified access terms in a relational generator")
    }
}

fn make_bind(
    fresh_var: VarDecl,
    relational_generator: Expr<Typed>,
    evaluated_expr: Expr<Typed>,
) -> Expr<Typed> {
    Expr {
        expr: Expr_::Bind {
            bound_var: fresh_var,
            relation_expr: Box::new(relational_generator),
            evaluated_expr: Box::new(evaluated_expr)
        },
        type_: Type::PrimBoolean,
    }
}

/// Match a relational generator (i.e., a function call that generates a
/// relation) so that we can transform it into a binder against a fresh variable:
///
/// - x.get_relational().<foo>  ->  Bind(fresh, x.get_relational(), fresh.<foo>)
///
/// We have to rewrite operators applied to relational values because they need
/// to be incorporated into the binding term.  Evaluations of relational values
/// in all other contexts can just be rewritten independent of that context, so
/// we just handle the `QualifiedAccess` case, trusting the rest of the compiler
/// to handle them in any context.
fn rewrite_relationals_to_binds(
    plan_env: &mut PlanEnv,
    e: &Expr<Typed>
) -> anyhow::Result<Option<Expr<Typed>>> {
    match &e.expr {
        Expr_::QualifiedAccess { .. } => {
            match e.type_ {
                Type::Relational(_) => {
                    let decomp = decompose_relational_expression(plan_env, e)?;
                    Ok(Some(make_bind(decomp.fresh_var, decomp.relational_generator, decomp.scalar_evaluation)))
                }
                _ => Ok(None),
            }
        }
        Expr_::RelationalComparison { lhs, op, rhs } => {
            match (&lhs.type_, &rhs.type_) {
                (Type::Relational(_), Type::Relational(_)) => {
                    panic!("Comparisons of two relational values are not supported yet");
                }
                (Type::Relational(_), _) => {
                    let decomp = decompose_relational_expression(plan_env, lhs)?;
                    let rhs_b = insert_explicit_binders(plan_env, rhs)?;
                    let eval_expr = Expr {
                        expr: Expr_::RelationalComparison {
                            lhs: Box::new(decomp.scalar_evaluation),
                            op: *op,
                            rhs: Box::new(rhs_b),
                        },
                        type_: Type::PrimBoolean,
                    };
                    Ok(Some(make_bind(decomp.fresh_var, decomp.relational_generator, eval_expr)))
                }
                (_, Type::Relational(_)) => {
                    let decomp = decompose_relational_expression(plan_env, rhs)?;
                    let lhs_b = insert_explicit_binders(plan_env, lhs)?;
                    let eval_expr = Expr {
                        expr: Expr_::RelationalComparison {
                            lhs: Box::new(lhs_b),
                            op: *op,
                            rhs: Box::new(decomp.scalar_evaluation),
                        },
                        type_: Type::PrimBoolean,
                    };
                    Ok(Some(make_bind(decomp.fresh_var, decomp.relational_generator, eval_expr)))
                }
                _ => Ok(None),
            }
        }
        Expr_::EqualityComparison { lhs, op, rhs } => {
            match (&lhs.type_, &rhs.type_) {
                (Type::Relational(_), Type::Relational(_)) => {
                    panic!("Comparisons of two relational values are not supported yet");
                }
                (Type::Relational(_), _) => {
                    let decomp = decompose_relational_expression(plan_env, lhs)?;
                    let rhs_b = insert_explicit_binders(plan_env, rhs)?;
                    let eval_expr = Expr {
                        expr: Expr_::EqualityComparison {
                            lhs: Box::new(decomp.scalar_evaluation),
                            op: *op,
                            rhs: Box::new(rhs_b),
                        },
                        type_: Type::PrimBoolean,
                    };
                    Ok(Some(make_bind(decomp.fresh_var, decomp.relational_generator, eval_expr)))
                }
                (_, Type::Relational(_)) => {
                    let decomp = decompose_relational_expression(plan_env, rhs)?;
                    let lhs_b = insert_explicit_binders(plan_env, lhs)?;
                    let eval_expr = Expr {
                        expr: Expr_::EqualityComparison {
                            lhs: Box::new(lhs_b),
                            op: *op,
                            rhs: Box::new(decomp.scalar_evaluation),
                        },
                        type_: Type::PrimBoolean,
                    };
                    Ok(Some(make_bind(decomp.fresh_var, decomp.relational_generator, eval_expr)))
                }
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

/// Matches a conjunction that binds on the left and then evaluates a sub-tree on the right
///
/// While this could normalize binders to come before evaluated sub-expressions,
/// we currently don't support any rewriting of that form until we work out what
/// the semantics should be.
fn match_binder_form(
    plan_env: &mut PlanEnv,
    e: &Expr<Typed>,
) -> anyhow::Result<Option<(VarDecl, Box<Expr<Typed>>, Box<Expr<Typed>>)>> {
    match &e.expr {
        Expr_::LogicalConjunction {
            lhs: log_lhs,
            rhs: log_rhs,
        } => match &log_lhs.expr {
            Expr_::EqualityComparison {
                lhs: eq_lhs,
                op,
                rhs: eq_rhs,
            } => match (&eq_lhs.expr, op, &eq_rhs.expr) {
                (Expr_::VarRef(name), EqualityOp::EQ, Expr_::QualifiedAccess { .. }) => {
                    let var_decl = VarDecl {
                        name: name.clone(),
                        type_: eq_lhs.type_.clone(),
                    };
                    let eval_expr = insert_explicit_binders(plan_env, log_rhs)?;
                    Ok(Some((var_decl, Box::new(*eq_rhs.clone()), Box::new(eval_expr))))
                }
                (Expr_::QualifiedAccess { .. }, EqualityOp::EQ, Expr_::VarRef(name)) => {
                    let var_decl = VarDecl {
                        name: name.clone(),
                        type_: eq_lhs.type_.clone(),
                    };
                    let eval_expr = insert_explicit_binders(plan_env, log_rhs)?;
                    Ok(Some((var_decl, Box::new(*eq_lhs.clone()), Box::new(eval_expr))))
                }
                _ => Ok(None),
            },
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

/// Replace any relational equalities with explicit binders (the `Bind`
/// expression form)
///
/// Our restricted form of relational equalities have a variable on one side and
/// a method access on the other.  In general (i.e., in the full datalog
/// evaluation model), all equalities would be relational and evaluated by the
/// datalog engine.
fn insert_explicit_binders(plan_env: &mut PlanEnv, e: &Expr<Typed>) -> anyhow::Result<Expr<Typed>> {
    let mbinder = match_binder_form(plan_env, e)?;
    match mbinder {
        None => {}
        Some((var_decl, access_b, eval_expr)) => {
            let binder = Expr {
                expr: Expr_::Bind {
                    bound_var: var_decl,
                    relation_expr: access_b,
                    evaluated_expr: eval_expr,
                },
                type_: Type::PrimBoolean,
            };
            return Ok(binder);
        }
    }

    match rewrite_relationals_to_binds(plan_env, e)? {
        None => {},
        Some(binder_form) => {
            return Ok(binder_form);
        },
    }

    match &e.expr {
        Expr_::EqualityComparison { lhs, op, rhs } => {
            let lhs_b = insert_explicit_binders(plan_env, lhs)?;
            let rhs_b = insert_explicit_binders(plan_env, rhs)?;
            let e_b = Expr {
                expr: Expr_::EqualityComparison {
                    lhs: Box::new(lhs_b),
                    op: *op,
                    rhs: Box::new(rhs_b),
                },
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::Bind { .. } => {
            panic!("The binder rewriter should never have a `Bind` term as input")
        }
        Expr_::ConstantExpr(_) => Ok(e.clone()),
        Expr_::VarRef(_) => Ok(e.clone()),
        Expr_::RelationalComparison { lhs, op, rhs } => {
            let lhs_b = insert_explicit_binders(plan_env, lhs)?;
            let rhs_b = insert_explicit_binders(plan_env, rhs)?;
            let e_b = Expr {
                expr: Expr_::RelationalComparison {
                    lhs: Box::new(lhs_b),
                    op: *op,
                    rhs: Box::new(rhs_b),
                },
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::LogicalNegation { predicate } => {
            let predicate_b = insert_explicit_binders(plan_env, predicate)?;
            let e_b = Expr {
                expr: Expr_::LogicalNegation {
                    predicate: Box::new(predicate_b),
                },
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::LogicalConjunction { lhs, rhs } => {
            let lhs_b = insert_explicit_binders(plan_env, lhs)?;
            let rhs_b = insert_explicit_binders(plan_env, rhs)?;
            let e_b = Expr {
                expr: Expr_::LogicalConjunction {
                    lhs: Box::new(lhs_b),
                    rhs: Box::new(rhs_b),
                },
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::LogicalDisjunction { lhs, rhs } => {
            let lhs_b = insert_explicit_binders(plan_env, lhs)?;
            let rhs_b = insert_explicit_binders(plan_env, rhs)?;
            let e_b = Expr {
                expr: Expr_::LogicalDisjunction {
                    lhs: Box::new(lhs_b),
                    rhs: Box::new(rhs_b),
                },
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::QualifiedAccess {
            base,
            method_name,
            operands,
        } => {
            let base_b = insert_explicit_binders(plan_env, base)?;
            let mut args_b = Vec::new();
            for arg in operands.iter() {
                let arg_b = insert_explicit_binders(plan_env, arg)?;
                args_b.push(arg_b);
            }

            let e_b = Expr {
                expr: Expr_::QualifiedAccess {
                    base: Box::new(base_b),
                    method_name: method_name.clone(),
                    operands: args_b,
                },
                type_: e.type_.clone(),
            };

            Ok(e_b)
        }
        Expr_::Aggregate { op, operands } => {
            let mut args_b = Vec::new();

            for arg in operands.iter() {
                let arg_b = insert_explicit_binders(plan_env, &arg.expr)?;
                let as_arg_b = AsExpr {
                    expr: arg_b,
                    ident: arg.ident.clone(),
                };
                args_b.push(as_arg_b);
            }

            let e_b = Expr {
                expr: Expr_::Aggregate {
                    op: *op,
                    operands: args_b,
                },
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
    }
}

/// Generate a query plan for the given query that decides the order of
/// evaluation for each variable and expression
pub fn plan_query(typed_query: &TypedQuery) -> anyhow::Result<QueryPlan> {
    let mut plan_env = PlanEnv::new();
    // First, order the declared variables by precedence
    let outermost_var = find_outermost_var_decl(&typed_query.query.var_decls)?;
    let rewritten_expr = insert_explicit_binders(&mut plan_env, &typed_query.query.where_formula)?;

    let mut var_decls = typed_query.query.var_decls.clone();
    var_decls.extend(plan_env.synthetic_vars);

    let qp = QueryPlan {
        selected_exprs: typed_query.query.select_exprs.clone(),
        where_formula: rewritten_expr,
        var_decls: var_decls,
        root_var: outermost_var,
        file_preprocessing: typed_query.needed_file_preprocessing_passes.clone(),
    };

    Ok(qp)
}

impl QueryPlan {
    pub fn to_doc(&self) -> RcDoc<()> {
        let from = RcDoc::text("from ").append(RcDoc::intersperse(
            self.var_decls.iter().map(|v| {
                let doc = RcDoc::as_string(&v.type_)
                    .append(RcDoc::space())
                    .append(RcDoc::as_string(&v.name));
                parens(doc)
            }),
            RcDoc::space(),
        ));
        let select = RcDoc::text("select ").append(RcDoc::intersperse(
            self.selected_exprs.iter().map(|e| e.expr.to_doc()),
            RcDoc::space(),
        ));
        let root = RcDoc::text("root ").append(RcDoc::as_string(&self.root_var.name));
        parens(
            RcDoc::text("query").append(RcDoc::hardline()).append(
                parens(from)
                    .append(RcDoc::hardline())
                    .append(self.where_formula.to_doc())
                    .append(RcDoc::hardline())
                    .append(parens(select))
                    .append(RcDoc::hardline())
                    .append(parens(root))
                    .nest(2),
            ),
        )
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn parens(d: RcDoc<()>) -> RcDoc<()> {
    RcDoc::text("(").append(d).append(RcDoc::text(")"))
}

#[cfg(test)]
fn make_test_plan(ql_query: &str) -> QueryPlan {
    let parsed_query = parse_query(ql_query).unwrap();
    let typed_query = typecheck_query(&parsed_query).unwrap();
    plan_query(&typed_query).unwrap()
}

#[test]
fn test_decompose_single_call() {
    let mut plan_env = PlanEnv::new();
    let obj_ref = Expr {
        expr: Expr_::VarRef(VarIdent::StringIdent("t".into())),
        type_: Type::Type,
    };
    let call_expr = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(obj_ref.clone()), method_name: "getName".into(), operands: Vec::new() },
        type_: Type::PrimString,
    };
    let decomp = decompose_relational_expression(&mut plan_env, &call_expr).unwrap();
    assert!(decomp.fresh_var.type_ == Type::PrimString);

    let expected_eval = Expr {
        expr: Expr_::VarRef(VarIdent::SyntheticIdent(0)),
        type_: Type::PrimString,
    };

    assert!(decomp.scalar_evaluation == expected_eval);
    assert!(decomp.relational_generator == call_expr);
}

#[test]
fn test_decompose_nested_call() {
    let mut plan_env = PlanEnv::new();
    let obj_ref_0 = Expr {
        expr: Expr_::VarRef(VarIdent::StringIdent("f".into())),
        type_: Type::Function,
    };
    let gen_expr_0 = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(obj_ref_0.clone()), method_name: "getAParameter".into(), operands: Vec::new() },
        type_: Type::Relational(Box::new(Type::Parameter)),
    };
    let call_expr_0 = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(gen_expr_0.clone()), method_name: "getName".into(), operands: Vec::new() },
        type_: Type::PrimString,
    };
    let decomp = decompose_relational_expression(&mut plan_env, &call_expr_0).unwrap();
    assert!(decomp.fresh_var.type_ == Type::Parameter);

    let expected_eval_var_ref = Expr {
        expr: Expr_::VarRef(VarIdent::SyntheticIdent(0)),
        type_: Type::Parameter,
    };

    let expected_eval = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(expected_eval_var_ref), method_name: "getName".into(), operands: Vec::new() },
        type_: Type::PrimString,
    };
    assert!(decomp.scalar_evaluation == expected_eval);
    assert!(decomp.relational_generator == gen_expr_0.clone());
}

#[test]
fn test_decompose_nested_call_2() {
    // This one has the generator requiring two levels of call
    let mut plan_env = PlanEnv::new();
    let obj_ref_0 = Expr {
        expr: Expr_::VarRef(VarIdent::StringIdent("f".into())),
        type_: Type::Function,
    };
    let call_expr_0 = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(obj_ref_0.clone()), method_name: "getFile".into(), operands: Vec::new() },
        type_: Type::File,
    };
    let gen_expr_0 = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(call_expr_0.clone()), method_name: "getAnImport".into(), operands: Vec::new() },
        type_: Type::Relational(Box::new(Type::Import)),
    };
    let transform_expr_0 = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(gen_expr_0.clone()), method_name: "getName".into(), operands: Vec::new() },
        type_: Type::Relational(Box::new(Type::PrimString)),
    };
    let decomp = decompose_relational_expression(&mut plan_env, &transform_expr_0).unwrap();
    assert!(decomp.fresh_var.type_ == Type::Import);

    let expected_eval_var_ref = Expr {
        expr: Expr_::VarRef(VarIdent::SyntheticIdent(0)),
        type_: Type::Import,
    };

    let expected_eval = Expr {
        expr: Expr_::QualifiedAccess { base: Box::new(expected_eval_var_ref), method_name: "getName".into(), operands: Vec::new() },
        type_: Type::PrimString,
    };

    assert!(decomp.scalar_evaluation == expected_eval, "Got {:?} but expected\n    {:?}", &decomp.scalar_evaluation, &expected_eval);
    assert!(decomp.relational_generator == gen_expr_0);
}

#[test]
fn rewrites_relational_comparison() {
    // This should be rewritten with explicit binders
    let ql = "from Method m where m.getAParameter().getName() = \"log\" select m";
    let query_plan = make_test_plan(ql);
    // We should see an extra variable
    assert!(query_plan.var_decls.len() == 2);
}

#[test]
fn rewrites_simple_binders() {
    let ql =
        "from Method m, Parameter p where p = m.getAParameter() and p.getName() = \"log\" select p";
    let plan = make_test_plan(ql);
    // We only really need to check the where clause
    assert!(plan.var_decls.len() == 2);
    let var_decl = VarDecl {
        type_: Type::Parameter,
        name: VarIdent::StringIdent("p".into()),
    };
    let method_ref = Expr {
        expr: Expr_::VarRef(VarIdent::StringIdent("m".into())),
        type_: Type::Method,
    };
    let bound_val = Expr {
        expr: Expr_::QualifiedAccess {
            base: Box::new(method_ref),
            method_name: "getAParameter".into(),
            operands: Vec::new(),
        },
        type_: Type::Relational(Box::new(Type::Parameter)),
    };
    let param_ref = Expr {
        expr: Expr_::VarRef(VarIdent::StringIdent("p".into())),
        type_: Type::Parameter,
    };
    let get_name_expr = Expr {
        expr: Expr_::QualifiedAccess {
            base: Box::new(param_ref),
            method_name: "getName".into(),
            operands: Vec::new(),
        },
        type_: Type::PrimString,
    };
    let log_string_expr = Expr {
        expr: Expr_::ConstantExpr(Constant::String_("log".into())),
        type_: Type::PrimString,
    };
    let eval_expr = Expr {
        expr: Expr_::EqualityComparison {
            lhs: Box::new(get_name_expr),
            op: EqualityOp::EQ,
            rhs: Box::new(log_string_expr),
        },
        type_: Type::PrimBoolean,
    };
    let expected_where = Expr {
        expr: Expr_::Bind {
            bound_var: var_decl,
            relation_expr: Box::new(bound_val),
            evaluated_expr: Box::new(eval_expr),
        },
        type_: Type::PrimBoolean,
    };
    assert!(
        plan.where_formula == expected_where,
        "Expected \n`{:?}`, but found \n`{:?}`",
        expected_where,
        plan.where_formula
    );
}

/* [tag:query_plan_structure]

This module builds a query plan that nests queries for relational values in an
efficient order for evaluation.  As an example, if a query selects:

>  Function f, Stmt s

we want the query evaluation to first query tree sitter for all of the functions
and then evaluate the rest of the query for each function.  During that
evaluation, we then want to issue a query for all of the statements in each
function (but only while evaluating that function).  Note that queries are not
required to join these relations in any particular order, as they are completely
declarative.

Because queries must declare all of their variables, we know up-front all of the
types that will be required.  We also know the containment relationships between
each type, as they are declared in the library.

# Approach 1

1. Generate a shell query structure based on the declared variables (most general in the outermost positions)

2. Traverse the where clause to evaluate each clause in the outermost position possible

This might be best phrased as a kind of stratification (by relation level, with
splitting on disjunction).  Problem: this doesn't handle disjunction very well.

# Approach 2

1. Find the "root" variable type to search, which is the most general type
   referenced in the query (this is just a special case because there is no
   explicit binding step for that)

2. Introduce an explicit binding expression as the root

3. Move equalities that bind variables "up" to occur as early as possible in
   evaluation so that bindings occur before uses.  At least they have to be
   floated up before any uses.  The compiler will then handle the binding.  It would become
   an error for a variable to be referenced before it was bound this way (a
   scoping analysis)

   We could explicitly convert relational equalities into binding forms that
   make their iterations explicit

Local search restriction: all of the declared variables must be in a single
sub-tree of the type containment graph. For any query, one of the variable types
must contain all of the others. That variable will be the outermost binder that
is wrapped around the matching query.

Current limitations:

- Queries with two variables of the same type are not yet supported (these are big cross products)

# Examples

Query:

    from Function f, Call c
    where f.getName().regexpMatch("^build.*")
      and f.getACall() = c
      and c.isIndirect()
    select f

Result:

    0: EvaluationStratum(0, Function(f), .getName().regexpMatch(..))
    1: EvaluationStratum(1, Call(c), .isIndirect())

Query:

    from Function f, Call c
    where f.getName().regexpMatch(..)
      and (f.getACall() = c and c.isIndirect())
      or f.isStatic()

*/
