/// This module implements the query planner.
///
/// The query planner runs after type checking and before compilation.  Its
/// primary job is to re-arrange the query to be more reflective of the ultimate
/// evaluation strategy. In particular, it collects joins into a natural order
/// that can be efficiently evaluated. The order is determined by the type
/// containment relationships defined in the library.
use std::collections::HashSet;

use crate::library::index::library_index;
use crate::query::ir::{AsExpr, EqualityOp, Expr, Expr_, Select, Typed, VarDecl};
use crate::query::val_type::Type;

#[cfg(test)]
use crate::query::ir::Constant;
#[cfg(test)]
use crate::query::parse_query;
#[cfg(test)]
use crate::query::typecheck::typecheck_query;

/// This wraps up the contents of the original query in a more structured form
pub struct QueryPlan {
    pub selected_exprs: Vec<AsExpr<Typed>>,
    pub where_formula: Expr<Typed>,
    pub var_decls: Vec<VarDecl>,
    pub root_var: VarDecl,
}

#[derive(thiserror::Error, Debug)]
pub enum PlanError {
    #[error(
        "At least one variable is not related to the others by a containment relationship: {0:?}"
    )]
    UnrelatedVariablesError(Vec<VarDecl>),
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
    // let queued_types : HashSet<Type> = HashSet::new();

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

/// Matches a conjunction that binds on the left and then evaluates a sub-tree on the right
///
/// While this could normalize binders to come before evaluated sub-expressions,
/// we currently don't support any rewriting of that form until we work out what
/// the semantics should be.
fn match_binder_form(
    e: &Expr<Typed>,
) -> anyhow::Result<Option<(VarDecl, Box<Expr<Typed>>, Box<Expr<Typed>>)>> {
    match &e.expr {
        Expr_::LogicalConjunction(lhs, rhs) => match &lhs.expr {
            Expr_::EqualityComparison(binder_lhs, op, binder_rhs) => {
                match (&binder_lhs.expr, op, &binder_rhs.expr) {
                    (Expr_::VarRef(name), EqualityOp::EQ, Expr_::QualifiedAccess(_, _, _)) => {
                        let access_b = insert_explicit_binders(binder_rhs)?;
                        let var_decl = VarDecl {
                            name: name.clone(),
                            type_: binder_lhs.type_.clone(),
                        };
                        let eval_expr = insert_explicit_binders(rhs)?;
                        Ok(Some((var_decl, Box::new(access_b), Box::new(eval_expr))))
                    }
                    (Expr_::QualifiedAccess(_, _, _), EqualityOp::EQ, Expr_::VarRef(name)) => {
                        let access_b = insert_explicit_binders(binder_lhs)?;
                        let var_decl = VarDecl {
                            name: name.clone(),
                            type_: binder_lhs.type_.clone(),
                        };
                        let eval_expr = insert_explicit_binders(rhs)?;
                        Ok(Some((var_decl, Box::new(access_b), Box::new(eval_expr))))
                    }
                    _ => Ok(None),
                }
            }
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
fn insert_explicit_binders(e: &Expr<Typed>) -> anyhow::Result<Expr<Typed>> {
    let mbinder = match_binder_form(e)?;
    match mbinder {
        None => {}
        Some((var_decl, access_b, eval_expr)) => {
            let binder = Expr {
                expr: Expr_::Bind(var_decl, access_b, eval_expr),
                type_: Type::PrimBoolean,
            };
            return Ok(binder);
        }
    }

    match &e.expr {
        Expr_::EqualityComparison(lhs, op, rhs) => {
            let lhs_b = insert_explicit_binders(lhs)?;
            let rhs_b = insert_explicit_binders(rhs)?;
            let e_b = Expr {
                expr: Expr_::EqualityComparison(Box::new(lhs_b), *op, Box::new(rhs_b)),
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::Bind(_, _, _) => {
            panic!("The binder rewriter should never have a `Bind` term as input")
        }
        Expr_::ConstantExpr(_) => Ok(e.clone()),
        Expr_::VarRef(_) => Ok(e.clone()),
        Expr_::RelationalComparison(lhs, op, rhs) => {
            let lhs_b = insert_explicit_binders(lhs)?;
            let rhs_b = insert_explicit_binders(rhs)?;
            let e_b = Expr {
                expr: Expr_::RelationalComparison(Box::new(lhs_b), *op, Box::new(rhs_b)),
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::LogicalConjunction(lhs, rhs) => {
            let lhs_b = insert_explicit_binders(lhs)?;
            let rhs_b = insert_explicit_binders(rhs)?;
            let e_b = Expr {
                expr: Expr_::LogicalConjunction(Box::new(lhs_b), Box::new(rhs_b)),
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::LogicalDisjunction(lhs, rhs) => {
            let lhs_b = insert_explicit_binders(lhs)?;
            let rhs_b = insert_explicit_binders(rhs)?;
            let e_b = Expr {
                expr: Expr_::LogicalDisjunction(Box::new(lhs_b), Box::new(rhs_b)),
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
        Expr_::QualifiedAccess(base, name, args) => {
            let base_b = insert_explicit_binders(base)?;
            let mut args_b = Vec::new();
            for arg in args.iter() {
                let arg_b = insert_explicit_binders(arg)?;
                args_b.push(arg_b);
            }

            let e_b = Expr {
                expr: Expr_::QualifiedAccess(Box::new(base_b), name.clone(), args_b),
                type_: e.type_.clone(),
            };

            Ok(e_b)
        }
        Expr_::Aggregate(op, args) => {
            let mut args_b = Vec::new();

            for arg in args.iter() {
                let arg_b = insert_explicit_binders(&arg.expr)?;
                let as_arg_b = AsExpr {
                    expr: arg_b,
                    ident: arg.ident.clone(),
                };
                args_b.push(as_arg_b);
            }

            let e_b = Expr {
                expr: Expr_::Aggregate(*op, args_b),
                type_: e.type_.clone(),
            };
            Ok(e_b)
        }
    }
}

/// Generate a query plan for the given query that decides the order of
/// evaluation for each variable and expression
pub fn plan_query(query: &Select<Typed>) -> anyhow::Result<QueryPlan> {
    // First, order the declared variables by precedence
    // let stratified = stratify_actions(&sel.where_formula)?;
    let outermost_var = find_outermost_var_decl(&query.var_decls)?;
    let rewritten_expr = insert_explicit_binders(&query.where_formula)?;

    let qp = QueryPlan {
        selected_exprs: query.select_exprs.clone(),
        where_formula: rewritten_expr,
        var_decls: query.var_decls.clone(),
        root_var: outermost_var,
    };

    Ok(qp)
}

#[test]
fn rewrites_simple_binders() {
    let ql =
        "from Method m, Parameter p where p = m.getAParameter() and p.getName() = \"log\" select p";
    let parsed_query = parse_query(ql).unwrap();
    let typed_select = typecheck_query(parsed_query).unwrap();
    let plan = plan_query(&typed_select).unwrap();
    // We only really need to check the where clause
    assert!(plan.var_decls.len() == 2);
    let var_decl = VarDecl {
        type_: Type::Parameter,
        name: "p".into(),
    };
    let method_ref = Expr {
        expr: Expr_::VarRef("m".into()),
        type_: Type::Method,
    };
    let bound_val = Expr {
        expr: Expr_::QualifiedAccess(Box::new(method_ref), "getAParameter".into(), Vec::new()),
        type_: Type::Relational(Box::new(Type::Parameter)),
    };
    let param_ref = Expr {
        expr: Expr_::VarRef("p".into()),
        type_: Type::Parameter,
    };
    let get_name_expr = Expr {
        expr: Expr_::QualifiedAccess(Box::new(param_ref), "getName".into(), Vec::new()),
        type_: Type::PrimString,
    };
    let log_string_expr = Expr {
        expr: Expr_::ConstantExpr(Constant::String_("log".into())),
        type_: Type::PrimString,
    };
    let eval_expr = Expr {
        expr: Expr_::EqualityComparison(
            Box::new(get_name_expr),
            EqualityOp::EQ,
            Box::new(log_string_expr),
        ),
        type_: Type::PrimBoolean,
    };
    let expected_where = Expr {
        expr: Expr_::Bind(var_decl, Box::new(bound_val), Box::new(eval_expr)),
        type_: Type::PrimBoolean,
    };
    assert!(
        plan.where_formula == expected_where,
        "Expected \n`{:?}`, but found \n`{:?}`",
        expected_where,
        plan.where_formula
    );
}

/* Note [Query Plan Structure]

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
