use std::collections::HashMap;

use crate::library::index::{library_index, MethodIndex, MethodSignature};
use crate::query::ir::*;

#[derive(thiserror::Error, Debug)]
pub enum TypecheckError {
    #[error("Reference to variable `{0}` without declaration")]
    MissingVariableDeclaration(String),
    #[error("Invalid comparison of type `{1:?}` (term `{0:?}`) against `{3:?}` (term `{2:?}`)")]
    InvalidRelationalComparison(Expr<Syntax>, Type, Expr<Syntax>, Type),
    #[error("Invalid receiver type `{0:?}` for method `{1}`")]
    InvalidReceiverTypeForMethod(Type, String),
    #[error("Invalid method `{0}` for expression of type `{1:?}`")]
    InvalidMethodForType(String, Type),
    #[error("Invalid arity for method `{0}`; found {1} but expected {2} arguments")]
    InvalidArityForMethod(String, usize, usize),
    #[error("Invalid type for operand {1} in call to method `{0}`: expected `{3:?}` but found `{2:?}`")]
    InvalidTypeForMethodOperand(String, usize, Type, Type),
    #[error("Non-literal regular expression in call to method `{0}` in operand {1}")]
    NonLiteralRegex(String, usize),
    #[error("Invalid regular expression `{0}`: {1}")]
    InvalidRegularExpression(String, regex::Error),
    #[error("Invalid type for expression `{0:?}`, expected `{1:?}` but found `{2:?}`")]
    UnexpectedExpressionType(Expr<Syntax>, Type, Type)
}

struct TypeEnv {
    /// Records the type of each variable in scope
    env: HashMap<String, Type>,
    /// Provides type signatures for all of the library types and their methods
    type_index: &'static HashMap<Type, MethodIndex>,
}

fn build_initial_type_environment(syntax : &Select<Syntax>) -> TypeEnv {
    let mut res = HashMap::new();

    for decl in &syntax.var_decls {
        res.insert(decl.name.clone(), decl.type_);
    }

    TypeEnv {
        env: res,
        type_index: library_index(),
    }
}

/// Promote typed operands in argument positions when needed
///
/// There are some operands that we might have to promote implicitly based on
/// the expected argument types of a method (according to the library).  For
/// example, we promote strings to regular expressions (by compiling them).
fn typecheck_operand(method_name : &str,
                     idx : usize,
                     typed_expr : &Expr<Typed>,
                     expected_type : Type) -> anyhow::Result<Expr<Typed>> {
    match (typed_expr.type_, expected_type) {
        (Type::PrimString, Type::Regex) => {
            // We do an implicit promotion (since there is no other way to construct these values)
            match &typed_expr.expr {
                Expr_::ConstantExpr(Constant::String_(s)) => {
                    let rx = regex::Regex::new(&s).or_else(|rx_err| Err(anyhow::anyhow!(TypecheckError::InvalidRegularExpression(method_name.into(), rx_err))))?;
                    let rx_val = CachedRegex(s.clone(), rx);
                    let tv = Expr {
                        expr: Expr_::ConstantExpr(Constant::Regex(rx_val)),
                        type_: Type::Regex
                    };
                    return Ok(tv);
                },
                _ => {
                    return Err(anyhow::anyhow!(TypecheckError::NonLiteralRegex(method_name.into(), idx)));
                }
            }
        },
        (t1, t2) => {
            if t1 == t2 {
                return Ok(typed_expr.clone());
            } else {
                let err = TypecheckError::InvalidTypeForMethodOperand(method_name.into(), idx, t1, t2);
                return Err(anyhow::anyhow!(err));
            }
        }
    }
}

fn typecheck_expr(env : &mut TypeEnv, expr : &Expr<Syntax>) -> anyhow::Result<Expr<Typed>> {
    match &expr.expr {
        Expr_::ConstantExpr(c) => {
            match c {
                Constant::Boolean(_) => {
                    return Ok(Expr {
                        expr: Expr_::ConstantExpr(c.clone()),
                        type_: Type::PrimBoolean
                    })
                },
                Constant::Integer(_) => {
                    return Ok(Expr {
                        expr: Expr_::ConstantExpr(c.clone()),
                        type_: Type::PrimInteger
                    })
                },
                Constant::String_(_) => {
                    return Ok(Expr {
                        expr: Expr_::ConstantExpr(c.clone()),
                        type_: Type::PrimString
                    })
                },
                Constant::Regex(_) => {
                    return Ok(Expr {
                        expr: Expr_::ConstantExpr(c.clone()),
                        type_: Type::Regex
                    })
                }
            }
        },
        Expr_::VarRef(var_name) => {
            let ty = env.env.get(var_name).ok_or(anyhow::anyhow!(TypecheckError::MissingVariableDeclaration(var_name.into())))?;
            return Ok(Expr {
                expr: Expr_::VarRef(var_name.into()),
                type_: *ty
            });
        },
        Expr_::RelationalComparison(lhs, op, rhs) => {
            let lhs_ty = typecheck_expr(env, &lhs)?;
            let rhs_ty = typecheck_expr(env, &rhs)?;

            // The actual check is to ensure that the comparison operators are
            // actually used safely.  The only allowed types are int (and float
            // which is not yet supported).
            //
            // We could support lexicographic string ordering, but I'd rather
            // not until there is a need.
            match (lhs_ty.type_, rhs_ty.type_) {
                (Type::PrimInteger, Type::PrimInteger) => (),
                (lt, rt) => {
                    return Err(anyhow::anyhow!(TypecheckError::InvalidRelationalComparison(*lhs.clone(), lt, *rhs.clone(), rt)));
                }
            }

            return Ok(Expr {
                expr: Expr_::RelationalComparison(Box::new(lhs_ty), *op, Box::new(rhs_ty)),
                type_: Type::PrimBoolean
            });
        },
        Expr_::EqualityComparison(lhs, op, rhs) => {
            let lhs_ty = typecheck_expr(env, &lhs)?;
            let rhs_ty = typecheck_expr(env, &rhs)?;

            // The actual check is to ensure that the comparison operators are
            // actually used safely.
            if lhs_ty.type_ != rhs_ty.type_ {
                return Err(anyhow::anyhow!(TypecheckError::InvalidRelationalComparison(*lhs.clone(), lhs_ty.type_, *rhs.clone(), rhs_ty.type_)));
            }

            return Ok(Expr {
                expr: Expr_::EqualityComparison(Box::new(lhs_ty), *op, Box::new(rhs_ty)),
                type_: Type::PrimBoolean
            });
        },
        Expr_::LogicalConjunction(lhs, rhs) => {
            let lhs_ty = typecheck_expr(env, &lhs)?;
            let rhs_ty = typecheck_expr(env, &rhs)?;

            if lhs_ty.type_ != Type::PrimBoolean {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(*lhs.clone(), Type::PrimBoolean, lhs_ty.type_)));
            }

            if rhs_ty.type_ != Type::PrimBoolean {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(*rhs.clone(), Type::PrimBoolean, rhs_ty.type_)));
            }

            return Ok(Expr {
                expr: Expr_::LogicalConjunction(Box::new(lhs_ty), Box::new(rhs_ty)),
                type_: Type::PrimBoolean
            });
        },
        Expr_::LogicalDisjunction(lhs, rhs) => {
            let lhs_ty = typecheck_expr(env, &lhs)?;
            let rhs_ty = typecheck_expr(env, &rhs)?;

            if lhs_ty.type_ != Type::PrimBoolean {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(*lhs.clone(), Type::PrimBoolean, lhs_ty.type_)));
            }

            if rhs_ty.type_ != Type::PrimBoolean {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(*rhs.clone(), Type::PrimBoolean, rhs_ty.type_)));
            }

            return Ok(Expr {
                expr: Expr_::LogicalDisjunction(Box::new(lhs_ty), Box::new(rhs_ty)),
                type_: Type::PrimBoolean
            });
        },
        Expr_::Aggregate(op, exprs) => {
            let mut typed_exprs = Vec::new();
            for expr in exprs {
                let typed_as_expr = typecheck_as_expr(env, &expr)?;
                typed_exprs.push(typed_as_expr);
            }

            // We will want more type checking here in the future, but the only
            // supported operator right now is Count, so anything would be
            // acceptable.  The interpretation is actually a bit tricky in that
            // it makes sense for "relational" values, but not really literals.

            let ty = match op {
                AggregateOp::Count => Type::PrimInteger,
            };

            return Ok(Expr {
                expr: Expr_::Aggregate(*op, typed_exprs),
                type_: ty
            });
        },
        Expr_::QualifiedAccess(base, accessor, operands) => {
            let mut typed_operands = Vec::new();
            for op in operands {
                let typed_op = typecheck_expr(env, op)?;
                typed_operands.push(typed_op);
            }

            let base_ty = typecheck_expr(env, &base)?;
            let MethodIndex(method_idx) = env.type_index.get(&base_ty.type_)
                .ok_or(anyhow::anyhow!(TypecheckError::InvalidReceiverTypeForMethod(base_ty.type_, accessor.clone())))?;
            let MethodSignature(method_name, arg_types, ret_ty, _status) = method_idx.get(accessor)
                .ok_or(anyhow::anyhow!(TypecheckError::InvalidMethodForType(accessor.clone(), base_ty.type_)))?;

            if operands.len() != arg_types.len() {
                let err = TypecheckError::InvalidArityForMethod(method_name.into(), operands.len(), arg_types.len());
                return Err(anyhow::anyhow!(err));
            }

            let mname = method_name.clone();

            let mut promoted_operands = Vec::new();
            // Next, we have to typecheck any operands
            //
            // This is a bit tricky since we can do some kinds of implicit
            // promotion here. For example, we learn that the string literal in
            // the first argument to `string.regexpMatch` is actually a Regex,
            // so we can parse and promote it here.
            for (idx, operand) in typed_operands.iter().enumerate() {
                let expected_type = arg_types[idx];
                let typed_operand = typecheck_operand(&mname, idx, operand, expected_type)?;
                promoted_operands.push(typed_operand);
            }

            return Ok(Expr {
                expr: Expr_::QualifiedAccess(Box::new(base_ty), accessor.clone(), promoted_operands),
                type_: *ret_ty
            });
        },
    }
}

fn typecheck_as_expr(env : &mut TypeEnv, as_expr : &AsExpr<Syntax>) -> anyhow::Result<AsExpr<Typed>> {
    let typed_expr = typecheck_expr(env, &as_expr.expr)?;
    match &as_expr.ident {
        None => {},
        Some(name) => {
            env.env.insert(name.clone(), typed_expr.type_);
        }
    };

    let typed_as_expr = AsExpr {
        expr: typed_expr,
        ident: as_expr.ident.clone(),
    };
    Ok(typed_as_expr)
}

/// Run a type checking pass over a query, filling in types for each IR node
///
/// The return types for methods (which are otherwise not obvious) are
/// automatically derived from the documentation (see `library.kdl`)
pub fn typecheck_query(syntax : Select<Syntax>) -> anyhow::Result<Select<Typed>> {
    // Note that the type environment never really updates because new
    // identifiers are not introduced outside of the declaration clause
    //
    // That might not technically be true because there are `as` operators to
    // bind names to intermediate values.  We may want to support those later.
    let mut type_env = build_initial_type_environment(&syntax);

    // Type the selected expressions first, since they are `as` expressions that
    // could in principle bind new names
    let mut typed_selected_exprs = Vec::new();
    for sel in &syntax.select_exprs {
        let typed_sel = typecheck_as_expr(&mut type_env, &sel)?;
        typed_selected_exprs.push(typed_sel);
    }

    let mut typed_where = None;
    match syntax.where_formula {
        None => {},
        Some(f) => {
            let typed_f = typecheck_expr(&mut type_env, &f)?;
            typed_where = Some(typed_f);
        }
    }

    let typed_select = Select {
        select_exprs: typed_selected_exprs,
        where_formula: typed_where,
        var_decls: syntax.var_decls,
    };
    Ok(typed_select)
}
