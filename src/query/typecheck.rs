use std::collections::{HashMap, HashSet};

use crate::library::index::{library_index, MethodSignature, TypeIndex};
use crate::preprocess::FilePreprocessingPass;
use crate::query::ir::*;
use crate::query::val_type::Type;

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
    #[error(
        "Invalid type for operand {1} in call to method `{0}`: expected `{3:?}` but found `{2:?}`"
    )]
    InvalidTypeForMethodOperand(String, usize, Type, Type),
    #[error("Non-literal regular expression in call to method `{0}` in operand {1}")]
    NonLiteralRegex(String, usize),
    #[error("Invalid regular expression `{0}`: {1}")]
    InvalidRegularExpression(String, regex::Error),
    #[error("Invalid type for expression `{0:?}`, expected `{1:?}` but found `{2:?}`")]
    UnexpectedExpressionType(Expr<Syntax>, Type, Type),
    #[error("Too many arguments provided to the `count` aggregate; expected 1 but found {0}")]
    TooManyArgumentsForCount(usize),
    #[error("Invalid operand type for `count` aggregate; expected either a List or Relation but found `{0:?}`")]
    InvalidOperandTypeForCount(Type),
}

struct TypeEnv {
    /// Records the type of each variable in scope
    env: HashMap<String, Type>,
    /// Provides type signatures for all of the library types and their methods
    type_index: &'static HashMap<Type, TypeIndex>,
    /// File-level preprocessing passes required by methods referenced in the query
    needed_file_preprocessing_passes: HashSet<FilePreprocessingPass>,
}

fn build_initial_type_environment(syntax: &Select<Syntax>) -> TypeEnv {
    let mut res = HashMap::new();

    for decl in &syntax.var_decls {
        res.insert(decl.name.clone(), decl.type_.clone());
    }

    TypeEnv {
        env: res,
        type_index: library_index(),
        needed_file_preprocessing_passes: HashSet::new(),
    }
}

fn evaluates_to_boolean(ty: &Type) -> bool {
    *ty == Type::PrimBoolean || *ty == Type::Relational(Box::new(Type::PrimBoolean))
}

/// If any of the input types are relational, promote the return type to
/// a relational type (otherwise just return the scalar return type)
fn promote_return(scalar_ret_ty: &Type, input_types: &Vec<&Type>) -> Type {
    for inp in input_types {
        if let Type::Relational(_) = inp {
            return Type::Relational(Box::new(scalar_ret_ty.clone()));
        }
    }

    scalar_ret_ty.clone()
}

/// Promote typed operands in argument positions when needed
///
/// There are some operands that we might have to promote implicitly based on
/// the expected argument types of a method (according to the library).  For
/// example, we promote strings to regular expressions (by compiling them).
fn typecheck_operand(
    method_name: &str,
    idx: usize,
    typed_expr: &Expr<Typed>,
    expected_type: Type,
) -> anyhow::Result<Expr<Typed>> {
    match (typed_expr.type_.clone(), expected_type) {
        (Type::PrimString, Type::Regex) => {
            // We do an implicit promotion (since there is no other way to construct these values)
            match &typed_expr.expr {
                Expr_::ConstantExpr(Constant::String_(s)) => {
                    let rx = regex::Regex::new(s).map_err(|rx_err| {
                        anyhow::anyhow!(TypecheckError::InvalidRegularExpression(
                            method_name.into(),
                            rx_err
                        ))
                    })?;
                    let rx_val = CachedRegex(s.clone(), rx);
                    let tv = Expr {
                        expr: Expr_::ConstantExpr(Constant::Regex(rx_val)),
                        type_: Type::Regex,
                    };
                    Ok(tv)
                }
                _ => Err(anyhow::anyhow!(TypecheckError::NonLiteralRegex(
                    method_name.into(),
                    idx
                ))),
            }
        }
        (t1, t2) => {
            if t1 == t2 {
                Ok(typed_expr.clone())
            } else {
                let err =
                    TypecheckError::InvalidTypeForMethodOperand(method_name.into(), idx, t1, t2);
                Err(anyhow::anyhow!(err))
            }
        }
    }
}

fn typecheck_expr(env: &mut TypeEnv, expr: &Expr<Syntax>) -> anyhow::Result<Expr<Typed>> {
    match &expr.expr {
        Expr_::ConstantExpr(c) => match c {
            Constant::Boolean(_) => Ok(Expr {
                expr: Expr_::ConstantExpr(c.clone()),
                type_: Type::PrimBoolean,
            }),
            Constant::Integer(_) => Ok(Expr {
                expr: Expr_::ConstantExpr(c.clone()),
                type_: Type::PrimInteger,
            }),
            Constant::String_(_) => Ok(Expr {
                expr: Expr_::ConstantExpr(c.clone()),
                type_: Type::PrimString,
            }),
            Constant::Regex(_) => Ok(Expr {
                expr: Expr_::ConstantExpr(c.clone()),
                type_: Type::Regex,
            }),
        },
        Expr_::VarRef(var_name) => {
            let ty = env.env.get(var_name).ok_or_else(|| {
                anyhow::anyhow!(TypecheckError::MissingVariableDeclaration(var_name.into()))
            })?;
            Ok(Expr {
                expr: Expr_::VarRef(var_name.into()),
                type_: ty.clone(),
            })
        }
        Expr_::RelationalComparison { lhs, op, rhs } => {
            let lhs_ty = typecheck_expr(env, lhs)?;
            let rhs_ty = typecheck_expr(env, rhs)?;

            // The actual check is to ensure that the comparison operators are
            // actually used safely.  The only allowed types are int (and float
            // which is not yet supported).
            //
            // We could support lexicographic string ordering, but I'd rather
            // not until there is a need.
            match (&lhs_ty.type_, &rhs_ty.type_) {
                (Type::PrimInteger, Type::PrimInteger) => (),
                (Type::Relational(t), Type::PrimInteger) => match **t {
                    Type::PrimInteger => (),
                    _ => {
                        return Err(anyhow::anyhow!(
                            TypecheckError::InvalidRelationalComparison(
                                *lhs.clone(),
                                Type::Relational(t.clone()),
                                *rhs.clone(),
                                Type::PrimInteger
                            )
                        ));
                    }
                },
                (Type::PrimInteger, Type::Relational(t)) => match **t {
                    Type::PrimInteger => (),
                    _ => {
                        return Err(anyhow::anyhow!(
                            TypecheckError::InvalidRelationalComparison(
                                *lhs.clone(),
                                Type::PrimInteger,
                                *rhs.clone(),
                                Type::Relational(t.clone())
                            )
                        ));
                    }
                },
                (lt, rt) => {
                    return Err(anyhow::anyhow!(
                        TypecheckError::InvalidRelationalComparison(
                            *lhs.clone(),
                            lt.clone(),
                            *rhs.clone(),
                            rt.clone()
                        )
                    ));
                }
            }

            let ret_ty = promote_return(&Type::PrimBoolean, &vec![&lhs_ty.type_, &rhs_ty.type_]);
            Ok(Expr {
                expr: Expr_::RelationalComparison {
                    lhs: Box::new(lhs_ty),
                    op: *op,
                    rhs: Box::new(rhs_ty),
                },
                type_: ret_ty,
            })
        }
        Expr_::EqualityComparison { lhs, op, rhs } => {
            let lhs_ty = typecheck_expr(env, lhs)?;
            let rhs_ty = typecheck_expr(env, rhs)?;

            // The actual check is to ensure that the comparison operators are
            // actually used safely.
            if lhs_ty.type_.base_if_relational() != rhs_ty.type_.base_if_relational() {
                return Err(anyhow::anyhow!(
                    TypecheckError::InvalidRelationalComparison(
                        *lhs.clone(),
                        lhs_ty.type_,
                        *rhs.clone(),
                        rhs_ty.type_
                    )
                ));
            }

            let ret_ty = promote_return(&Type::PrimBoolean, &vec![&lhs_ty.type_, &rhs_ty.type_]);
            Ok(Expr {
                expr: Expr_::EqualityComparison {
                    lhs: Box::new(lhs_ty),
                    op: *op,
                    rhs: Box::new(rhs_ty),
                },
                type_: ret_ty,
            })
        }
        Expr_::LogicalNegation { predicate } => {
            let pred_ty = typecheck_expr(env, predicate)?;

            if !evaluates_to_boolean(&pred_ty.type_) {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(
                    *predicate.clone(),
                    Type::PrimBoolean,
                    pred_ty.type_
                )));
            }

            let ret_ty = promote_return(&Type::PrimBoolean, &vec![&pred_ty.type_]);
            Ok(Expr {
                expr: Expr_::LogicalNegation {
                    predicate: Box::new(pred_ty)
                },
                type_: ret_ty
            })
        }
        Expr_::LogicalConjunction { lhs, rhs } => {
            let lhs_ty = typecheck_expr(env, lhs)?;
            let rhs_ty = typecheck_expr(env, rhs)?;

            if !evaluates_to_boolean(&lhs_ty.type_) {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(
                    *lhs.clone(),
                    Type::PrimBoolean,
                    lhs_ty.type_
                )));
            }

            if !evaluates_to_boolean(&rhs_ty.type_) {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(
                    *rhs.clone(),
                    Type::PrimBoolean,
                    rhs_ty.type_
                )));
            }

            let ret_ty = promote_return(&Type::PrimBoolean, &vec![&lhs_ty.type_, &rhs_ty.type_]);
            Ok(Expr {
                expr: Expr_::LogicalConjunction {
                    lhs: Box::new(lhs_ty),
                    rhs: Box::new(rhs_ty),
                },
                type_: ret_ty,
            })
        }
        Expr_::LogicalDisjunction { lhs, rhs } => {
            // Either operand (or both) may be Relational<bool>, in which case we treat that as any(val)
            let lhs_ty = typecheck_expr(env, lhs)?;
            let rhs_ty = typecheck_expr(env, rhs)?;

            if !evaluates_to_boolean(&lhs_ty.type_) {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(
                    *lhs.clone(),
                    Type::PrimBoolean,
                    lhs_ty.type_
                )));
            }

            if !evaluates_to_boolean(&rhs_ty.type_) {
                return Err(anyhow::anyhow!(TypecheckError::UnexpectedExpressionType(
                    *rhs.clone(),
                    Type::PrimBoolean,
                    rhs_ty.type_
                )));
            }

            let ret_ty = promote_return(&Type::PrimBoolean, &vec![&lhs_ty.type_, &rhs_ty.type_]);
            Ok(Expr {
                expr: Expr_::LogicalDisjunction {
                    lhs: Box::new(lhs_ty),
                    rhs: Box::new(rhs_ty),
                },
                type_: ret_ty,
            })
        }
        Expr_::Aggregate { op, operands } => {
            let mut typed_exprs = Vec::new();
            for expr in operands {
                let typed_as_expr = typecheck_as_expr(env, expr)?;
                typed_exprs.push(typed_as_expr);
            }

            // We will want more type checking here in the future, but the only
            // supported operator right now is Count, so anything would be
            // acceptable.  The interpretation is actually a bit tricky in that
            // it makes sense for "relational" values, but not really literals.

            let ty = match op {
                AggregateOp::Count => {
                    // For Count, we only support a single argument that is
                    // either a list or a relational value (which will be
                    // evaluated as a list)
                    if typed_exprs.len() != 1 {
                        return Err(anyhow::anyhow!(TypecheckError::TooManyArgumentsForCount(
                            typed_exprs.len()
                        )));
                    }

                    match &typed_exprs[0].expr.type_ {
                        Type::List(_) => Type::PrimInteger,
                        Type::Relational(_) => Type::PrimInteger,
                        ty => {
                            return Err(anyhow::anyhow!(
                                TypecheckError::InvalidOperandTypeForCount(ty.clone())
                            ));
                        }
                    }
                }
            };

            Ok(Expr {
                expr: Expr_::Aggregate {
                    op: *op,
                    operands: typed_exprs,
                },
                type_: ty,
            })
        }
        Expr_::QualifiedAccess {
            base,
            method_name,
            operands,
        } => {
            let mut typed_operands = Vec::new();
            for op in operands {
                let typed_op = typecheck_expr(env, op)?;
                typed_operands.push(typed_op);
            }

            let base_ty = typecheck_expr(env, base)?;

            // We have an interesting case here. If the base is of type
            // "Relational<T>", we type check it as a single value of type T (but
            // evaluate it as a List<T>).
            let scalar_base_ty = base_ty.type_.base_if_relational();

            let ty_index = env.type_index.get(&scalar_base_ty).ok_or_else(|| {
                anyhow::anyhow!(TypecheckError::InvalidReceiverTypeForMethod(
                    base_ty.type_.clone(),
                    method_name.clone()
                ))
            })?;
            let method_idx = &ty_index.method_index;
            let MethodSignature(accessor, arg_types, ret_ty, _status, requires) =
                method_idx.get(method_name).ok_or_else(|| {
                    anyhow::anyhow!(TypecheckError::InvalidMethodForType(
                        method_name.clone(),
                        base_ty.type_.clone()
                    ))
                })?;

            requires.map(|tag| {
                env.needed_file_preprocessing_passes.insert(tag);
            });

            if operands.len() != arg_types.len() {
                let err = TypecheckError::InvalidArityForMethod(
                    accessor.into(),
                    operands.len(),
                    arg_types.len(),
                );
                return Err(anyhow::anyhow!(err));
            }

            let mname = accessor.clone();

            let mut promoted_operands = Vec::new();
            // Next, we have to typecheck any operands
            //
            // This is a bit tricky since we can do some kinds of implicit
            // promotion here. For example, we learn that the string literal in
            // the first argument to `string.regexpMatch` is actually a Regex,
            // so we can parse and promote it here.
            //
            // NOTE: We do some implicit promotion here, but we do not yet
            // support promotion of relational values as method arguments.
            for (idx, operand) in typed_operands.iter().enumerate() {
                let expected_type = arg_types[idx].clone();
                let typed_operand = typecheck_operand(&mname, idx, operand, expected_type)?;
                promoted_operands.push(typed_operand);
            }

            // If the base type was relational, we need to promote the result
            // type to relational.  We can tell this is the case if the base
            // type and scalarized base type are not equal.
            let promoted_ret_ty = if base_ty.type_ != scalar_base_ty {
                Type::Relational(Box::new(ret_ty.clone()))
            } else {
                ret_ty.clone()
            };

            Ok(Expr {
                expr: Expr_::QualifiedAccess {
                    base: Box::new(base_ty),
                    method_name: method_name.clone(),
                    operands: promoted_operands,
                },
                type_: promoted_ret_ty,
            })
        }
        Expr_::Bind { .. } => {
            panic!("Bind expressions should not exist during type checking");
        }
    }
}

fn typecheck_as_expr(env: &mut TypeEnv, as_expr: &AsExpr<Syntax>) -> anyhow::Result<AsExpr<Typed>> {
    let typed_expr = typecheck_expr(env, &as_expr.expr)?;
    match &as_expr.ident {
        None => {}
        Some(name) => {
            env.env.insert(name.clone(), typed_expr.type_.clone());
        }
    };

    let typed_as_expr = AsExpr {
        expr: typed_expr,
        ident: as_expr.ident.clone(),
    };
    Ok(typed_as_expr)
}

pub struct TypedQuery {
    pub query: Select<Typed>,
    pub needed_file_preprocessing_passes: HashSet<FilePreprocessingPass>,
}

/// Run a type checking pass over a query, filling in types for each IR node
///
/// The return types for methods (which are otherwise not obvious) are
/// automatically derived from the documentation (see `library.kdl`)
pub fn typecheck_query(syntax: &Select<Syntax>) -> anyhow::Result<TypedQuery> {
    // Note that the type environment never really updates because new
    // identifiers are not introduced outside of the declaration clause
    //
    // That might not technically be true because there are `as` operators to
    // bind names to intermediate values.  We may want to support those later.
    let mut type_env = build_initial_type_environment(syntax);

    // Type the selected expressions first, since they are `as` expressions that
    // could in principle bind new names
    let mut typed_selected_exprs = Vec::new();
    for sel in &syntax.select_exprs {
        let typed_sel = typecheck_as_expr(&mut type_env, sel)?;
        typed_selected_exprs.push(typed_sel);
    }

    let typed_where = typecheck_expr(&mut type_env, &syntax.where_formula)?;

    let typed_select = Select {
        select_exprs: typed_selected_exprs,
        where_formula: typed_where,
        var_decls: syntax.var_decls.clone(),
    };

    let tq = TypedQuery {
        query: typed_select,
        needed_file_preprocessing_passes: type_env.needed_file_preprocessing_passes,
    };
    Ok(tq)
}
