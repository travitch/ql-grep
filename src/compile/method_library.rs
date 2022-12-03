use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::library::index::{library_index, MethodSignature};
use crate::library::Status;
use crate::compile::errors::PlanError;
use crate::compile::interface::{LanguageType, NodeMatcher, TreeInterface};
use crate::compile::NodeFilter;
use crate::query::ir::*;
use crate::query::val_type::Type;

/// A handler for method calls (qualified accesses) in the planner
///
/// The arguments are:
///
/// 1. The `TreeInterface` that abstracts the tree-sitter AST query facility
/// 2. The translated node filter of the receiver object of the method (which may not be needed)
/// 3. The typed operands of the method call
///
/// The callback returns a node filter that is suitable for evaluating the method call
///
/// FIXME: The arguments need to be run-time values
pub struct Handler(
    pub  Arc<
        dyn for<'a> Fn(
                Rc<dyn TreeInterface>,
                &'a NodeFilter,
                &'a Vec<Expr<Typed>>,
            ) -> anyhow::Result<NodeFilter>
            + Send
            + Sync,
    >,
);

fn callable_get_name<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    // This is not necessarily an assertion, as this may not be supported for
    // the current language (which we don't know).
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let name_matcher = ti
                .callable_name(callable_matcher)
                .ok_or_else(|| PlanError::NotSupported("getNames".into(), "callable".into()))?;
            Ok(NodeFilter::StringComputation(name_matcher))
        }
        _ => {
            panic!("Implementation error: only a callable should be possible here");
        }
    }
}

/// This is a relational method that returns a *list* of parameters
fn callable_get_a_parameter<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let arg_matcher = ti
                .callable_arguments(callable_matcher)
                .ok_or_else(|| PlanError::NotSupported("arguments".into(), "callable".into()))?;
            Ok(NodeFilter::ArgumentListComputation(arg_matcher))
        }
        _ => {
            panic!("Implementation error: only a callable should be possible here");
        }
    }
}

fn string_regexp_match<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.len() == 1);
    // The base must be a string computation (and we require that it was a literal that we were able to already compile)
    let c = match base {
        NodeFilter::StringComputation(c) => c,
        _ => panic!("Invalid base computation for `regexpMatch`"),
    };
    let rx: regex::Regex = match &operands[0].expr {
        Expr_::ConstantExpr(Constant::Regex(CachedRegex(_, rx))) => rx.clone(),
        _ => {
            // This should have been caught during type checking, which promotes
            // literal strings to regexes where needed.
            panic!("Invalid regex for `regexpMatch`")
        }
    };
    let x = Rc::clone(&c.extract);
    let comp = NodeMatcher {
        extract: Rc::new(move |ctx, source| {
            let matched_string = x(ctx, source);
            rx.is_match(matched_string.as_ref())
        }),
    };
    Ok(NodeFilter::Predicate(comp))
}

fn parameter_get_name<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let x = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    x(ctx, source).name.unwrap_or_else(|| "<none>".into())
                }),
            };
            Ok(NodeFilter::StringComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getName");
        }
    }
}

fn parameter_get_type<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let x = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    // FIXME: Probably want to have a better (i.e., more
                    // structured) representation if the type is missing
                    let default_ty = LanguageType::new("<Any>");
                    x(ctx, source).declared_type.unwrap_or(default_ty)
                }),
            };

            Ok(NodeFilter::TypeComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getType");
        }
    }
}

fn type_get_name<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::TypeComputation(tc) => {
            let x = Rc::clone(&tc.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| x(ctx, source).as_type_string()),
            };

            Ok(NodeFilter::StringComputation(comp))
        }
        _ => {
            panic!(
                "Invalid base value of type `{}` for Type.getName",
                base.kind()
            );
        }
    }
}

/// Validate the implementations of method calls against the claims in the
/// library documentation.  The intent is that the library documentation should
/// always correctly reflect what subset of CodeQL ql-grep supports.  Any
/// discrepancies should turn into panics, as they are programming errors.
///
/// 1. All of the implemented methods are present in the library (and are marked as implemented)
///
/// 2. None of the methods in the library marked as implemented and not present here
fn validate_library(impls: &HashMap<(Type, String), Handler>) {
    let lib_idx = library_index();
    for (base_type, method_name) in impls.keys() {
        match lib_idx.get(base_type) {
            None => {
                panic!("Missing library definition for type `{:?}`", base_type);
            }
            Some(ty_index) => {
                let sigs = &ty_index.method_index;
                match sigs.get(method_name) {
                    Some(MethodSignature(_name, _arg_types, _ret_type, status)) => {
                        if *status == Some(Status::Unimplemented) {
                            panic!("Method `{}` for type `{:?}` is marked as unimplemented, but has an implementation defined", method_name, base_type);
                        }
                    }
                    None => {
                        panic!(
                            "Missing library definition for method `{}` of type `{:?}`",
                            method_name, base_type
                        );
                    }
                }
            }
        }
    }

    for (ty, ty_index) in lib_idx {
        for (method_name, MethodSignature(_name, _arg_types, _ret_type, status)) in
            &ty_index.method_index
        {
            // Implemented corresponds to None; in the future, this might be a
            // more complex type (e.g., an ImplementedSince)
            if *status == Some(Status::Unimplemented) {
                continue;
            }

            match impls.get(&(ty.clone(), method_name.into())) {
                None => {
                    panic!("Method `{}` for type `{:?}` is claimed to be implemented in the library, but has no implementation at runtime", method_name, ty);
                }
                Some(_) => {}
            }
        }
    }
}

/// Implementations of all of the methods supported by ql-grep
///
/// This is used in the query planner to handle the evaluation of predicates
///
/// The keys of the map are the base type and the method name being looked
/// up. The value in the map is the handler for that method in the planner.
static METHOD_IMPLS: Lazy<HashMap<(Type, String), Handler>> = Lazy::new(|| {
    let mut impls = HashMap::new();

    impls.insert(
        (Type::Method, "getName".into()),
        Handler(Arc::new(callable_get_name)),
    );
    impls.insert(
        (Type::Function, "getName".into()),
        Handler(Arc::new(callable_get_name)),
    );
    impls.insert(
        (Type::Callable, "getName".into()),
        Handler(Arc::new(callable_get_name)),
    );

    impls.insert(
        (Type::Method, "getAParameter".into()),
        Handler(Arc::new(callable_get_a_parameter)),
    );
    impls.insert(
        (Type::Function, "getAParameter".into()),
        Handler(Arc::new(callable_get_a_parameter)),
    );
    impls.insert(
        (Type::Callable, "getAParameter".into()),
        Handler(Arc::new(callable_get_a_parameter)),
    );

    impls.insert(
        (Type::Parameter, "getName".into()),
        Handler(Arc::new(parameter_get_name)),
    );
    impls.insert(
        (Type::Parameter, "getType".into()),
        Handler(Arc::new(parameter_get_type)),
    );

    impls.insert(
        (Type::PrimString, "regexpMatch".into()),
        Handler(Arc::new(string_regexp_match)),
    );

    impls.insert(
        (Type::Type, "getName".into()),
        Handler(Arc::new(type_get_name)),
    );

    validate_library(&impls);

    impls
});

pub fn method_impl_for(base_type: Type, method_name: String) -> Option<&'static Handler> {
    Lazy::force(&METHOD_IMPLS).get(&(base_type, method_name))
}
