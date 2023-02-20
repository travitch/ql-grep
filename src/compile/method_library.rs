mod call;
mod callable;
mod expr;
pub mod handler;
mod parameter;

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::interface::{NodeListMatcher, NodeMatcher, TreeInterface};
use crate::compile::NodeFilter;
use crate::library::index::{library_index, MethodSignature};
use crate::library::Status;
use crate::query::ir::*;
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;
pub use crate::compile::method_library::handler::Handler;
use crate::compile::method_library::call::CALL_METHODS;
use crate::compile::method_library::callable::CALLABLE_METHODS;
use crate::compile::method_library::expr::EXPR_METHODS;
use crate::compile::method_library::parameter::PARAMETER_METHODS;

/// Match a string against a regular expression
///
/// Implements:
/// - [ref:library:string:regexpMatch]
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
    let get_string = Rc::clone(&c.extract);
    let comp = NodeMatcher {
        extract: Rc::new(move |ctx, source| {
            get_string(ctx, source).map(|matched_string| {
                WithRanges::new(
                    rx.is_match(matched_string.value.as_ref()),
                    vec![matched_string.ranges],
                )
            })
        }),
    };
    Ok(NodeFilter::Predicate(comp))
}

/// Get the name of a Type as a String
///
/// Implements:
/// - [ref:library:Type:getName]
fn type_get_name<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::TypeComputation(tc) => {
            let get_type = Rc::clone(&tc.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_type(ctx, source).map(|type_result| {
                        WithRanges::new(type_result.value.as_type_string(), vec![type_result.ranges])
                    })
                }),
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

/// Get the relational list of imports for a file.
///
/// Since there is only ever one file in scope at a time, we never inspect the
/// base and just pull the data out of the evaluation context.
///
/// Implements:
/// - [ref:library:File:getAnImport]
fn file_get_an_import<'a>(
    _ti: Rc<dyn TreeInterface>,
    _base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    let comp = NodeListMatcher {
        extract: Rc::new(move |ctx, _source| {
            ctx.imports()
                .iter()
                .cloned()
                .map(WithRanges::value)
                .collect::<Vec<_>>()
        }),
    };
    Ok(NodeFilter::ImportListComputation(comp))
}

/// Get the name (text) of an import
///
/// Implements:
/// - [ref:library:Import:getName]
fn import_get_name<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ImportComputation(c) => {
            let get_import = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_import(ctx, source).map(|import_res| {
                        WithRanges::new(import_res.value.to_string(), vec![import_res.ranges])
                    })
                }),
            };
            Ok(NodeFilter::StringComputation(comp))
        }
        nf => {
            panic!("Impossible value type for `import_get_name`: {}", nf.kind());
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
                panic!("Missing library definition for type `{base_type:?}`");
            }
            Some(ty_index) => {
                let sigs = &ty_index.method_index;
                match sigs.get(method_name) {
                    Some(MethodSignature(_name, _arg_types, _ret_type, status, _requires)) => {
                        if *status == Some(Status::Unimplemented) {
                            panic!("Method `{method_name}` for type `{base_type:?}` is marked as unimplemented, but has an implementation defined");
                        }
                    }
                    None => {
                        panic!("Missing library definition for method `{method_name}` of type `{base_type:?}`");
                    }
                }
            }
        }
    }

    for (ty, ty_index) in lib_idx {
        for (method_name, MethodSignature(_name, _arg_types, _ret_type, status, _requires)) in
            &ty_index.method_index
        {
            // Implemented corresponds to None; in the future, this might be a
            // more complex type (e.g., an ImplementedSince)
            if *status == Some(Status::Unimplemented) {
                continue;
            }

            match impls.get(&(ty.clone(), method_name.into())) {
                None => {
                    panic!("Method `{method_name}` for type `{ty:?}` is claimed to be implemented in the library, but has no implementation at runtime");
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
///
/// [tag:method_impls_map]
static METHOD_IMPLS: Lazy<HashMap<(Type, String), Handler>> = Lazy::new(|| {
    let mut impls = HashMap::new();

    impls.insert(
        (Type::PrimString, "regexpMatch".into()),
        Handler(Arc::new(string_regexp_match)),
    );

    impls.insert(
        (Type::Type, "getName".into()),
        Handler(Arc::new(type_get_name)),
    );

    impls.insert(
        (Type::File, "getAnImport".into()),
        Handler(Arc::new(file_get_an_import)),
    );

    impls.insert(
        (Type::Import, "getName".into()),
        Handler(Arc::new(import_get_name)),
    );

    CALLABLE_METHODS.iter().for_each(|(name, handler)| {
        impls.insert((Type::Callable, (*name).into()), handler.clone());
        impls.insert((Type::Function, (*name).into()), handler.clone());
        impls.insert((Type::Method, (*name).into()), handler.clone());
    });

    PARAMETER_METHODS.iter().for_each(|(ty, name, handler)| {
        impls.insert((ty.clone(), (*name).into()), handler.clone());
    });

    CALL_METHODS.iter().for_each(|(ty, name, handler)| {
        impls.insert((ty.clone(), (*name).into()), handler.clone());
    });

    EXPR_METHODS.iter().for_each(|(ty, name, handler)| {
        impls.insert((ty.clone(), (*name).into()), handler.clone());
    });

    validate_library(&impls);

    impls
});

pub fn method_impl_for(base_type: Type, method_name: String) -> Option<&'static Handler> {
    Lazy::force(&METHOD_IMPLS).get(&(base_type, method_name))
}
