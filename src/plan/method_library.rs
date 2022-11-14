use once_cell::sync::Lazy;
use std::collections::HashMap;

use crate::query::ir::*;
use crate::library::Status;
use crate::library::index::{library_index, MethodIndex, MethodSignature};
use crate::plan::NodeFilter;
use crate::plan::errors::PlanError;
use crate::plan::interface::{NodeMatcher, TreeInterface};

/// A handler for method calls (qualified accesses) in the planner
///
/// The arguments are:
///
/// 1. The `TreeInterface` that abstracts the tree-sitter AST query facility
/// 2. The translated node filter of the receiver object of the method (which may not be needed)
/// 3. The typed operands of the method call
///
/// The callback returns a node filter that is suitable for evaluating the method call
pub struct Handler(pub Box<dyn for <'a> Fn(&'a Box<dyn TreeInterface + 'a>, NodeFilter, &'a Vec<Expr<Typed>>) -> anyhow::Result<NodeFilter> + Send + Sync>);

fn callable_get_name<'a>(ti : &Box<dyn TreeInterface + 'a>, _base : NodeFilter, operands : &'a Vec<Expr<Typed>>) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    // This is not necessarily an assertion, as this may not be supported for
    // the current language (which we don't know).
    let name_matcher = ti.callable_name().ok_or_else(|| PlanError::NotSupported("getNames".into(), "callable".into()))?;
    Ok(NodeFilter::StringComputation(name_matcher))
}

fn string_regexp_match<'a>(_ti : &Box<dyn TreeInterface + 'a>, base : NodeFilter, operands : &'a Vec<Expr<Typed>>) -> anyhow::Result<NodeFilter> {
    assert!(operands.len() == 1);
    // The base must be a string computation (and we require that it was a literal that we were able to already compile)
    let c = match base {
        NodeFilter::StringComputation(c) => c,
        _ => panic!("Invalid base computation for `regexpMatch`")
    };
    let rx : regex::Regex = match &operands[0].expr {
        Expr_::ConstantExpr(Constant::Regex(CachedRegex(_, rx))) => rx.clone(),
        _ => {
            // This should have been caught during type checking, which promotes
            // literal strings to regexes where needed.
            panic!("Invalid regex for `regexpMatch`")
        }
    };
    let comp = NodeMatcher {
        query: c.query,
        extract: Box::new(move |matches, src| {
                    let matched_string = (c.extract)(matches, src);
                    rx.is_match(matched_string.as_ref())
                })
    };
    Ok(NodeFilter::Predicate(comp))
}

/// Validate the implementations of method calls against the claims in the
/// library documentation.  The intent is that the library documentation should
/// always correctly reflect what subset of CodeQL ql-grep supports.  Any
/// discrepancies should turn into panics, as they are programming errors.
///
/// 1. All of the implemented methods are present in the library (and are marked as implemented)
///
/// 2. None of the methods in the library marked as implemented and not present here
fn validate_library(impls : &HashMap<(Type, String), Handler>) {
    let lib_idx = library_index();
    for (base_type, method_name) in impls.keys() {
        match lib_idx.get(base_type) {
            None => {
                panic!("Missing library definition for type `{:?}`", base_type);
            },
            Some(MethodIndex(ref sigs)) => {
                match sigs.get(method_name) {
                    Some(MethodSignature(_name, _arg_types, _ret_type, status)) => {
                        if *status == Some(Status::Unimplemented) {
                            panic!("Method `{}` for type `{:?}` is marked as unimplemented, but has an implementation defined", method_name, base_type);
                        }
                    },
                    None => {
                        panic!("Missing library definition for method `{}` of type `{:?}`", method_name, base_type);
                    }
                }
            }
        }
    }

    // FIXME: This is commented out for now because some methods are handled
    // specially in aggregate contexts and those are not reflected in this
    // check.
    //
    // That could be fixed by actually incorporating them into this machinery
    // and changing the evaluation of the count aggregate.
    //
    // for (ty, MethodIndex(method_idx)) in lib_idx {
    //     for (method_name, MethodSignature(_name, _arg_types, _ret_type, status)) in method_idx {
    //         // Implemented corresponds to None; in the future, this might be a
    //         // more complex type (e.g., an ImplementedSince)
    //         if *status == Some(Status::Unimplemented) {
    //             continue;
    //         }

    //         match impls.get(&(*ty, method_name.into())) {
    //             None => {
    //                 panic!("Method `{}` for type `{:?}` is claimed to be implemented in the library, but has no implementation at runtime", method_name, ty);
    //             },
    //             Some(_) => {}
    //         }
    //     }
    // }
}

/// Implementations of all of the methods supported by ql-grep
///
/// This is used in the query planner to handle the evaluation of predicates
///
/// The keys of the map are the base type and the method name being looked
/// up. The value in the map is the handler for that method in the planner.
static METHOD_IMPLS: Lazy<HashMap<(Type, String), Handler>> = Lazy::new(|| {
    let mut impls = HashMap::new();

    impls.insert((Type::Method, "getName".into()), Handler(Box::new(callable_get_name)));
    impls.insert((Type::Function, "getName".into()), Handler(Box::new(callable_get_name)));
    impls.insert((Type::Callable, "getName".into()), Handler(Box::new(callable_get_name)));

    impls.insert((Type::PrimString, "regexpMatch".into()), Handler(Box::new(string_regexp_match)));

    validate_library(&impls);

    impls
});

pub fn method_impl_for(base_type : Type, method_name : &str) -> Option<&Handler> {
    Lazy::force(&METHOD_IMPLS).get(&(base_type, method_name.into()))
}