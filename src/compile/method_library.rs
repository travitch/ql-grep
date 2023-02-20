use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::compile::errors::PlanError;
use crate::compile::interface::{LanguageType, NodeListMatcher, NodeMatcher, TreeInterface};
use crate::compile::NodeFilter;
use crate::library::index::{library_index, MethodSignature};
use crate::library::Status;
use crate::query::ir::*;
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;

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

/// Get the name of a callable using the language interface
///
/// Implements:
/// - [ref:library:Function:getName]
/// - [ref:library:Method:getName]
/// - [ref:library:Callable:getName]
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
///
/// Implements:
/// - [ref:library:Function:getAParameter]
/// - [ref:library:Method:getAParameter]
/// - [ref:library:Callable:getAParameter]
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

/// Returns True if the method contains a parse error
///
/// Implements:
/// - [ref:library:Callable:hasParseError]
/// - [ref:library:Function:hasParseError]
/// - [ref:library:Method:hasParseError]
fn callable_has_parse_error<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let error_matcher = ti.callable_has_parse_error(callable_matcher);
            Ok(NodeFilter::Predicate(error_matcher))
        }
        _ => {
            panic!("Implementation error: only callables should be possible here");
        }
    }
}

/// Gets the return type of the callable
///
/// Implements:
/// - [ref:library:Callable:getType]
/// - [ref:library:Function:getType]
/// - [ref:library:Method:getType]
fn callable_get_return_type<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(callable_matcher) => {
            let ty_matcher = ti.callable_return_type(callable_matcher).ok_or_else(|| {
                PlanError::NotSupported("get-return-type".into(), "callable".into())
            })?;
            Ok(NodeFilter::TypeComputation(ty_matcher))
        }
        _ => {
            panic!("Implementation error: only callables should be possible here");
        }
    }
}

/// Get a reference to the file containing the callable.
///
/// Note that this doesn't actually do much, as there is only ever one file in
/// scope and the reference is in the evaluation context.
///
/// Implements:
/// - [ref:library:Callable:getFile]
/// - [ref:library:Function:getFile]
/// - [ref:library:Method:getFile]
fn callable_get_file<'a>(
    _ti: Rc<dyn TreeInterface>,
    _base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    Ok(NodeFilter::FileComputation)
}

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

/// Get the name of a formal parameter (of a callable object)
///
/// Implements:
/// - [ref:library:Parameter:getName]
fn parameter_get_name<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let get_argument = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_argument(ctx, source).map(|argument_result| {
                        WithRanges::new(
                            argument_result
                                .value
                                .name
                                .unwrap_or_else(|| "<none>".into()),
                            vec![argument_result.ranges],
                        )
                    })
                }),
            };
            Ok(NodeFilter::StringComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getName");
        }
    }
}

/// Get the type of a formal parameter (of a callable object)
///
/// Implements:
/// - [ref:library:Parameter:getType]
fn parameter_get_type<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let get_argument = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    // FIXME: Probably want to have a better (i.e., more
                    // structured) representation if the type is missing
                    let default_ty = LanguageType::new("<Any>");
                    get_argument(ctx, source).map(|argument_result| {
                        WithRanges::new(
                            argument_result.value.declared_type.unwrap_or(default_ty),
                            vec![argument_result.ranges],
                        )
                    })
                }),
            };

            Ok(NodeFilter::TypeComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getType");
        }
    }
}

/// Get the index of the parameter in the parameter list
///
/// Implements:
/// - [ref:library:Parameter:getIndex]
fn parameter_get_index<'a>(
    _ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::ArgumentComputation(c) => {
            let get_argument = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_argument(ctx, source).map(|argument_result| {
                        WithRanges::new(
                            argument_result.value.index as i32,
                            vec![argument_result.ranges],
                        )
                    })
                }),
            };
            Ok(NodeFilter::NumericComputation(comp))
        }
        _ => {
            panic!("Invalid base value for Parameter.getIndex");
        }
    }
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

/// Get all of the call sites in the given callable
///
/// Implements:
/// - [ref:library:Callable:getACall]
/// - [ref:library:Function:getACall]
/// - [ref:library:Method:getACall]
fn callable_call_sites<'a>(
    ti: Rc<dyn TreeInterface>,
    base: &'a NodeFilter,
    operands: &'a Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallableComputation(c) => {
            Ok(NodeFilter::CallsiteListComputation(ti.callable_call_sites(c)))
        }
        nf => {
            panic!("Impossible value type for `callable_call_sites`: {}", nf.kind());
        }
    }
}

/// Get the (string) target of a call
///
/// This is usually the name of the function, but could be the name of a
/// function pointer in C/C++
///
/// Implements:
/// - [ref:library:Call:getTarget]
fn call_get_target(
    _ti: Rc<dyn TreeInterface>,
    base: &NodeFilter,
    operands: &Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.is_empty());
    match base {
        NodeFilter::CallsiteComputation(c) => {
            let get_callsite = Rc::clone(&c.extract);
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_callsite(ctx, source).map(|callsite_res| {
                        WithRanges::new(callsite_res.value.target_name.clone(), vec![callsite_res.ranges])
                    })
                }),
            };
            Ok(NodeFilter::StringComputation(comp))
        }
        nf => {
            panic!("Impossible value for `call_get_target`: {}", nf.kind());
        }
    }
}

/// Get the argument at a given index in a call's argument list
///
/// Implements:
/// - [ref:library:Call:getArgument]
fn call_get_argument(
    _ti: Rc<dyn TreeInterface>,
    base: &NodeFilter,
    operands: &Vec<Expr<Typed>>,
) -> anyhow::Result<NodeFilter> {
    assert!(operands.len() == 1);
    match base {
        NodeFilter::CallsiteComputation(c) => {
            let get_callsite = Rc::clone(&c.extract);
            let arg_idx = match operands[0].expr {
                Expr_::ConstantExpr(Constant::Integer(i)) => {
                    assert!(i >= 0);
                    i as usize
                },
                // FIXME: Argument lists should be evaluated into NodeFilters at
                // this point so that we can just execute it to get a concrete
                // value
                _ => panic!("Invalid argument to Call.getArgument"),
            };
            let comp = NodeMatcher {
                extract: Rc::new(move |ctx, source| {
                    get_callsite(ctx, source).map(|callsite_res| {
                        let arg_list = callsite_res.value.arguments;
                        if arg_idx < arg_list.len() {
                            Some(WithRanges::value(arg_list[arg_idx]))
                        } else {
                            None
                        }
                    }).flatten()
                }),
            };
            Ok(NodeFilter::ExprComputation(comp))
        }
        nf => {
            panic!("Impossible value for `call_get_argument`: {}", nf.kind());
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
        (Type::Method, "hasParseError".into()),
        Handler(Arc::new(callable_has_parse_error)),
    );
    impls.insert(
        (Type::Function, "hasParseError".into()),
        Handler(Arc::new(callable_has_parse_error)),
    );
    impls.insert(
        (Type::Callable, "hasParseError".into()),
        Handler(Arc::new(callable_has_parse_error)),
    );

    impls.insert(
        (Type::Method, "getFile".into()),
        Handler(Arc::new(callable_get_file)),
    );
    impls.insert(
        (Type::Function, "getFile".into()),
        Handler(Arc::new(callable_get_file)),
    );
    impls.insert(
        (Type::Callable, "getFile".into()),
        Handler(Arc::new(callable_get_file)),
    );

    impls.insert(
        (Type::Method, "getType".into()),
        Handler(Arc::new(callable_get_return_type)),
    );
    impls.insert(
        (Type::Function, "getType".into()),
        Handler(Arc::new(callable_get_return_type)),
    );
    impls.insert(
        (Type::Callable, "getType".into()),
        Handler(Arc::new(callable_get_return_type)),
    );

    impls.insert(
        (Type::Method, "getACall".into()),
        Handler(Arc::new(callable_call_sites)),
    );
    impls.insert(
        (Type::Function, "getACall".into()),
        Handler(Arc::new(callable_call_sites)),
    );
    impls.insert(
        (Type::Callable, "getACall".into()),
        Handler(Arc::new(callable_call_sites)),
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
        (Type::Parameter, "getIndex".into()),
        Handler(Arc::new(parameter_get_index)),
    );

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

    impls.insert(
        (Type::Call, "getTarget".into()),
        Handler(Arc::new(call_get_target)),
    );
    impls.insert(
        (Type::Call, "getArgument".into()),
        Handler(Arc::new(call_get_argument)),
    );

    validate_library(&impls);

    impls
});

pub fn method_impl_for(base_type: Type, method_name: String) -> Option<&'static Handler> {
    Lazy::force(&METHOD_IMPLS).get(&(base_type, method_name))
}
