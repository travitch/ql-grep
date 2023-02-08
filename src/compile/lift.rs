use std::rc::Rc;

use crate::compile::interface::{LanguageType, NodeMatcher};
use crate::compile::node_filter::NodeFilter;
use crate::query::val_type::Type;

fn as_predicate(nf: NodeFilter) -> NodeMatcher<bool> {
    match nf {
        NodeFilter::Predicate(nm) => nm,
        _ => panic!("Impossible, expected Predicate"),
    }
}

fn as_string(nf: NodeFilter) -> NodeMatcher<String> {
    match nf {
        NodeFilter::StringComputation(nm) => nm,
        _ => panic!("Impossible, expected String"),
    }
}

fn as_type(nf: NodeFilter) -> NodeMatcher<LanguageType> {
    match nf {
        NodeFilter::TypeComputation(nm) => nm,
        _ => panic!("Impossible, expected Type"),
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
fn transform_body<From, To, W, F, X>(
    base_elts: &NodeMatcher<Vec<From>>,
    wrap_one: W,
    extract_one: F,
    transformer: Rc<X>,
) -> NodeMatcher<Vec<To>>
where
    From: Clone + 'static,
    W: Fn(NodeMatcher<From>) -> NodeFilter + 'static,
    F: Fn(NodeFilter) -> NodeMatcher<To> + 'static,
    X: Fn(Rc<NodeFilter>) -> anyhow::Result<NodeFilter> + 'static,
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
                    extract: Rc::new(move |_ctx, _source| arg.clone()),
                };

                let wrapper_filter = wrap_one(wrapper_matcher);
                let result_filter = xfrm(Rc::new(wrapper_filter)).unwrap();
                let comp = extract_one(result_filter);
                let val = (comp.extract)(ctx, source);
                res.push(val);
            }

            res
        }),
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
///
/// This function returns None if the given `relation` is not a list.
/// Otherwise, it always returns a NodeFilter.
pub fn transform_node_filter<F>(
    result_type: Type,
    relation: &NodeFilter,
    transformer: Rc<F>,
) -> Option<NodeFilter>
where
    F: Fn(Rc<NodeFilter>) -> anyhow::Result<NodeFilter> + 'static,
{
    let xfrm: Rc<F> = Rc::clone(&transformer);
    match relation {
        NodeFilter::ArgumentListComputation(arg_list_matcher) => {
            match result_type.base_if_relational() {
                Type::PrimString => {
                    let matcher = transform_body(arg_list_matcher, NodeFilter::ArgumentComputation, as_string, xfrm);
                    Some(NodeFilter::StringListComputation(matcher))
                },
                Type::Type => {
                    let matcher = transform_body(arg_list_matcher, NodeFilter::ArgumentComputation, as_type, xfrm);
                    Some(NodeFilter::TypeListComputation(matcher))
                },
                _ => panic!("Unsupported conversion from Argument (only String is supported), result type `{result_type}`")
            }
        }
        NodeFilter::StringListComputation(string_list_matcher) => {
            match result_type.base_if_relational() {
                Type::PrimBoolean => {
                    let matcher = transform_body(
                        string_list_matcher,
                        NodeFilter::StringComputation,
                        as_predicate,
                        xfrm,
                    );
                    Some(NodeFilter::PredicateListComputation(matcher))
                }
                _ => {
                    panic!("Unsupported conversion from `string` to `{result_type}`");
                }
            }
        }
        NodeFilter::TypeListComputation(type_list_matcher) => {
            match result_type.base_if_relational() {
                Type::PrimString => {
                    let matcher = transform_body(
                        type_list_matcher,
                        NodeFilter::TypeComputation,
                        as_string,
                        xfrm,
                    );
                    Some(NodeFilter::StringListComputation(matcher))
                }
                _ => {
                    panic!("Unsupported conversion from `Type` to `{result_type}`");
                }
            }
        }
        _ => None,
    }
}
