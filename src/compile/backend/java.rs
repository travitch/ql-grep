use std::rc::Rc;
use tracing::error;
use tree_sitter::Node;

use crate::compile::interface::*;
use crate::preprocess::{FileImportIndex, Import};
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;

pub struct JavaTreeInterface {}

impl JavaTreeInterface {
    pub fn new() -> Self {
        JavaTreeInterface {}
    }
}

impl TreeInterface for JavaTreeInterface {
    // FIXME: If we associate the top-level query with the value of a var ref,
    // we get rid of var ref and just have a typed computation
    fn top_level_type(&self, t: &Type) -> Option<TopLevelMatcher> {
        match t {
            Type::Method | Type::Callable => {
                let matcher = TopLevelMatcher {
                    query: "(method_declaration) @method.declaration".into(),
                    tag: "@method.declaration".into(),
                };

                Some(matcher)
            }
            _ => None,
        }
    }

    fn file_imports(&self, root: &Node, source: &[u8]) -> FileImportIndex {
        let mut cur = tree_sitter::QueryCursor::new();
        let ql_query = "(import_declaration (_) @import)";
        let query = tree_sitter::Query::new(root.language(), ql_query)
            .unwrap_or_else(|e| panic!("Error while querying for imports in java file {e:?}"));
        let qms = cur.matches(&query, *root, source);

        let mut import_index = FileImportIndex::new();

        qms.for_each(|qm| {
            qm.captures[0].node.utf8_text(source).map_or_else(
                |_e| error!("Error decoding source as UTF8 for import"),
                |s| {
                    let imp = Import::Import(s.into());
                    import_index.add(imp);
                },
            );
        });

        import_index
    }

    fn callable_arguments(
        &self,
        base: &NodeMatcher<CallableRef>,
    ) -> Option<NodeListMatcher<FormalArgument>> {
        let get_callable = Rc::clone(&base.extract);
        let matcher = NodeListMatcher {
            extract: Rc::new(move |ctx, source| {
                get_callable(ctx, source).map(|callable_ref| {
                    let node = ctx.lookup_callable(&callable_ref.value);
                    let mut cur = tree_sitter::QueryCursor::new();
                    let ql_query = "(formal_parameter) @parameter";
                    let query = tree_sitter::Query::new(node.language(), ql_query)
                        .unwrap_or_else(|e| panic!("Error while querying arguments {e:?}"));
                    let qms = cur.matches(&query, *node, source);
                    qms.enumerate()
                        .map(|(idx, m)| {
                            let param_node = &m.captures[0].node;
                            let formal_argument = parameter_node_to_argument(param_node, source, idx);
                            WithRanges::new_single(formal_argument, param_node.range())
                        })
                        .collect()
                }).unwrap_or(Vec::new())
            }),
        };
        Some(matcher)
    }

    fn callable_name(&self, base: &NodeMatcher<CallableRef>) -> Option<NodeMatcher<String>> {
        let get_callable = Rc::clone(&base.extract);
        let matcher = NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                get_callable(ctx, source).map(|callable_ref| {
                    let node = ctx.lookup_callable(&callable_ref.value);
                    let mut cur = tree_sitter::QueryCursor::new();
                    let ql_query = "(method_declaration (identifier) @method.name)";
                    let query = tree_sitter::Query::new(node.language(), ql_query)
                        .unwrap_or_else(|e| panic!("Error while querying name {e:?}"));
                    let mut qms = cur.matches(&query, *node, source);
                    let m = qms.next().unwrap();
                    let name_node = &m.captures[0].node;
                    let name_string = callable_name_node_to_string(name_node, source);
                    WithRanges::new_single(name_string, name_node.range())
                })
            }),
        };
        Some(matcher)
    }

    fn callable_return_type(
        &self,
        node: &NodeMatcher<CallableRef>,
    ) -> Option<NodeMatcher<LanguageType>> {
        let get_callable_ref = Rc::clone(&node.extract);
        let matcher = NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                get_callable_ref(ctx, source).map(|callable_ref| {
                    // NOTE: We discard the ranges for the callable because
                    // highlighting an entire function body is not very useful
                    let node = ctx.lookup_callable(&callable_ref.value);
                    let mut cur = tree_sitter::QueryCursor::new();
                    let ql_query = "(method_declaration type: (_) @ty)";
                    let query = tree_sitter::Query::new(node.language(), ql_query)
                        .unwrap_or_else(|e| panic!("Error while querying for return types {e:?}"));
                    let mut qms = cur.matches(&query, *node, source);
                    let m = qms.next().unwrap();
                    let type_node = &m.captures[0].node;
                    let lang = parse_type_node(type_node, source);
                    WithRanges::new_single(lang, type_node.range())
                })
            }),
        };
        Some(matcher)
    }

    fn callable_has_parse_error(&self, base: &NodeMatcher<CallableRef>) -> NodeMatcher<bool> {
        let get_callable_ref = Rc::clone(&base.extract);
        NodeMatcher {
            extract: Rc::new(move |ctx, source| {
               get_callable_ref(ctx, source).map(|callable_ref| {
                   // NOTE: We discard the ranges for callables because
                   // highlighting an entire function is not very useful
                   let node = ctx.lookup_callable(&callable_ref.value);
                   let mut cur = tree_sitter::QueryCursor::new();
                   let ql_query = "(method_declaration body: (_ (ERROR) @err))";
                   let query = tree_sitter::Query::new(node.language(), ql_query)
                       .unwrap_or_else(|e| panic!("Error while querying for errors {e:?}"));
                   let qms = cur.matches(&query, *node, source);
                   let ranges = qms.map(|m| m.captures[0].node.range()).collect::<Vec<_>>();
                   // We compute the number of matches against ranges instead of
                   // the query matches because the `map` operation consumes the
                   // iterator
                   WithRanges::new(!ranges.is_empty(), vec![ranges])
               })
            }),
        }
    }

    fn callable_call_sites(&self, node: &NodeMatcher<CallableRef>) -> NodeListMatcher<Callsite> {
        let get_callable_ref = Rc::clone(&node.extract);
        NodeListMatcher {
            extract: Rc::new(move |ctx, source| {
                let mut callsites = Vec::new();

                get_callable_ref(ctx, source).map(|callable_ref| {
                    let callable_node = ctx.lookup_callable(&callable_ref.value);
                    let mut cur = tree_sitter::QueryCursor::new();
                    let ql_query = "(method_invocation name: (_) @name arguments: (argument_list) @args)";
                    let query = tree_sitter::Query::new(callable_node.language(), ql_query)
                        .unwrap_or_else(|e| panic!("Error while querying for call sites {e:?}"));
                    let query_matches = cur.matches(&query, *callable_node, source);

                    for query_match in query_matches {
                        // Note that this currently only captures the name of the
                        // method called; the receiver object or static class name
                        // (if any) is not captured by the query above
                        let call_name_node = query_match.captures[0].node;
                        let arglist_node = query_match.captures[1].node;

                        let (callsite, expr_bindings) = callable_call_sites_from_nodes(source, &call_name_node, &arglist_node);

                        ctx.add_expression_bindings(expr_bindings);

                        let ranges = vec![call_name_node.range(), arglist_node.range()];
                        let res = WithRanges::new(callsite, vec![ranges]);
                        callsites.push(res);
                    }
                });
                callsites
            }),
        }
    }
}

fn callable_call_sites_from_nodes<'a>(
    source: &[u8],
    call_name_node: &Node<'a>,
    arglist_node: &Node<'a>,
) -> (Callsite, Vec<(ExprRef, Node<'a>)>) {
    let name = call_name_node.utf8_text(source).unwrap().into();
    let mut args = Vec::new();
    let mut arg_refs = Vec::new();

    let mut cur = arglist_node.walk();
    for arg_node in arglist_node.named_children(&mut cur) {
        let expr_ref = ExprRef::new(arg_node.id());
        arg_refs.push(expr_ref);
        args.push((expr_ref, arg_node));
    }

    let callsite = Callsite::new(name, arg_refs);
    (callsite, args)
}

fn callable_name_node_to_string(n: &Node, src: &[u8]) -> String {
    n.utf8_text(src).unwrap().into()
}

fn parse_type_node<'a>(n: &'a Node, src: &'a [u8]) -> LanguageType {
    LanguageType::new(n.utf8_text(src).unwrap())
}

fn parameter_node_to_argument<'a>(n: &'a Node, src: &'a [u8], idx: usize) -> FormalArgument {
    let ty_node = n.child_by_field_name("type").unwrap();
    let ty = parse_type_node(&ty_node, src);
    let ident_node = n.child_by_field_name("name").unwrap();
    let ident = ident_node.utf8_text(src).unwrap();

    FormalArgument {
        name: Some(ident.into()),
        declared_type: Some(ty),
        index: idx,
    }
}
