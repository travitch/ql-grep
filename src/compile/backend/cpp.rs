use std::rc::Rc;
use tracing::error;
use tree_sitter::Node;

use crate::compile::interface::*;
use crate::preprocess::{FileImportIndex, Import};
use crate::query::parser::get_child_of_kind;
use crate::query::val_type::Type;
use crate::with_ranges::WithRanges;

pub struct CPPTreeInterface {}

impl CPPTreeInterface {
    pub fn new() -> Self {
        CPPTreeInterface {}
    }
}

fn parse_include_string(s: &str) -> Option<Import> {
    if s.starts_with("\"") {
        let name = s.strip_prefix("\"").unwrap().strip_suffix("\"").unwrap();
        let imp = Import::IncludeLocal(name.into());
        Some(imp)
    } else if s.starts_with("<") {
        let name = s.strip_prefix("<").unwrap().strip_suffix(">").unwrap();
        let imp = Import::IncludeSystem(name.into());
        Some(imp)
    } else {
        None
    }
}

impl TreeInterface for CPPTreeInterface {
    fn top_level_type(&self, t: &Type) -> Option<TopLevelMatcher> {
        match t {
            Type::Function | Type::Callable => {
                let matcher = TopLevelMatcher {
                    query: "(function_definition) @function.definition".into(),
                    tag: "@function.definition".into(),
                };

                Some(matcher)
            }
            _ => None,
        }
    }

    fn file_imports<'a>(&self, root: &Node, source: &'a [u8]) -> FileImportIndex {
        let mut cur = tree_sitter::QueryCursor::new();
        let ql_query = "(preproc_include (_) @include)";
        let query = tree_sitter::Query::new(root.language(), ql_query)
            .unwrap_or_else(|e| panic!("Error while querying for includes in C/C++ file {e:}"));
        let qms = cur.matches(&query, *root, source);

        let mut import_index = FileImportIndex::new();

        qms.for_each(|qm| {
            qm.captures[0].node.utf8_text(source)
                .map_or_else(|_e| error!("Error decoding source as UTF-8 for include"), |s| {
                    import_index.add(parse_include_string(s).unwrap());
                });
        });

        import_index
    }

    fn callable_arguments(
        &self,
        base: &NodeMatcher<CallableRef>,
    ) -> Option<NodeListMatcher<FormalArgument>> {
        let x = Rc::clone(&base.extract);
        let matcher = NodeListMatcher {
            extract: Rc::new(move |ctx, source| {
                let callable_ref = x(ctx, source);
                let node = ctx.lookup_callable(&callable_ref.value);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(parameter_declaration) @parameter";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying arguments {e:?}"));
                let qms = cur.matches(&query, *node, source);
                let params = qms.enumerate()
                    .map(|(idx, m)| {
                        let param_node = &m.captures[0].node;
                        let formal_argument = parameter_node_to_argument(param_node, source, idx);
                        WithRanges::new_single(formal_argument, param_node.range())
                    })
                    .collect();
                params
            }),
        };
        Some(matcher)
    }

    fn callable_name(&self, base: &NodeMatcher<CallableRef>) -> Option<NodeMatcher<String>> {
        let x = Rc::clone(&base.extract);
        let matcher = NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                let callable_ref = x(ctx, source);
                let node = ctx.lookup_callable(&callable_ref.value);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(function_declarator (identifier) @function.name)";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying name {e:?}"));
                let mut qms = cur.matches(&query, *node, source);
                let m = qms.next().unwrap();
                let name_node = &m.captures[0].node;
                WithRanges::new_single(callable_name_node_to_string(name_node, source), name_node.range())
            }),
        };
        Some(matcher)
    }

    fn callable_return_type(&self, node: &NodeMatcher<CallableRef>) -> Option<NodeMatcher<LanguageType>> {
        let get_callable_ref = Rc::clone(&node.extract);
        let matcher = NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                let callable_ref = get_callable_ref(ctx, source);
                let node = ctx.lookup_callable(&callable_ref.value);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(function_definition (type_qualifier)? @qual type: (_) @ty declarator: (_)? @decl)";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying return type {e:?}"));
                let mut qms = cur.matches(&query, *node, source);
                // There should be exactly one match with three capture indices
                // (the declarator and qualifier could be empty)
                let m = qms.next().unwrap();

                for _qual in m.nodes_for_capture_index(0) {
                    // FIXME: Collect these
                }

                let mut ty_node = None;
                for ty in m.nodes_for_capture_index(1) {
                    ty_node = Some(ty);
                }

                let mut decl_node = None;
                for decl in m.nodes_for_capture_index(2) {
                    decl_node = Some(decl);
                }

                let parsed_decl = decl_node.as_ref().map(|d| parse_declarator(d, source));
                // FIXME: Grab the declarator range too, if present
                let ranges = vec!(ty_node.unwrap().range());
                WithRanges::new(parse_type_node(&parsed_decl, &ty_node.unwrap(), source), vec!(ranges))
            }),
        };
        Some(matcher)
    }

    fn callable_has_parse_error(&self, base: &NodeMatcher<CallableRef>) -> NodeMatcher<bool> {
        let get_callable_ref = Rc::clone(&base.extract);
        NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                let callable_ref = get_callable_ref(ctx, source);
                let node = ctx.lookup_callable(&callable_ref.value);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(function_definition body: (_ (ERROR) @err))";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying for errors {e:?}"));
                let qms = cur.matches(&query, *node, source);
                let ranges = qms.map(|m| m.captures[0].node.range()).collect::<Vec<_>>();
                WithRanges::new(ranges.len() != 0, vec!(ranges))
            }),
        }
    }
}

fn callable_name_node_to_string(n: &Node, src: &[u8]) -> String {
    n.utf8_text(src).unwrap().into()
}

/// Parse any `type` node in the tree-sitter AST for C/C++
///
/// NOTE: For now this just captures the string representation of the
/// type. Eventually we will probably want a structured representation. If we
/// do, it will need to be language-specific, so we would need language-specific
/// storage in the evaluation context.
///
/// This takes an optional declarator to enable us to capture pointer types and
/// modify the textual type, as the tree-sitter parser splits the pointer
/// declarator out.
fn parse_type_node<'a>(decl: &Option<Declarator>, n: &'a Node, src: &'a [u8]) -> LanguageType {
    let mut s = String::new();

    s.push_str(n.utf8_text(src).unwrap());

    if let Some(d) = decl.as_ref() {
        d.append_declarators(&mut s);
    };

    LanguageType::new(&s)
}

#[derive(Debug)]
enum Declarator {
    Pointer(Box<Declarator>),
    Reference(Box<Declarator>),
    Array(Box<Declarator>),
    Function(Box<Declarator>),
    Unnamed,
    Ident(String),
}

impl Declarator {
    fn type_name(&self) -> &str {
        match self {
            Declarator::Ident(s) => s,
            Declarator::Array(a) => a.type_name(),
            // FIXME: Can do better here
            Declarator::Function(_) => "function-pointer",
            Declarator::Pointer(d) => d.type_name(),
            Declarator::Unnamed => "<unnamed>",
            Declarator::Reference(d) => d.type_name(),
        }
    }

    fn append_declarators(&self, s: &mut String) {
        match self {
            Declarator::Ident(_) => {}
            Declarator::Pointer(d) => {
                s.push('*');
                d.append_declarators(s);
            }
            Declarator::Function(_) => {
                // FIXME
                s.push_str("function-pointer");
            }
            Declarator::Array(d) => {
                s.push_str("[]");
                d.append_declarators(s);
            }
            Declarator::Reference(d) => {
                s.push('&');
                d.append_declarators(s);
            }
            Declarator::Unnamed => {}
        }
    }
}

fn parse_declarator<'a>(n: &'a Node, src: &'a [u8]) -> Declarator {
    // This node can either be an identifier (the name of the declarator) or a
    // pointer wrapper
    match n.kind() {
        "identifier" => Declarator::Ident(n.utf8_text(src).unwrap().into()),
        "array_declarator" => {
            let next_child = n.child_by_field_name("declarator").unwrap();
            Declarator::Array(Box::new(parse_declarator(&next_child, src)))
        }
        "pointer_declarator" => {
            let next_child = n.child_by_field_name("declarator").unwrap();
            Declarator::Pointer(Box::new(parse_declarator(&next_child, src)))
        }
        "function_declarator" => {
            // This is a parenthesized declarator that we can skip for now
            let next_child = n.child_by_field_name("declarator").unwrap();
            let next_declarator_child = get_child_of_kind(next_child, "pointer_declarator");
            match next_declarator_child {
                Err(_) => parse_declarator(&next_child, src),
                Ok(next_child) => {
                    Declarator::Function(Box::new(parse_declarator(&next_child, src)))
                }
            }
        }
        "reference_declarator" => {
            let next_child = get_child_of_kind(*n, "identifier").unwrap();
            Declarator::Reference(Box::new(parse_declarator(&next_child, src)))
        }
        "abstract_pointer_declarator" => Declarator::Unnamed,
        "abstract_function_declarator" => Declarator::Unnamed,
        "abstract_array_declarator" => Declarator::Unnamed,
        "parenthesized_declarator" => {
            // FIXME: Figure out what this actually is
            Declarator::Unnamed
        }
        "qualified_identifier" => {
            Declarator::Unnamed
        }
        "abstract_parenthesized_declarator" => {
            Declarator::Unnamed
        }
        k => {
            panic!("Unsupported C/C++ declarator type `{k}`");
        }
    }
}

fn parameter_node_to_argument<'a>(n: &'a Node, src: &'a [u8], idx: usize) -> FormalArgument {
    let decl = n
        .child_by_field_name("declarator")
        .map(|n| parse_declarator(&n, src));
    let ty_node = n.child_by_field_name("type");
    let ty = ty_node.map(|s| parse_type_node(&decl, &s, src));
    FormalArgument {
        name: decl.map(|d| d.type_name().into()),
        declared_type: ty,
        index: idx,
    }
}


#[test]
fn test_parse_system_include() {
    let expected = Import::IncludeSystem("stdint.h".into());
    assert_eq!(Some(expected), parse_include_string("<stdint.h>"));
}

#[test]
fn test_parse_local_include() {
    let expected = Import::IncludeLocal("foo.h".into());
    assert_eq!(Some(expected), parse_include_string("\"foo.h\""));
}
