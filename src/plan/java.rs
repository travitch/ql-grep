use std::rc::Rc;
use tree_sitter::Node;

use crate::query::val_type::Type;
use crate::plan::interface::*;
use crate::source_file::SourceFile;

pub struct JavaTreeInterface {
}

impl JavaTreeInterface {
    pub fn new(_f : &SourceFile) -> Self {
        JavaTreeInterface {
        }
    }
}

impl TreeInterface for JavaTreeInterface {
    // FIXME: If we associate the top-level query with the value of a var ref,
    // we get rid of var ref and just have a typed computation
    fn top_level_type(&self, t : &Type) -> Option<TopLevelMatcher> {
        match t {
            Type::Method => {
                let matcher = TopLevelMatcher {
                    query: "(method_declaration) @method.declaration".into(),
                    tag : "@method.declaration".into()
                };

                Some(matcher)
            },
            _ => None
        }
    }

    fn callable_arguments(&self, base : &NodeMatcher<CallableRef>) ->
        Option<NodeMatcher<Vec<FormalArgument>>>
    {
        let x = Rc::clone(&base.extract);
        let matcher = NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                let callable_ref = x(ctx, source);
                let node = ctx.lookup_callable(&callable_ref);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(formal_parameter) @parameter";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying arguments {:?}", e));
                let qms = cur.matches(&query, *node, source);
                qms.map(|m| parameter_node_to_argument(&m.captures[0].node, source)).collect()
            })
        };
        Some(matcher)
    }

    fn callable_name(&self, base : &NodeMatcher<CallableRef>) -> Option<NodeMatcher<String>>
    {
        let x = Rc::clone(&base.extract);
        let matcher = NodeMatcher {
            extract: Rc::new(move |ctx, source| {
                let callable_ref = x(ctx, source);
                let node = ctx.lookup_callable(&callable_ref);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(method_declaration (identifier) @method.name)";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying name {:?}", e));
                let mut qms = cur.matches(&query, *node, source);
                let m = qms.next().unwrap();
                callable_name_node_to_string(&m.captures[0].node, source)
            })
        };
        Some(matcher)
    }
}

fn callable_name_node_to_string(n : &Node, src : & [u8]) -> String {
    n.utf8_text(src).unwrap().into()
}

fn parse_type_node<'a>(n : &'a Node, src : &'a [u8]) -> LanguageType {
    LanguageType::new(n.utf8_text(src).unwrap())
}

fn parameter_node_to_argument<'a>(n : &'a Node, src : &'a [u8]) -> FormalArgument {
    let ty_node = n.child_by_field_name("type").unwrap();
    let ty = parse_type_node(&ty_node, src);
    let ident_node = n.child_by_field_name("name").unwrap();
    let ident = ident_node.utf8_text(src).unwrap();

    FormalArgument {
        name: Some(ident.into()),
        declared_type: Some(ty)
    }
}
