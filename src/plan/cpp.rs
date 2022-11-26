use tree_sitter::Node;

use crate::query::val_type::Type;
use crate::plan::interface::*;
use crate::source_file::SourceFile;

pub struct CPPTreeInterface<'a> {
    file : &'a SourceFile
}

impl<'a> CPPTreeInterface<'a> {
    pub fn new(f : &'a SourceFile) -> Self {
        CPPTreeInterface {
            file : f
        }
    }
}

impl<'a> TreeInterface for CPPTreeInterface<'a> {
    fn source(&self) -> &'a SourceFile {
        self.file
    }

    fn top_level_type(&self, t : &Type) -> Option<TopLevelMatcher> {
        match t {
            Type::Function => {
                let matcher = TopLevelMatcher {
                    query: "(function_definition) @function.definition".into(),
                    tag : "@function.definition".into()
                };

                Some(matcher)
            },
            _ => None
        }
    }

    fn callable_arguments(&self, base : NodeMatcher<CallableRef>) -> Option<NodeMatcher<Vec<FormalArgument>>>
    {
        let matcher = NodeMatcher {
            extract: Box::new(move |ctx, source| {
                let callable_ref = (base.extract)(ctx, source);
                let node = ctx.lookup_callable(&callable_ref);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(parameter_declaration) @parameter";
                let query = tree_sitter::Query::new(node.language(), ql_query)
                    .unwrap_or_else(|e| panic!("Error while querying arguments {:?}", e));
                let qms = cur.matches(&query, *node, source);
                qms.map(|m| parameter_node_to_argument(&m.captures[0].node, source)).collect()
            })
        };
        Some(matcher)
    }

    fn callable_name(&self, base : NodeMatcher<CallableRef>) -> Option<NodeMatcher<String>>
    {
        let matcher = NodeMatcher {
            extract: Box::new(move |ctx, source| {
                let callable_ref = (base.extract)(ctx, source);
                let node = ctx.lookup_callable(&callable_ref);
                let mut cur = tree_sitter::QueryCursor::new();
                let ql_query = "(function_declarator (identifier) @function.name)";
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
fn parse_type_node<'a>(decl : &Option<Declarator>, n : &'a Node, src : &'a [u8]) -> LanguageType {
    let mut s = String::new();

    s.push_str(n.utf8_text(src).unwrap());

    decl.as_ref().map(|d| {
        d.append_declarators(&mut s);
    });

    LanguageType::new(&s)
}

enum Declarator {
    Pointer(Box<Declarator>),
    Array(Box<Declarator>),
    Function(Box<Declarator>),
    Ident(String)
}

impl Declarator {
    fn type_name(&self) -> &str {
        match self {
            Declarator::Ident(s) => s,
            Declarator::Array(a) => a.type_name(),
            // FIXME: Can do better here
            Declarator::Function(_) => "function-pointer",
            Declarator::Pointer(d) => d.type_name(),
        }
    }

    fn append_declarators(&self, s : &mut String) {
        match self {
            Declarator::Ident(_) => {},
            Declarator::Pointer(d) => {
                s.push('*');
                d.append_declarators(s);
            },
            Declarator::Function(_) => {
                // FIXME
                s.push_str("function-pointer");
            },
            Declarator::Array(d) => {
                s.push_str("[]");
                d.append_declarators(s);
            },
        }
    }
}

fn parse_declarator<'a>(n : &'a Node, src : &'a [u8]) -> Declarator {
    // This node can either be an identifier (the name of the declarator) or a
    // pointer wrapper
    match n.kind() {
        "identifier" => Declarator::Ident(n.utf8_text(src).unwrap().into()),
        "array_declarator" => {
            let next_child = n.child_by_field_name("declarator").unwrap();
            Declarator::Array(Box::new(parse_declarator(&next_child, src)))
        },
        "pointer_declarator" => {
            let next_child = n.child_by_field_name("declarator").unwrap();
            Declarator::Pointer(Box::new(parse_declarator(&next_child, src)))
        },
        "function_declarator" => {
            // This is a parenthesized declarator that we can skip for now
            let next_child = n.child_by_field_name("declarator").unwrap();
            println!("next after function decl: {:?}", next_child);
            let next_child = next_child.child_by_field_name("declarator").unwrap();
            Declarator::Function(Box::new(parse_declarator(&next_child, src)))
        },
        k => {
            panic!("Unsupported C/C++ declarator type `{}`", k);
        }
    }
}

fn parameter_node_to_argument<'a>(n : &'a Node, src : &'a [u8]) -> FormalArgument {
    let decl = n.child_by_field_name("declarator").map(|n| parse_declarator(&n, src));
    let ty_node = n.child_by_field_name("type");
    let ty = ty_node.map(|s| parse_type_node(&decl, &s, src));
    FormalArgument {
        name: decl.map(|d| d.type_name().into()),
        declared_type: ty
    }
}
