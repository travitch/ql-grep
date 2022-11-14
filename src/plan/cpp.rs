use tree_sitter::Node;

use crate::query::ir::Type;
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

    fn callable_arguments(&self) -> Option<NodeMatcher<Vec<FormalArgument>>>
    {
        let matcher = NodeMatcher {
            query: "(parameter_declaration) @parameter".into(),
            extract: Box::new(|qms, src| qms.map(|m| parameter_node_to_argument(&m.captures[0].node, src)).collect())
        };
        Some(matcher)
    }

    fn callable_name(&self) -> Option<NodeMatcher<String>>
    {
        let matcher = NodeMatcher {
            query: "(function_declarator (identifier) @function.name)".into(),
            extract: Box::new(|mut qms, src| {
                let m = qms.next().unwrap();
                callable_name_node_to_string(&m.captures[0].node, src)
            })
        };
        Some(matcher)
    }
}

fn callable_name_node_to_string(n : &Node, src : & [u8]) -> String {
    n.utf8_text(src).unwrap().into()
}

fn parameter_node_to_argument<'a>(n : &'a Node, src : &'a [u8]) -> FormalArgument {
    if n.child_count() == 1 {
        // In this case, it is just an unused argument with a type and no name
        let type_node = n.child(0).unwrap();
        let ty = type_node.utf8_text(src).unwrap();
        FormalArgument {
            name: "".into(), // FIXME: Change this type
            declared_type: Some(ty.into())
        }
    } else if n.child_count() == 2 {
        // FIXME: Rearrange the types to enable clean error handling (i.e., add a Result to the return)
        let ident_node = n.child(1).unwrap();
        let ident = ident_node.utf8_text(src).unwrap();

        let type_node = n.child(0).unwrap();
        let ty = type_node.utf8_text(src).unwrap();
        FormalArgument {
            name: ident.into(),
            declared_type: Some(ty.into())
        }
    } else if n.child_count() == 3 {
        // FIXME: Capture qualifiers
        let ident_node = n.child(2).unwrap();
        let ident = ident_node.utf8_text(src).unwrap();

        let type_node = n.child(1).unwrap();
        let ty = type_node.utf8_text(src).unwrap();
        FormalArgument {
            name: ident.into(),
            declared_type: Some(ty.into())
        }
    } else {
        panic!("Unknown argument structure with {} children", n.child_count());
    }
}
