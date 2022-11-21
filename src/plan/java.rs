use tree_sitter::Node;

use crate::query::val_type::Type;
use crate::plan::interface::*;
use crate::source_file::SourceFile;

pub struct JavaTreeInterface<'a> {
    file : &'a SourceFile
}

impl<'a> JavaTreeInterface<'a> {
    pub fn new(f : &'a SourceFile) -> Self {
        JavaTreeInterface {
            file : f
        }
    }
}

impl<'a> TreeInterface for JavaTreeInterface<'a> {
    fn source(&self) -> &'a SourceFile {
        self.file
    }

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

    fn callable_arguments(&self) ->
        Option<NodeMatcher<Vec<FormalArgument>>>
    {
        let matcher = NodeMatcher {
            query: "(formal_parameter) @parameter".into(),
            extract: Box::new(|qms, src| qms.map(|m| parameter_node_to_argument(&m.captures[0].node, src)).collect())
        };
        Some(matcher)
    }

    fn callable_name(&self) -> Option<NodeMatcher<String>>
    {
        let matcher = NodeMatcher {
            query: "(method_declaration (identifier) @method.name)".into(),
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
    // FIXME: Rearrange the types to enable clean error handling (i.e., add a Result to the return)
    //
    // It seems like this should be impossible if the parser is correct
    let ident_node = n.child(1).unwrap();
    let ident = ident_node.utf8_text(src).unwrap();

    let type_node = n.child(0).unwrap();
    let ty = type_node.utf8_text(src).unwrap();
    FormalArgument {
        name: ident.into(),
        declared_type: Some(ty.into())
    }
}
