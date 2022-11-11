use tree_sitter::Node;

use crate::query::ir::Type;
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
}

fn parameter_node_to_argument<'a>(n : &'a Node, src : &'a [u8]) -> FormalArgument {
    // FIXME: Rearrange the types to enable clean error handling (i.e., add a Result to the return)
    let ident_node = n.child(1).unwrap();
    let n = ident_node.utf8_text(src).unwrap();
    FormalArgument {
        name: n.into(),
        declared_type: None
    }
}