use tree_sitter::TextProvider;

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

impl<'a, 'tree, T : TextProvider<'a>> TreeInterface<'a, 'tree, T> for JavaTreeInterface<'a> {
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
        Option<NodeMatcher<'a, 'tree, T, Vec<FormalArgument<'a>>>>
    {
        unimplemented!()
    }
}
