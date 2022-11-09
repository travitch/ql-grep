use tree_sitter::TextProvider;

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

impl<'a, 'tree, T : TextProvider<'a>> TreeInterface<'a, 'tree, T> for CPPTreeInterface<'a> {
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

    fn callable_arguments(&self) ->
        Option<NodeMatcher<'a, 'tree, T, Vec<FormalArgument<'a>>>>
    {
        unimplemented!()
    }
}
