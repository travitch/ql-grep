pub mod cpp;
pub mod java;

use std::rc::Rc;

use crate::compile::interface::TreeInterface;
use crate::source_file::Language;

/// Examine the given source file and determine which tree-sitter adapter to use
/// while processing it.
///
/// [tag:tree_sitter_interface_dispatcher]
pub fn make_tree_interface(lang: Language) -> Rc<dyn TreeInterface> {
    match lang {
        Language::Cpp => Rc::new(cpp::CPPTreeInterface::new()) as Rc<dyn TreeInterface>,
        Language::Java => Rc::new(java::JavaTreeInterface::new()) as Rc<dyn TreeInterface>,
        Language::Python => unimplemented!(),
    }
}
