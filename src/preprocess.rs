/// File-level preprocessing passes required for evaluation
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum FilePreprocessingPass {
    /// Preprocess the file and collect all of the imports and includes
    Imports,
}

/// The concrete types of imports supported
///
/// No language supports all of these, but having this top-level type simplifies
/// things substantially over having language-specific imports.  This could
/// change in the future if we want additional precision in our representation.
#[derive(Debug)]
pub enum Import {
    /// A C/C++ include using angle brackets to reference a system-level include
    /// file on the search path
    IncludeSystem(String),
    /// A C/C++ include using double quotes to include a local file
    IncludeLocal(String),
    /// An unqualified import (at least for Java). Note that this is currently
    /// used for all imports; Java imports could be broken down into more types
    Import(String),
}

/// An index of all of the imports in a single file.
///
/// This is a simple wrapper right now, but is abstract to make it easier to
/// refactor later.
pub struct FileImportIndex {
    imports: Vec<Import>,
}

impl FileImportIndex {
    pub fn new() -> Self {
        FileImportIndex {
            imports: Vec::new(),
        }
    }

    pub fn add(&mut self, i: Import) {
        self.imports.push(i);
    }
}
