use std::error::Error;
use std::fmt::Display;
use std::str::FromStr;

/// File-level preprocessing passes required for evaluation
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum FilePreprocessingPass {
    /// Preprocess the file and collect all of the imports and includes
    Imports,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FilePreprocessingPassError;

impl Display for FilePreprocessingPassError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error parsing FilePreprocessingPass")
    }
}

impl Error for FilePreprocessingPassError {}

impl FromStr for FilePreprocessingPass {
    type Err = FilePreprocessingPassError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Imports" => Ok(FilePreprocessingPass::Imports),
            _ => Err(FilePreprocessingPassError),
        }
    }
}

/// The concrete types of imports supported
///
/// No language supports all of these, but having this top-level type simplifies
/// things substantially over having language-specific imports.  This could
/// change in the future if we want additional precision in our representation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Import {
    /// A C/C++ include using angle brackets to reference a system-level include
    /// file on the search path
    IncludeSystem(String),
    /// A C/C++ include using double quotes to include a local file
    IncludeLocal(String),
    /// An unqualified import (at least for Java). Note that this is currently
    /// used for all imports; Java imports could be broken down into more types
    Import(String),
    /// An import that couldn't be interpreted and is just the raw syntax from
    /// tree-sitter (most likely from C/C++ with unexpanded CPP in the include).
    RawImport(String),
}

impl ToString for Import {
    fn to_string(&self) -> String {
        match self {
            Import::IncludeSystem(s) => s.clone(),
            Import::IncludeLocal(s) => s.clone(),
            Import::Import(s) => s.clone(),
            Import::RawImport(s) => s.clone(),
        }
    }
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

    pub fn imports(&self) -> &[Import] {
        self.imports.as_slice()
    }
}

impl Default for FileImportIndex {
    fn default() -> Self {
        Self::new()
    }
}
