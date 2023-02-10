use anyhow::anyhow;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SourceError {
    #[error("No extension for file `{0}`")]
    MissingExtension(PathBuf),
    #[error("Unsupported source file type `{0}`")]
    UnsupportedFileType(PathBuf),
    #[error("Failed to parse file `{0}`")]
    ParseError(PathBuf),
}

/// The languages supported by ql-grep; these are used for run-time selection of
/// language-specific adapters in the query engine.
///
/// [tag:language_enum_definition]
#[derive(Debug, Copy, Clone)]
pub enum Language {
    Cpp,
    Java,
    Python,
}

lazy_static! {
    /// A map of file extensions to their corresponding parsers
    static ref LANGUAGES: HashMap<OsString, (tree_sitter::Language, Language)> = {
        let c_lang = unsafe { tree_sitter_c() };
        let cpp_lang = unsafe { tree_sitter_cpp() };
        let java_lang = unsafe { tree_sitter_java() };
        // let python_lang = unsafe { tree_sitter_python() };

        let mut m = HashMap::new();
        m.insert("c".into(), (c_lang, Language::Cpp));
        m.insert("cpp".into(), (cpp_lang, Language::Cpp));
        m.insert("cxx".into(), (cpp_lang, Language::Cpp));
        m.insert("cc".into(), (cpp_lang, Language::Cpp));
        m.insert("C".into(), (cpp_lang, Language::Cpp));
        m.insert("hpp".into(), (cpp_lang, Language::Cpp));
        m.insert("hh".into(), (cpp_lang, Language::Cpp));
        m.insert("hxx".into(), (cpp_lang, Language::Cpp));
        m.insert("h".into(), (cpp_lang, Language::Cpp));
        m.insert("java".into(), (java_lang, Language::Java));
        // m.insert("py".into(), (python_lang, Language::Python));
        m
    };
}

extern "C" {
    fn tree_sitter_c() -> tree_sitter::Language;
}
extern "C" {
    fn tree_sitter_cpp() -> tree_sitter::Language;
}
extern "C" {
    fn tree_sitter_java() -> tree_sitter::Language;
}
// extern "C" { fn tree_sitter_python() -> tree_sitter::Language; }

pub struct SourceFile {
    /// The original buffer for the source file
    pub source: String,
    /// The path on disk for the file; used for reporting
    pub file_path: PathBuf,
    /// A reified data value for the language that we can match on later
    pub lang: Language,
}

impl SourceFile {
    pub fn new(path: &Path) -> anyhow::Result<(Self, tree_sitter::Tree)> {
        let ext = path
            .extension()
            .ok_or_else(|| anyhow!(SourceError::MissingExtension(path.into())))?;
        let (ts_lang, language) = LANGUAGES
            .get(ext)
            .ok_or_else(|| anyhow!(SourceError::UnsupportedFileType(ext.into())))?;
        let mut parser = tree_sitter::Parser::new();
        // Unwrap is technically unsafe but it is a programming error if this
        // fails (i.e., we haven't set up the tree-sitter parsers correctly)
        parser.set_language(*ts_lang).unwrap();

        let bytes = std::fs::read_to_string(path)?;
        let t = parser
            .parse(&bytes, None)
            .ok_or_else(|| anyhow!(SourceError::ParseError(path.into())))?;
        let sf = SourceFile {
            source: bytes,
            file_path: path.into(),
            lang: *language,
        };
        Ok((sf, t))
    }
}
