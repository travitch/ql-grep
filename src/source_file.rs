use lazy_static::{lazy_static};
use std::collections::HashMap;
use std::ffi::OsString;
use std::path::{Path,PathBuf};
use thiserror::Error;
use anyhow::anyhow;
use tree_sitter;

#[derive(Error, Debug)]
pub enum SourceError {
    #[error("No extension for file `{0}`")]
    MissingExtension(PathBuf),
    #[error("Unsupported source file type `{0}`")]
    UnsupportedFileType(PathBuf),
    #[error("Failed to parse file `{0}`")]
    ParseError(PathBuf)
}

lazy_static! {
    /// A map of file extensions to their corresponding parsers
    static ref LANGUAGES: HashMap<OsString, tree_sitter::Language> = {
        let c_lang = unsafe { tree_sitter_c() };
        let cpp_lang = unsafe { tree_sitter_cpp() };
        let java_lang = unsafe { tree_sitter_java() };
        let python_lang = unsafe { tree_sitter_python() };

        let mut m = HashMap::new();
        m.insert("c".into(), c_lang);
        m.insert("cpp".into(), cpp_lang);
        m.insert("cxx".into(), cpp_lang);
        m.insert("cc".into(), cpp_lang);
        m.insert("C".into(), cpp_lang);
        m.insert("hpp".into(), cpp_lang);
        m.insert("hh".into(), cpp_lang);
        m.insert("hxx".into(), cpp_lang);
        m.insert("h".into(), cpp_lang);
        m.insert("java".into(), java_lang);
        m.insert("py".into(), python_lang);
        m
    };
}

extern "C" { fn tree_sitter_c() -> tree_sitter::Language; }
extern "C" { fn tree_sitter_cpp() -> tree_sitter::Language; }
extern "C" { fn tree_sitter_java() -> tree_sitter::Language; }
extern "C" { fn tree_sitter_python() -> tree_sitter::Language; }

pub struct SourceFile {
    /// The AST from the Tree Sitter parser
    ///
    /// Note that this contains the `tree_sitter::Language`
    pub ast : tree_sitter::Tree,
    /// The original buffer for the source file
    pub source : String,
    /// The path on disk for the file; used for reporting
    pub file_path : PathBuf
}

impl SourceFile {
    pub fn new(path : &Path) -> anyhow::Result<Self> {
        let ext = path.extension().ok_or(anyhow!(SourceError::MissingExtension(path.into())))?;
        let lang = LANGUAGES.get(ext).ok_or(anyhow!(SourceError::UnsupportedFileType(ext.into())))?;
        let mut parser = tree_sitter::Parser::new();
        // Unwrap is technically unsafe but it is a programming error if this
        // fails (i.e., we haven't set up the tree-sitter parsers correctly)
        parser.set_language(*lang).unwrap();

        let bytes = std::fs::read_to_string(path)?;
        let t = parser.parse(&bytes, None).ok_or(anyhow!(SourceError::ParseError(path.into())))?;
        let sf = SourceFile {
            ast: t,
            source: bytes,
            file_path: path.into()
        };
        Ok(sf)
    }
}
