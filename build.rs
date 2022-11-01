use cc;
use std::path::PathBuf;


fn main() {
    // Build all of the tree-sitter grammars that we will need into the tg binary
    let c_dir : PathBuf = ["third_party", "tree-sitter-c", "src"].iter().collect();
    cc::Build::new()
        .include(&c_dir)
        .file(c_dir.join("parser.c"))
        .flag("-std=c99")
        .flag("-w")
        .compile("tree-sitter-c");

    let cpp_dir : PathBuf = ["third_party", "tree-sitter-cpp", "src"].iter().collect();
    cc::Build::new()
        .include(&cpp_dir)
        .file(cpp_dir.join("parser.c"))
        .flag("-w")
        .compile("tree-sitter-cpp-parser");
    cc::Build::new()
        .cpp(true)
        .include(&cpp_dir)
        .file(cpp_dir.join("scanner.cc"))
        .flag("-w")
        .compile("tree-sitter-cpp-scanner");

    let python_dir : PathBuf = ["third_party", "tree-sitter-python", "src"].iter().collect();
    cc::Build::new()
        .include(&python_dir)
        .file(python_dir.join("parser.c"))
        .flag("-w")
        .compile("tree-sitter-python-parser");
    cc::Build::new()
        .cpp(true)
        .include(&python_dir)
        .file(python_dir.join("scanner.cc"))
        .flag("-w")
        .compile("tree-sitter-python-scanner");

    let java_dir : PathBuf = ["third_party", "tree-sitter-java", "src"].iter().collect();
    cc::Build::new()
        .include(&java_dir)
        .file(java_dir.join("parser.c"))
        .flag("-std=c99")
        .flag("-w")
        .compile("tree-sitter-java");

    // Note that this grammar is slightly special - we don't support it as a
    // search target, but we use it to parse user queries
    let ql_dir : PathBuf = ["third_party", "tree-sitter-ql", "src"].iter().collect();
    cc::Build::new()
        .include(&ql_dir)
        .file(ql_dir.join("parser.c"))
        .flag("-std=c99")
        .flag("-w")
        .compile("tree-sitter-ql");
}
