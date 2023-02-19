use std::path::PathBuf;

fn main() {
    // While we would like to pull all of our grammars from crates.io, there is
    // one that we need that isn't available yet: tree-sitter-ql. We build it here.

    // Note that this grammar is slightly special - we don't support it as a
    // search target, but we use it to parse user queries
    let ql_dir: PathBuf = ["third_party", "tree-sitter-ql", "src"].iter().collect();
    cc::Build::new()
        .include(&ql_dir)
        .file(ql_dir.join("parser.c"))
        .flag("-std=c99")
        .flag("-w")
        .compile("tree-sitter-ql");
}
