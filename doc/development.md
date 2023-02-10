This document contains notes for ql-grep developers.

# General

- Note the design goals in the README
- This project uses GHC-style documentation cross-references and uses [tagref](https://github.com/stepchowfun/tagref) to ensure that they remain consistent
- You may want to prefer release builds, as debug builds generate enormous (> 700MB) binaries

# Adding Support for new CodeQL Methods

The process for adding a new method to an existing type is as follows:

1. Add the method to [the library](./library.kdl)
2. Implement a Rust function in [method_library.rs](../src/compile/method_library.rs) that is evaluated at run-time
  - If necessary (e.g., if the feature requires additional tree-sitter queries), implement the language-specific queries in the `TreeInterface` (`[ref:tree_interface_definition]`).  This requires adding a new method to the interface and providing implementations for each supported language.
3. Add the method to the `METHOD_IMPLS` map (see `[ref:method_impls_map]`) in [method_library.rs](../src/compile/method_library.rs)
4. Add tests to the integration test suite
  - Add at least one toml test description in the integration test [expected results directory](../tests/integration)
  - They can refer to codebases in the [features](../tests/codebases/feature) directory (you can re-use existing feature-oriented test codebases, but it is better to add new targeted ones as-needed)

Note that the type checker is driven by the definitions in the library and most of the rest of the pipeline is generic enough that most new methods will not require additional changes.

# Adding Support for a new Programming Language

If your desired language already has a tree-sitter grammar:

1. Add the tree-sitter grammar for your language by either:
   - Making the crates.io package a dependency in the [Cargo.toml](../Cargo.toml) file or, or
   - Adding the grammar as a git submodule under the [third-party](../third-party) directory and making sure that it is built by [build.rs](../build.rs)
2. Add an enumeration value to the `Language` enumeration in [source_file.rs](../src/source_file.rs) (`[ref:language_enum_definition]`)
3. Add an entry to the `LANGUAGES` map in the same file, mapping the supported file extensions to the associated tree-sitter parser
4. Define a new type to implement the tree-sitter adapter for your language in a new file in the [backends](../src/compile/backend) directory and implement the `TreeInterface` trait for it
5. Export your new module from [backend.rs](../src/compile/backend.rs)
6. Add a case for your language to the `make_tree_interface` function in [compile.rs](../src/compile.rs) (see `[ref:tree_sitter_interface_dispatcher]`) to instantiate your new tree-sitter adapter

If your desired language does not already have a tree-sitter grammar, write one and then follow the instructions above.

# Adding Support for a new CodeQL Type

(This case is a lot more complex)
