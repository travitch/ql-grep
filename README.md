# Introduction

`ql-grep` (`qg`) is an implementation of [CodeQL](https://codeql.github.com/) that parses codebases using tree-sitter.  It aims to be a convenient tool that answers moderately complex queries with no setup and without building code.  It trades off soundness and completeness (it is neither) for ease of use.  It has two goals that are not addressed by other similar tools:

1. Provide a uniform query interface for all supported languages
2. Support searches more powerful than just syntactic search (i.e., supporting some deeper semantic properties)

The CodeQL constructs supporting in `ql-grep` are documented in [The Library](doc/library.kdl). The type checker and query planner are driven by this file, so it is kept up-to-date.

By using `ql-grep`, you acknowledge that you understand that it is unsound and incomplete. It is a liar and a cheat. It may produce useful results, but provides no guarantees about anything.

# Getting Started

Building `ql-grep` requires a working C++ compiler (for the tree-sitter grammars) and a rust toolchain.  To build it, run the following commands:

```
git submodule update --init --depth 1
cargo build --release

# Run ql-grep
./target/release/qg --help

# Example: find all of the functions with two parameters that tree-sitter cannot completely parse correctly in the Linux kernel
./target/release/qg --root ./tests/codebases/linux/ 'from Function f where count(f.getAParameter()) = 2 and f.hasParseError() select f'
```

Queries can be passed directly as an argument or via the `--query-path` flag.

Note that building in release mode is strongly encouraged.  The debug build works fine, but produces enormous (>700MB) binaries.

## More Examples

See the [examples](examples/) directory for numerous CodeQL query examples with explanatory commentary.  These examples are part of the test suite for the parser and type checker, so they should be reflective of what `ql-grep` supports.  There are additional examples in the [integration test suite](tests/integration), which are executed on a set of large codebases in the test suite.

# Design

## Goals

The primary objectives of `ql-grep` are:

1. Ease of use

    `ql-grep` should require zero setup to use.

2. Speed

    `ql-grep` should be fast even for enormous codebases.

3. Compatibility

    `ql-grep` should adhere as closely as possible to the official CodeQL language spec so that documentation for CodeQL will inform users of `ql-grep` effectively.

Some of the major design choices that `ql-grep` adopts to satisfy these goals include:

- Build on tree-sitter for fast and robust parsing of incomplete code in many languages, without requiring code to be built or indexed
- Exploit local parallelism to the greatest extent possible
- Spend extra effort to efficiently compile and plan queries to maximize the efficiency of per-file query execution
- Attempt to support generic queries unmodified across languages; this introduces slight differences compared to the official CodeQL
- Ensure that the documentation always matches the implementation by including the documentation in the binary and checking it against the implementation

## Extended Design Notes

The [documentation](doc) directory has extended design documentation on various components of `ql-grep`.  This section lists some high level points that might be of interest to users.

### Query Parsing

Queries are written in CodeQL, which `ql-grep` parses using the QL tree-sitter grammar.  The CodeQL language is agnostic to the target language (though specific operations are not necessarily supported for all target languages).

### Query Planning

The parsed query is turned into a query plan for each language encountered in the filesystem traversal.  The plan is reused to avoid recomputation.  The planning phase consists of constructing a sequence of lambda functions that implement the query semantics when they are composed.  This chain of lambda functions is a query plan.  The plan is executed on each source file, which is parsed using tree-sitter.

Note that tree-sitter parses files into a lightweight AST that is fully elaborated syntactically, but does not actually carry any data.  Instead, the AST carries enough information to read tokens if they are needed.  This means that parsing is very fast and the only extra intermediate data structures that are computed are those needed to evaluate the query.

### Documentation Checking

The supported CodeQL objects and methods are documented in [a file](doc/library.kdl).  This file is included in the binary and used to generate documentation from the help system.  This file also drives the type checker by providing type signatures to check queries against.  Further, the query planner checks its internal database of implemented evaluation rules against the claims of the documentation:

- It will raise an error if any methods are implemented but not documented
- It will raise an error if a method that is documented as implemented but is not really
- It will raise an error if a method is implemented bu marked as not implemented

# Roadmap

`ql-grep` is in a very early prototype stage.  There will be rough edges and many missing features.

- It is currently primarily a command line tool
- There is a library interface, but it is poorly thought-out and incomplete (requests welcome)

## 1.0

This will be the initial release with a focus on largely syntactic and local queries.  Planned supported languages include:

- C/C++
- Java
- Javascript
- Python

## 2.0

The second major release will focus on adding support for slightly deeper semantic queries and some global analysis, but not global deeply semantic properties.  Additional language targets planned include:

- Rust
- Haskell
- TypeScript
- Go

# Alternatives

There are other fantastic code search tools, some of which are based on the tree-sitter infrastructure.

## weggli

The [weggli](https://github.com/googleprojectzero/weggli) project from Google Project Zero is a code search tool designed to support reverse engineering and bug hunting workflows.  As such, it focuses on C and C++ support.  To provide an ergonomic search experience, weggli queries are written as C/C++ sketches that bind variables that the tree-sitter based search fills in to instantiate those patterns in terms of code in the target codebase.

This system is very clever, but comes with a few downsides:

- Supporting the sketch-based query syntax requires patches to the tree-sitter grammars
- Extending the sketch-based query interface with deeper semantic properties would be quite difficult
- Extending the sketch-based query interface to support other languages would be a lot of work and would prevent writing one query that could work on multiple languages at once

## tree-grepper

The [tree-grepper](https://github.com/BrianHicks/tree-grepper) project has support for a large number of languages and uses the tree-sitter query language as its interface.  This is powerful, but restricts tree-grepper to purely syntactic queries.  Moreover, each query must be constructed for the target language; there is a great deal of variation between the concrete syntax trees produced by tree-sitter, so this is a non-trivial burden.

## GitHub CodeQL

The official implementation of [CodeQL](https://codeql.github.com/) is part of GitHub.  Its syntax is the inspiration for `ql-grep`.  CodeQL requires code to be uploaded to GitHub, which indexes code to enable efficient queries.  There are two reasons that CodeQL might not be usable in some contexts:

1. This indexing step is slow
2. It requires uploading your code to GitHub

In exchange for these two requirements, GitHub's CodeQL is able to answer much more sophisticated and semantically interesting queries through static analysis.  CodeQL supports an enormous library of search primitives and a large number of programming languages.

## Joern

The [Joern](https://joern.io/) tool is intended for bug hunting in code.  It represents programs as a Code Property Graph (CPG), which is like an extended system dependence graph.  It supports queries written in a graph traversal language.  It is able to run locally, but also requires an expensive graph construction step.  It is able to handle fuzzy code import (i.e., partially working code).
