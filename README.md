# Introduction

`ql-grep` (`qg`) is an implementation of [CodeQL](https://codeql.github.com/) that parses codebases using tree-sitter.  It aims to be a convenient tool that answers moderately complex queries with no setup and without building code.  It trades off soundness and completeness (it is neither) for ease of use.  It has two goals that are not addressed by other similar tools:

1. Provide a uniform query interface for all supported languages
2. Support searches more powerful than just syntactic search (i.e., supporting some deeper semantic properties)

# Plan

1. Parse CodeQL queries (using treesitter)
2. Translate CodeQL queries into query plans
   - Multiple parts of that query plan might be tree-sitter cursor queries
   - Hierarchical queries may be necessary/possible where an indexing pass records some information to drive a rough topological ordering for subsequent analysis
3. Execute the queries in parallel
4. Print results (add line numbers and configurable context window)
5. Add decompilation of jars
   - Start with a fixed decompiler (http://java-decompiler.github.io/)
   - Support others

# Alternatives

There are other fantastic code search tools based on the tree-sitter infrastructure.

## wegli

The [weggli](https://github.com/googleprojectzero/weggli) project from Google Project Zero is a code search tool designed to support reverse engineering and bug hunting workflows.  As such, it focuses on C and C++ support.  To provide an ergonomic search experience, weggli queries are written as C/C++ sketches that bind variables that the tree-sitter based search fills in to instantiate those patterns in terms of code in the target codebase.

This system is very clever, but comes with a few downsides:

- Supporting the sketch-based query syntax requires patches to the tree-sitter grammars
- Extending the sketch-based query interface with deeper semantic properties would be quite difficult
- Extending the sketch-based query interface to support other languages would be a lot of work and would prevent writing one query that could work on multiple languages at once

## tree-grepper

The [tree-grepper](https://github.com/BrianHicks/tree-grepper) project has support for a large number of languages and uses the tree-sitter query language as its interface.  This is powerful, but restricts tree-grepper to purely syntactic queries.  Moreover, each query must be constructed for the target language; there is a great deal of variation between the concrete syntax trees produced by tree-sitter, so this is a non-trivial burden.
