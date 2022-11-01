# Introduction

`ql-grep` (`qg`) is an implementation of [CodeQL](https://codeql.github.com/) that parses codebases using tree-sitter.  It aims to be a convenient tool that answers moderately complex queries with no setup and without building code.  It trades off soundness and completeness (it is neither) for ease of use.

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

## wegli

## tree-grepper
