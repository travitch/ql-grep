/// This module defines the types and methods available in the subset of CodeQL supported by ql-grep
///
/// The actual definitions and types are provided by the doc/library.kdl file,
/// which is both documentation and the source of truth for the type checker.

use knuffel;
use std::collections::HashSet;

const LIBRARY: &str = include_str!("../doc/library.kdl");

#[derive(knuffel::Decode)]
pub struct Parameter {
    #[knuffel(argument)]
    pub name: String,
    #[knuffel(property(name="type"))]
    pub type_: String
}

#[derive(knuffel::Decode)]
pub struct Method {
    #[knuffel(argument)]
    pub name: String,
    #[knuffel(property(name="type"))]
    pub type_: String,
    #[knuffel(children(name="parameter"))]
    pub parameters: Vec<Parameter>,
}

#[derive(knuffel::Decode)]
pub struct Type {
    #[knuffel(argument)]
    pub name: String,
    #[knuffel(children(name="method"))]
    pub methods: Vec<Method>,
}

fn validate_parameter(seen_types : &HashSet<String>, m : &Method, p : &Parameter) {
    if seen_types.get(&p.type_).is_none() {
        panic!("Parameter {} in {} references undefined type {}", p.name, m.name, p.type_);
    }
}

fn validate_method(seen_types : &HashSet<String>, m : &Method) {
    if seen_types.get(&m.type_).is_none() {
        panic!("Method {} references undefined type {}", m.name, m.type_);
    }

    for p in &m.parameters {
        validate_parameter(seen_types, &m, &p);
    }
}

fn validate_type(seen_types : &HashSet<String>, t : &Type) {
    for m in &t.methods {
        validate_method(seen_types, &m);
    }
}

/// Perform a validation pass to ensure that all referenced types are present
fn validate_library(types : &Vec<Type>) {
    let mut seen_types : HashSet<String> = HashSet::new();

    // Seed with primitive types
    seen_types.insert("int".into());
    seen_types.insert("float".into());
    seen_types.insert("string".into());
    seen_types.insert("boolean".into());

    // Collect the top-level type definitions in the library
    for t in types {
        seen_types.insert(t.name.clone());
    }

    // Now traverse all of the definitions
    for t in types {
        validate_type(&seen_types, &t);
    }
}

pub fn library_types() -> Vec<Type> {
    // Aggressively let this fail, as this should be statically correct
    let types = knuffel::parse::<Vec<Type>>("library.kdl", LIBRARY).unwrap();
    validate_library(&types);
    types
}
