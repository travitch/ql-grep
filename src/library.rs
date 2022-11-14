/// This module defines the types and methods available in the subset of CodeQL supported by ql-grep
///
/// The actual definitions and types are provided by the doc/library.kdl file,
/// which is both documentation and the source of truth for the type checker.

pub mod index;

use knuffel;
use once_cell::sync::Lazy;
use std::collections::HashSet;

pub const LIBRARY_DATA: &str = include_str!("../doc/library.kdl");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    Unimplemented,
    Deprecated
}

#[derive(Debug, PartialEq, Eq)]
pub struct StatusParseError;

impl std::fmt::Display for StatusParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error parsing status")
    }
}

impl std::error::Error for StatusParseError {}

impl std::str::FromStr for Status {
    type Err = StatusParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Unimplemented" => Ok(Status::Unimplemented),
            "Deprecated" => Ok(Status::Deprecated),
            _ => Err(StatusParseError)
        }
    }
}

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
    #[knuffel(property, str)]
    pub status: Option<Status>,
    #[knuffel(property)]
    pub docstring: Option<String>,
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
        validate_parameter(seen_types, m, p);
    }
}

fn validate_type(seen_types : &HashSet<String>, t : &Type) {
    for m in &t.methods {
        validate_method(seen_types, m);
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
        validate_type(&seen_types, t);
    }
}

static LIBRARY: Lazy<Vec<Type>> = Lazy::new(|| {
    let types = knuffel::parse::<Vec<Type>>("library.kdl", LIBRARY_DATA).unwrap();
    validate_library(&types);
    types
});

pub fn library_types() -> &'static Vec<Type> {
    // Aggressively let this fail, as this should be statically correct
    Lazy::force(&LIBRARY)
}
