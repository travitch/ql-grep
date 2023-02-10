/// This module defines the types and methods available in the subset of CodeQL supported by ql-grep
///
/// The actual definitions and types are provided by the doc/library.kdl file,
/// which is both documentation and the source of truth for the type checker.
pub mod index;

use once_cell::sync::Lazy;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::Display;
use std::str::FromStr;

use crate::query::val_type;

pub const LIBRARY_DATA: &str = include_str!("../doc/library.kdl");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    Unimplemented,
    Deprecated,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StatusParseError;

impl Display for StatusParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error parsing status")
    }
}

impl Error for StatusParseError {}

impl FromStr for Status {
    type Err = StatusParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Unimplemented" => Ok(Status::Unimplemented),
            "Deprecated" => Ok(Status::Deprecated),
            _ => Err(StatusParseError),
        }
    }
}

#[derive(knuffel::Decode)]
pub struct Parameter {
    #[knuffel(argument)]
    pub name: String,
    #[knuffel(property(name = "type"), str)]
    pub type_: val_type::Type,
}

#[derive(knuffel::Decode)]
pub struct Method {
    #[knuffel(argument)]
    pub name: String,
    #[knuffel(property(name = "type"), str)]
    pub type_: val_type::Type,
    #[knuffel(property, str)]
    pub status: Option<Status>,
    #[knuffel(property)]
    pub docstring: Option<String>,
    #[knuffel(children(name = "parameter"))]
    pub parameters: Vec<Parameter>,
    #[knuffel(property, str)]
    pub tag: Option<String>,
}

/// This type is factored out so that we can use the `str` knuffel parser; it
/// doesn't happen to work in the context of the `children` parser, so we can't
/// directly have a Vec<val_type::Type> below.
#[derive(knuffel::Decode, Clone)]
pub struct ContainedType {
    #[knuffel(argument, str)]
    pub type_: val_type::Type,
}

#[derive(knuffel::Decode)]
pub struct Type {
    #[knuffel(argument)]
    pub name: String,
    #[knuffel(children(name = "method"))]
    pub methods: Vec<Method>,
    #[knuffel(children(name = "contains"))]
    pub contains: Vec<ContainedType>,
}

/// We want to only reason about the presence/absence of the non-aggregate types
/// so we remove any `List` or `Relational` type constructors.  Other types are
/// returned as-is.
fn drop_aggregate_types(ty: val_type::Type) -> val_type::Type {
    match ty {
        val_type::Type::Function => ty,
        val_type::Type::Method => ty,
        val_type::Type::Callable => ty,
        val_type::Type::Field => ty,
        val_type::Type::Class => ty,
        val_type::Type::Type => ty,
        val_type::Type::Regex => ty,
        val_type::Type::Expr => ty,
        val_type::Type::Call => ty,
        val_type::Type::Parameter => ty,
        val_type::Type::PrimString => ty,
        val_type::Type::PrimInteger => ty,
        val_type::Type::PrimBoolean => ty,
        val_type::Type::List(inner) => drop_aggregate_types(*inner),
        val_type::Type::Relational(inner) => drop_aggregate_types(*inner),
    }
}

fn validate_parameter(seen_types: &HashSet<val_type::Type>, m: &Method, p: &Parameter) {
    let p_ty = drop_aggregate_types(p.type_.clone());
    if seen_types.get(&p_ty).is_none() {
        panic!(
            "Parameter {} in {} references undefined type {}",
            p.name, m.name, p.type_
        );
    }
}

fn validate_method(seen_types: &HashSet<val_type::Type>, m: &Method) {
    let ret_ty = drop_aggregate_types(m.type_.clone());
    if seen_types.get(&ret_ty).is_none() {
        panic!("Method {} references undefined type {}", m.name, m.type_);
    }

    for p in &m.parameters {
        validate_parameter(seen_types, m, p);
    }
}

fn validate_type(seen_types: &HashSet<val_type::Type>, t: &Type) {
    for m in &t.methods {
        validate_method(seen_types, m);
    }
}

/// Perform a validation pass to ensure that all referenced types are present
fn validate_library(types: &Vec<Type>) {
    let mut seen_types: HashSet<val_type::Type> = HashSet::new();

    // Seed with primitive types
    seen_types.insert(val_type::Type::PrimInteger);
    seen_types.insert(val_type::Type::PrimBoolean);
    seen_types.insert(val_type::Type::PrimString);

    // Collect the top-level type definitions in the library
    for t in types {
        seen_types.insert(val_type::Type::from_str(&t.name).unwrap());
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
