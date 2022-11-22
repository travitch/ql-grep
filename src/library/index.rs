use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::str::FromStr;

use crate::library;
use crate::query::val_type::Type;

pub struct MethodSignature(pub String, pub Vec<Type>, pub Type, pub Option<library::Status>);
pub struct MethodIndex(pub HashMap<String, MethodSignature>);

fn build_method_signature(method : &library::Method) -> MethodSignature {
    let mut param_types = Vec::new();
    for p in &method.parameters {
        param_types.push(p.type_.clone());
    }

    MethodSignature(method.name.clone(), param_types, method.type_.clone(), method.status)
}

fn index_library_type(lib_ty : &library::Type) -> MethodIndex {
    let mut res = HashMap::new();
    for m in &lib_ty.methods {
        let sig = build_method_signature(m);
        res.insert(m.name.clone(), sig);
    }

    MethodIndex(res)
}

static LIBRARY_INDEX: Lazy<HashMap<Type, MethodIndex>> = Lazy::new(|| {
    let mut ty_idx = HashMap::new();

    for ty in library::library_types() {
        let midx = index_library_type(ty);
        let ty = Type::from_str(ty.name.as_str()).unwrap();
        ty_idx.insert(ty, midx);
    }

    ty_idx
});

/// An index of the types defined in the library
///
/// Note that this just panics if it encounters something malformed because it
/// is just an error in the data file (rather than bad user input).
pub fn library_index() -> &'static HashMap<Type, MethodIndex> {
    Lazy::force(&LIBRARY_INDEX)
}
