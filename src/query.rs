/// This module supports parsing CodeQL queries

pub mod error;
pub mod ir;
pub mod parser;
pub mod typecheck;
pub mod val_type;

use tree_sitter;

use crate::query::ir::*;
use crate::query::error::QueryError;
use crate::query::parser::parse_query_ast;
use crate::query::val_type::Type;

/// An abstract representation of queries
pub struct Query<R: Repr> {
    pub query_ast: tree_sitter::Tree,
    pub select: Select<R>
}

extern "C" { fn tree_sitter_ql() -> tree_sitter::Language; }

pub fn parse_query(text : impl AsRef<[u8]>) -> anyhow::Result<Query<Syntax>> {
    let mut parser = tree_sitter::Parser::new();

    // If this fails, it really is a programming error as this parser should be
    // included at compile time
    let language = unsafe { tree_sitter_ql() };
    parser.set_language(language).unwrap();

    match parser.parse(&text, None) {
        None => {
            Err(anyhow::anyhow!(QueryError::QueryParseError))
        },
        Some(t) => {
            let sel = parse_query_ast(&t, &text)?;
            let q = Query {
                query_ast: t,
                select: sel
            };
            Ok(q)
        }
    }
}

// These two helpers are used in the tests and look like dead code when not
// building test cases.

#[allow(dead_code)]
fn var_ref(name : &str) -> Expr<Syntax> {
    untyped(Expr_::VarRef(name.into()))
}

#[allow(dead_code)]
fn untyped(e : Expr_<Syntax>) -> Expr<Syntax> {
    Expr {
        expr: e,
        type_: Untyped
    }
}

#[allow(dead_code)]
fn declare(vars : &[(Type, &str)]) -> Vec<VarDecl> {
    let mut res = Vec::new();

    for (ty, var_name) in vars {
        let decl = VarDecl {
            type_: ty.clone(),
            name: (*var_name).into()
        };
        res.push(decl);
    }

    res
}

#[test]
fn select_one_constant() {
    let ql = "select 5";
    let r = parse_query(ql);
    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: untyped(Expr_::ConstantExpr(Constant::Integer(5))),
        ident: None
    };
    exprs.push(as_expr);
    let expected = Select {
        select_exprs: exprs,
        where_formula: None,
        var_decls: Vec::new()
    };
    assert_eq!(expected, r.unwrap().select);
}

#[test]
fn select_named_constant() {
    let ql = "select 5 as bar";
    let r = parse_query(ql);
    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: untyped(Expr_::ConstantExpr(Constant::Integer(5))),
        ident: Some("bar".into())
    };
    exprs.push(as_expr);
    let expected = Select {
        select_exprs: exprs,
        where_formula: None,
        var_decls: Vec::new()
    };
    assert_eq!(expected, r.unwrap().select);
}

#[test]
fn select_two_constants() {
    let ql = "select 5, 10";
    let r = parse_query(ql);
    let mut exprs = Vec::new();
    let as_expr1 = AsExpr {
        expr: untyped(Expr_::ConstantExpr(Constant::Integer(5))),
        ident: None
    };
    let as_expr2 = AsExpr {
        expr: untyped(Expr_::ConstantExpr(Constant::Integer(10))),
        ident: None
    };
    exprs.push(as_expr1);
    exprs.push(as_expr2);
    let expected = Select {
        select_exprs: exprs,
        where_formula: None,
        var_decls: Vec::new()
    };
    assert_eq!(expected, r.unwrap().select);
}

#[test]
fn select_with_decls() {
    let ql = "from Function x, Method m select f";
    let r = parse_query(ql);
    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: untyped(Expr_::VarRef("f".into())),
        ident: None
    };
    exprs.push(as_expr);

    let expected = Select {
        select_exprs: exprs,
        where_formula: None,
        var_decls: declare(&[(Type::Function, "x"), (Type::Method, "m")])
    };

    assert_eq!(expected, r.unwrap().select);
}

#[test]
fn string_literal() {
    let ql = "from Method m where m.getName() = \"foo\" select m";
    let r = parse_query(ql);

    let get_name = untyped(Expr_::QualifiedAccess(Box::new(var_ref("m")), "getName".into(), Vec::new()));
    let str_lit = untyped(Expr_::ConstantExpr(Constant::String_("foo".into())));
    let cmp = untyped(Expr_::EqualityComparison(Box::new(get_name), EqualityOp::EQ, Box::new(str_lit)));

    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: var_ref("m"),
        ident: None
    };
    exprs.push(as_expr);

    let expected = Select {
        select_exprs: exprs,
        where_formula: Some(cmp),
        var_decls: declare(&[(Type::Method, "m")])
    };

    assert_eq!(expected, r.unwrap().select);
}

#[test]
/// Ensure that predicate arguments are parsed properly
fn predicate_argument() {
    let ql = "from Method m where m.regexpMatch(\"foo\") select m";
    let r = parse_query(ql);

    let str_lit = untyped(Expr_::ConstantExpr(Constant::String_("foo".into())));
    let mut args = Vec::new();
    args.push(str_lit);
    let rx_match = untyped(Expr_::QualifiedAccess(Box::new(var_ref("m")), "regexpMatch".into(), args));

    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: var_ref("m"),
        ident: None
    };
    exprs.push(as_expr);

    let expected = Select {
        select_exprs: exprs,
        where_formula: Some(rx_match),
        var_decls: declare(&[(Type::Method, "m")])
    };

    assert_eq!(expected, r.unwrap().select);
}

#[test]
fn select_filter_parameter_count() {
    let ql = "from Method m where count(m.getAParameter()) > 5 select m";
    let r = parse_query(ql);

    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: untyped(Expr_::VarRef("m".into())),
        ident: None
    };
    exprs.push(as_expr);

    let agg_body = AsExpr {
        expr: Expr {
            expr: Expr_::QualifiedAccess(Box::new(var_ref("m")), "getAParameter".into(), Vec::new()),
            type_: Untyped
        },
        ident: None
    };
    let mut agg_exprs = Vec::new();
    agg_exprs.push(agg_body);
    let lhs = untyped(Expr_::Aggregate(AggregateOp::Count, agg_exprs));
    let rhs = untyped(Expr_::ConstantExpr(Constant::Integer(5)));
    let cmp = untyped(Expr_::RelationalComparison(Box::new(lhs), CompOp::GT, Box::new(rhs)));

    let expected = Select {
        select_exprs: exprs,
        where_formula: Some(cmp),
        var_decls: declare(&[(Type::Method, "m")])
    };

    assert_eq!(expected, r.unwrap().select);
}
