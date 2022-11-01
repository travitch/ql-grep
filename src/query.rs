/// This module supports parsing CodeQL queries

pub mod error;
pub mod ir;
pub mod parser;

use tree_sitter;

use crate::query::ir::{AsExpr, Expr, Type, VarDecl, Select, CompOp, QLValue, AggregateOp};
use crate::query::error::QueryError;
use crate::query::parser::parse_query_ast;

/// An abstract representation of queries
pub struct Query {
    pub query_ast: tree_sitter::Tree,
    pub select: Select
}

extern "C" { fn tree_sitter_ql() -> tree_sitter::Language; }

pub fn parse_query(text : impl AsRef<[u8]>) -> anyhow::Result<Query> {
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

#[test]
fn select_one_constant() {
    let ql = "select 5";
    let r = parse_query(ql);
    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: Expr::ValueExpr(QLValue::QLInteger(5)),
        ident: None
    };
    exprs.push(as_expr);
    let expected = Select {
        select_expr: exprs,
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
        expr: Expr::ValueExpr(QLValue::QLInteger(5)),
        ident: Some("bar".into())
    };
    exprs.push(as_expr);
    let expected = Select {
        select_expr: exprs,
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
        expr: Expr::ValueExpr(QLValue::QLInteger(5)),
        ident: None
    };
    let as_expr2 = AsExpr {
        expr: Expr::ValueExpr(QLValue::QLInteger(10)),
        ident: None
    };
    exprs.push(as_expr1);
    exprs.push(as_expr2);
    let expected = Select {
        select_expr: exprs,
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
        expr: Expr::VarRef("f".into()),
        ident: None
    };
    exprs.push(as_expr);

    let mut decls = Vec::new();
    let function_var = VarDecl {
        type_: Type::Function,
        name: "x".into()
    };
    let method_var = VarDecl {
        type_: Type::Method,
        name: "m".into()
    };
    decls.push(function_var);
    decls.push(method_var);
    let expected = Select {
        select_expr: exprs,
        where_formula: None,
        var_decls: decls
    };

    assert_eq!(expected, r.unwrap().select);
}

#[test]
fn select_filter_parameter_count() {
    let ql = "from Method m where count(m.getAParameter()) > 5 select m";
    let r = parse_query(ql);

    let mut decls = Vec::new();
    let method_var = VarDecl {
        type_: Type::Method,
        name: "m".into()
    };
    decls.push(method_var);

    let mut exprs = Vec::new();
    let as_expr = AsExpr {
        expr: Expr::VarRef("m".into()),
        ident: None
    };
    exprs.push(as_expr);

    let agg_body = AsExpr {
        expr: Expr::QualifiedAccess(Box::new(Expr::VarRef("m".into())), "getAParameter".into()),
        ident: None
    };
    let mut agg_exprs = Vec::new();
    agg_exprs.push(agg_body);
    let lhs = Expr::Aggregate(AggregateOp::Count, agg_exprs);
    let rhs = Expr::ValueExpr(QLValue::QLInteger(5));
    let cmp = Expr::Comparison(Box::new(lhs), CompOp::GT, Box::new(rhs));

    let expected = Select {
        select_expr: exprs,
        where_formula: Some(cmp),
        var_decls: decls
    };

    assert_eq!(expected, r.unwrap().select);
}
