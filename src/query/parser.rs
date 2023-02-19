use std::str::FromStr;
use tracing::error;
use tree_sitter;

use crate::query::error::QueryError;
use crate::query::ir::{
    AggregateOp, AsExpr, CompOp, Constant, EqualityOp, Expr, Expr_, Select, Syntax, Untyped,
    VarDecl,
};
use crate::query::val_type::Type;

/// Get the first child of the given node that has the provided type
///
/// We need to use this to be robust against comments appearing as children of
/// any node, which would throw off any indexing.
pub fn get_child_of_kind<'a>(
    node: tree_sitter::Node<'a>,
    target_kind: &'static str,
) -> anyhow::Result<tree_sitter::Node<'a>> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == target_kind {
            return Ok(child);
        }
    }

    Err(anyhow::anyhow!(QueryError::MissingExpectedChildNode(
        node.range(),
        target_kind.into()
    )))
}

/// Expect a single child node with any type
fn any_single_child(node: tree_sitter::Node) -> anyhow::Result<tree_sitter::Node> {
    if node.named_child_count() != 1 {
        error!(
            "node count: {:?} ({:?}, {:?})",
            node.named_child_count(),
            node.named_child(0).unwrap(),
            node.named_child(1).unwrap()
        );
        return Err(anyhow::anyhow!(QueryError::MissingExpectedChildNode(
            node.range(),
            "$ANY".into()
        )));
    }

    // Size checked already; the separate check is also to ensure that there is
    // exactly one child
    let child = node.named_child(0).unwrap();
    Ok(child)
}

/// Expect a single child node with the given node kind
fn single_child<'a>(
    node: tree_sitter::Node<'a>,
    expected_kind: &'static str,
) -> anyhow::Result<tree_sitter::Node<'a>> {
    let child = any_single_child(node)?;

    if child.kind() == expected_kind {
        return Ok(child);
    }

    Err(anyhow::anyhow!(QueryError::MissingExpectedChildNode(
        node.range(),
        expected_kind.into()
    )))
}

fn parse_literal<'a>(node: tree_sitter::Node<'a>, source: &'a [u8]) -> anyhow::Result<Constant> {
    match node.kind() {
        "integer" => {
            let txt = node.utf8_text(source)?;
            let num: i32 = txt.parse()?;
            Ok(Constant::Integer(num))
        }
        "string" => {
            // The range in the AST node includes the quotes, which we need to trim off
            let txt = node.utf8_text(source)?;
            let str: String = txt.parse()?;
            let str1 = str.strip_prefix('"').unwrap();
            let str2 = str1.strip_suffix('"').unwrap();
            Ok(Constant::String_(str2.into()))
        }
        _ => {
            unimplemented!()
        }
    }
}

enum SomeComparison {
    EqComp(EqualityOp),
    RelComp(CompOp),
}

fn parse_comp_op<'a>(
    node: tree_sitter::Node<'a>,
    source: &'a [u8],
) -> anyhow::Result<SomeComparison> {
    expect_node_kind(node, "compop")?;

    let op_str = node.utf8_text(source)?;

    match op_str {
        "<" => Ok(SomeComparison::RelComp(CompOp::LT)),
        "<=" => Ok(SomeComparison::RelComp(CompOp::LE)),
        ">" => Ok(SomeComparison::RelComp(CompOp::GT)),
        ">=" => Ok(SomeComparison::RelComp(CompOp::GE)),
        "=" => Ok(SomeComparison::EqComp(EqualityOp::EQ)),
        "!=" => Ok(SomeComparison::EqComp(EqualityOp::NE)),
        _ => Err(anyhow::anyhow!(QueryError::InvalidComparisonOp(
            op_str.into(),
            node.range()
        ))),
    }
}

fn parse_aggregate_op<'a>(
    node: tree_sitter::Node<'a>,
    source: &'a [u8],
) -> anyhow::Result<AggregateOp> {
    expect_node_kind(node, "aggId")?;
    match node.utf8_text(source)? {
        "count" => Ok(AggregateOp::Count),
        o => Err(anyhow::anyhow!(QueryError::InvalidAggregateOp(
            o.into(),
            node.range()
        ))),
    }
}

/// Parse a QL expression from the given node
fn parse_expr<'a>(node: tree_sitter::Node<'a>, source: &'a [u8]) -> anyhow::Result<Expr<Syntax>> {
    let inner_expr = match node.kind() {
        "literal" => {
            let child = any_single_child(node)?;
            let val = parse_literal(child, source)?;
            Expr_::ConstantExpr(val)
        }
        "variable" => {
            let var_name_node = get_child_of_kind(node, "varName")?;
            let var_name = parse_var_name(var_name_node, source)?;
            Expr_::VarRef(var_name)
        }
        "comp_term" => {
            // EXPR COMP_OP EXPR
            let lhs_node = node.child_by_field_name("left").ok_or_else(|| {
                anyhow::anyhow!(QueryError::MalformedNode("comp_term".into(), node.range()))
            })?;
            let lhs = parse_expr(lhs_node, source)?;
            let op_node = get_child_of_kind(node, "compop")?;
            let op = parse_comp_op(op_node, source)?;
            let rhs_node = node.child_by_field_name("right").ok_or_else(|| {
                anyhow::anyhow!(QueryError::MalformedNode("comp_term".into(), node.range()))
            })?;
            let rhs = parse_expr(rhs_node, source)?;
            match op {
                SomeComparison::RelComp(rop) => {
                    Expr_::RelationalComparison {
                        lhs: Box::new(lhs),
                        op: rop,
                        rhs: Box::new(rhs),
                    }
                }
                SomeComparison::EqComp(eop) => {
                    Expr_::EqualityComparison {
                        lhs: Box::new(lhs),
                        op: eop,
                        rhs: Box::new(rhs)
                    }
                }
            }
        }
        "conjunction" => {
            let lhs_node = node.child_by_field_name("left").ok_or_else(|| {
                anyhow::anyhow!(QueryError::MalformedNode(
                    "conjunction".into(),
                    node.range()
                ))
            })?;
            let lhs = parse_expr(lhs_node, source)?;
            let rhs_node = node.child_by_field_name("right").ok_or_else(|| {
                anyhow::anyhow!(QueryError::MalformedNode(
                    "conjunction".into(),
                    node.range()
                ))
            })?;
            let rhs = parse_expr(rhs_node, source)?;
            Expr_::LogicalConjunction {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        "disjunction" => {
            let lhs_node = node.child_by_field_name("left").ok_or_else(|| {
                anyhow::anyhow!(QueryError::MalformedNode(
                    "disjunction".into(),
                    node.range()
                ))
            })?;
            let lhs = parse_expr(lhs_node, source)?;
            let rhs_node = node.child_by_field_name("right").ok_or_else(|| {
                anyhow::anyhow!(QueryError::MalformedNode(
                    "disjunction".into(),
                    node.range()
                ))
            })?;
            let rhs = parse_expr(rhs_node, source)?;
            Expr_::LogicalDisjunction {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        "aggregate" => {
            let op_node = get_child_of_kind(node, "aggId")?;
            let op = parse_aggregate_op(op_node, source)?;
            let agg_body = get_child_of_kind(node, "expr_aggregate_body")?;
            let as_exprs_node = get_child_of_kind(agg_body, "asExprs")?;
            let exprs = parse_as_exprs(as_exprs_node, source)?;
            Expr_::Aggregate { op, operands: exprs }
        }
        "par_expr" => {
            // Parenthesized expressions have 3 children:
            //
            // LPAREN expr RPAREN
            //
            // The expr is the single named child
            let inner_expr = parse_expr(node.named_child(0).unwrap(), source)?;
            inner_expr.expr
        }
        "qualified_expr" => {
            if node.named_child_count() != 2 {
                return Err(anyhow::anyhow!(QueryError::MalformedNode(
                    "qualified_expr".into(),
                    node.range()
                )));
            }
            let expr = parse_expr(node.named_child(0).unwrap(), source)?;
            let rhs_node = node.named_child(1).unwrap();
            expect_node_kind(rhs_node, "qualifiedRhs")?;

            // Each of the arguments are parallel children to the predicate
            // name, so we get the predicate name at index 0 and take the rest
            // as arguments.
            if rhs_node.named_child_count() == 0 {
                return Err(anyhow::anyhow!(QueryError::MalformedNode(
                    "qualified_expr".into(),
                    rhs_node.range()
                )));
            }

            let name_node = rhs_node.named_child(0).unwrap();

            let mut arguments = Vec::new();
            for i in 1..rhs_node.named_child_count() {
                let argument_node = rhs_node.named_child(i).unwrap();
                let argument = parse_expr(argument_node, source)?;
                arguments.push(argument);
            }

            let pred_name = name_node.utf8_text(source)?;
            Expr_::QualifiedAccess {
                base: Box::new(expr),
                method_name: pred_name.into(),
                operands: arguments,
            }
        }
        _ => {
            panic!("Unsupported expression: {}", node.kind())
        }
    };

    Ok(Expr {
        expr: inner_expr,
        type_: Untyped,
    })
}

fn expect_node_kind(node: tree_sitter::Node, kind: &str) -> anyhow::Result<()> {
    if node.kind() != kind {
        return Err(anyhow::anyhow!(QueryError::UnexpectedNodeType(
            node.kind().into(),
            kind.into(),
            node.range()
        )));
    }

    Ok(())
}

fn parse_var_name<'a>(node: tree_sitter::Node<'a>, source: &'a [u8]) -> anyhow::Result<String> {
    expect_node_kind(node, "varName")?;

    let simple_id = single_child(node, "simpleId")?;
    let s = simple_id.utf8_text(source)?;
    Ok(s.into())
}

fn parse_as_expr<'a>(
    node: tree_sitter::Node<'a>,
    source: &'a [u8],
) -> anyhow::Result<AsExpr<Syntax>> {
    expect_node_kind(node, "asExpr")?;

    if node.child_count() == 1 {
        let e = parse_expr(node.child(0).unwrap(), source)?;
        return Ok(AsExpr {
            expr: e,
            ident: None,
        });
    }

    if node.child_count() == 3 {
        let e = parse_expr(node.child(0).unwrap(), source)?;
        let var_name = parse_var_name(node.child(2).unwrap(), source)?;
        return Ok(AsExpr {
            expr: e,
            ident: Some(var_name),
        });
    }

    unimplemented!()
}

/// Parse an expression like `EXPR as FOO`
///
/// Note that the `as` variants are actually not yet supported
fn parse_as_exprs<'a>(
    node: tree_sitter::Node<'a>,
    source: &'a [u8],
) -> anyhow::Result<Vec<AsExpr<Syntax>>> {
    expect_node_kind(node, "asExprs")?;

    let mut res = Vec::new();
    let mut cur = node.walk();
    // Note that we have to use the named children here because the commas
    // between elements are represented in the concrete syntax tree. We want to
    // ignore them - luckily they happen to be represented as "non-named" nodes.
    for child in node.named_children(&mut cur) {
        let as_expr = parse_as_expr(child, source)?;
        res.push(as_expr);
    }

    Ok(res)
}

fn parse_type_expr<'a>(node: tree_sitter::Node<'a>, source: &'a [u8]) -> anyhow::Result<Type> {
    expect_node_kind(node, "typeExpr")?;
    let class_name = single_child(node, "className")?;
    let s = class_name.utf8_text(source)?;
    Type::from_str(s)
        .map_err(|_e| anyhow::anyhow!(QueryError::UnsupportedType(s.into(), node.range())))
}

fn parse_var_decl<'a>(node: tree_sitter::Node<'a>, source: &'a [u8]) -> anyhow::Result<VarDecl> {
    expect_node_kind(node, "varDecl")?;
    if node.named_child_count() != 2 {
        return Err(anyhow::anyhow!(QueryError::MalformedNode(
            "varDecl".into(),
            node.range()
        )));
    }

    let ty = parse_type_expr(node.named_child(0).unwrap(), source)?;
    let var_name = parse_var_name(node.named_child(1).unwrap(), source)?;
    let decl = VarDecl {
        name: var_name,
        type_: ty,
    };
    Ok(decl)
}

/// Parse the concrete Tree Sitter QL syntax tree into a more abstract syntax tree
///
/// This parser uses simple recursive descent, as Tree Sitter has already
/// resolved all of the interesting precedence parsing challenges.
pub fn parse_query_ast(
    ast: &tree_sitter::Tree,
    source: impl AsRef<[u8]>,
) -> anyhow::Result<Select<Syntax>> {
    let root = ast.root_node();
    if root.has_error() {
        error!("Error parsing query:\n{}", ast.root_node().to_sexp());
        return Err(anyhow::anyhow!(QueryError::QueryParseError));
    }
    let module_member = get_child_of_kind(root, "moduleMember")?;
    let sel = get_child_of_kind(module_member, "select")?;

    let mut selected_exprs = Vec::new();
    let mut declared_vars = Vec::new();
    // The default filter accepts all selected values
    let mut filter = Expr {
        expr: Expr_::ConstantExpr(Constant::Boolean(true)),
        type_: Untyped,
    };

    let mut cursor = sel.walk();
    for child in sel.named_children(&mut cursor) {
        match child.kind() {
            "asExprs" => {
                // This determines all of the expressions
                let mut selections = parse_as_exprs(child, source.as_ref())?;
                selected_exprs.append(&mut selections);
            }
            "varDecl" => {
                let decl = parse_var_decl(child, source.as_ref())?;
                declared_vars.push(decl);
            }
            // FIXME: Ensure that there are no others required to handle
            // decls/select, but just have everything else be a where clause
            "comp_term" => {
                let e = parse_expr(child, source.as_ref())?;
                filter = e;
            }
            "qualified_expr" => {
                let e = parse_expr(child, source.as_ref())?;
                filter = e;
            }
            "conjunction" => {
                let e = parse_expr(child, source.as_ref())?;
                filter = e;
            }
            "disjunction" => {
                let e = parse_expr(child, source.as_ref())?;
                filter = e;
            }
            _ => {
                return Err(anyhow::anyhow!(QueryError::InvalidSelect(sel.range(), 0)));
            }
        }
    }

    let res = Select {
        select_exprs: selected_exprs,
        where_formula: filter,
        var_decls: declared_vars,
    };

    Ok(res)
}
