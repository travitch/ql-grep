use tree_sitter;

use crate::query::ir::{AsExpr, Expr, Type, VarDecl, Select, CompOp, QLValue, AggregateOp};
use crate::query::error::QueryError;

/// Expect a single child node with any type
fn any_single_child<'a>(node : tree_sitter::Node<'a>) -> anyhow::Result<tree_sitter::Node<'a>> {
    if node.named_child_count() != 1 {
        return Err(anyhow::anyhow!(QueryError::MissingExpectedChildNode(node.range(), "$ANY".into())));
    }

    // Size checked already; the separate check is also to ensure that there is
    // exactly one child
    let child = node.named_child(0).unwrap();
    return Ok(child);
}

/// Expect a single child node with the given node kind
fn single_child<'a>(node : tree_sitter::Node<'a>, expected_kind : &'static str) -> anyhow::Result<tree_sitter::Node<'a>> {
    let child = any_single_child(node)?;

    if child.kind() == expected_kind {
        return Ok(child);
    }

    return Err(anyhow::anyhow!(QueryError::MissingExpectedChildNode(node.range(), expected_kind.into())));
}

fn parse_literal<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<QLValue> {
    match node.kind() {
        "integer" => {
            let txt = node.utf8_text(source)?;
            let num : i32 = txt.parse()?;
            return Ok(QLValue::QLInteger(num));
        },
        _ => {
            unimplemented!()
        }
    }
}

fn parse_comp_op<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<CompOp> {
    expect_node_kind(node, "compop")?;

    let op_str = node.utf8_text(source)?;

    match op_str {
        "<" => Ok(CompOp::LT),
        "<=" => Ok(CompOp::LE),
        ">" => Ok(CompOp::GT),
        ">=" => Ok(CompOp::GE),
        "=" => Ok(CompOp::EQ),
        "!=" => Ok(CompOp::NE),
        _ => Err(anyhow::anyhow!(QueryError::InvalidComparisonOp(op_str.into(), node.range())))
    }
}

fn parse_aggregate_op<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<AggregateOp> {
    expect_node_kind(node, "aggId")?;
    match node.utf8_text(source)? {
        "count" => Ok(AggregateOp::Count),
        o => Err(anyhow::anyhow!(QueryError::InvalidAggregateOp(o.into(), node.range())))
    }
}

/// Parse a QL expression from the given node
fn parse_expr<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<Expr> {
    match node.kind() {
        "literal" => {
            let child = any_single_child(node)?;
            let val = parse_literal(child, source)?;
            return Ok(Expr::ValueExpr(val));
        },
        "variable" => {
            let var_name_node = single_child(node, "varName")?;
            let var_name = parse_var_name(var_name_node, source)?;
            return Ok(Expr::VarRef(var_name));
        },
        "comp_term" => {
            // EXPR COMP_OP EXPR
            if node.named_child_count() != 3 {
                return Err(anyhow::anyhow!(QueryError::MalformedNode("comp_term".into(), node.range())));
            }

            let lhs = parse_expr(node.child(0).unwrap(), source)?;
            let op = parse_comp_op(node.child(1).unwrap(), source)?;
            let rhs = parse_expr(node.child(2).unwrap(), source)?;
            return Ok(Expr::Comparison(Box::new(lhs), op, Box::new(rhs)));
        },
        "aggregate" => {
            // aggId expr_aggregate_body
            if node.named_child_count() != 2 {
                return Err(anyhow::anyhow!(QueryError::MalformedNode("aggregate".into(), node.range())));
            }

            let op_node = node.named_child(0).unwrap();
            let op = parse_aggregate_op(op_node, source)?;
            let agg_body = node.named_child(1).unwrap();
            expect_node_kind(agg_body, "expr_aggregate_body")?;
            let as_exprs_node = single_child(agg_body, "asExprs")?;
            let exprs = parse_as_exprs(as_exprs_node, source)?;
            return Ok(Expr::Aggregate(op, exprs));
        },
        "qualified_expr" => {
            if node.named_child_count() != 2 {
                return Err(anyhow::anyhow!(QueryError::MalformedNode("qualified_expr".into(), node.range())));
            }
            let expr = parse_expr(node.named_child(0).unwrap(), source)?;
            let rhs_node = node.named_child(1).unwrap();
            expect_node_kind(rhs_node, "qualifiedRhs")?;
            let name_node = single_child(rhs_node, "predicateName")?;
            let pred_name = name_node.utf8_text(source)?;
            return Ok(Expr::QualifiedAccess(Box::new(expr), pred_name.into()));
        },
        _ => {
            panic!("Unsupported expression: {}", node.kind())
        }
    }
}

fn expect_node_kind(node : tree_sitter::Node, kind : &str) -> anyhow::Result<()> {
    if node.kind() != kind {
        return Err(anyhow::anyhow!(QueryError::UnexpectedNodeType(node.kind().into(), kind.into(), node.range())));
    }

    Ok(())
}

fn parse_var_name<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<String> {
    expect_node_kind(node, "varName")?;

    let simple_id = single_child(node, "simpleId")?;
    let s = simple_id.utf8_text(source)?;
    Ok(s.into())
}

fn parse_as_expr<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<AsExpr> {
    expect_node_kind(node, "asExpr")?;

    if node.child_count() == 1 {
        let e = parse_expr(node.child(0).unwrap(), source)?;
        return Ok(AsExpr {
            expr: e,
            ident: None
        });
    }

    if node.child_count() == 3 {
        let e = parse_expr(node.child(0).unwrap(), source)?;
        let var_name = parse_var_name(node.child(2).unwrap(), source)?;
        return Ok(AsExpr {
            expr: e,
            ident: Some(var_name)
        });
    }

    unimplemented!()
}

/// Parse an expression like `EXPR as FOO`
///
/// Note that the `as` variants are actually not yet supported
fn parse_as_exprs<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<Vec<AsExpr>> {
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

fn parse_type_expr<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<Type> {
    expect_node_kind(node, "typeExpr")?;
    let class_name = single_child(node, "className")?;
    let s = class_name.utf8_text(source)?;
    match s {
        "Function" => Ok(Type::Function),
        "Method" => Ok(Type::Method),
        "Class" => Ok(Type::Class),
        "Parameter" => Ok(Type::Parameter),
        "Field" => Ok(Type::Field),
        _ => Err(anyhow::anyhow!(QueryError::UnsupportedType(s.into(), node.range())))
    }
}

fn parse_var_decl<'a>(node : tree_sitter::Node<'a>, source : &'a [u8]) -> anyhow::Result<VarDecl> {
    expect_node_kind(node, "varDecl")?;
    if node.named_child_count() != 2 {
        return Err(anyhow::anyhow!(QueryError::MalformedNode("varDecl".into(), node.range())));
    }

    let ty = parse_type_expr(node.named_child(0).unwrap(), source)?;
    let var_name = parse_var_name(node.named_child(1).unwrap(), source)?;
    let decl = VarDecl {
        type_: ty,
        name: var_name
    };
    Ok(decl)
}

/// Parse the concrete Tree Sitter QL syntax tree into a more abstract syntax tree
///
/// This parser uses simple recursive descent, as Tree Sitter has already
/// resolved all of the interesting precedence parsing challenges.
pub fn parse_query_ast(ast : &tree_sitter::Tree, source : impl AsRef<[u8]>) -> anyhow::Result<Select> {
    println!("Query: \n{:?}", ast.root_node().to_sexp());
    let root = ast.root_node();
    let module_member = single_child(root, "moduleMember")?;
    let sel = single_child(module_member, "select")?;

    let mut selected_exprs = Vec::new();
    let mut declared_vars = Vec::new();
    let mut filter = None;

    println!("# children = {}, #named children={}", sel.child_count(), sel.named_child_count());
    let mut cursor = sel.walk();
    for child in sel.named_children(&mut cursor) {
        println!("child: {:?}", child.to_sexp());
        match child.kind() {
            "asExprs" => {
                // This determines all of the expressions
                let mut selections = parse_as_exprs(child, source.as_ref())?;
                selected_exprs.append(&mut selections);
            },
            "varDecl" => {
                let decl = parse_var_decl(child, source.as_ref())?;
                declared_vars.push(decl);
            },
            "comp_term" => {
                let e = parse_expr(child, source.as_ref())?;
                filter = Some(e);
            },
            _ => {
                return Err(anyhow::anyhow!(QueryError::InvalidSelect(sel.range(), 0)));
            }
        }
    }

    let res = Select {
        select_exprs: selected_exprs,
        where_formula: filter,
        var_decls: declared_vars
    };

    return Ok(res);
}
